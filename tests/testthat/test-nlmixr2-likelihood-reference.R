test_that("mixed endpoint sampling likelihood restores only lognormal response Jacobians", {
    skip_if_not_installed("nlmixr2est")
    skip_on_cran()
    base <- interop_fixture()$export$problem
    model <- add_observable(base$model, D = exp(CL/10))
    studies <- c(first=experiment(observations=observation_data(time=c(1,2,4),
        observable=c("C","D","C"),value=c(9,2.8,11))),
        second=experiment(observations=observation_data(time=c(1,2,4),
        observable=c("C","D","C"),value=c(8.5,2.5,10))))
    problem <- estimation_problem(model,studies,
        c(base$parameters,sdlog=parameter_spec(.2)),
        statistical_model(CL=normal(mean="mu",sd="omega"),C=normal(sd="sigma"),
                          D=lognormal(sdlog="sdlog")))
    exported <- to_nlmixr2(problem)
    native <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"impmap",
        control=nlmixr2est::impmapControl(nIter=0,isample=1000L,print=0,covMethod="")))
    reference <- sum(vapply(list(c(9,2.8,11),c(8.5,2.5,10)),function(y) {
        transformed <- c(y[1],log(y[2]),y[3])
        loading <- c(1,.1,1)
        covariance <- outer(loading,loading)+diag(c(4,.04,4))
        residual <- transformed-10*loading
        as.numeric((3*log(2*pi)+determinant(covariance,logarithm=TRUE)$modulus+
            t(residual)%*%solve(covariance,residual))/2)+log(y[2])
    },numeric(1)))
    expect_equal(.nlmixr2_sampling_nll(native,exported),reference,tolerance=1e-4)
})

test_that("proportional normal sampling likelihood agrees with independent integration", {
    skip_if_not_installed("nlmixr2est")
    skip_on_cran()
    base <- interop_fixture()$export$problem
    specs <- base$parameters
    specs$sigma <- parameter_spec(.2)
    problem <- estimation_problem(base$model,base$experiments,specs,
        statistical_model(CL=normal(mean="mu",sd="omega"),C=normal(sd=proportional("sigma"))))
    exported <- to_nlmixr2(problem)
    native <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"impmap",
        control=nlmixr2est::impmapControl(nIter=0,isample=3000L,impSeed=42,print=0,covMethod="")))
    reference <- sum(vapply(list(c(9,10.5,11),c(8.5,9.5,10)),function(y) {
        density <- Vectorize(function(eta) {
            exp(sum(dnorm(y,10+eta,.2*abs(10+eta),log=TRUE))+dnorm(eta,log=TRUE))
        })
        -log(integrate(density,-Inf,Inf,rel.tol=1e-9)$value)
    },numeric(1)))
    expect_equal(.nlmixr2_sampling_nll(native,exported),reference,tolerance=.005)
})

test_that("unit-aware sampling likelihood matches the analytic density in supplied units", {
    skip_if_not_installed("nlmixr2est")
    skip_on_cran()
    model <- compartment_model() |>
        add_compartment(name="Central",volume=1 [L]) |>
        add_molecule(name="drug",cmt="Central",initial=100 [mg],type="amount") |>
        add_transport(from="Central",to=NULL,molec="drug",rate="CL * c[drug, Central]") |>
        add_observable(C=CL)
    u <- units::set_units(1,"L/h",mode="standard")
    studies <- c(first=experiment(observations=observation_data(
        time=with_units(c(1,2,4) [h]),observable="C",value=c(9,10.5,11)*u)),
        second=experiment(observations=observation_data(
        time=with_units(c(1,2,4) [h]),observable="C",value=c(8.5,9.5,10)*u)))
    problem <- estimation_problem(model,studies,
        c(mu=parameter_spec(10 [L/h]),omega=parameter_spec(1 [L/h]),sigma=parameter_spec(2 [L/h])),
        statistical_model(CL=normal(mean="mu",sd="omega"),C=normal(sd="sigma")))
    exported <- to_nlmixr2(problem,dimensions=list(time="min",length="dm",mass="mg"))
    native <- suppressMessages(nlmixr2est::nlmixr2(exported$model,exported$data,"focei",
        control=nlmixr2est::foceiControl(maxOuterIterations=0,print=0,covMethod="")))
    result <- suppressMessages(from_nlmixr2(native,exported,likelihood="impmap",
        likelihood_control=list(isample=1000L)))
    expect_equal(result$neg_log_lik,independent_interop_nll("normal"),tolerance=1e-3)
    expect_equal(exported$log_likelihood_unit_jacobian,6*log(60),tolerance=1e-10)
    expect_equal(as.numeric(coef(result)$mu),10,tolerance=1e-6)
    expect_true(inherits(coef(result)$mu,"units"))
})
