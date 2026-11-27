#' @examples
#' \donttest{
#' if (requireNamespace("nlmixr2est", quietly = TRUE)) {
#'     # One-compartment model with first-order elimination
#'     model <- compartment_model() |>
#'         add_compartment(name = "Central", volume = 1) |>
#'         add_molecule(name = "drug", cmt = "Central", initial = 100, type = "amount") |>
#'         add_transport(from = "Central", to = NULL, molec = "drug",
#'                       rate = "CL * c[drug, Central]") |>
#'         add_observable(C = c[drug, Central])
#'
#'     # Two individuals with observed concentrations
#'     studies <- c(
#'         first = experiment(observations = observation_data(
#'             time = c(1, 2, 4), observable = "C", value = c(82, 67, 45))),
#'         second = experiment(observations = observation_data(
#'             time = c(1, 2, 4), observable = "C", value = c(78, 61, 37)))
#'     )
#'
#'     # Log-transformed parameters without finite bounds, as required by SAEM
#'     problem <- estimation_problem(
#'         model, studies,
#'         c(CL_pop = parameter_spec(0.2, transform = "log"),
#'           omega_CL = parameter_spec(0.2, transform = "log"),
#'           sigma = parameter_spec(2, transform = "log")),
#'         statistical_model(
#'             CL = lognormal(median = "CL_pop", sdlog = "omega_CL"),
#'             C = normal(sd = "sigma")
#'         )
#'     )
#'
#'     # Export, fit natively with nlmixr2est (short run), and import the result
#'     exported <- to_nlmixr2(problem, method = "saem")
#'     control <- utils::modifyList(exported$control,
#'         list(seed = 99L, nBurn = 30L, nEm = 50L, print = 0L, covMethod = ""))
#'     fit <- nlmixr2est::nlmixr2(exported$model, exported$data, "saem",
#'         control = do.call(nlmixr2est::saemControl, control))
#'     result <- from_nlmixr2(fit, exported) # likelihood is NA for SAEM
#'     coef(result)
#'
#'     # Request an explicit likelihood evaluation by importance sampling
#'     sampled <- from_nlmixr2(fit, exported, likelihood = "impmap")
#'     sampled$neg_log_lik
#'
#'     # Equivalently, use the estimation wrapper
#'     result <- estimate(problem, nlmixr2_engine("saem", likelihood = "impmap",
#'         control = list(seed = 99L, nBurn = 30L, nEm = 50L, print = 0L, covMethod = "")))
#' }
#' }
