test_that("multiCompModel creates correct number of compartments", {
    M <- multiCompModel(3, "micro")
    expect_equal(names(M$compartments), c("C1", "C2", "C3"))
    expect_true(names(M$observables) == "C1Conc")
})

test_that("multiCompModel micro parametrization flows are correct", {
    M <- multiCompModel(2, "micro")
    flow_strings <- paste(M$flows$from, M$flows$to, vapply(M$flows$rate, deparse, character(1)))
    # Elimination flow
    expect_true(any(grepl("C1 NA k10 \\* C1", flow_strings)))
    # Inter-compartment flows
    expect_true(any(grepl("C1 C2 k12 \\* C1", flow_strings)))
    expect_true(any(grepl("C2 C1 k21 \\* C2", flow_strings)))
})

test_that("multiCompModel macro parametrization flows are correct", {
    M <- multiCompModel(2, "macro")
    flow_strings <- paste(M$flows$from, M$flows$to, vapply(M$flows$rate, deparse, character(1)))
    # Elimination scaled by volume
    expect_true(any(grepl("C1 NA CL/V1 \\* C1", flow_strings)))
    # Inter-compartment flows
    expect_true(any(grepl("C1 C2 Q12/V1 \\* C1", flow_strings)))
    expect_true(any(grepl("C2 C1 Q12/V2 \\* C2", flow_strings)))
})

test_that("12-CMT well-stirred PBPK model behaves as expected under long-term infusion", {

    dur <- 1000
    M <- sMD_PBPK_12CMT_wellstirred() |>
        add_dosing(target = "ven", time = 0, amount = 1, duration = dur) |>   # long-term infusion to test steady-state behaviour
        add_observable(Cpla = BP * ven / Vven)

    paramValues <- list(
        BP = 1,
        CL = 5,
        Kadi = 1,
        Kbon = 2,
        Kgut = 3,
        Khea = 4,
        Kkid = 5,
        Kliv = 6,
        Klun = 7,
        Kmus = 8,
        Kski = 9,
        Kspl = 10,
        Qadi = 0.5,
        Qbon = 0.5,
        Qgut = 0.5,
        Qhea = 0.5,
        Qkid = 0.5,
        Qliv = 1,
        Qmus = 0.5,
        Qski = 0.5,
        Qspl = 0.5,
        Vadi = 1,
        Vart = 1,
        Vbon = 1,
        Vgut = 1,
        Vhea = 1,
        Vkid = 1,
        Vliv = 1,
        Vlun = 1,
        Vmus = 1,
        Vski = 1,
        Vspl = 1,
        Vven = 1
    )
    odeinfo <- to_ode(M, paramValues)
    times <- c(0, dur)
    out <- deSolve::ode(
        y = odeinfo$y0,
        times = times,
        func = odeinfo$odefun,
        parms = paramValues,
        events = odeinfo$events
    )
    out_df <- as.data.frame(out)

    # Terminal concentration relationships close to partition coefficients
    tissues <- c("adi", "bon", "gut", "hea", "kid", "liv", "lun", "mus", "ski", "spl")
    eK_sim <- (out_df[2,tissues]/out_df[2,"ven"]) |> 
        as.numeric()
    eK_ref <- with(paramValues, c(Kadi, Kbon, Kgut, Khea, Kkid, Kliv*Qliv/(CL+Qliv), Klun, Kmus, Kski, Kspl))
    expect_equal(eK_sim, eK_ref, tolerance = 0.001)
})


test_that("Permeation-based model reduces to well-stirred in the fast permeability limit", {
    
    dose <- dosing(target = "ven", amount = 1, time = 0)
    times <- 0:24

    Mws <- sMD_PBPK_12CMT_wellstirred() |>
        add_dosing(dose = dose)
    Mpb <- sMD_PBPK_12CMT_permbased() |>
        add_dosing(dose = dose)

    paramValues_ws <- list(
        BP = 1,
        CL = 5,
        Kadi = 1,
        Kbon = 1,
        Kgut = 1,
        Khea = 1,
        Kkid = 1,
        Kliv = 1,
        Klun = 1,
        Kmus = 1,
        Kski = 1,
        Kspl = 1,
        Qadi = 0.5,
        Qbon = 0.5,
        Qgut = 0.5,
        Qhea = 0.5,
        Qkid = 0.5,
        Qliv = 1,
        Qmus = 0.5,
        Qski = 0.5,
        Qspl = 0.5,
        Vadi = 1,
        Vart = 1,
        Vbon = 1,
        Vgut = 1,
        Vhea = 1,
        Vkid = 1,
        Vliv = 1,
        Vlun = 1,
        Vmus = 1,
        Vski = 1,
        Vspl = 1,
        Vven = 1
    )

    paramValues_pb <- list(
        BP = 1,
        CL = 5,
        P = 1e6, # very high permeability to approximate well-stirred limit
        fuP = 1,
        fuCadi = 1,
        fuCbon = 1,
        fuCgut = 1,
        fuChea = 1,
        fuCkid = 1,
        fuCliv = 1,
        fuClun = 1,
        fuCmus = 1,
        fuCski = 1,
        fuCspl = 1,
        fuEadi = 1,
        fuEbon = 1,
        fuEgut = 1,
        fuEhea = 1,
        fuEkid = 1,
        fuEliv = 1,
        fuElun = 1,
        fuEmus = 1,
        fuEski = 1,
        fuEspl = 1,
        Qadi = 0.5,
        Qbon = 0.5,
        Qgut = 0.5,
        Qhea = 0.5,
        Qkid = 0.5,
        Qliv = 1,
        Qmus = 0.5,
        Qski = 0.5,
        Qspl = 0.5,
        SAadi = 1,
        SAbon = 1,
        SAgut = 1,
        SAhea = 1,
        SAmus = 1,
        SAkid = 1,
        SAliv = 1,
        SAlun = 1,
        SAski = 1,
        SAspl = 1,
        Vadi_exc = 0.5,
        Vadi_cel = 0.5,
        Vbon_exc = 0.5,
        Vbon_cel = 0.5,
        Vgut_exc = 0.5,
        Vgut_cel = 0.5,
        Vhea_exc = 0.5,
        Vhea_cel = 0.5,
        Vmus_exc = 0.5,
        Vmus_cel = 0.5,
        Vkid_exc = 0.5,
        Vkid_cel = 0.5,
        Vliv_exc = 0.5,
        Vliv_cel = 0.5,
        Vlun_exc = 0.5,
        Vlun_cel = 0.5,
        Vmus_exc = 0.5,
        Vmus_cel = 0.5,
        Vski_exc = 0.5,
        Vski_cel = 0.5,
        Vspl_exc = 0.5,
        Vspl_cel = 0.5,
        Vart = 1,
        Vven = 1
    )

    odeinfo_ws <- to_ode(Mws, paramValues_ws)
    odeinfo_pb <- to_ode(Mpb, paramValues_pb)
    out_ws <- deSolve::ode(
        y = odeinfo_ws$y0,
        times = times,
        func = odeinfo_ws$odefun,
        parms = paramValues_ws,
        events = odeinfo_ws$events
    )

    out_pb <- deSolve::ode(
        y = odeinfo_pb$y0,
        times = times,
        func = odeinfo_pb$odefun,
        parms = paramValues_pb,
        events = odeinfo_pb$events
    )

    # Compare amounts in tissues at all time points - should be very close in the fast permeability limit
    tissue_comps <- c("adi", "bon", "gut", "hea", "kid", "liv", "lun", "mus", "ski", "spl")
    for (tissue in tissue_comps) {
        comp_pb <- paste0(tissue, c("_exc", "_cel"))
        expect_equal(out_ws[,tissue], rowSums(out_pb[,comp_pb]), tolerance = 0.0001)
    }

    # Compare amounts in blood compartments
    expect_equal(out_ws[,"ven"], out_pb[,"ven"], tolerance = 0.0001)
    expect_equal(out_ws[,"art"], out_pb[,"art"], tolerance = 0.0001)

})