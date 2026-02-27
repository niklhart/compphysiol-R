# Fixtures for testing symbolic lumping


# Lumped well-stirred model
sMD_PBPK_xCMT_lumped <- function(partitioning) {
    M <- sMD_PBPK_12CMT_wellstirred()
    # General lumping logic
    L <- lump_model(
        M, 
        partitioning = partitioning,
        normalize = list(
            ven = "Vven",
            art = "Vart",
            adi = "Vadi*Kadi",
            bon = "Vbon*Kbon",
            gut = "Vgut*Kgut",
            hea = "Vhea*Khea",
            mus = "Vmus*Kmus",
            kid = "Vkid*Kkid",
            liv = "Vliv*Kliv*Q/(Q+CL)",
            lun = "Vlun*Klun",
            ski = "Vski*Kski",
            spl = "Vspl*Kspl"
        )
    )
    # Specific for first-pass effect

}


test_that("symbolic lumping works (experimental)", {

    skip("Symbolic lumping is not functional yet")


    M <- multiCompModel(ncomp = 3, type = "micro") |>
        add_dosing(target = "C1", time = 0, amount = 10)

    L <- symbolic_lumping(M, partitioning = list(c("C2","C3")))


})