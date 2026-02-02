# Fixtures for testing symbolic lumping

# Two compartment model (`"blo"` and `"tis"`) for testing, with clearance from tissue.
test_2CMT <- function() {

    M <- CompartmentModel$new()

    M$addCompartment("blo", 10)
    M$addCompartment("tis", 0)

    M$addReaction("blo","tis","Q*blo/Vblo")
    M$addReaction("tis","blo","Q*tis/(Vtis*Ktis)")
    M$addReaction("tis","","CL*tis/(Vtis*Ktis)")

    M
}

test_scc_part <- function() {
    M <- CompartmentModel$new()

    M$addCompartment("A", 0)
    M$addCompartment("B", 0)
    M$addCompartment("C", 0)
    M$addCompartment("D", 0)

    M$addReaction("A","B","kAB*A")
    M$addReaction("B","C","kBC*B")
    M$addReaction("C","B","kCB*C")
    M$addReaction("C","D","kCD*C")
    M$addReaction("D","","kD0*D")

    M
}


#' Miminal model for testing first-pass effect issues.
test_firstpass_model <- function() {

    M <- CompartmentModel$new()

    M$addCompartment("gut", 10)
    M$addCompartment("liv", 0)
    M$addCompartment("blo", 0)

    M$addReaction("blo","liv","Q*blo/Vblo")
    M$addReaction("liv","blo","Q*liv/(Vliv*Kliv)")
    M$addReaction("liv","",   "CL*liv/(Vliv*Kliv)")
    M$addReaction("gut","liv","ka*gut")

    M
}

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


    M <- multiCompModel(ncomp = 3, type = "micro")
    M$addDosing(Dosing$new(target = "C1", time = 0, amount = 10))

    L <- symbolic_lumping(M, partitioning = list(c("C2","C3")))


})