test_that("lumping is exact in a simple toy model", {

    M <- CompartmentModel$new()

    M$addCompartment("art", 0)
   # M$addCompartment("gut", 0)
   # M$addCompartment("mus", 0)
    M$addCompartment("liv", 0)
    M$addCompartment("lun", 0)
    M$addCompartment("ven", 0)

    # organs with arterial inflow
   # M$addReaction("art", "gut", "Qgut * art / Vart")
   # M$addReaction("art", "mus", "Qmus * art / Vart")

    # handle organ topology w.r.t. liver
    M$addReaction("art", "liv", "(Qliv-Qgut) * art / Vart")
    # M$addReaction("art", "liv", "(Qliv-Qgut) * art / Vart")
    #    M$addReaction("gut", "liv", "Qgut * gut / (Vgut * Kgut)")

    # organs with venous outflow
    M$addReaction("liv", "ven", "Qliv * liv / (Vliv * Kliv)")
   # M$addReaction("mus", "ven", "Qmus * mus / (Vmus * Kmus)")

    # lung
    M$addReaction("ven", "lun", "co * ven / Vven")
    M$addReaction("lun", "art", "co * lun / (Vlun * Klun)")

    # liver metabolism
   # M$addReaction("liv", "", "CL * liv / (Vliv * Kliv)")

    L <- lump_model(M, partitioning = list(c("art", "lun", "ven")))

#    print(vapply(L$compartments, `[[`, "", "name"))

    L$reactions[[1]]$from  # "art+lun+ven"
    L$reactions[[1]]$to    # "liv"

    skip("Lumping scheme not fully finished yet, skipping.")

})
