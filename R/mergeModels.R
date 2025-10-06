# experimental feature: merge two CompartmentModels

#' Merge two CompartmentModel objects into one.
#'
#' @param M1,M2 CompartmentModel objects
#' @param suffix1,suffix2 Optional character suffixes for compartment names in M1/M2.
#'        Use NULL to leave names unchanged.
#' @param collision What to do in case of name collisions:
#'        "error" (default), "auto" (rename with suffixes 1/2), or "merge"
#'        (merge compartments). Suffixing is done before checking for collisions
#'        and potentially merging.
#' @return A new CompartmentModel containing both.
#' @examples
#' # Example 1: Merging oral absorption and one-compartment PK
#' abs <- CompartmentModel$new()
#' abs$addCompartment("Gut", 100)
#' abs$addReaction("Gut", "Central", "ka * Gut")
#' pk <- CompartmentModel$new()
#' pk$addCompartment("Central", 0)
#' pk$addReaction("Central", "", "k10 * Central")
#' M <- mergeModels(abs, pk, collision = "merge")
#'
#' # Example 2: Two-drug PK model with DDI
#' drugA <- twoCompModel()
#' drugB <- twoCompModel()
#' merged <- mergeModels(drugA, drugB, suffix1 = "A", suffix2 = "B")
#' merged$addReaction(
#'   from = "Central_B",
#'   to = "Central_A",
#'   rate = "-(Central_B / (IC50 + Central_B)) * k10 * Central_A"
#' )
#' @export
mergeModels <- function(M1, M2, suffix1 = NULL, suffix2 = NULL, collision = c("error","auto","merge")) {
    collision <- match.arg(collision)
    merged <- CompartmentModel$new()

    renameCompartments <- function(model, suffix) {
        if (is.null(suffix)) return(model)
        model_copy <- CompartmentModel$new()
        for (c in model$compartments) {
            model_copy$addCompartment(paste0(c$name, suffix), c$initial)
        }
        for (r in model$reactions) {
            from <- if (nzchar(r$from)) paste0(r$from, suffix) else ""
            to   <- if (nzchar(r$to))   paste0(r$to, suffix)   else ""
            model_copy$addReaction(from, to, deparse(r$rate))
        }
        for (o in model$observables) {
            model_copy$addObservable(paste0(o$name, suffix), deparse(o$expr))
        }
        for (d in model$doses) {
            dose_copy <- Dosing$new(
                target = paste0(d$target, suffix),
                amount = d$amount,
                time   = d$time,
                rate   = d$rate,
                duration = d$duration
            )
            model_copy$addDosing(dose_copy)
        }
        model_copy
    }

    # renaming logic (with or without leading "_")
    if (!is.null(suffix1) && !startsWith(suffix1,"_")) suffix1 <- paste0("_",suffix1)
    if (!is.null(suffix2) && !startsWith(suffix2,"_")) suffix2 <- paste0("_",suffix2)

    M1r <- renameCompartments(M1, suffix1)
    M2r <- renameCompartments(M2, suffix2)

    # collision handling (currently simplified)
    n1 <- M1r$getStateNames()
    n2 <- M2r$getStateNames()
    if (length(intersect(n1, n2)) > 0) {
        switch(collision,
               error = stop("Name collision in merged models: ",
                            paste(intersect(n1, n2), collapse=", ")),
               auto = {
                   M1r <- renameCompartments(M1r, suffix = "_1")
                   M2r <- renameCompartments(M2r, suffix = "_2")
               }
        )
    }

    # copy everything into merged
    for (c in M1r$compartments) merged$addCompartment(c$name, c$initial)
    for (c in M2r$compartments) {
        if (collision == "merge" && !is.na(idx <- match(c$name, n1))) {
            merged$compartments[[idx]]$initial <- merged$compartments[[idx]]$initial + c$initial
        } else {
            merged$addCompartment(c$name, c$initial)
        }
    }
    for (r in M1r$reactions) merged$addReaction(r$from, r$to, deparse(r$rate))
    for (r in M2r$reactions) merged$addReaction(r$from, r$to, deparse(r$rate))
    for (o in M1r$observables) merged$addObservable(o$name, deparse(o$expr))
    for (o in M2r$observables) merged$addObservable(o$name, deparse(o$expr))
    for (d in M1r$doses) merged$addDosing(d)
    for (d in M2r$doses) merged$addDosing(d)

    merged
}
