
#' Merge two CompartmentModel objects into one.
#'
#' @param M1,M2 CompartmentModel objects
#' @param suffix1,suffix2 Optional character suffixes for compartment names in M1/M2.
#'        Use NULL to leave names unchanged.
#' @param collision What to do in case of name collisions:
#'        "error" (default), "auto" (rename with suffixes 1/2), or "merge"
#'        (merge compartments, adding initial amounts).
#'        Since suffixing is done before checking for collisions, the
#'        `collisions` argument has no effect if distinct non-`NULL` suffixes are used.
#' @param shared An optional character vector or parameter or compartment names
#'        that should be shared, i.e. they are not suffixed.
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
#' M
#'
#' # Example 2: Two-drug PK model with drug-drug interaction
#' drugA <- multiCompModel(ncomp = 1)
#' drugB <- multiCompModel(ncomp = 2)
#' merged <- mergeModels(drugA, drugB, suffix1 = "A", suffix2 = "B")
#' merged$addReaction(
#'   from = "C1_B",
#'   to = "C1_A",
#'   rate = "-(C1_B / (IC50 + C1_B)) * k10_A * C1_A"
#' )
#' merged
#' @export
mergeModels <- function(M1, M2, suffix1 = NULL, suffix2 = NULL,
                        collision = c("error","auto","merge"),
                        shared = character()) {
    collision <- match.arg(collision)
    merged <- CompartmentModel$new()

    renameCompartments <- function(model, suffix, skip = character()) {
        if (is.null(suffix)) return(model)
        oldStateNames <- model$getStateNames()
        newStateNames <- paste0(oldStateNames,suffix)
        model_copy <- CompartmentModel$new()
        for (c in model$compartments) {
            model_copy$addCompartment(paste0(c$name, suffix), c$initial)
        }
        for (r in model$reactions) {
            from <- if (!is.null(r$from)) paste0(r$from, suffix)
            to   <- if (!is.null(r$to))   paste0(r$to, suffix)

            rate <- .suffix_symbols(r$rate, suffix = suffix, skip = skip)
            model_copy$addReaction(from, to, rate)
        }
        for (o in model$observables) {
            expr <- .suffix_symbols(o$expr, suffix = suffix, skip = skip)
            model_copy$addObservable(paste0(o$name, suffix), expr)
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
    with_underscore <- function(sfx) {
        if (!is.null(sfx) && !startsWith(sfx,"_")) paste0("_",sfx) else sfx
    }
    M1r <- renameCompartments(M1, with_underscore(suffix1), skip = shared)
    M2r <- renameCompartments(M2, with_underscore(suffix2), skip = shared)

    # collision handling (currently simplified)
    n1 <- M1r$getStateNames()
    n2 <- M2r$getStateNames()
    if (length(intersect(n1, n2)) > 0) {
        switch(collision,
               error = stop("Name collision in merged models: ",
                            paste(intersect(n1, n2), collapse=", ")),
               auto = {
                   M1r <- renameCompartments(M1r, suffix = "_1", skip = shared)
                   M2r <- renameCompartments(M2r, suffix = "_2", skip = shared)
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
