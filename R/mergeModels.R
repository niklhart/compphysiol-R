
#' Merge two `CompartmentModel` objects into one.
#'
#' This function allows to combine two `CompartmentModel` objects into a single model, 
#' with options for handling name collisions and shared parameters/compartments.
#' 
#' @param M1,M2 `CompartmentModel` objects
#' @param suffix1,suffix2 Optional character suffixes for compartment names in `M1`/`M2`.
#'        Use `NULL` to leave names unchanged.
#' @param collision What to do in case of name collisions:
#'        "error" (default), "auto" (rename with suffixes 1/2), or "merge"
#'        (merge compartments, adding initial amounts).
#'        Since suffixing is done before checking for collisions, the
#'        `collisions` argument has no effect if distinct non-`NULL` suffixes are used.
#' @param shared An optional character vector or parameter or compartment names
#'        that should be shared, i.e. they are not suffixed.
#' @returns A new `CompartmentModel` containing both.
#' @examples
#' # Example 1: Merging oral absorption and one-compartment PK
#' abs <- compartment_model() |>
#'     add_compartment("Gut", 100) |>
#'     add_flow("Gut", "Central", const = "ka")
#' pk <- compartment_model() |>
#'     add_compartment("Central", 0) |>
#'     add_flow("Central", "", const = "k10")
#' mergeModels(abs, pk, collision = "merge")
#'
#' # Example 2: Two-drug PK model with drug-drug interaction
#' drugA <- multiCompModel(ncomp = 1)
#' drugB <- multiCompModel(ncomp = 2)
#' mergeModels(drugA, drugB, suffix1 = "A", suffix2 = "B") |>
#'     add_flow(from = "C1_B", to = "C1_A", rate = "-(C1_B / (IC50 + C1_B)) * k10_A * C1_A")
#' @export
mergeModels <- function(
    M1,
    M2,
    suffix1 = NULL,
    suffix2 = NULL,
    collision = c("error", "auto", "merge"),
    shared = character()
) {
    collision <- match.arg(collision)

    renameCompartments <- function(model, suffix) {
        if (is.null(suffix)) {
            return(model)
        }

        # TODO: this is a bit hacky, we should ideally use a more direct way to rename 
        # compartments, flows, observables, and dosing targets in a model without having 
        # to reconstruct it from scratch. But for now this works.

        compartment_model() |>
            add_compartment(
                name = paste0(names(model$compartments), suffix),
                initial = model$compartments$initial
            ) |>
            add_flow(
                from = ifelse(is.na(model$flows$from), NA_character_, paste0(model$flows$from, suffix)),
                to = ifelse(is.na(model$flows$to), NA_character_, paste0(model$flows$to, suffix)),
                rate = lapply(model$flows$rate, .suffix_symbols, suffix = suffix, skip = shared)
            ) |>
            add_observable(
                obs = observables(
                    name = paste0(names(model$observables), suffix),
                    expr = lapply(model$observables, .suffix_symbols, suffix = suffix, skip = shared)
                )
            ) |>
            add_dosing(
                target = paste0(model$doses$target, suffix),
                amount = model$doses$amount,
                time = model$doses$time,
                rate = model$doses$rate,
                duration = model$doses$duration
            )
    }

    # renaming logic (with or without leading "_")
    with_underscore <- function(sfx) {
        if (!is.null(sfx) && !startsWith(sfx, "_")) paste0("_", sfx) else sfx
    }
    M1r <- renameCompartments(M1, suffix = with_underscore(suffix1))
    M2r <- renameCompartments(M2, suffix = with_underscore(suffix2))

    # collision handling ("error" or "auto")
    n1 <- c(names(M1r$compartments), names(M1r$observables))
    n2 <- c(names(M2r$compartments), names(M2r$observables))
    if (length(intersect(n1, n2)) > 0) {
        switch(
            collision,
            error = stop(
                "Name collision in merged models: ",
                paste(intersect(n1, n2), collapse = ", ")
            ),
            auto = {
                M1r <- renameCompartments(M1r, suffix = "_1")
                M2r <- renameCompartments(M2r, suffix = "_2")
            }
        )
    }

    # Handle "merge" collision by summing initial amounts of colliding compartments
    merged_comps <- M1r$compartments
    for (i in seq_along(M2r$compartments)) {
        if (
            collision == "merge" &&
                !is.na(idx <- match(M2r$compartments$name[[i]], n1))
        ) {
            merged_comps$initial[[idx]] <- merged_comps$initial[[idx]] +
                M2r$compartments$initial[[i]]
        } else {
            merged_comps <- c(merged_comps, M2r$compartments[i])
        }
    }
    
    # construct merged model
    compartment_model() |>
        add_compartment(comp = merged_comps) |>
        add_flow(flow = M1r$flows) |>
        add_flow(flow = M2r$flows) |>
        add_observable(obs = M1r$observables) |>
        add_observable(obs = M2r$observables) |>
        add_dosing(dose = M1r$doses) |>
        add_dosing(dose = M2r$doses)

}
