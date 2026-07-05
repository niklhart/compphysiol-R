
#' Merge two `CompartmentModel` objects into one.
#'
#' This function combines two `CompartmentModel` objects into a single model.
#' In overlay mode, matching names are interpreted as the same modelling entity:
#' compartments with the same name are the same location, molecules with the same
#' name are the same substance, and matching molecule-in-compartment combinations
#' are the same state.
#' 
#' @param M1,M2 `CompartmentModel` objects
#' @param mode Merge mode. Currently only `"overlay"` is implemented.
#' @returns A new `CompartmentModel` containing both.
#' @examples
#' abs <- compartment_model() |>
#'     add_compartment(c("Gut", "Central")) |>
#'     add_molecule("drug", cmt = c("Gut", "Central"), initial = c(100, 0), type = "amount") |>
#'     add_transport("Gut", "Central", const = "ka", molec = "drug")
#' pk <- compartment_model() |>
#'     add_compartment(c("Central", "Peripheral")) |>
#'     add_molecule("drug", cmt = c("Central", "Peripheral"), type = "amount") |>
#'     add_transport("Central", "Peripheral", const = "k12", molec = "drug")
#' mergeModels(abs, pk, mode = "overlay")
#' @export
mergeModels <- function(
    M1,
    M2,
    mode = "overlay"
) {
    .check_class(M1, "CompartmentModel")
    .check_class(M2, "CompartmentModel")
    mode <- match.arg(mode, "overlay")

    same_or_error <- function(x, y, what) {
        if (!identical(x, y)) {
            stop("Cannot overlay models with conflicting ", what, ".", call. = FALSE)
        }
        invisible(TRUE)
    }

    merge_named_list <- function(x, y, what) {
        merged <- x
        for (nm in names(y)) {
            idx <- match(nm, names(merged))
            if (is.na(idx)) {
                merged <- c(merged, y[nm])
            } else {
                same_or_error(
                    unclass(merged)[[idx]],
                    unclass(y)[[nm]],
                    paste0(what, " '", nm, "'")
                )
            }
        }
        merged
    }

    merged_compartments <- M1$compartments
    for (i in seq_along(M2$compartments)) {
        name <- M2$compartments$name[[i]]
        idx <- match(name, merged_compartments$name)
        if (is.na(idx)) {
            merged_compartments <- c(merged_compartments, M2$compartments[i])
        } else {
            same_or_error(
                merged_compartments$volume[[idx]],
                M2$compartments$volume[[i]],
                paste0("compartment '", name, "'")
            )
        }
    }

    state_key <- function(molec) {
        paste(molec$name, ifelse(is.na(molec$cmt), "<all cmt>", molec$cmt), sep = "\r")
    }

    merged_molecules <- M1$molecules
    merged_keys <- state_key(merged_molecules)
    for (i in seq_along(M2$molecules)) {
        key <- state_key(M2$molecules[i])
        idx <- match(key, merged_keys)
        if (is.na(idx)) {
            merged_molecules <- c(merged_molecules, M2$molecules[i])
            merged_keys <- c(merged_keys, key)
        } else {
            name <- paste0(M2$molecules$name[[i]], " in ", M2$molecules$cmt[[i]])
            same_or_error(
                merged_molecules$type[[idx]],
                M2$molecules$type[[i]],
                paste0("state type for molecule '", name, "'")
            )
            merged_molecules$init[[idx]] <- merged_molecules$init[[idx]] +
                M2$molecules$init[[i]]
        }
    }

    structure(
        list(
            compartments = merged_compartments,
            molecules = merged_molecules,
            transports = c(M1$transports, M2$transports),
            reactions = c(M1$reactions, M2$reactions),
            equations = merge_named_list(M1$equations, M2$equations, "equation"),
            observables = merge_named_list(M1$observables, M2$observables, "observable"),
            parameters = merge_named_list(M1$parameters, M2$parameters, "parameter"),
            doses = c(M1$doses, M2$doses),
            metadata = M1$metadata
        ),
        class = "CompartmentModel"
    )
}
