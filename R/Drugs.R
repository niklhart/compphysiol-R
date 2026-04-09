# Drug and Drugs classes and related methods

#' Class representing drug data
#'
#' This class is used to store drug-specific data, such as physicochemical properties and ADME parameters, that can be used in PBPK models.
#' It is designed to be flexible and extensible, allowing users to add new properties as needed.
#'
#' @param ... Named arguments representing drug parameters (possibly with units).
#' @param data A data frame with the following columns: "Parameter", "Species", "Value", "Unit", "Reference", "Assumption". 
#'   This can be used as an alternative to specifying parameters via `...`.
drug <- function(..., data = NULL) {

    args <- eval(substitute(alist(...)))

    if (length(args) > 0) {
        if (!is.null(data)) stop("Cannot use both '...' and 'data' arguments.")
        value <- lapply(args, .process_nse_arg, envir = parent.frame())

        data <- data.frame(
            Parameter = names(value),
            Species = "",                  # TODO: support %species% tag in NSE arguments
            Value = I(value),
            Reference = NA_character_,     # TODO: support %ref% tag in NSE arguments
            Assumption = NA_character_     # TODO: support %assum% tag in NSE arguments
        )
    }

    return(structure(data, class = c("Drug", "data.frame")))
}

#' Create an empty Drugs object
#' @returns An empty `Drugs` object.
drugs <- function() structure(
    data.frame(
        Drug = character(0), 
        Parameter = character(0), 
        Value = I(list()),
        Reference = character(0),
        Assumption = character(0)
    ), 
    class = "Drugs"
)


#' Print method for Drug objects
#'
#' @param x An object of class `Drug`.
#' @param meta Logical indicating whether to print parameter metadata as well (default: `FALSE`).
#' @param string Logical indicating whether to return a short string representation instead of printing (default: `FALSE`).
print.Drug <- function(x, meta = FALSE, string = FALSE) {
    if (string) {
        sprintf("%s = %s", names(x$param), vapply(x$param, format, character(1))) |>
            paste(collapse = ", ")
    } else {
        cat("Drug object\n")
        val_str <- vapply(x$Value, format, "")
        vars <- c("Parameter", "Species")
        if (meta) vars <- c(vars, "Reference", "Assumption")
        x[, vars] |>
            cbind(Value = val_str) |>
            print() 
    }
}

# #' Accessor for parameters in a Drug object
# #' @param x An object of class `Drug`.
# #' @param name The name(s) of the parameter(s) to access (character scalar or vector).
# #' @param species Optional species argument for species-specific parameters (character scalar or vector).
# #'   The default `NULL` means that species-specific parameters will be returned for all species available in the drug data.
# #' @param simplify Boolean indicating whether to simplify the output to a named vector instead of a `Parameters` object (default: `FALSE`). 
# #'   Simplification is possible across species, but not across drug parameters since the output may result in inconsistent units in such cases.
# #'   Therefore, when `name` is not scalar, the `simplify` argument is ignored with a warning and the output is a `Parameters` object.
# #' @returns The requested parameter(s), either as a `Parameters` object (if `simplify = FALSE`) or a named vector (if `simplify = TRUE`).
# #' @export
# param.Drug <- function(x, name, species = NULL, simplify = FALSE) {

#     param <- x$param[name]
#     if (is.null(x$param)) stop(paste("Parameter", name, "not found in Drug object"))
#     if (simplify && length(name) > 1) {
#         warning("Simplification to named vector is not possible across multiple parameters. Returning a Parameters object instead.")
#         simplify <- FALSE
#     }
#     if (simplify) param <- param[[1]]
#     if (!is.null(species)) param <- if (simplify) param[[species]] else parameters(name = name, value = lapply(param, `[`, species))

#     return(param)
# }



# #' Replace parameters in a Drug object
# #' @param x An object of class `Drug`.
# #' @param name The name of the parameter to replace.
# #' @param value A `Parameters` object containing the new parameter values.
# #' @param species Optional species argument for species-specific parameters (not currently used).
# #' @returns A modified `Drug` object with the updated parameter.
# #' @export
# `param<-.Drug` <- function(x, name, value, species = NULL) {
#     if (!inherits(value, "Parameters")) stop("Value must be a Parameters object")
#     x$param[[name]] <- value
#     if (name %in% names(x$meta)) {
#         x$meta[[name]] <- "manually updated via param<-"
#     }
#     return(x)
# }

#' Subset method for Drugs objects
#'
#' This method allows you to subset a `Drugs` object using standard indexing.
#' 
#' @param x An object of class `Drugs`.
#' @param i Index(es) or name(s) of drug(s) to select
#' @returns A subsetted `Drugs` object.
`[.Drugs` <- function(x, i) {
    class(x) <- "data.frame"
    x[x$Drug %in% i, c("Drug", "Parameter", "Species", "Value"), drop = FALSE] |>
        structure(class = c("Drugs", "data.frame"))
}


#' Implementation of the c() function for combining Drug and Drugs objects
#' @noRd
.c_drug_drugs <- function(...) {
    dots <- list(...)
    if (!all(sapply(dots, function(x) inherits(x, c("Drug", "Drugs"))))) {
        stop("All arguments must be of class 'Drug' or 'Drugs'")
    }

    is_drug <- vapply(dots, inherits, logical(1), "Drug")
    dots[is_drug] <- lapply(dots[is_drug], list) # wrap Drug objects in a list for uniform processing
    dots[!is_drug] <- lapply(dots[!is_drug], unclass) # wrap Drugs objects in a list for uniform processing

    structure(do.call(c, dots), class = "Drugs")
}

#' Combine multiple Drug or Drugs objects into a Drugs object
#'
#' This method allows you to combine multiple `Drug` or `Drugs` objects into a `Drugs` using the `c()` function.
#'
#' @param ... `Drug` or `Drugs` objects to combine
#' @returns A `Drugs` object containing the combined drugs.
c.Drug <- .c_drug_drugs

#' @rdname c.Drug
c.Drugs <- .c_drug_drugs

#' Print method for Drugs objects
#'
#' @param x An object of class `Drugs`.
#' @param meta Logical indicating whether to print parameter metadata as well (default: `FALSE`).
print.Drugs <- function(x, meta = FALSE) {
    if (length(x) > 0) {
        cat("Drugs object\n")
        val_str <- vapply(x$Value, format, "")
        vars <- c("Drug", "Parameter", "Species")
        if (meta) vars <- c(vars, "Reference", "Assumption")
        x[, vars] |>
            cbind(Value = val_str) |>
            print() 
    } else {
        cat("Drugs: (none)\n")
    }
    invisible(x)
}

#' Accessor for parameters in a Drugs object (experimental)
#' @param x An object of class `Drugs`.
#' @param drug Optional name of the drug to access (character scalar or vector). The default `NULL` means that parameters will be returned for all drugs in the database.
#' @param species Optional species argument for species-specific parameters.
#' @param name The name of the parameter(s) to access (optional). If not provided, all parameters will be returned and a warning will be issued if some are missing.
#' @returns The requested parameter(s) in the database for the requested drug and species as a `Parameters` object.
#' @noRd
param.Drugs <- function(x, drug, species, name = NULL) {
    drug <- match.arg(drug, choices = unique(x$Drug))
    species <- match.arg(species, choices = unique(x$Species))

    hits <- x[x$Drug == drug & x$Species %in% c("", species), ]

    all_names <- unique(hits$Parameter)
    name <- name %||% all_names
    missing <- setdiff(name, all_names)
    if (length(missing) > 0) {
        warning(paste(
            "Requested parameter(s)",
            paste(missing, collapse = ", "),
            "not found in DrugDB for drug",
            drug,
            "and species",
            species
        ))
    }
    name <- setdiff(name, missing)
    map <- setNames(hits$Value, hits$Parameter)
    do.call(parameters, map[name])
}

#' Extract method for Drugs objects.
#' 
#' @param x An object of class `Drugs`.
#' @param i The name of the drug to extract.
#' @returns A `Drug` object containing the parameters for the specified drug.
`[[.Drugs` <- function(x, i) {
    class(x) <- "data.frame"
    x[x$Drug == i, c("Parameter", "Species", "Value"), drop = FALSE] |>
        structure(class = c("Drug", "data.frame"))
}
#' Load a drug from the drug database
#' @param name The name of the drug to load.
#' @param species Character string, for which species to load species-specific parameters (default: all species).
#' @returns A `Drug` object containing all parameters for the specified drug and species.
loaddrugdata <- function(name = "", species = NULL) {

    name <- match.arg(name, choices = unique(.drugdb$Drug))
    if (!is.null(species)) species <- match.arg(species, choices = unique(.drugdb$Species))
    drug <- .drugdb[[name]]

    if (is.null(species)) return(drug)
    
    drug[drug$Species %in% c("", species), , drop = FALSE] |>
        structure(class = c("Drug", "data.frame"))

}
