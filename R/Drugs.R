# Drug and Drugs classes and related methods

#' Class representing drug data
#'
#' This class is used to store drug-specific data, such as physicochemical properties and ADME parameters, that can be used in PBPK models.
#' It is designed to be flexible and extensible, allowing users to add new properties as needed.
#'
#' @param ... Named arguments representing drug parameters (possibly with units).
#' @param data A data frame with the following columns: "Parameter", "Species", "Value", "Unit", "Reference", "Assumption". 
#'   This can be used as an alternative to specifying parameters via `...`.
#' @noRd
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
    } else {
        data <- data %||% data.frame(
            Parameter = character(0),
            Species = character(0),
            Value = I(list()),
            Reference = character(0),
            Assumption = character(0)
        )
    }
    return(structure(data, class = .compphysiol_class(
        "compphysiol_drug", base = "data.frame"
    )))
}

#' Create an empty compphysiol_drugs
#' @returns An empty `compphysiol_drugs` object.
#' @noRd
drugs <- function() structure(
    data.frame(
        Drug = character(0), 
        Parameter = character(0), 
        Value = I(list()),
        Reference = character(0),
        Assumption = character(0)
    ), 
    class = .compphysiol_class("compphysiol_drugs")
)


#' Print method for compphysiol_drugs
#'
#' @param x An object of class `compphysiol_drug`.
#' @param ... Additional arguments (not used).
#' @param meta Logical indicating whether to print parameter metadata as well (default: `FALSE`).
#' @param string Logical indicating whether to return a short string representation instead of printing (default: `FALSE`).
#' @noRd
print.compphysiol_drug <- function(x, ..., meta = FALSE, string = FALSE) {
    if (string) {
        sprintf("%s = %s", names(x$param), vapply(x$param, format, character(1))) |>
            paste(collapse = ", ")
    } else {
        cat("Drug\n")
        val_str <- vapply(x$Value, format, "")
        vars <- c("Parameter", "Species")
        if (meta) vars <- c(vars, "Reference", "Assumption")
        x[, vars] |>
            cbind(Value = val_str) |>
            print() 
    }
}

# #' Accessor for parameters in a compphysiol_drug
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
#     if (is.null(x$param)) stop(paste("Parameter", name, "not found in drug"))
#     if (simplify && length(name) > 1) {
#         warning("Simplification to named vector is not possible across multiple parameters. Returning a Parameters object instead.")
#         simplify <- FALSE
#     }
#     if (simplify) param <- param[[1]]
#     if (!is.null(species)) param <- if (simplify) param[[species]] else parameters(name = name, value = lapply(param, `[`, species))

#     return(param)
# }



# #' Replace parameters in a compphysiol_drug
# #' @param x An object of class `Drug`.
# #' @param name The name of the parameter to replace.
# #' @param value A `Parameters` object containing the new parameter values.
# #' @param species Optional species argument for species-specific parameters (not currently used).
# #' @returns A modified `Drug` object with the updated parameter.
# #' @export
# `param<-.Drug` <- function(x, name, value, species = NULL) {
#     if (!inherits(value, "compphysiol_parameters")) stop("Value must be a Parameters object")
#     x$param[[name]] <- value
#     if (name %in% names(x$meta)) {
#         x$meta[[name]] <- "manually updated via param<-"
#     }
#     return(x)
# }

#' Subset method for compphysiol_drugss
#'
#' This method allows you to subset a `compphysiol_drugs` object using standard indexing.
#' 
#' @param x An object of class `compphysiol_drugs`.
#' @param i Index(es) or name(s) of drug(s) to select
#' @returns A subsetted `compphysiol_drugs` object.
#' @noRd
`[.compphysiol_drugs` <- function(x, i) {
    class(x) <- "data.frame"
    x[x$Drug %in% i, c("Drug", "Parameter", "Species", "Value"), drop = FALSE] |>
        structure(class = .compphysiol_class(
            "compphysiol_drugs", base = "data.frame"
        ))
}


#' Implementation of the c() function for combining compphysiol_drug and compphysiol_drugss
#' @noRd
.c_drug_drugs <- function(...) {
    dots <- list(...)
    if (!all(sapply(dots, function(x) inherits(
        x, c("compphysiol_drug", "compphysiol_drugs")
    )))) {
        stop("All arguments must be of class 'drug' or 'drugs'")
    }

    is_drug <- vapply(dots, inherits, logical(1), "compphysiol_drug")
    dots[is_drug] <- lapply(dots[is_drug], list) # wrap compphysiol_drugs in a list for uniform processing
    dots[!is_drug] <- lapply(dots[!is_drug], unclass) # wrap compphysiol_drugss in a list for uniform processing

    structure(do.call(c, dots), class = .compphysiol_class(
        "compphysiol_drugs"
    ))
}

#' Combine multiple compphysiol_drug or compphysiol_drugss into a compphysiol_drugs
#'
#' This method allows you to combine multiple `compphysiol_drug` or `compphysiol_drugs` objects into a `compphysiol_drugs` using the `c()` function.
#'
#' @param ... `compphysiol_drug` or `compphysiol_drugs` objects to combine
#' @returns A `compphysiol_drugs` object containing the combined drugs.
#' @noRd
c.compphysiol_drug <- .c_drug_drugs

#' @rdname c.compphysiol_drug
#' @noRd
c.compphysiol_drugs <- .c_drug_drugs

#' Print method for compphysiol_drugss
#'
#' @param x An object of class `compphysiol_drugs`.
#' @param ... Additional arguments (not used).
#' @param meta Logical indicating whether to print parameter metadata as well (default: `FALSE`).
#' @noRd
print.compphysiol_drugs <- function(x, ..., meta = FALSE) {
    if (length(x) > 0) {
        cat("Drugs\n")
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

#' Accessor for parameters in a compphysiol_drugs (experimental)
#' @param x An object of class `compphysiol_drugs`.
#' @param name The name of the parameter(s) to access (optional). If not provided, all parameters will be returned and a warning will be issued if some are missing.
#' @param ... Additional arguments (not used).
#' @param drug Optional name of the drug to access (character scalar or vector). The default `NULL` means that parameters will be returned for all drugs in the database.
#' @param species Optional species argument for species-specific parameters.
#' @returns The requested parameter(s) in the database for the requested drug and species as a `compphysiol_parameters` object.
#' @noRd
param.compphysiol_drugs <- function(x, name = NULL, ..., drug, species) {
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

#' Extract method for compphysiol_drugss.
#' 
#' @param x An object of class `compphysiol_drugs`.
#' @param i The name of the drug to extract.
#' @returns A `compphysiol_drug` object containing the parameters for the specified drug.
#' @noRd
`[[.compphysiol_drugs` <- function(x, i) {
    class(x) <- "data.frame"
    x[x$Drug == i, c("Parameter", "Species", "Value"), drop = FALSE] |>
        structure(class = .compphysiol_class(
            "compphysiol_drug", base = "data.frame"
        ))
}
#' Load a drug from the drug database
#' @param name The name of the drug to load.
#' @param species Character string, for which species to load species-specific parameters (default: all species).
#' @returns A `compphysiol_drug` object containing all parameters for the specified drug and species.
#' @noRd
loaddrugdata <- function(name = "", species = NULL) {

    name <- match.arg(name, choices = unique(.drugdb$Drug))
    if (!is.null(species)) species <- match.arg(species, choices = unique(.drugdb$Species))
    drug <- .drugdb[[name]]

    if (is.null(species)) return(drug)
    
    drug[drug$Species %in% c("", species), , drop = FALSE] |>
        structure(class = .compphysiol_class(
            "compphysiol_drug", base = "data.frame"
        ))

}
