#' Create a new `Physiology` object
#' 
#' @param params A data frame with physiology parameters
#' @param meta A list of metadata (e.g. species, sex)
#' @returns A new `Physiology` object
#' @export
physiology <- function(params = NULL, meta = NULL) {

    param_default <- data.frame(
        parameter = character(),
        context = character(), # "scalar" or tissue name
        value = numeric(),
        unit = character(),
        type = character(), # "scalar" / "tissue" / "derived"
        reference = character(),
        assumption = character(),
        stringsAsFactors = FALSE
    )

    structure(
        list(
            param_table = rbind(param_default, params), 
            meta = as.list(meta)
        ), 
        class = "Physiology"
    )
}

#' Add a scalar parameter to a `Physiology` object
#' 
#' @param phys A `Physiology` object
#' @param name Name of the parameter
#' @param value Numeric value
#' @param unit Character, unit of measurement
#' @param reference Source reference
#' @param assumption Assumptions made
#' @returns The updated `Physiology` object
#' @examples
#' physiology() |>
#'     add_scalar(name = "BW", value = 250)
#' @export
add_scalar <- function(phys, name, value, unit = "", reference = "", assumption = "") {
    phys$param_table <- rbind(
        phys$param_table,
        data.frame(
            parameter = name,
            context = "scalar",
            value = value,
            unit = unit,
            type = "scalar",
            reference = reference,
            assumption = assumption,
            stringsAsFactors = FALSE
        )
    )
    phys
}

#' Add a tissue parameter (e.g. volume, flow) to a `Physiology` object
#' 
#' @param phys A `Physiology` object
#' @param tissue Tissue name (e.g. "Liver")
#' @param name Parameter name (e.g. "V" or "Q")
#' @param value Numeric value
#' @param unit Character, unit of measurement
#' @param reference Source reference
#' @param assumption Assumptions made
#' @returns The updated `Physiology` object
#' @export
add_tissue_param <- function(phys, tissue, name, value, unit = "", reference = "", assumption = "") {
    phys$param_table <- rbind(
        phys$param_table,
        data.frame(
            parameter = name, 
            context = tissue,
            value = value, 
            unit = unit, 
            type = "tissue",
            reference = reference, 
            assumption = assumption,
            stringsAsFactors = FALSE
        )
    )
    phys
}

#' Add metadata (categorical attributes) to a `Physiology` object as name-value pairs.
#' 
#' @param phys A `Physiology` object
#' @param ... Name-value pairs.
#' @examples
#' physiology() |>
#'     add_meta(species = "rat", strain = "Wistar", sex = "male")
#' @returns The updated `Physiology` object
#' @export
add_meta <- function(phys, ...) {
    dots <- list(...)
    if (length(dots) == 0) {
        return(phys)
    }
    phys$meta <- utils::modifyList(phys$meta, dots)
    phys
}

#' Print method for `Physiology` objects, showing metadata and a summary of parameters.
#' 
#' @param x A `Physiology` object
#' @param ... Additional arguments (not used)
#' @return The `Physiology` object (invisible)
#' @export
print.Physiology <- function(x, ...) {
    # ---- 1. Header with metadata ----
    meta_str <- ""
    if (length(x$meta) > 0) {
        shown_meta <- head(paste0(names(x$meta), "=", x$meta), 3)
        meta_str <- paste(shown_meta, collapse = ", ")
        if (length(x$meta) > 3) meta_str <- paste0(meta_str, ", ...")
    }

    header <- "<Physiology>"
    if (meta_str != "") {
        header <- paste0(header, " (", meta_str, ")")
    }

    cat(header, "\n", sep = "")

    # ---- 2. Scalars ----
    isScalar <- x$param_table$type == "scalar"
    scalars <- x$param_table[isScalar, ]
    if (nrow(scalars) > 0) {
        shown_scalars <- head(paste0(scalars$parameter, "=", scalars$value), 3)
        scalar_str <- paste(shown_scalars, collapse = ", ")
        if (nrow(scalars) > 3) scalar_str <- paste0(scalar_str, ", ...")
    } else {
        scalar_str <- "none"
    }
    cat("  Scalars: ", scalar_str, "\n", sep = "")

    # ---- 3. Tissue stats ----
    tissues <- x$param_table[!isScalar, ]
    if (nrow(tissues) > 0) {
        ntis <- length(unique(tissues$context))
        npar <- length(unique(tissues$parameter))
        cat("  Tissue parameters: ", nrow(tissues),
            " entries (", ntis, " tissues, ", npar, " parameters)\n", sep = "")
    } else {
        cat("  Tissue parameters: none\n")
    }

    invisible(x)
}

 #' Summarize a `Physiology` object as a string.
 #' 
 #' @param object A `Physiology` object
 #' @param ... Additional arguments (not used)
 #' @return A summary string
 #' @export
summary.Physiology <- function(object, ...) {
    paste0("Physiology: ", nrow(object$param_table), " parameters")
}


#' Get numeric parameter or metadata from a `Physiology` object by name (and optionally context)
#' @param phys A `Physiology` object
#' @param name Name of the parameter or metadata entry to be queried
#' @param context A string, defaulting to `"scalar"`
#' @returns The value of the parameter or metadata, or `NA` if not found
#' @examples
#' P <- physiology() |>
#'     add_scalar(name = "BW", value = 250)
#' parameter(P, "BW") # 250
#' @export
parameter <- function(phys, name, context = "scalar") {
    # First check metadata
    if (name %in% names(phys$meta)) {
        return(phys$meta[[name]])
    }
    # Otherwise, numeric parameters
    row <- phys$param_table[phys$param_table$parameter == name & phys$param_table$context == context, ]
    if (nrow(row) == 0) return(NA)
    row$value
}

#' Export parameters as a named list
#' 
#' This function converts the parameters in the `param_table` of a `Physiology` object into a named list, 
#' where the names are either the parameter name (for scalars) or `parameter[context]` (for tissue parameters).
#' 
#' @param phys A `Physiology` object
#' @returns A named list of parameters and metadata
#' @export
to_param_list <- function(phys) {
    out <- list()
    for (i in seq_len(nrow(phys$param_table))) {
        row <- phys$param_table[i, ]
        if (row$context == "scalar") {
            out[[row$parameter]] <- row$value
        } else {
            nm <- paste0(row$parameter, "[", row$context, "]")
            out[[nm]] <- row$value
        }
    }
    # merge metadata at the end
    c(out, phys$meta)
}
