#' Install a model unit
#'
#' `install_model_unit()` installs a unit through `units::install_unit()` and
#' records its definition in compphysiol's session-local unit registry.
#'
#' @param symbol Character scalar unit symbol.
#' @param def Optional character scalar unit definition. If omitted, `symbol`
#'   is installed as a new base unit. The special `units::install_unit()`
#'   definition `"unitless"` is intentionally not supported.
#'
#' @returns Invisibly returns the registered unit table row.
#' @export
install_model_unit <- function(symbol, def = character(0)) {
    .check_model_unit_args(symbol, def)
    def <- .normalize_model_unit_def(def)

    units::install_unit(symbol = symbol, def = def)
    .register_model_unit(symbol = symbol, def = def)
}

#' Register a model unit definition
#'
#' `register_model_unit()` records a unit definition for compphysiol without
#' installing it in the `units` package. Use this only when the unit has already
#' been installed elsewhere.
#'
#' @param symbol Character scalar unit symbol.
#' @param def Optional character scalar unit definition. If omitted, `symbol`
#'   is treated as a custom base unit. The special definition `"unitless"` is
#'   intentionally not supported.
#'
#' @returns Invisibly returns the registered unit table row.
#' @export
register_model_unit <- function(symbol, def = character(0)) {
    .check_model_unit_args(symbol, def)
    def <- .normalize_model_unit_def(def)
    .register_model_unit(symbol = symbol, def = def)
}

#' Inspect registered model units
#'
#' @returns A data frame with registered unit symbols and definitions.
#' @export
model_unit_registry <- function() {
    .the$registry$model_units
}

#' Reset registered model units
#'
#' This resets only compphysiol's session-local registry. It does not remove
#' units from the `units` package registry.
#'
#' @returns Invisibly returns `NULL`.
#' @export
reset_model_unit_registry <- function() {
    .the$registry$model_units <- .empty_model_unit_registry()
    invisible(NULL)
}

.empty_model_unit_registry <- function() {
    data.frame(
        symbol = character(),
        def = character(),
        stringsAsFactors = FALSE
    )
}

.normalize_model_unit_def <- function(def) {
    if (is.null(def)) character(0) else def
}

.check_model_unit_args <- function(symbol, def) {
    if (!is.character(symbol) || length(symbol) != 1 || is.na(symbol) || !nzchar(symbol)) {
        stop("Argument 'symbol' must be a non-empty character scalar.", call. = FALSE)
    }
    if (grepl("%", symbol, fixed = TRUE)) {
        stop("Argument 'symbol' cannot contain '%'.", call. = FALSE)
    }
    if (grepl("[^[:alnum:].]", symbol)) {
        stop("Argument 'symbol' must be a single unit symbol, not a compound unit expression.", call. = FALSE)
    }

    if (is.null(def)) {
        def <- character(0)
    }
    if (!is.character(def) || length(def) > 1 || anyNA(def)) {
        stop("Argument 'def' must be omitted or a character scalar.", call. = FALSE)
    }
    if (identical(def, "unitless")) {
        stop("Model units cannot be registered with definition 'unitless'.", call. = FALSE)
    }

    invisible(TRUE)
}

.register_model_unit <- function(symbol, def = character(0)) {
    def_value <- if (length(def) == 0) "" else def
    row <- data.frame(
        symbol = symbol,
        def = def_value,
        stringsAsFactors = FALSE
    )

    registry <- .the$registry$model_units
    keep <- registry$symbol != symbol
    .the$registry$model_units <- rbind(registry[keep, , drop = FALSE], row)

    invisible(row)
}
