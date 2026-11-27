#' Custom model units
#'
#' compphysiol keeps a session-local registry of custom units used in models.
#' Units in this registry can appear in model components, parameters, and
#' observations, and are expanded to their definitions where needed.
#'
#' - `install_model_unit()` installs a unit through [units::install_unit()] and
#'   records its definition in the registry.
#' - `register_model_unit()` records a unit definition without installing it in
#'   the `units` package. Use this only when the unit has already been installed
#'   elsewhere.
#' - `model_unit_registry()` returns the registered units.
#' - `reset_model_unit_registry()` empties the registry. It does not remove
#'   units from the `units` package; use [units::remove_unit()] for that.
#'
#' @param symbol Character scalar unit symbol.
#' @param def Optional character scalar unit definition. If omitted, `symbol` is
#'   a new (custom) base unit. The special `units::install_unit()` definition
#'   `"unitless"` is intentionally not supported.
#' @returns `install_model_unit()` and `register_model_unit()` invisibly return
#'   the registered unit table row. `model_unit_registry()` returns a data frame
#'   with registered unit symbols and definitions. `reset_model_unit_registry()`
#'   invisibly returns `NULL`.
#' @examples
#' # Save the current registry so that it can be restored at the end
#' saved <- model_unit_registry()
#'
#' # Install a custom base unit and a unit derived from it
#' install_model_unit("cellcount")
#' install_model_unit("kilocell", def = "1000 cellcount")
#' model_unit_registry()
#'
#' # Custom units can then be used like any other unit
#' parameters(N0 = 5 [kilocell])
#'
#' # Restore the previous state: remove the units from the 'units' package and
#' # re-register the saved entries in compphysiol's registry
#' units::remove_unit(c("kilocell", "cellcount"))
#' reset_model_unit_registry()
#' for (i in seq_len(nrow(saved))) {
#'     register_model_unit(saved$symbol[[i]], saved$def[[i]])
#' }
#' @name model_units
NULL

#' @rdname model_units
#' @export
install_model_unit <- function(symbol, def = character(0)) {
    .check_model_unit_args(symbol, def)
    def <- .normalize_model_unit_def(def)

    units::install_unit(symbol = symbol, def = def)
    .register_model_unit(symbol = symbol, def = def)
}

#' @rdname model_units
#' @export
register_model_unit <- function(symbol, def = character(0)) {
    .check_model_unit_args(symbol, def)
    def <- .normalize_model_unit_def(def)
    .register_model_unit(symbol = symbol, def = def)
}

#' @rdname model_units
#' @export
model_unit_registry <- function() {
    .the$model_units
}

#' @rdname model_units
#' @export
reset_model_unit_registry <- function() {
    .the$model_units <- data.frame(
        symbol = character(),
        def = character(),
        stringsAsFactors = FALSE
    )
    invisible(NULL)
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

    registry <- .the$model_units
    keep <- registry$symbol != symbol
    .the$model_units <- rbind(registry[keep, , drop = FALSE], row)

    invisible(row)
}
