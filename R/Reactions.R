#' `Reactions` class and methods
#'
#' The `Reactions` class can hold several reactions, each with an input, output, rate/constant, and compartment.
#' However, each reaction needs to be defined separately, i.e., vectorized definitions are not supported for reactions 
#' as they can be for flows and molecules -- vector arguments are instead used for defining multiple reacting species within a single reaction.
#' 
#' The special expression `{cmt}` can be used in `rate` or `const` to automatically substitute the compartment name, 
#' e.g., `k_{cmt}` will become `k_cytoplasm` for reactions in the cytoplasm and `k_nucleus` for reactions in the nucleus.
#' Replacement is only applied if the `cmt` argument is provided (otherwise, parameter names would change dynamically as compartments change).  
#' 
#' @param input Character vector representing the input of the reaction, e.g. `c("A", "B")` 
#'   for a reaction where one molecule of `A` and one molecule of `B` are consumed. 
#'   For synthesis reactions, use an empty character vector or `NULL`.
#' @param output Character string representing the output of the reaction, e.g. `"C"` 
#'   for a reaction where one molecule of `C` is produced. 
#'   For degradation reactions, use an empty character vector or `NULL`.
#' @param cmt Character vector of compartment names where the reaction(s) occur (optional, default: all compartments)
#' @param ... Errors if used, enforces `rate` and `const` to be specified as named arguments only, not positional.
#' @param rate Character string representing the reaction rate (for nonlinear reactions).
#'   Use `c[A]` to refer to the concentration of molecule A, and `a[A]` to refer to its amount.
#' @param const Character string representing the reaction constant (for mass-action reactions).
#'   Formally, this corresponds to `rate = const * c[input[1]] * c[input[2]] * ...`. 
#'   In addition, the information that the reaction is elementary is encoded in the `type` 
#'   column of the resulting `Reactions` object.
#' @return A `Reactions` object containing the reaction information.
#' @examples
#' # empty `Reactions` object
#' reactions()
#' # association reaction (second-order)
#' reactions(input = c("A", "B"), output = "C", const = "k1")
#' # synthesis reaction (zero-order)
#' reactions(input = "", output = "A", const = "k2")
#' # Michelis-Menten reaction (saturable, non-elementary)
#' reactions(input = "A", output = "B", rate = "Vmax * c[A] / (Km + c[A])")
#' # Reactions in several compartments (same rate constants)
#' reactions(input = "A", output = "B", cmt = c("cytoplasm", "nucleus"), const = "kAB")
#' # Reactions in several compartments (different rate constants)
#' reactions(input = "A", output = "B", cmt = c("cytoplasm", "nucleus"), const = "kAB_{cmt}")
#' 
#' @export
reactions <- function(
    input = character(0),
    output = character(0),
    cmt = NA_character_,
    ...,
    rate = NULL,
    const = NULL
) {

    # Error if any additional positional arguments are provided (enforces named arguments for rate and const)
    if (length(list(...)) > 0) {
        stop(
            "Additional arguments are not allowed. Please specify 'rate' and 'const' as named arguments only, not positional.",
            call. = FALSE
        )
    }
    # Early return for empty reactions
    if ((length(input) == 0 && length(output) == 0) || length(cmt) == 0) {
        return(
            structure(
                data.frame(
                    input = I(list()),
                    output = I(list()),
                    rate = I(list()),
                    const = I(list()),
                    type = character(),
                    cmt = character(),
                    stringsAsFactors = FALSE
                ),
                class = "Reactions"
            )
        )
    }

    # Convert NULL / "" input/output to empty character vectors for easier handling of source/sink compartments
    if (is.null(input) || all(input == "")) input <- character(0)
    if (is.null(output) || all(output == "")) output <- character(0)

    # Input lengths
    nRate <- length(rate)
    nConst <- length(const)
    nReact <- length(cmt)

    # Check that all inputs are either NULL, scalar or vector of the same length
    if (!all(c(nRate, nConst) %in% c(0, 1, nReact))) {
        stop("All inputs must be either NULL, scalar, or vector of the same length.")
    }

    # Check that if rate is provided, const is not provided and vice versa
    if (!xor(is.null(rate), is.null(const))) {
        stop(
            "Exactly one of 'rate' or 'const' must be provided.\n",
            "Note: these arguments must be named; positional arguments are not allowed."
        )
    }

    # Reaction type and order
    type <- if (is.null(rate)) "elementary" else "complex"
    order <- length(input)

    # If rate/const is scalar and cmt is provided, apply special substitution rule
    replace_pattern <- function(x) {
        lapply(cmt, function(y) if (is.na(y)) x else gsub(pattern = "{cmt}", replacement = y, x = x, fixed = TRUE))
    }

    # Construction of rate/const lists
    switch(
        type,
        complex = {
            rate <- if (nRate == 1 && is.character(rate)) {
                replace_pattern(rate)
            } else {
                rate
            }
            rate <- lapply(rate, .as_call)
            const <- rep(list(NULL), nReact)
        },
        elementary = {
            if (nConst == 1 && is.character(const)) {
                const <- replace_pattern(const)
            }
            const <- lapply(const, .as_call)
            input_cl <- input |> lapply(function(x) paste0("c[", x, "]")) |> lapply(.as_call)
            rate <- lapply(const, function(k) Reduce(.mul, c(list(k), input_cl)))
        }

    )

    # Replicate input/output to length of cmt
    input <- rep(list(input), nReact)
    output <- rep(list(output), nReact)

    # Construct the Reactions object as a data frame with class "Reactions"
    return(
        structure(
            data.frame(
                input = I(input),
                output = I(output),
                rate = I(rate),
                const = I(const),
                type = type,
                cmt = cmt,
                stringsAsFactors = FALSE
            ),
            class = "Reactions"
        )
    )
}

#' Add one or several reactions to a `CompartmentModel` object.
#'
#' @inherit reactions description details
#' @param model A `CompartmentModel` object.
#' @inheritParams reactions
#' @param react A `Reactions` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @export
#' @seealso [reactions()]
add_reaction <- function(
    model,
    input = character(0),
    output = character(0),
    cmt = NA_character_,
    ...,
    rate = NULL,
    const = NULL,
    react = NULL
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    react <- .forward_or_use(
        object_arg_name = "react",
        constructor_name = "reactions",
        call = call,
        parent_env = parent.frame()
    )

    model$reactions <- c(model$reactions, react)

    model
}

#' Convert a `Reactions` object to a data frame
#'
#' @param x A `Reactions` object
#' @param ... Additional arguments (not used)
#' @return A data frame representation of the `Reactions` object
#' @export
as.data.frame.Reactions <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' Length of a `Reactions` object
#'
#' @param x A `Reactions` object
#' @return The number of reactions in the object
#' @export
length.Reactions <- function(x) {
    nrow(as.data.frame(x))
}

#' Combine multiple `Reactions` objects into one
#'
#' @param ... Multiple `Reactions` objects to combine
#' @return A combined `Reactions` object
#' @export
c.Reactions <- function(...) .combine_df_like(...)

#' Subset method for `Reactions` class
#' 
#' Allows subsetting a `Reactions` object while preserving its class.
#' @param x A `Reactions` object
#' @param i Row indices to subset
#' @param ... Additional arguments (not used)
#' @return A subsetted `Reactions` object
#' @export
`[.Reactions` <- function(x, i, ...) .subset_df_like(x, i, byname = FALSE)

#' Extraction method for `Reactions` class
#'
#' This method is intentionally not implemented to prevent direct element access,
#' which could lead to confusion given the internal data frame-like structure of `Reactions` objects.
#' Instead, users should use subsetting with `[` and a scalar name/index to access specific reactions.
#'
#' @param x A `Reactions` object
#' @param i Row index to access
#' @param ... Additional arguments (not used)
#' @return Nothing (errors)
#' @export
`[[.Reactions` <- function(x, i, ...) .extract_df_like(x, i)

#' Print a `Reactions` object
#'
#' @param x A `Reactions` object
#' @param ... Additional arguments (not used)
#' @return The `Reactions` object (invisible)
#' @export
print.Reactions = function(x, ...) {

    if (length(x) > 0) {

        empty_or_collapse <- function(x) {
            if (length(x) > 0) paste0(x, collapse = "+") else "\u2205" # empty set symbol for source/sink compartments
        }

        in_str <- vapply(x$input, empty_or_collapse, character(1))
        out_str <- vapply(x$output, empty_or_collapse, character(1))
        cmt_str <- ifelse(is.na(x$cmt), "<all cmt>", x$cmt)
        rate_str <- vapply(x$rate, function(r) paste(deparse(r), collapse = ""), character(1))

        cat(" Reactions:\n")
        cat(
            sprintf(
                "  (%i) %s \u2192 %s in %s, rate = %s\n",
                seq_along(x),
                in_str,
                out_str,
                cmt_str,
                rate_str
            ),
            sep = ""
        )
    } else {
        cat(" Reactions: (none)\n")
    }

    invisible(x)
}

