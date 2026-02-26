# S3 class "Flows" for flows between compartments, formerly known as R6 class "Reaction"

#' Flows Class
#'
#' Represents a flow between two compartments in a pharmacokinetic model.
#' @param from The name of the source compartment(s) (character vector or NULL for source compartments)
#' @param to The name of the destination compartment(s) (character vector or NULL for sink compartments)
#' @param ... Unused, enforces `rate` and `const` to be specified as named arguments only, not positional
#' @param rate The flow rate (numeric, optional, mutually exclusive with `const`)
#' @param const Rate constant for first-order flows (numeric, optional, mutually exclusive with `rate`)
#' @return A `Flows` object
#' @examples
#' # Linear flow
#' f1 <- flows(from = "A", to = "B", const = "k1")   # rate will be k1 * A
#' # Nonlinear flow
#' f2 <- flows(from = "A", to = "B", rate = "k1 * A*B/(B+K)")
#' @export
flows <- function(from, to, ..., rate = NULL, const = NULL) {

    # Convert NULL/"" from/to to NA for easier handling of source/sink compartments
    from <- if (is.null(from)) NA_character_ else from
    to <- if (is.null(to)) NA_character_ else to
    from[from == ""] <- NA_character_ 
    to[to == ""] <- NA_character_ 

    # Input lengths
    nFrom <- length(from)
    nTo <- length(to)
    nRate <- length(rate)
    nConst <- length(const)

    # Check that all inputs are either NULL, scalar or vector of the same length
    nMax <- max(nFrom, nTo)
    if (!all(c(nFrom, nTo, nRate, nConst) %in% c(0, 1, nMax))) {
        stop(
            "All inputs must be either NULL, scalar, or vector of the same length."
        )
    }

    # Check that if rate is provided, const is not provided and vice versa
    if (!xor(is.null(rate), is.null(const))) {
        stop(
            "Exactly one of 'rate' or 'const' must be provided.\n",
            "Note: these arguments must be named; positional arguments are not allowed."
        )
    }
    type <- if (is.null(rate)) "linear" else "nonlinear"

    from <- rep(from, nMax / nFrom)
    to <- rep(to, nMax / nTo)

    # If rate/const is scalar, apply special substitution rule
    replace_pattern <- function(x) {
        Map(
            function(f_, t_) {
                x |>
                    gsub(pattern = "_from", replacement = f_) |>
                    gsub(pattern = "_to", replacement = t_)
            },
            f_ = from,
            t_ = to,
            USE.NAMES = FALSE
        )
    }

    # Construction of rate/const lists
    switch(
        type,
        nonlinear = {
            rate <- if (nRate == 1 && is.character(rate)) {
                replace_pattern(rate)
            } else {
                rate
            }
            rate <- lapply(rate, .as_call)
            const <- rep(list(NULL), nMax)
        },
        linear = {
            if (any(is.na(from))) {
                stop("Linear flows must have a valid source compartment.")
            }
            if (nConst == 1 && is.character(const)) const <- replace_pattern(const)
            const <- lapply(const, .as_call)
            rate <- Map(function(f,c) .mul(c, .as_call(f)), from, const, USE.NAMES = FALSE)
        }
    )

    # Assemble into a data frame (`I()` to prevent data.frame from attempting to convert list columns)
    structure(
        data.frame(
            from = from,
            to = to,
            rate = I(rate),
            const = I(const),
            type = type,
            stringsAsFactors = FALSE
        ),
        class = "Flows"
    )
}

#' Create an empty `Flows` object
#' @return An empty `Flows` object
#' @export
empty_flow <- function() {
    structure(
        data.frame(
            from = character(),
            to = character(),
            rate = I(list()),
            const = I(list()),
            type = character(),
            stringsAsFactors = FALSE
        ),
        class = "Flows"
    )
}


#' Print a `Flows` object
#' @param x A `Flows` object
#' @param ... Additional arguments (not used)
#' @return The `Flows` object (invisible)
#' @export
print.Flows = function(x, ...) {

    from <- x$from
    from[is.na(from)] <- "\u2205" # empty set symbol for source compartments
    to <- x$to
    to[is.na(to)] <- "\u2205" # empty set symbol for sink compartments

    if (length(x) > 0) {
        cat(" Flows:\n")
        cat(sprintf(
            "  (%i) %s \u2192 %s, rate = %s\n",
            seq_along(x),
            from,
            to,
            vapply(x$rate, deparse, character(1))
        ), sep = "")
    } else {
        cat(" Flows: (none)\n")
    }

    invisible(x)
}


#' Length of a `Flows` object
#' @param x A `Flows` object
#' @return The number of flows in the object
#' @export
length.Flows <- function(x) {
    nrow(as.data.frame(x))
}

#' Convert a `Flows` object to a data frame
#' @param x A `Flows` object
#' @param ... Additional arguments (not used)
#' @return A data frame representation of the `Flows` object
#' @export
as.data.frame.Flows <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' Subset a `Flows` object
#' @param x A `Flows` object
#' @param i Row indices to subset
#' @param ... Additional arguments (not used)
#' @return A subsetted `Flows` object
#' @export
`[.Flows` <- function(x, i, ...) {
    structure(
         as.data.frame(x)[i, , drop = FALSE],
         class = "Flows"
    )
}

#' Combine multiple `Flows` objects into one
#' @param ... Multiple `Flows` objects to combine
#' @return A combined `Flows` object
#' @export
c.Flows <- function(...) {

    objs <- list(...)
    if (!all(sapply(objs, function(o) inherits(o, "Flows")))) {
        stop("All inputs must be Flows objects.")
    }
    
    # Combine the data frames by row-binding
    objs |>
        lapply(FUN = as.data.frame) |>
        do.call(what = rbind) |>
        structure(class = "Flows")
}
