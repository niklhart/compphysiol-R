#' Transports Class
#'
#' Transports represent mass/amount-preserving transports of molecules between two compartments in a compartment model.
#' 
#' Transports can be linear (first-order) or nonlinear, depending on whether the transport rate is defined by a constant 
#' (argument `const`) or a general expression (argument `rate`). 
#' The special syntax `{from}`, `{to}` and `{molec}` can be used in `rate` or `const` to automatically substitute 
#' the source and destination compartment names and molecule names, respectively, e.g., `"k_{from}_{to}"` will become 
#' `k_ven_bon` for a transport from the venous blood (ven) to bone (bon). This allows for vector definition of multiple 
#' transports with different compartments and/or molecules involved.
#' 
#' @param from The name of the source compartment(s) (character vector or NULL for source compartments)
#' @param to The name of the destination compartment(s) (character vector or NULL for sink compartments)
#' @param molec The name of the molecule(s) being transferred (character vector, optional, default: all molecules)
#' @param ... Unused, enforces `rate` and `const` to be specified as named arguments only, not positional
#' @param rate The transport rate (character or call, optional, mutually exclusive with `const`)
#' @param const Rate constant for first-order transports (character or call, optional, mutually exclusive with `rate`)
#' @return A `Transports` object
#' @examples
#' # Linear transport
#' f1 <- transports(from = "A", to = "B", const = "k1")   # rate will be k1 * a[A]
#' # Nonlinear transport
#' f2 <- transports(from = "A", to = "B", rate = "k1 * A*B/(B+K)")
#' @export
transports <- function(from, to, molec = NA_character_, ..., rate = NULL, const = NULL) {

    # Error if any additional positional arguments are provided (enforces named arguments for rate and const)
    if (length(list(...)) > 0) {
        stop(
            "Additional arguments are not allowed. Please specify 'rate' and 'const' as named arguments only, not positional.",
            call. = FALSE
        )
    }

    # Early return for empty transports (if from or to is missing or empty)
    if (missing(from) || missing(to) || identical(from, character(0)) || identical(to, character(0))) {
        return(structure(
            data.frame(
                from = character(),
                to = character(),
                molec = character(),
                rate = I(list()),
                const = I(list()),
                type = character(),
                stringsAsFactors = FALSE
            ),
            class = "Transports"
        ))
    }

    # Convert NULL/"" from/to to NA for easier handling of source/sink compartments
    from <- if (is.null(from)) NA_character_ else from
    to <- if (is.null(to)) NA_character_ else to
    from[from == ""] <- NA_character_ 
    to[to == ""] <- NA_character_ 

    # Input lengths
    nFrom <- length(from)
    nTo <- length(to)
    nMolec <- length(molec)
    nRate <- length(rate)
    nConst <- length(const)

    # Check that all inputs are either NULL, scalar or vector of the same length
    nMax <- max(nFrom, nTo, nMolec)
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
    molec <- rep(molec, nMax / nMolec)

    # If rate/const is scalar, apply special substitution rule
    replace_pattern <- function(x) {
        Map(
            function(f_, t_, m_) {
                x |>
                    gsub(pattern = "\\{from\\}", replacement = f_) |>
                    gsub(pattern = "\\{to\\}", replacement = t_) |>
                    gsub(pattern = "\\{molec\\}", replacement = m_)
            },
            f_ = from,
            t_ = to,
            m_ = molec,
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
            rate <- rate |> lapply(.as_call) |> rep(length.out = nMax)
            const <- rep(list(NULL), nMax)
        },
        linear = {
            if (any(is.na(from))) {
                stop("Linear transports must have a valid source compartment.")
            }
            if (nConst == 1 && is.character(const)) const <- replace_pattern(const)
            const <- const |>
                lapply(.as_call) |>
                rep(length.out = nMax)
            rate <- Map(
                function(f, c) .mul(c, .as_call(f)),
                ifelse(is.na(molec), paste0("a[", from, "]"), paste0("a[", molec, ",", from, "]")),
                const,
                USE.NAMES = FALSE
            )
        }
    )

    # Assemble into a data frame (`I()` to prevent data.frame from attempting to convert list columns)
    structure(
        data.frame(
            from = from,
            to = to,
            molec = if (is.null(molec)) NA_character_ else molec,
            rate = I(rate),
            const = I(const),
            type = type,
            stringsAsFactors = FALSE
        ),
        class = "Transports"
    )
}


#' Add one or several transports to a `CompartmentModel` object.
#'
#' @inherit transports description details
#' @param model A `CompartmentModel` object.
#' @inheritParams transports
#' @param trans A `Transports` object. Constructed from the other inputs if not provided.
#' @returns The modified `CompartmentModel` object.
#' @examples
#' # Add a linear transport
#' compartment_model() |>
#'     add_transport(from = "A", to = "B", const = "k1")
#' # Add a nonlinear transport
#' compartment_model() |>
#'     add_transport(from = "A", to = "B", rate = "k2 * c[A] / (c[A] + Km)")
#' # Vectorized calls using naming abbreviation
#' compartment_model() |>
#'     add_transport("A", c("B", "C"), const = "k{to}")
#' compartment_model() |>
#'     add_transport(c("A", "B"), c("B", "C"), const = "k_{from}_{to}")
#' # Transports with specific molecules
#' compartment_model() |>
#'     add_transport(from = "cen", to = "per", molec = c("DrugA","DrugB"), const = "k_{molec}")
#' @export
add_transport <- function(
    model,
    from,
    to,
    molec = NA_character_,
    ...,
    rate = NULL,
    const = NULL,
    trans
) {
    .check_class(model, "CompartmentModel")

    call <- match.call()

    trans <- .forward_or_use(
        object_arg_name = "trans",
        constructor_name = "transports",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(trans, "Transports")

    model$transports <- c(model$transports, trans)
    return(model)
}


#' Print a `Transports` object
#'
#' @param x A `Transports` object
#' @param ... Additional arguments (not used)
#' @return The `Transports` object (invisible)
#' @export
print.Transports = function(x, ...) {

    from <- x$from
    to <- x$to

    empty_symbol <- "\u2205" # empty set symbol for source/sink compartments
    from[is.na(from)] <- empty_symbol
    to[is.na(to)] <- empty_symbol

    molec <- ifelse(is.na(x$molec), "<all molec>", x$molec)
    rate <- vapply(x$rate,deparse1,character(1))

    if (length(x) > 0) {
        cat(" Transports:\n")
        cat(
            sprintf(
                "  (%i) %s: %s \u2192 %s, rate = %s\n",
                seq_along(x),
#                paste0("a[", molec, "]"),
                molec,
                from,
                to,
                rate
            ),
            sep = ""
        )
    } else {
        cat(" Transports: (none)\n")
    }

    invisible(x)
}


#' Length of a `Transports` object
#' 
#' @param x A `Transports` object
#' @return The number of transports in the object
#' @export
length.Transports <- function(x) {
    nrow(as.data.frame(x))
}

#' Convert a `Transports` object to a data frame
#' 
#' @param x A `Transports` object
#' @param ... Additional arguments (not used)
#' @return A data frame representation of the `Transports` object
#' @export
as.data.frame.Transports <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' Convert a `Transports` object to a list of `Transport` objects (experimental).
#' 
#' `Transport` objects are light-weighted representations of individual transports, 
#' where each transport is represented as a list with entries 
#' `from`, `to`, `molec`, `rate`, `const`, and `type`. 
#' This function is useful for iterating over transports in a `Transports` object.
#' 
#' @param x A `Transports` object
#' @param ... Additional arguments (not used)
#' @return A list of `Transport` objects, where each component is a list of the form `list(from, to, molec, rate, const, type)`
#' @export
as.list.Transports <- function(x, ...) {
    lst <- do.call(
        what = Map, 
        args = c(
            list(list), 
            as.list(as.data.frame(x)),
            USE.NAMES = FALSE
        )
    )
    lapply(lst, function(l) {
        class(l) <- "Transport"
        l
    })
}

#' Subset a `Transports` object
#' 
#' @param x A `Transports` object
#' @param i Row indices to subset
#' @param ... Additional arguments (not used)
#' @return A subsetted `Transports` object
#' @export
`[.Transports` <- function(x, i, ...) .subset_df_like(x, i, byname = FALSE)

#' Combine multiple `Transports` objects into one
#' 
#' @param ... Multiple `Transports` objects to combine
#' @return A combined `Transports` object
#' @export
c.Transports <- function(...) .combine_df_like(...)

#' Accessing an element in a `Transports` object
#' 
#' @param x A `Transports` object
#' @param i Row index to access
#' @returns A `Transport` object, which is a list with entries `from`, `to`, `molec`, `rate`, `const`, and `type`.
#' @export
`[[.Transports` <- function(x, i) {
    if (!is.numeric(i) || length(i) != 1) stop("Index must be a single numeric value.")
    if (i < 1 || i > length(x)) stop("Index out of bounds.")

    structure(
        list(
            from = x$from[i],
            to = x$to[i],
            molec = x$molec[i],
            rate = x$rate[[i]],
            const = x$const[[i]],
            type = x$type[i]
        ),
        class = "Transport"
    )
}

#' Print a `Transport` object
#'
#' @param x A `Transport` object
#' @param ... Additional arguments (not used)
#' @return The `Transport` object (invisible)
#' @export
print.Transport <- function(x, ...) {
    empty_symbol <- "\u2205" # empty set symbol for source/sink compartments
    from <- if (is.na(x$from)) empty_symbol else x$from
    to <- if (is.na(x$to)) empty_symbol else x$to
    molec <- if (is.na(x$molec)) "<all molec>" else x$molec
    rate <- deparse1(x$rate)

    cat(sprintf(
        " Transport: %s \u2192 %s, molec = %s, rate = %s\n",
        from,
        to,
        molec,
        rate
    ))
    invisible(x)
}

#' Combine multiple `Transport` objects into a `Transports` object
#' 
#' @param ... Multiple `Transport` objects to combine
#' @return A combined `Transports` object
#' @export
c.Transport <- function(...) {
    objs <- list(...)
    if (!all(sapply(objs, function(o) inherits(o, "Transport")))) {
        stop("All inputs must be Transport objects.")
    }
    
    # Combine the components into vectors/lists
    from <- vapply(objs, function(o) o$from, character(1))
    to <- vapply(objs, function(o) o$to, character(1))
    molec <- vapply(objs, function(o) o$molec, character(1))
    rate <- lapply(objs, function(o) o$rate)
    const <- lapply(objs, function(o) o$const)
    type <- vapply(objs, function(o) o$type, character(1))

    # Assemble into a Transports object
    structure(
        data.frame(
            from = from,
            to = to,
            molec = molec,
            rate = I(rate),
            const = I(const),
            type = type,
            stringsAsFactors = FALSE
        ),
        class = "Transports"
    )
}