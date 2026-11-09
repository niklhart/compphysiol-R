#' Create a `compphysiol_transports` object
#'
#' compphysiol_transports represent mass/amount-preserving transports of molecules between two compartments in a compartment model.
#' 
#' Transport rates are always represented as amount-change rates.
#' compphysiol_transports can be linear (first-order) or nonlinear, depending on whether the transport rate is defined by a constant 
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
#' @param rate The amount-change transport rate (character or call, optional,
#'   mutually exclusive with `const`)
#' @param const Rate constant for first-order transports (character, call, numeric,
#'   or unit-bearing value, optional, mutually exclusive with `rate`)
#' @param const_basis Whether `const` multiplies the source amount (default) or
#'   source concentration. Both define linear transports with amount/time rates.
#'   Amount-based constants have units 1/time; concentration-based constants have
#'   units compartment size/time (usually volume/time). Concentration-based
#'   transports require a source compartment size when preparing the model.
#'   Only applicable with `const`.
#' @return A `compphysiol_transports` object
#' @examples
#' # Linear transport
#' f1 <- transports(from = "A", to = "B", const = "k1")   # rate will be k1 * a[A]
#' # Concentration-based linear transport: rate will be Q * c[drug,A]
#' transports("A", "B", molec = "drug", const = "Q",
#'            const_basis = "concentration")
#' # Nonlinear transport
#' f2 <- transports(from = "A", to = "B", rate = "k1 * A*B/(B+K)")
#' @seealso [inspect] for printing, structure inspection, and component access.
#' @seealso [add_transport()] for adding transports to a model and
#'   [modify_compartment_model] for the complete component map.
#' @export
transports <- function(from, to, molec = NA_character_, ..., rate = NULL, const = NULL,
                       const_basis = c("amount", "concentration")) {
    if (!is.null(rate) && !missing(const_basis)) {
        stop("Argument 'const_basis' can only be used with 'const'.", call. = FALSE)
    }
    const_basis <- match.arg(const_basis)
    const_expr <- substitute(const)
    if (.dsl_has_state_ref(const_expr)) {
        stop(
            "Argument 'const' cannot contain state references such as 'a[...]' or 'c[...]'. ",
            "Use 'rate' for state-dependent transport expressions.",
            call. = FALSE
        )
    }
    const <- .process_unit_shorthand_arg(
        expr = const_expr,
        value = const,
        envir = parent.frame(n = 1)
    )

    # Error if any additional positional arguments are provided (enforces named arguments for rate and const)
    if (length(list(...)) > 0) {
        stop(
            "Additional arguments are not allowed. Please specify 'rate' and 'const' as named arguments only, not positional.",
            call. = FALSE
        )
    }

    # Early return for empty transports (if from or to is missing or empty)
    if (missing(from) || missing(to) || identical(from, character(0)) || identical(to, character(0))) {
        return(.new_transports(
            data.frame(
                from = character(),
                to = character(),
                molec = character(),
                rate = I(list()),
                const = I(list()),
                type = character(),
                const_basis = character(),
                stringsAsFactors = FALSE
            )
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
    nRate <- length(.as_expr_arg_list(rate))
    nConst <- length(.as_expr_arg_list(const))

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
            rate <- rate |> .as_expr_arg_list() |> lapply(.as_call) |> rep(length.out = nMax)
            const <- rep(list(NULL), nMax)
        },
        linear = {
            if (any(is.na(from))) {
                stop("Linear transports must have a valid source compartment.")
            }
            if (nConst == 1 && is.character(const)) const <- replace_pattern(const)
            const <- const |> .as_const_arg_list() |> rep(length.out = nMax)
            is_value <- vapply(const, .is_const_value, logical(1))
            const[!is_value] <- lapply(const[!is_value], .as_call)
            if (any(vapply(const, .dsl_has_state_ref, logical(1)))) {
                stop(
                    "Argument 'const' cannot contain state references such as 'a[...]' or 'c[...]'. ",
                    "Use 'rate' for state-dependent transport expressions.",
                    call. = FALSE
                )
            }
            prefix <- if (const_basis == "amount") "a" else "c"
            rate <- Map(
                function(f, c) .mul(c, .as_call(f)),
                ifelse(is.na(molec), paste0(prefix, "[", from, "]"), paste0(prefix, "[", molec, ",", from, "]")),
                const,
                USE.NAMES = FALSE
            )
        }
    )

    # Assemble into a data frame (`I()` to prevent data.frame from attempting to convert list columns)
    .new_transports(data.frame(
            from = from,
            to = to,
            molec = if (is.null(molec)) NA_character_ else molec,
            rate = I(rate),
            const = I(const),
            type = type,
            const_basis = if (type == "linear") const_basis else NA_character_,
            stringsAsFactors = FALSE
        ))
}

.new_transports <- function(x) {
    structure(x, class = .compphysiol_class("compphysiol_transports"))
}


#' Add one or several transports to a `compphysiol_compartment_model` object.
#'
#' @inherit transports description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams transports
#' @param trans A `compphysiol_transports` object. Constructed from the other inputs if not provided.
#' @returns The modified `compphysiol_compartment_model` object.
#' @examples
#' # Add a linear transport
#' compartment_model() |>
#'     add_transport(from = "A", to = "B", const = "k1")
#' # Add a nonlinear transport
#' compartment_model() |>
#'     add_transport(from = "A", to = "B", rate = "k2 * c[A] / (c[A] + Km)")
#' # Vectorized calls using naming abbreviation
#' compartment_model() |>
#'     add_transport(from = "A", to = c("B", "C"), const = "k{to}")
#' compartment_model() |>
#'     add_transport(from = c("A", "B"), to = c("B", "C"), const = "k_{from}_{to}")
#' # compphysiol_transports with specific molecules
#' compartment_model() |>
#'     add_transport(from = "cen", to = "per", molec = c("DrugA","DrugB"), const = "k_{molec}")
#' @seealso [transports()] for construction and [modify_compartment_model] for
#'   other model components.
#' @export
add_transport <- function(
    model,
    trans,
    ...,
    from,
    to,
    molec = NA_character_,
    rate = NULL,
    const = NULL,
    const_basis = c("amount", "concentration")
) {
    UseMethod("add_transport")
}

#' @rdname add_transport
#' @export
add_transport.compphysiol_compartment_model <- function(
    model,
    trans,
    ...,
    from,
    to,
    molec = NA_character_,
    rate = NULL,
    const = NULL,
    const_basis = c("amount", "concentration")
) {
    .check_class(model, "compphysiol_compartment_model")

    call <- match.call()

    trans <- .forward_or_use(
        object_arg_name = "trans",
        constructor_name = "transports",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(trans, "compphysiol_transports")

    model$transports <- c(model$transports, trans)
    return(model)
}


#' Print a `compphysiol_transports` object
#'
#' @param x A `compphysiol_transports` object
#' @param ... Additional arguments (not used)
#' @return The `compphysiol_transports` object (invisible)
#' @export
print.compphysiol_transports = function(x, ...) {

    from <- x$from
    to <- x$to

    empty_symbol <- "\u2205" # empty set symbol for source/sink compartments
    from[is.na(from)] <- empty_symbol
    to[is.na(to)] <- empty_symbol

    molec <- ifelse(is.na(x$molec), "<all molec>", x$molec)
    rate <- vapply(x$rate, .ode_model_deparse_with_unit_placeholders, character(1))

    if (length(x) > 0) {
        cat(" Transports:\n")
        cat(
            sprintf(
                "  (%i) %s: %s \u2192 %s, amount rate = %s\n",
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


#' Length of a `compphysiol_transports` object
#' 
#' @param x A `compphysiol_transports` object
#' @return The number of transports in the object
#' @export
length.compphysiol_transports <- function(x) {
    nrow(as.data.frame(x))
}

#' Convert a `compphysiol_transports` object to a data frame
#' 
#' @param x A `compphysiol_transports` object
#' @param ... Additional arguments (not used)
#' @return A data frame representation of the `compphysiol_transports` object
#' @export
as.data.frame.compphysiol_transports <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' Convert transports to ordinary component lists
#'
#' Each entry is an ordinary list with `from`, `to`, `molec`, `rate`, `const`,
#' `type`, and `const_basis` components. Expressions and constants are unwrapped
#' from their list columns; an unused constant remains `NULL`.
#'
#' @param x A `compphysiol_transports` object.
#' @param ... Unused.
#' @returns A list of component lists, one per transport.
#' @seealso [inspect] for structure inspection and component access.
#' @export
as.list.compphysiol_transports <- function(x, ...) {
    do.call(
        what = Map,
        args = c(
            list(list),
            as.list(as.data.frame(x)),
            USE.NAMES = FALSE
        )
    )
}

#' Subset a `compphysiol_transports` object
#' 
#' @param x A `compphysiol_transports` object
#' @param i Row indices to subset
#' @param ... Additional arguments (not used)
#' @return A subsetted `compphysiol_transports` object
#' @export
`[.compphysiol_transports` <- function(x, i, ...) {
    .new_transports(.subset_df_like(x, i, byname = FALSE))
}

#' Combine multiple `compphysiol_transports` objects into one
#' 
#' @param ... Multiple `compphysiol_transports` objects to combine
#' @return A combined `compphysiol_transports` object
#' @export
c.compphysiol_transports <- function(...) {
    .new_transports(.combine_df_like(...))
}

#' Direct transport extraction is not supported
#'
#' @param x A `compphysiol_transports` object.
#' @param i Row index to access.
#' @param ... Unused.
#' @returns This method always errors. Use `[` to retain the collection class,
#'   or `as.list()` to inspect ordinary component lists.
#' @seealso [inspect] for structure inspection and component access.
#' @export
`[[.compphysiol_transports` <- function(x, i, ...) .extract_df_like(x, i)


# Convert a linear transport coefficient to the amount basis at a model boundary.
.transport_amount_const <- function(model, i) {
    trans <- model$transports
    const <- trans$const[[i]]
    if (!identical(trans$const_basis[[i]], "concentration")) return(const)
    from <- trans$from[[i]]
    idx <- match(from, names(model$compartments))
    vol <- if (is.na(idx)) NULL else model$compartments$volume[[idx]]
    if (is.null(vol) || .is_missing_volume(vol)) {
        stop("Concentration-based transport requires a size for source compartment '",
             from, "'.", call. = FALSE)
    }
    call("/", const, .as_call(vol))
}

.transport_amount_basis <- function(model) {
    for (i in seq_along(model$transports)) {
        if (identical(model$transports$const_basis[[i]], "concentration")) {
            model$transports$const[[i]] <- .transport_amount_const(model, i)
            model$transports$rate[[i]] <- .mul(
                model$transports$const[[i]],
                .as_call(.dsl_make_state(model$transports$molec[[i]],
                                         model$transports$from[[i]], prefix = "a"))
            )
            model$transports$const_basis[[i]] <- "amount"
        }
    }
    model
}

#' Passive bidirectional exchange between two compartments
#'
#' Specify passive exchange using a shared exchange clearance rather than two
#' first-order constants. The directional amount rates are clearance times the
#' source concentration; the net flux from the first compartment to the second
#' is `clearance * (c_first - c_second)`. Compartment sizes are taken from the
#' model during preparation, so no volume parameter names need to be supplied.
#'
#' This law conserves molecular identity and total amount and, for positive
#' clearance and fixed positive volumes, equilibrates at equal concentrations.
#' It assumes no partitioning correction: tissue partitioning would instead
#' require a law such as `Q * (c_plasma - c_tissue / Kp)`. The exchange clearance
#' has units volume/time for volumetric compartments and need not represent a
#' literal fluid flow; it can also represent a permeability-surface area product.
#'
#' @param between Character vector naming two distinct compartments. Sources and
#'   sinks are not supported.
#' @param molec Molecule names; omitted means all molecules, resolved by `wire()`.
#' @param clearance Shared concentration-based coefficient, supplied as a symbolic
#'   character expression or an explicit numeric or unit-bearing value.
#' @returns An ordinary `compphysiol_transports` collection containing both
#'   directions for each molecule, with linearity metadata preserved.
#' @examples
#' passive_exchange(c("pla", "tis"), molec = "parent", clearance = "Q")
#' compartment_model() |>
#'   add_passive_exchange(c("pla", "tis"), clearance = 2 [L/h])
#' @export
passive_exchange <- function(between, molec = NA_character_, clearance) {
    if (!is.character(between) || length(between) != 2L ||
        anyNA(between) || any(!nzchar(between)) || anyDuplicated(between)) {
        stop("Argument 'between' must name two distinct compartments.", call. = FALSE)
    }
    clearance <- .process_unit_shorthand_arg(
        expr = substitute(clearance), value = clearance, envir = parent.frame()
    )
    c(
        transports(between[1], between[2], molec = molec,
                   const = clearance, const_basis = "concentration"),
        transports(between[2], between[1], molec = molec,
                   const = clearance, const_basis = "concentration")
    )
}

#' Add passive bidirectional exchange to a model
#' @inherit passive_exchange description details params
#' @param model A `compphysiol_compartment_model` object.
#' @returns The modified model.
#' @export
add_passive_exchange <- function(model, between, molec = NA_character_, clearance) {
    .check_class(model, "compphysiol_compartment_model")
    trans <- .forward_or_use(
        object_arg_name = "trans",
        constructor_name = "passive_exchange",
        call = match.call(),
        parent_env = parent.frame()
    )
    add_transport(model, trans = trans)
}
