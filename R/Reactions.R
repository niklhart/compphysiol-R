#' Create a `States` object
#'
#' `States` represent molecule-compartment pairs. In reactions, repeated states
#' are interpreted as stoichiometric multiplicity during reaction construction.
#'
#' @param ... Unused. `molec` and `cmt` must be named explicitly.
#' @param molec Character vector of molecule names.
#' @param cmt Character vector of compartment names.
#' @return A `States` object.
#' @export
state <- function(..., molec, cmt) {
    if (length(list(...)) > 0) {
        stop("Arguments 'molec' and 'cmt' must be named explicitly.", call. = FALSE)
    }

    if (missing(molec)) stop("Argument 'molec' is required.", call. = FALSE)
    if (missing(cmt)) stop("Argument 'cmt' is required.", call. = FALSE)

    n <- max(length(molec), length(cmt))
    if (!all(c(length(molec), length(cmt)) %in% c(1, n))) {
        stop("Arguments 'molec' and 'cmt' must be scalar or have the same length.", call. = FALSE)
    }

    if (length(molec) == 1) molec <- rep(molec, n)
    if (length(cmt) == 1) cmt <- rep(cmt, n)

    structure(
        data.frame(
            molec = molec,
            cmt = cmt,
            stringsAsFactors = FALSE
        ),
        class = c("States", "data.frame")
    )
}

#' @export
as.data.frame.States <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' @export
length.States <- function(x) {
    nrow(as.data.frame(x))
}

#' @export
c.States <- function(...) {
    objs <- list(...)
    if (!all(vapply(objs, inherits, logical(1), what = "States"))) {
        stop("All inputs must be of class 'States'.", call. = FALSE)
    }
    structure(
        do.call(rbind, lapply(objs, as.data.frame)),
        class = c("States", "data.frame")
    )
}

#' @export
`[.States` <- function(x, i, ...) .subset_df_like(x, i, byname = FALSE)

#' @export
`[[.States` <- function(x, i, ...) .extract_df_like(x, i)

#' @export
print.States <- function(x, ...) {
    if (length(x) > 0) {
        molec <- ifelse(is.na(x$molec), "<all molec>", x$molec)
        cmt <- ifelse(is.na(x$cmt), "<all cmt>", x$cmt)
        state_str <- paste0(molec, "[", cmt, "]")
        cat(" States:\n")
        cat(
            sprintf("  (%s) %s\n", seq_along(x), state_str),
            sep = ""
        )
    } else {
        cat(" States: (none)\n")
    }
    invisible(x)
}

.empty_reaction_participants <- function() {
    data.frame(
        role = character(),
        molec = character(),
        cmt = character(),
        stoich = numeric(),
        stringsAsFactors = FALSE
    )
}

.normalize_reaction_participants <- function(participants) {
    if (nrow(participants) == 0) return(participants)

    key <- paste(participants$role, participants$molec, participants$cmt, sep = "\r")
    idx <- !duplicated(key)
    out <- participants[idx, c("role", "molec", "cmt", "stoich")]
    out$stoich <- as.numeric(rowsum(participants$stoich, key, reorder = FALSE)[key[idx], 1])
    rownames(out) <- NULL
    out
}

.states_to_reaction_participants <- function(x, role) {
    if (inherits(x, "States")) {
        states <- as.data.frame(x)
        return(data.frame(
            role = role,
            molec = states$molec,
            cmt = states$cmt,
            stoich = rep(1, nrow(states)),
            stringsAsFactors = FALSE
        ))
    }

    if (is.null(x) || all(x == "") || length(x) == 0) {
        return(.empty_reaction_participants())
    }

    data.frame(
        role = role,
        molec = x,
        cmt = NA_character_,
        stoich = rep(1, length(x)),
        stringsAsFactors = FALSE
    )
}

.participants_to_molecules <- function(participants, role, repeat_stoich = FALSE) {
    role_participants <- participants[participants$role == role, , drop = FALSE]
    if (!repeat_stoich) return(role_participants$molec)

    rep(role_participants$molec, role_participants$stoich)
}

.participants_to_states <- function(participants, role, repeat_stoich = FALSE) {
    role_participants <- participants[participants$role == role, , drop = FALSE]
    if (nrow(role_participants) == 0) return(character(0))

    if (repeat_stoich) {
        idx <- rep(seq_len(nrow(role_participants)), role_participants$stoich)
        role_participants <- role_participants[idx, , drop = FALSE]
    }

    state(molec = role_participants$molec, cmt = role_participants$cmt)
}

.participants_to_rate_terms <- function(participants) {
    input <- participants[participants$role == "input", , drop = FALSE]
    if (nrow(input) == 0) return(list())

    unlist(
        lapply(seq_len(nrow(input)), function(i) {
            term <- if (is.na(input$cmt[[i]])) {
                paste0("c[", input$molec[[i]], "]")
            } else {
                paste0("c[", input$molec[[i]], ",", input$cmt[[i]], "]")
            }
            rep(list(.as_call(term)), input$stoich[[i]])
        }),
        recursive = FALSE
    )
}

.is_syntactic_model_name <- function(x) {
    is.character(x) &&
        length(x) == 1 &&
        !is.na(x) &&
        nzchar(x) &&
        identical(make.names(x), x)
}

.parse_reaction_formula_side <- function(x, cmt = NA_character_) {
    x <- trimws(x)
    if (identical(x, "") || identical(x, "NULL")) {
        return(list(
            value = character(0),
            has_localization = FALSE
        ))
    }

    tokens <- trimws(strsplit(x, "+", fixed = TRUE)[[1]])
    if (grepl("^\\s*\\+|\\+\\s*$|\\+\\s*\\+", x) || any(!nzchar(tokens))) {
        stop("Reaction formula contains an empty participant.", call. = FALSE)
    }
    if (any(tokens == "NULL")) {
        stop("'NULL' can only be used as an empty reaction side.", call. = FALSE)
    }

    parsed <- lapply(tokens, function(token) {
        count_fixed <- function(pattern) {
            matches <- gregexpr(pattern, token, fixed = TRUE)[[1]]
            if (identical(matches[[1]], -1L)) 0L else length(matches)
        }
        n_open <- count_fixed("[")
        n_close <- count_fixed("]")
        if (n_open != n_close || n_open > 1) {
            stop("Reaction formula participants must be of the form 'molec' or 'molec[cmt]'.", call. = FALSE)
        }

        if (n_open == 1) {
            open <- regexpr("[", token, fixed = TRUE)[[1]]
            close <- regexpr("]", token, fixed = TRUE)[[1]]
            if (close != nchar(token)) {
                stop("Reaction formula participants must be of the form 'molec' or 'molec[cmt]'.", call. = FALSE)
            }
            molec <- trimws(substr(token, 1, open - 1))
            cmt_i <- trimws(substr(token, open + 1, close - 1))
        } else {
            molec <- trimws(token)
            cmt_i <- NA_character_
        }

        if (!.is_syntactic_model_name(molec)) {
            stop("Reaction formula molecule names must be syntactic R names.", call. = FALSE)
        }
        if (!is.na(cmt_i) && !.is_syntactic_model_name(cmt_i)) {
            stop("Reaction formula compartment names must be syntactic R names.", call. = FALSE)
        }

        list(molec = molec, cmt = cmt_i)
    })

    molec <- vapply(parsed, `[[`, character(1), "molec")
    cmt_parsed <- vapply(parsed, `[[`, character(1), "cmt")
    has_localization <- any(!is.na(cmt_parsed))

    if (has_localization) {
        if (length(cmt) > 1 && any(!is.na(cmt))) {
            stop("Argument 'cmt' must be scalar when used to fill localized reaction formulas.", call. = FALSE)
        }
        if (length(cmt) == 1 && !is.na(cmt)) {
            cmt_parsed[is.na(cmt_parsed)] <- cmt
        }
        return(list(
            value = state(molec = molec, cmt = cmt_parsed),
            has_localization = TRUE
        ))
    }

    list(
        value = molec,
        has_localization = FALSE
    )
}

.parse_reaction_formula <- function(formula, cmt = NA_character_) {
    if (!is.character(formula) || length(formula) != 1) {
        stop("Reaction formula must be a character scalar.", call. = FALSE)
    }

    parts <- strsplit(formula, "->", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
        stop("Reaction formula must contain exactly one '->'.", call. = FALSE)
    }

    input <- .parse_reaction_formula_side(parts[[1]], cmt = cmt)
    output <- .parse_reaction_formula_side(parts[[2]], cmt = cmt)
    uses_states <- input$has_localization || output$has_localization

    if (uses_states) {
        if (!inherits(input$value, "States") && length(input$value) > 0) {
            input$value <- state(molec = input$value, cmt = cmt)
        }
        if (!inherits(output$value, "States") && length(output$value) > 0) {
            output$value <- state(molec = output$value, cmt = cmt)
        }
    }

    list(
        input = input$value,
        output = output$value,
        cmt = if (uses_states) NA_character_ else cmt
    )
}

.reaction_formula_arg <- function(x, i) {
    if (is.null(x)) return(NULL)
    if (length(x) == 0) return(x)
    if (length(x) == 1) return(x)
    x[[i]]
}

#' Create a `Reactions` object
#'
#' The `Reactions` class can hold several reactions, each with an input, output, rate/constant, and compartment.
#' However, each reaction needs to be defined separately, i.e., vectorized definitions are not supported for reactions 
#' as they can be for transports and molecules -- vector arguments are instead used for defining multiple reacting species within a single reaction.
#'
#' Reaction rates are concentration-change rates. For ODE export, reaction rates are multiplied by the reaction
#' compartment volume to obtain amount-change rates for the ODE states. This follows the usual mass-action
#' convention: zero-order reaction constants have units amount/volume/time, first-order constants have units
#' 1/time, and second-order constants have units volume/amount/time.
#' 
#' The special expression `{cmt}` can be used in `rate` or `const` to automatically substitute the compartment name, 
#' e.g., `k_{cmt}` will become `k_cytoplasm` for reactions in the cytoplasm and `k_nucleus` for reactions in the nucleus.
#' Replacement is only applied if the `cmt` argument is provided (otherwise, parameter names would change dynamically as compartments change).  
#' 
#' @param input Character vector or `States` object representing the input of the reaction, e.g. `c("A", "B")` 
#'   for a reaction where one molecule of `A` and one molecule of `B` are consumed. 
#'   For synthesis reactions, use an empty character vector or `NULL`.
#' @param output Character vector or `States` object representing the output of the reaction, e.g. `"C"` 
#'   for a reaction where one molecule of `C` is produced. 
#'   For degradation reactions, use an empty character vector or `NULL`.
#' @param cmt Character vector of compartment names where the reaction(s) occur (optional, default: all compartments)
#' @param scale_cmt Compartment whose size scales concentration-change reaction rates to amount-change rates.
#'   Inferred for same-compartment reactions and elementary reactions with a single input compartment.
#'   Required for other cross-compartment reactions.
#' @param formula Character scalar or vector with reaction formulas such as `"A + B -> C"` or
#'   `"L[plasma] + R[membrane] -> LR[membrane]"`. If `output` is missing and `input`
#'   contains `->`, `input` is interpreted as `formula`.
#' @param ... Errors if used, enforces `rate` and `const` to be specified as named arguments only, not positional.
#' @param rate Character string representing the concentration-change reaction rate (for nonlinear reactions).
#'   Use `c[A]` to refer to the concentration of molecule A, and `a[A]` to refer to its amount.
#' @param const Character string representing the reaction constant (for mass-action reactions).
#'   Formally, this corresponds to `rate = const * c[input[1]] * c[input[2]] * ...`. 
#'   In addition, the information that the reaction is elementary is encoded in the `type` 
#'   column of the resulting `Reactions` object.
#' @details
#' Reactions can be specified either by separate `input` and `output`
#' arguments or by a character reaction formula. The formula form is an
#' interactive shorthand for the same participant representation, for example
#' `reactions("A + B -> C", cmt = "cyt", const = "k")`. Participants may be
#' localized directly in the formula with `molec[cmt]`, as in
#' `reactions("L[plasma] + R[membrane] -> LR[membrane]", scale_cmt = "membrane", const = "kon")`.
#' If a formula participant has no compartment, `cmt` fills it when supplied;
#' otherwise the participant keeps the same wildcard compartment meaning as the
#' separate `input` and `output` character shorthand.
#'
#' Cross-compartment reactions are reactions whose participants are localized
#' in more than one compartment. Reaction rates are always interpreted as
#' concentration-change rates with respect to one reaction scaling compartment.
#' During ODE export, the rate is multiplied by the size of `scale_cmt` to
#' obtain amount-change rates. For ordinary same-compartment reactions,
#' `scale_cmt` is inferred as the shared compartment. For elementary
#' cross-compartment reactions with a single input compartment, it is inferred
#' as that input compartment. Other cross-compartment reactions require an
#' explicit `scale_cmt`, which must name one of the compartments involved in the
#' reaction. In membrane binding models, for example, choosing the membrane as
#' `scale_cmt` means that the reaction rate has membrane concentration units
#' such as amount per area per time.
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
#' # Interactive character formula shorthand
#' reactions("A + B -> C", cmt = "cytoplasm", const = "kABC")
#' # Cross-compartment formula with explicit reaction scaling compartment
#' reactions(
#'     "L[plasma] + R[membrane] -> LR[membrane]",
#'     scale_cmt = "membrane",
#'     const = "kon"
#' )
#' # Source and sink reactions in formula syntax
#' reactions("NULL -> A[cytoplasm]", const = "ksyn")
#' reactions("A[cytoplasm] -> NULL", const = "kdeg")
#' 
#' @export
reactions <- function(
    input = character(0),
    output = character(0),
    cmt = NA_character_,
    scale_cmt = NA_character_,
    formula = NULL,
    ...,
    rate = NULL,
    const = NULL
) {

    output_missing <- missing(output)
    formula_missing <- missing(formula)
    if (formula_missing &&
        output_missing &&
        is.character(input) &&
        any(grepl("->", input, fixed = TRUE))) {
        formula <- input
    }
    formula_active <- !is.null(formula)

    # Error if any additional positional arguments are provided (enforces named arguments for rate and const)
    if (length(list(...)) > 0) {
        stop(
            "Additional arguments are not allowed. Please specify 'rate' and 'const' as named arguments only, not positional.",
            call. = FALSE
        )
    }

    if (formula_active) {
        if (!output_missing) {
            stop("Arguments 'formula' and 'output' cannot be used together.", call. = FALSE)
        }
        if (!is.character(formula)) {
            stop("Reaction formula must be provided as a character string.", call. = FALSE)
        }

        if (length(formula) > 1) {
            nFormula <- length(formula)
            lengths <- c(length(cmt), length(scale_cmt), length(rate), length(const))
            if (!all(lengths %in% c(0, 1, nFormula))) {
                stop("Formula, cmt, scale_cmt, rate, and const must be scalar or have the same length.", call. = FALSE)
            }

            out <- lapply(seq_len(nFormula), function(i) {
                reactions(
                    formula = formula[[i]],
                    cmt = .reaction_formula_arg(cmt, i),
                    scale_cmt = .reaction_formula_arg(scale_cmt, i),
                    rate = .reaction_formula_arg(rate, i),
                    const = .reaction_formula_arg(const, i)
                )
            })
            return(do.call(what = "c", args = out) %||% reactions())
        }

        parsed <- .parse_reaction_formula(formula, cmt = cmt)
        return(reactions(
            input = parsed$input,
            output = parsed$output,
            cmt = parsed$cmt,
            scale_cmt = scale_cmt,
            rate = rate,
            const = const
        ))
    }

    # Early return for empty reactions
    if ((length(input) == 0 && length(output) == 0) || length(cmt) == 0) {
        return(
            structure(
                data.frame(
                    rate = I(list()),
                    const = I(list()),
                    type = character(),
                    scale_cmt = character(),
                    participants = I(list()),
                    stringsAsFactors = FALSE
                ),
                class = "Reactions"
            )
        )
    }

    uses_states <- inherits(input, "States") || inherits(output, "States")
    if (uses_states && !all(is.na(cmt))) {
        stop("Argument 'cmt' cannot be used with state() reaction participants.", call. = FALSE)
    }

    input_participants <- .states_to_reaction_participants(input, "input")
    output_participants <- .states_to_reaction_participants(output, "output")
    base_participants <- rbind(input_participants, output_participants)

    # Input lengths
    nRate <- length(rate)
    nConst <- length(const)
    nReact <- if (uses_states) 1 else length(cmt)

    # Check that all inputs are either NULL, scalar or vector of the same length
    if (!all(c(nRate, nConst, length(scale_cmt)) %in% c(0, 1, nReact))) {
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
    order <- sum(input_participants$stoich)

    # If rate/const is scalar and cmt is provided, apply special substitution rule
    replace_pattern <- function(x) {
        lapply(cmt, function(y) if (is.na(y)) x else gsub(pattern = "{cmt}", replacement = y, x = x, fixed = TRUE))
    }

    reaction_participants <- lapply(seq_len(nReact), function(i) {
        participants <- base_participants
        if (!uses_states && nrow(participants) > 0 && !is.na(cmt[[i]])) {
            participants$cmt <- ifelse(is.na(participants$cmt), cmt[[i]], participants$cmt)
        }
        .normalize_reaction_participants(participants)
    })

    involved_cmt <- lapply(reaction_participants, function(p) unique(p$cmt[!is.na(p$cmt)]))
    if (length(scale_cmt) == 0) scale_cmt <- NA_character_
    if (length(scale_cmt) == 1) scale_cmt <- rep(scale_cmt, nReact)
    infer_scale <- is.na(scale_cmt)
    for (i in seq_len(nReact)) {
        input_cmt <- unique(reaction_participants[[i]]$cmt[
            reaction_participants[[i]]$role == "input" &
                !is.na(reaction_participants[[i]]$cmt)
        ])
        if (infer_scale[[i]] && length(involved_cmt[[i]]) == 1) {
            scale_cmt[[i]] <- involved_cmt[[i]]
        }
        if (infer_scale[[i]] &&
            identical(type, "elementary") &&
            length(involved_cmt[[i]]) > 1 &&
            length(input_cmt) == 1) {
            scale_cmt[[i]] <- input_cmt
        }
        if (length(involved_cmt[[i]]) > 1 && is.na(scale_cmt[[i]])) {
            stop("Argument 'scale_cmt' is required for cross-compartment reactions with no unique input compartment.", call. = FALSE)
        }
        if (!is.na(scale_cmt[[i]]) && !(scale_cmt[[i]] %in% involved_cmt[[i]])) {
            stop("Argument 'scale_cmt' must name a compartment involved in the reaction.", call. = FALSE)
        }
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
            if (length(rate) == 1 && nReact > 1) rate <- rep(rate, nReact)
            if (!uses_states && !all(is.na(cmt))) {
                rate <- Map(f = .add_expr_index, expr = rate, pos = 2, val = cmt)
            }
            const <- rep(list(NULL), nReact)
        },
        elementary = {
            if (nConst == 1 && is.character(const)) {
                const <- replace_pattern(const)
            }
            const <- lapply(const, .as_call)
            if (length(const) == 1 && nReact > 1) const <- rep(const, nReact)

            rate <- Map(
                function(k, p) Reduce(.mul, c(list(k), .participants_to_rate_terms(p))),
                const,
                reaction_participants
            )

        }

    )

    # Construct the Reactions object as a data frame with class "Reactions"
    return(
        structure(
            data.frame(
                rate = I(rate),
                const = I(const),
                type = type,
                scale_cmt = scale_cmt,
                participants = I(reaction_participants),
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
    scale_cmt = NA_character_,
    formula = NULL,
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

#' Subset a `Reactions` object
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

        format_side <- function(participants, role) {
            side <- participants[participants$role == role, , drop = FALSE]
            if (nrow(side) == 0) return("\u2205")

            state_str <- ifelse(
                is.na(side$cmt),
                side$molec,
                paste0(side$molec, "[", side$cmt, "]")
            )
            ifelse(side$stoich == 1, state_str, paste0(side$stoich, "*", state_str)) |>
                paste0(collapse = "+")
        }

        in_str <- vapply(x$participants, format_side, character(1), role = "input")
        out_str <- vapply(x$participants, format_side, character(1), role = "output")
        rate_str <- vapply(x$rate, function(r) paste(deparse(r), collapse = ""), character(1))
        location_str <- vapply(x$participants, function(participants) {
            cmt <- unique(participants$cmt[!is.na(participants$cmt)])
            if (length(cmt) == 0) " (<all cmt>)" else ""
        }, character(1))
        show_scale <- vapply(seq_along(x$participants), function(i) {
            participants <- x$participants[[i]]
            involved_cmt <- unique(participants$cmt[!is.na(participants$cmt)])
            input_cmt <- unique(participants$cmt[
                participants$role == "input" & !is.na(participants$cmt)
            ])

            length(involved_cmt) > 1 &&
                !(identical(x$type[[i]], "elementary") && length(input_cmt) == 1)
        }, logical(1))
        scale_str <- ifelse(show_scale, paste0(", scale = ", x$scale_cmt), "")

        cat(" Reactions:\n")
        cat(
            sprintf(
                "  (%i) %s \u2192 %s%s%s, rate = %s\n",
                seq_along(x),
                in_str,
                out_str,
                location_str,
                scale_str,
                rate_str
            ),
            sep = ""
        )
    } else {
        cat(" Reactions: (none)\n")
    }

    invisible(x)
}

#' Convert a `Reactions` object to a list of lists, where each inner list represents a reaction with its properties
#' 
#' @param x A `Reactions` object
#' @param ... Additional arguments (not used)
#' @return A list of lists, where each inner list represents a reaction with its properties
#' @export
as.list.Reactions <- function(x, ...) .listify_df_like(x)
