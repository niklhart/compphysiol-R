#' Create a `compphysiol_states` object
#'
#' `compphysiol_states` represent molecule-compartment pairs. In reactions, repeated states
#' are interpreted as stoichiometric multiplicity during reaction construction.
#'
#' @param ... Unused. `molec` and `cmt` must be named explicitly.
#' @param molec Character vector of molecule names.
#' @param cmt Character vector of compartment names.
#' @return A `compphysiol_states` object.
#' @seealso [inspect] for printing, structure inspection, and component access.
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

    .new_states(data.frame(
            molec = molec,
            cmt = cmt,
            stringsAsFactors = FALSE
        ))
}

.new_states <- function(x) {
    structure(x, class = .compphysiol_class(
        "compphysiol_states", base = "data.frame"
    ))
}

#' @export
as.data.frame.compphysiol_states <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' @export
length.compphysiol_states <- function(x) {
    nrow(as.data.frame(x))
}

#' @export
c.compphysiol_states <- function(...) {
    objs <- list(...)
    if (!all(vapply(objs, inherits, logical(1), what = "compphysiol_states"))) {
        stop("All inputs must be of class 'states'.", call. = FALSE)
    }
    .new_states(do.call(rbind, lapply(objs, as.data.frame)))
}

#' @export
`[.compphysiol_states` <- function(x, i, ...) {
    .new_states(.subset_df_like(x, i, byname = FALSE))
}

#' @export
`[[.compphysiol_states` <- function(x, i, ...) .extract_df_like(x, i)

#' @export
print.compphysiol_states <- function(x, ...) {
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
    if (inherits(x, "compphysiol_states")) {
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

.participants_to_amount_rate_terms <- function(participants, scale_cmt = NA_character_) {
    input <- participants[participants$role == "input", , drop = FALSE]
    if (nrow(input) == 0) return(list())

    scale_idx <- if (is.na(scale_cmt)) {
        1L
    } else {
        which(!is.na(input$cmt) & input$cmt == scale_cmt)[[1]]
    }

    unlist(
        lapply(seq_len(nrow(input)), function(i) {
            lapply(seq_len(input$stoich[[i]]), function(j) {
                prefix <- if (i == scale_idx && j == 1L) "a" else "c"
                term <- if (is.na(input$cmt[[i]])) {
                    paste0(prefix, "[", input$molec[[i]], "]")
                } else {
                    paste0(prefix, "[", input$molec[[i]], ",", input$cmt[[i]], "]")
                }
                .as_call(term)
            })
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

.parse_reaction_scheme_side <- function(x, cmt = NA_character_) {
    x <- trimws(x)
    if (identical(x, "") || identical(x, "NULL")) {
        return(list(
            value = character(0),
            has_localization = FALSE
        ))
    }

    tokens <- trimws(strsplit(x, "+", fixed = TRUE)[[1]])
    if (grepl("^\\s*\\+|\\+\\s*$|\\+\\s*\\+", x) || any(!nzchar(tokens))) {
        stop("Reaction scheme contains an empty participant.", call. = FALSE)
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
            stop("Reaction scheme participants must be of the form 'molec' or 'molec[cmt]'.", call. = FALSE)
        }

        if (n_open == 1) {
            open <- regexpr("[", token, fixed = TRUE)[[1]]
            close <- regexpr("]", token, fixed = TRUE)[[1]]
            if (close != nchar(token)) {
                stop("Reaction scheme participants must be of the form 'molec' or 'molec[cmt]'.", call. = FALSE)
            }
            molec <- trimws(substr(token, 1, open - 1))
            cmt_i <- trimws(substr(token, open + 1, close - 1))
        } else {
            molec <- trimws(token)
            cmt_i <- NA_character_
        }

        if (!.is_syntactic_model_name(molec)) {
            stop("Reaction scheme molecule names must be syntactic R names.", call. = FALSE)
        }
        if (!is.na(cmt_i) && !.is_syntactic_model_name(cmt_i)) {
            stop("Reaction scheme compartment names must be syntactic R names.", call. = FALSE)
        }

        list(molec = molec, cmt = cmt_i)
    })

    molec <- vapply(parsed, `[[`, character(1), "molec")
    cmt_parsed <- vapply(parsed, `[[`, character(1), "cmt")
    has_localization <- any(!is.na(cmt_parsed))

    if (has_localization) {
        if (length(cmt) > 1 && any(!is.na(cmt))) {
            stop("Argument 'cmt' must be scalar when used to fill localized reaction schemes.", call. = FALSE)
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

.parse_reaction_scheme <- function(scheme, cmt = NA_character_) {
    if (!is.character(scheme) || length(scheme) != 1) {
        stop("Reaction scheme must be a character scalar.", call. = FALSE)
    }

    parts <- strsplit(scheme, "->", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
        stop("Reaction scheme must contain exactly one '->'.", call. = FALSE)
    }

    input <- .parse_reaction_scheme_side(parts[[1]], cmt = cmt)
    output <- .parse_reaction_scheme_side(parts[[2]], cmt = cmt)
    uses_states <- input$has_localization || output$has_localization

    if (uses_states) {
        if (!inherits(input$value, "compphysiol_states") && length(input$value) > 0) {
            input$value <- state(molec = input$value, cmt = cmt)
        }
        if (!inherits(output$value, "compphysiol_states") && length(output$value) > 0) {
            output$value <- state(molec = output$value, cmt = cmt)
        }
    }

    list(
        input = input$value,
        output = output$value,
        cmt = if (uses_states) NA_character_ else cmt
    )
}

.reaction_scheme_arg <- function(x, i) {
    if (is.null(x)) return(NULL)
    if (length(x) == 0) return(x)
    if (length(x) == 1) return(x)
    x[[i]]
}

#' Create a `compphysiol_reactions` object
#'
#' The `compphysiol_reactions` class can hold several reactions, each with an input, output, rate/constant, and compartment.
#' However, each reaction needs to be defined separately, i.e., vectorized definitions are not supported for reactions 
#' as they can be for transports and molecules -- vector arguments are instead used for defining multiple reacting species within a single reaction.
#'
#' Reaction rates may be represented as concentration-change or amount-change
#' rates. For mass-action reactions specified with `const`, `rate_type` selects
#' how the generated rate expression is represented; it does not change the
#' interpretation of the mass-action constant.
#' 
#' The special expression `{cmt}` can be used in `rate` or `const` to automatically substitute the compartment name, 
#' e.g., `k_{cmt}` will become `k_cytoplasm` for reactions in the cytoplasm and `k_nucleus` for reactions in the nucleus.
#' Replacement is only applied if the `cmt` argument is provided (otherwise, parameter names would change dynamically as compartments change).  
#' 
#' @param input Character vector or `compphysiol_states` object representing the input of the reaction, e.g. `c("A", "B")` 
#'   for a reaction where one molecule of `A` and one molecule of `B` are consumed. 
#'   For synthesis reactions, use an empty character vector or `NULL`.
#' @param output Character vector or `compphysiol_states` object representing the output of the reaction, e.g. `"C"` 
#'   for a reaction where one molecule of `C` is produced. 
#'   For degradation reactions, use an empty character vector or `NULL`.
#' @param cmt Character vector of compartment names where the reaction(s) occur (optional, default: all compartments)
#' @param scale_cmt Reaction scaling compartment. Its size converts a
#'   concentration-change rate to an amount-change rate. For amount-rate
#'   mass-action reactions, it selects the first reactant in this compartment
#'   to represent as an amount. Inferred where possible.
#' @param rate_type Representation of the reaction rate: `"concentration"` or
#'   `"amount"`. Required with `rate`; optional with `const`, where it defaults
#'   to concentration for one involved compartment and amount for several.
#' @param formula Deprecated alias for `scheme`.
#' @param scheme Character scalar or vector with reaction schemes such as `"A + B -> C"` or
#'   `"L[plasma] + R[membrane] -> LR[membrane]"`. If `output` is missing and `input`
#'   contains `->`, `input` is interpreted as `scheme`.
#' @param ... For `reactions()`, errors if used: it enforces `rate` and `const` to
#'   be specified as named arguments only, not positional. For `c()`,
#'   `compphysiol_reactions` objects to combine; unused by the other methods.
#' @param rate Character string representing an explicit reaction rate. Its
#'   representation is declared by `rate_type`.
#'   Use `c[A]` to refer to the concentration of molecule A, and `a[A]` to refer to its amount.
#' @param const Reaction constant for mass-action reactions (character string,
#'   call, numeric, or unit-bearing value).
#'   Formally, this corresponds to `rate = const * c[input[1]] * c[input[2]] * ...`. 
#'   In addition, the information that the reaction is elementary is encoded in the `type` 
#'   column of the resulting `compphysiol_reactions` object.
#' @param x For methods, a `compphysiol_reactions` object.
#' @param i For subsetting methods, indices or names of the reactions.
#' @details
#' compphysiol_reactions can be specified either by separate `input` and `output`
#' arguments or by a character reaction scheme. The scheme form is an
#' interactive shorthand for the same participant representation, for example
#' `reactions("A + B -> C", cmt = "cyt", const = "k")`. Participants may be
#' localized directly in the scheme with `molec[cmt]`, as in
#' `reactions("L[plasma] + R[membrane] -> LR[membrane]", scale_cmt = "membrane", const = "kon")`.
#' If a scheme participant has no compartment, `cmt` fills it when supplied;
#' otherwise the participant keeps the same wildcard compartment meaning as the
#' separate `input` and `output` character shorthand.
#'
#' Cross-compartment reactions are reactions whose participants are localized
#' in more than one compartment. With an explicit `rate`, `rate_type` declares
#' whether that expression has concentration/time or amount/time units.
#' Cross-compartment concentration rates require `scale_cmt`; explicit amount
#' rates must not supply it.
#'
#' With `const`, `rate_type` instead selects the representation generated from
#' the mass-action law. Concentration rates use concentration factors for every
#' reactant. Amount rates replace one concentration factor with an amount
#' factor. `scale_cmt` selects the first reactant in that compartment for this
#' replacement. It is inferred only when all reactants occur in one compartment;
#' otherwise it is required regardless of `rate_type`. Cross-compartment `const`
#' reactions default to amount rates, while same-compartment reactions default
#' to concentration rates. Amount-rate reactions require all participant states
#' to be convertible to amounts during ODE export.
#' @return A `compphysiol_reactions` object containing the reaction information.
#' @examples
#' # empty `compphysiol_reactions` object
#' reactions()
#' # association reaction (second-order)
#' reactions(input = c("A", "B"), output = "C", const = "k1")
#' # synthesis reaction (zero-order)
#' reactions(input = "", output = "A", const = "k2")
#' # Michelis-Menten reaction (saturable, non-elementary)
#' reactions(input = "A", output = "B", rate_type = "concentration",
#'     rate = "Vmax * c[A] / (Km + c[A])")
#' # compphysiol_reactions in several compartments (same rate constants)
#' reactions(input = "A", output = "B", cmt = c("cytoplasm", "nucleus"), const = "kAB")
#' # compphysiol_reactions in several compartments (different rate constants)
#' reactions(input = "A", output = "B", cmt = c("cytoplasm", "nucleus"), const = "kAB_{cmt}")
#' # Interactive character scheme shorthand
#' reactions("A + B -> C", cmt = "cytoplasm", const = "kABC")
#' # Cross-compartment scheme with explicit reaction scaling compartment
#' reactions(
#'     "L[plasma] + R[membrane] -> LR[membrane]",
#'     scale_cmt = "membrane",
#'     const = "kon"
#' )
#' # Source and sink reactions in scheme syntax
#' reactions("NULL -> A[cytoplasm]", const = "ksyn")
#' reactions("A[cytoplasm] -> NULL", const = "kdeg")
#' 
#' @seealso [add_reaction()] for adding reactions to a model and
#'   [modify_compartment_model] for the complete component map.
#' @seealso [inspect] for printing, structure inspection, and component access.
#' @export
reactions <- function(
    input = character(0),
    output = character(0),
    cmt = NA_character_,
    scale_cmt = NA_character_,
    rate_type = NULL,
    scheme = NULL,
    ...,
    formula = NULL,
    rate = NULL,
    const = NULL
) {
    const_expr <- substitute(const)
    if (.dsl_has_state_ref(const_expr)) {
        stop(
            "Argument 'const' cannot contain state references such as 'a[...]' or 'c[...]'. ",
            "Use 'rate' for state-dependent reaction expressions.",
            call. = FALSE
        )
    }
    const <- .process_unit_shorthand_arg(
        expr = const_expr,
        value = const,
        envir = parent.frame(n = 1)
    )

    if (!missing(formula)) {
        warning("Argument 'formula' is deprecated; use 'scheme' instead.", call. = FALSE)
        if (!missing(scheme)) {
            stop("Arguments 'scheme' and 'formula' cannot be used together.", call. = FALSE)
        }
        scheme <- formula
    }

    output_missing <- missing(output)
    scheme_missing <- missing(scheme) && missing(formula)
    if (scheme_missing &&
        output_missing &&
        is.character(input) &&
        any(grepl("->", input, fixed = TRUE))) {
        scheme <- input
    }
    scheme_active <- !is.null(scheme)

    # Error if any additional positional arguments are provided (enforces named arguments for rate and const)
    if (length(list(...)) > 0) {
        stop(
            "Additional arguments are not allowed. Please specify 'rate' and 'const' as named arguments only, not positional.",
            call. = FALSE
        )
    }

    if (scheme_active) {
        if (!output_missing) {
            stop("Arguments 'scheme' and 'output' cannot be used together.", call. = FALSE)
        }
        if (!is.character(scheme)) {
            stop("Reaction scheme must be provided as a character string.", call. = FALSE)
        }

        if (length(scheme) > 1) {
            nScheme <- length(scheme)
            lengths <- c(length(cmt), length(scale_cmt), length(rate_type), length(rate), length(const))
            if (!all(lengths %in% c(0, 1, nScheme))) {
                stop("Scheme, cmt, scale_cmt, rate_type, rate, and const must be scalar or have the same length.", call. = FALSE)
            }

            out <- lapply(seq_len(nScheme), function(i) {
                reactions(
                    scheme = scheme[[i]],
                    cmt = .reaction_scheme_arg(cmt, i),
                    scale_cmt = .reaction_scheme_arg(scale_cmt, i),
                    rate_type = .reaction_scheme_arg(rate_type, i),
                    rate = .reaction_scheme_arg(rate, i),
                    const = .reaction_scheme_arg(const, i)
                )
            })
            return(do.call(what = "c", args = out) %||% reactions())
        }

        parsed <- .parse_reaction_scheme(scheme, cmt = cmt)
        return(reactions(
            input = parsed$input,
            output = parsed$output,
            cmt = parsed$cmt,
            scale_cmt = scale_cmt,
            rate_type = rate_type,
            rate = rate,
            const = const
        ))
    }

    # Early return for empty reactions
    if ((length(input) == 0 && length(output) == 0) || length(cmt) == 0) {
        return(.new_reactions(
                data.frame(
                    rate = I(list()),
                    const = I(list()),
                    type = character(),
                    scale_cmt = character(),
                    rate_type = character(),
                    rate_type_auto = logical(),
                    participants = I(list()),
                    stringsAsFactors = FALSE
                )
        ))
    }

    uses_states <- inherits(input, "compphysiol_states") ||
        inherits(output, "compphysiol_states")
    if (uses_states && !all(is.na(cmt))) {
        stop("Argument 'cmt' cannot be used with state() reaction participants.", call. = FALSE)
    }

    input_participants <- .states_to_reaction_participants(input, "input")
    output_participants <- .states_to_reaction_participants(output, "output")
    base_participants <- rbind(input_participants, output_participants)

    # Input lengths
    nRate <- length(.as_expr_arg_list(rate))
    nConst <- length(.as_expr_arg_list(const))
    nReact <- if (uses_states) 1 else length(cmt)

    # Check that all inputs are either NULL, scalar or vector of the same length
    if (!all(c(nRate, nConst, length(scale_cmt), length(rate_type)) %in% c(0, 1, nReact))) {
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

    rate_type_auto <- is.null(rate_type)
    if (!rate_type_auto) {
        if (!is.character(rate_type) || any(!rate_type %in% c("concentration", "amount"))) {
            stop("Argument 'rate_type' must be 'concentration' or 'amount'.", call. = FALSE)
        }
    } else if (!is.null(rate)) {
        stop("Argument 'rate_type' is required when 'rate' is supplied.", call. = FALSE)
    }

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
    has_unresolved_cmt <- vapply(reaction_participants, function(p) any(is.na(p$cmt)), logical(1))
    if (rate_type_auto) {
        rate_type <- vapply(seq_len(nReact), function(i) {
            if (has_unresolved_cmt[[i]]) "concentration"
            else if (length(involved_cmt[[i]]) <= 1) "concentration"
            else "amount"
        }, character(1))
    } else if (length(rate_type) == 1) {
        rate_type <- rep(rate_type, nReact)
    }
    if (length(scale_cmt) == 0) scale_cmt <- NA_character_
    if (length(scale_cmt) == 1) scale_cmt <- rep(scale_cmt, nReact)
    infer_scale <- is.na(scale_cmt)
    for (i in seq_len(nReact)) {
        p <- reaction_participants[[i]]
        input_cmt_all <- p$cmt[p$role == "input"]
        input_cmt <- unique(input_cmt_all[!is.na(input_cmt_all)])
        effective_type <- rate_type[[i]]

        # Wildcard localization can change the default representation and scale;
        # finalize both when wire() reconstructs the resolved reaction.
        if (rate_type_auto && has_unresolved_cmt[[i]]) next

        if (identical(type, "complex") && identical(effective_type, "amount") && !is.na(scale_cmt[[i]])) {
            stop("Argument 'scale_cmt' cannot be used with an explicit amount rate.", call. = FALSE)
        }

        if (identical(effective_type, "concentration")) {
            if (infer_scale[[i]] && identical(type, "elementary") && length(input_cmt) == 1) {
                scale_cmt[[i]] <- input_cmt
            } else if (infer_scale[[i]] && length(involved_cmt[[i]]) == 1) {
                scale_cmt[[i]] <- involved_cmt[[i]]
            }
            if (identical(type, "elementary") && length(input_cmt) > 1 && is.na(scale_cmt[[i]])) {
                stop(
                    "Argument 'scale_cmt' is required when reactants occur in multiple compartments.",
                    call. = FALSE
                )
            }
            if (length(involved_cmt[[i]]) > 1 && is.na(scale_cmt[[i]]) && !has_unresolved_cmt[[i]]) {
                stop("Argument 'scale_cmt' is required for cross-compartment concentration rates.", call. = FALSE)
            }
            if (!is.na(scale_cmt[[i]]) && !(scale_cmt[[i]] %in% involved_cmt[[i]])) {
                stop("Argument 'scale_cmt' must name a compartment involved in the reaction.", call. = FALSE)
            }
        } else if (identical(type, "elementary")) {
            if (length(input_cmt_all) == 0) {
                if (!is.na(scale_cmt[[i]])) stop("Argument 'scale_cmt' is not applicable to an amount-rate synthesis reaction.", call. = FALSE)
            } else {
                if (infer_scale[[i]] && length(input_cmt) == 1) scale_cmt[[i]] <- input_cmt
                if (is.na(scale_cmt[[i]]) && !has_unresolved_cmt[[i]]) {
                    stop(
                        "Argument 'scale_cmt' is required when reactants occur in multiple compartments.",
                        call. = FALSE
                    )
                }
                if (!is.na(scale_cmt[[i]]) && !(scale_cmt[[i]] %in% input_cmt)) {
                    stop("For an amount-rate reaction, 'scale_cmt' must name an involved reactant compartment.", call. = FALSE)
                }
            }
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
            rate <- rate |> .as_expr_arg_list() |> lapply(.as_call)
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
            const <- const |> .as_const_arg_list()
            if (length(const) == 1 && nReact > 1) const <- rep(const, nReact)
            is_value <- vapply(const, .is_const_value, logical(1))
            const[!is_value] <- lapply(const[!is_value], .as_call)
            if (any(vapply(const, .dsl_has_state_ref, logical(1)))) {
                stop(
                    "Argument 'const' cannot contain state references such as 'a[...]' or 'c[...]'. ",
                    "Use 'rate' for state-dependent reaction expressions.",
                    call. = FALSE
                )
            }

            rate <- Map(function(k, p, rt, sc) {
                terms <- if (identical(rt, "amount")) {
                    .participants_to_amount_rate_terms(p, sc)
                } else {
                    .participants_to_rate_terms(p)
                }
                Reduce(.mul, c(list(k), terms))
            }, const, reaction_participants, rate_type, scale_cmt)

        }

    )

    # Construct the reactions object as a data frame with its package class.
    .new_reactions(
            data.frame(
                rate = I(rate),
                const = I(const),
                type = type,
                scale_cmt = scale_cmt,
                rate_type = rate_type,
                rate_type_auto = rep(rate_type_auto, nReact),
                participants = I(reaction_participants),
                stringsAsFactors = FALSE
            )
    )
}

.new_reactions <- function(x) {
    structure(x, class = .compphysiol_class("compphysiol_reactions"))
}

#' Add one or several reactions to a `compphysiol_compartment_model` object.
#'
#' @inherit reactions description details
#' @param model A `compphysiol_compartment_model` object.
#' @inheritParams reactions
#' @param react A `compphysiol_reactions` object. Constructed from the other inputs if not provided.
#' @returns The modified `compphysiol_compartment_model` object.
#' @seealso [reactions()] for construction and [modify_compartment_model] for
#'   other model components.
#' @export
add_reaction <- function(
    model,
    react,
    ...,
    input = character(0),
    output = character(0),
    cmt = NA_character_,
    scale_cmt = NA_character_,
    rate_type = NULL,
    scheme = NULL,
    formula = NULL,
    rate = NULL,
    const = NULL
) {
    UseMethod("add_reaction")
}

#' @rdname add_reaction
#' @export
add_reaction.compphysiol_compartment_model <- function(
    model,
    react,
    ...,
    input = character(0),
    output = character(0),
    cmt = NA_character_,
    scale_cmt = NA_character_,
    rate_type = NULL,
    scheme = NULL,
    formula = NULL,
    rate = NULL,
    const = NULL
) {
    .check_class(model, "compphysiol_compartment_model")

    call <- match.call()

    react <- .forward_or_use(
        object_arg_name = "react",
        constructor_name = "reactions",
        call = call,
        parent_env = parent.frame()
    )

    .check_class(react, "compphysiol_reactions")

    model$reactions <- c(model$reactions, react)

    model
}

#' @describeIn reactions Convert to a data frame.
#' @export
as.data.frame.compphysiol_reactions <- function(x, ...) {
    class(x) <- "data.frame"
    x
}

#' @describeIn reactions Number of reactions.
#' @export
length.compphysiol_reactions <- function(x) {
    nrow(as.data.frame(x))
}

#' @describeIn reactions Combine several objects into one.
#' @export
c.compphysiol_reactions <- function(...) {
    .new_reactions(.combine_df_like(...))
}

#' @describeIn reactions Subset by row index.
#' @export
`[.compphysiol_reactions` <- function(x, i, ...) {
    .new_reactions(.subset_df_like(x, i, byname = FALSE))
}

#' @describeIn reactions Not supported; always errors. Use `[` with a scalar name or index instead.
#' @export
`[[.compphysiol_reactions` <- function(x, i, ...) .extract_df_like(x, i)

#' @describeIn reactions Print a summary; returns `x` invisibly.
#' @export
print.compphysiol_reactions = function(x, ...) {

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
        rate_str <- vapply(x$rate, .ode_model_deparse_with_unit_placeholders, character(1))
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
                "  (%i) %s \u2192 %s%s%s, %s rate = %s\n",
                seq_along(x),
                in_str,
                out_str,
                location_str,
                scale_str,
                x$rate_type,
                rate_str
            ),
            sep = ""
        )
    } else {
        cat(" Reactions: (none)\n")
    }

    invisible(x)
}

#' @describeIn reactions Convert to a list of lists, one per reaction, holding its properties.
#' @export
as.list.compphysiol_reactions <- function(x, ...) .listify_df_like(x)
