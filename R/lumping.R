# generic lumping function

#' Lump a `CompartmentModel` object
#'
#' @param M A `CompartmentModel` object, the model to be lumped.
#' @param partitioning A list of character vectors (each a group of compartments)
#' @param normalize A named list of quoted calls giving normalizing factors for each compartment
#' @returns A `CompartmentModel` object representing the lumped model
#' @export
lump_model <- function(M, partitioning = list(), normalize = list()) {

    # --- validate model ------------------------------------------------------
    .check_class(M, "CompartmentModel")
    M <- wire(M)
    if (!.is_linear(M)) stop("Lumping is only supported for linear models.")
    if (length(M$reactions) > 0) {
        stop("Lumping reactions is not supported yet.")
    }
    if (any(M$molecules$type != "amount")) {
        stop("Lumping currently supports amount states only.")
    }

    # --- validate partitioning ------------------------------------------------
    cmt <- names(M$compartments)
    flatpart <- unlist(partitioning)
    if (any(duplicated(flatpart))) {
        stop("Partitioning must not contain any duplicate values.")
    }
    if (length(setdiff(flatpart, cmt)) > 0) {
        stop("Some compartment name(s) in 'partitioning' were not found in the model.")
    }
    remaining <- setdiff(cmt, flatpart)

    # --- validate normalize argument ------------------------------------------
    if (!all(flatpart %in% names(normalize))) {
        stop("Normalizing factors need to be defined for every compartment to be lumped.")
    }

    # Add missing entries for non-lumped compartments (default = quote(1))
    normalize_missing <- setdiff(cmt, names(normalize))
    normalize_missing <- setNames(
        lapply(rep(1, length(normalize_missing)), function(x) quote(1)),
        normalize_missing
    )
    normalize <- append(normalize, normalize_missing)

    # --- construct VKorig as list of calls ------------------------------------
    # ensure everything is quoted expressions
    VKorig <- lapply(normalize, .as_call)
    VKorig <- VKorig[cmt]  # ensure consistent ordering
    names(VKorig) <- cmt

    # --- naming of lumped compartments ----------------------------------------
    lumped_names <- vapply(partitioning,
                           function(p) paste(p, collapse = "_"),
                           character(1))
    nm_part <- names(partitioning)
    if (!is.null(nm_part)) {
        lumped_names[nm_part != ""] <- nm_part[nm_part != ""]
    }
    new_names <- c(lumped_names, remaining)

    # Mapping: original compartment → lumped group
    grp <- setNames(
        rep(new_names,
            times = c(lengths(partitioning), rep(1, length(remaining)))),
        c(flatpart, remaining)
    )

    # --- lump initial conditions & VK ----------------------------------------
    molecule_df <- as.data.frame(M$molecules)
    molecule_df$lump_cmt <- unname(grp[molecule_df$cmt])
    state_init <- setNames(
        unlist(molecule_df$init),
        paste(molecule_df$name, molecule_df$lump_cmt, sep = "\r")
    )

    # simple summation by molecule and lumped compartment
    X0lump <- tapply(state_init, names(state_init), sum)
    X0_parts <- strsplit(names(X0lump), "\r", fixed = TRUE)
    X0_molec <- vapply(X0_parts, `[[`, character(1), 1)
    X0_cmt <- vapply(X0_parts, `[[`, character(1), 2)

    # for completeness, lumped normalizing factors (symbolic sum)
    .sum_exprs <- function(expr_list) {
        if (length(expr_list) == 0) return(quote(0))
        if (length(expr_list) == 1) return(expr_list[[1]])
        Reduce(function(a, b) bquote(.(a) + .(b)), expr_list)
    }
    VKlump <- sapply(unique(grp), function(lump_name) {
        members <- names(grp)[grp == lump_name]
        .sum_exprs(VKorig[members])
    }, simplify = FALSE)

    # --- build new model ------------------------------------------------------
    L <- compartment_model() |>
        add_compartment(new_names) |>
        add_molecule(
            name = X0_molec,
            cmt = X0_cmt,
            initial = unname(X0lump),
            type = "amount"
        )

    new_from <- grp[M$transports$from] |> unname()
    new_to <- ifelse(
        is.na(M$transports$to),
        NA_character_,
        unname(grp[M$transports$to])
    )
    within_lump <- new_from == new_to & !is.na(new_to)

    # remove transports that are now within lumped compartments
    new_from <- new_from[!within_lump]
    new_to   <- new_to[!within_lump]
    new_molec <- M$transports$molec[!within_lump]

    # rewrite transport rate constants by AST substitution
    new_const <- Map(
        f = function(fr, cst) {
            .rewrite_const(expr = cst, cmt = fr, grp = grp, VKorig = VKorig)
        },
        fr = M$transports$from[!within_lump],
        cst = M$transports$const[!within_lump]
    )

    if (length(new_from) > 0) {
        L <- add_transport(
            L,
            from = new_from,
            to = new_to,
            molec = new_molec,
            const = new_const
        )
    }

    d <- M$doses
    if (length(d) > 0) {
        d$cmt <- unname(grp[d$cmt])
        L <- add_dosing(L, dose = d)
    }

    L <- add_parameter(L, param = M$parameters)
    L
}

#' Helper function to replace transport rate constants during (un-)lumping
#'
#' @param expr A quoted expression
#' @param cmt The original compartment associated with the transport (used for looking up the correct normalizing factor)
#' @param grp A named vector mapping original compartments -> lumped compartments
#' @param VKorig A named list of quoted calls (normalizing terms, e.g. quote(Vtis * Ktis))
#' @returns A modified rate constant epressed with respect to the lumped compartment
#' @noRd
.rewrite_const <- function(expr, cmt, grp, VKorig) {
    
    # helper: symbolic sum of expressions
    .sum_exprs <- function(expr_list) {
        if (length(expr_list) == 0) {
            return(quote(0))
        }
        if (length(expr_list) == 1) {
            return(expr_list[[1]])
        }
        Reduce(function(a, b) bquote(.(a) + .(b)), expr_list)
    }

    # ---- precompute denominators and lump sizes ----
    lump_names <- unique(grp)
    denom_exprs <- setNames(vector("list", length(lump_names)), lump_names)
    lump_sizes <- setNames(integer(length(lump_names)), lump_names)

    for (lump in lump_names) {
        group_members <- names(grp)[grp == lump]
        lump_sizes[[lump]] <- length(group_members)
        denom_exprs[[lump]] <- .sum_exprs(VKorig[group_members])
    }

    switch(as.character(lump_sizes[[grp[[cmt]]]]),
           "0" = stop("Compartment not found in partitioning."),
           "1" = expr,  # no scaling needed
           {
               vk_sum_expr <- denom_exprs[[grp[[cmt]]]]
               bquote((.(VKorig[[cmt]]) / .(vk_sum_expr)) * .(expr))
           }
    )

}


#' Helper function to replace transport rates during (un-)lumping by AST traversal
#'
#' This function works for linear and nonlinear transports, but it is currently unused:
#' - linear transports only need rewritten rate constants, which is handled by `.rewrite_const()`
#' - nonlinear transports are currently not handled by `lump_model()`, but if we wanted to support them in the future, 
#'   we would need to rewrite the entire transport rate expression, which is what this function does.
#' 
#' @param expr A quoted expression (AST)
#' @param grp A named vector mapping original compartments -> lumped compartments
#' @param VKorig A named list of quoted calls (normalizing terms, e.g. quote(Vtis * Ktis))
#' @returns A modified expression in which original variables are replaced by lumped ones
#' @noRd
.rewrite_rate <- function(expr, grp, VKorig) {

    # helper: symbolic sum of expressions
    .sum_exprs <- function(expr_list) {
        if (length(expr_list) == 0) return(quote(0))
        if (length(expr_list) == 1) return(expr_list[[1]])
        Reduce(function(a, b) bquote(.(a) + .(b)), expr_list)
    }

    # ---- precompute denominators and lump sizes ----
    lump_names <- unique(grp)
    denom_exprs <- setNames(vector("list", length(lump_names)), lump_names)
    lump_sizes  <- setNames(integer(length(lump_names)), lump_names)

    for (lump in lump_names) {
        group_members <- names(grp)[grp == lump]
        lump_sizes[[lump]] <- length(group_members)
        denom_exprs[[lump]] <- .sum_exprs(VKorig[group_members])
    }

    # ---- recursive AST substitution ----
    substitute_symbols <- function(e) {
        if (is.symbol(e)) {
            nm <- as.character(e)
            if (nm %in% names(grp)) {
                lump_name <- grp[[nm]]

                # If lump has only one member, no scaling needed
                if (lump_sizes[[lump_name]] == 1) {
                    subst <- as.symbol(lump_name)
                } else {
                    vk_sum_expr <- denom_exprs[[lump_name]]
                    subst <- bquote((.(VKorig[[nm]]) / .(vk_sum_expr)) * .(as.symbol(lump_name)))
                }
                return(subst)
            } else {
                return(e)
            }
        } else if (is.call(e)) {
            as.call(lapply(e, substitute_symbols))
        } else {
            e
        }
    }

    substitute_symbols(expr)
}
