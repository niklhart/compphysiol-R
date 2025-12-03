# generic lumping function

#' Lump a `CompartmentModel` object
#'
#' @param M A `CompartmentModel` object, the model to be lumped.
#' @param partitioning A list of character vectors (each a group of compartments)
#' @param normalize A named list of quoted calls giving normalizing factors for each compartment
#' @returns A `CompartmentModel` object representing the lumped model
#' @export
lump_model <- function(M, partitioning = list(), normalize = list()) {

    # --- validate partitioning ------------------------------------------------
    cmt <- M$getStateNames()
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
    X0orig <- M$getInitialState(named = TRUE)

    # simple summation by lump
    lump <- function(x) tapply(x, grp[names(x)], sum)
    X0lump <- lump(X0orig)

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
    L <- CompartmentModel$new()

    L$compartments <- lapply(names(X0lump), function(nm) {
        Compartment$new(name = nm, initial = X0lump[[nm]])
    })

    for (r in M$reactions) {
        from <- if (!is.null(r$from) && r$from != "") grp[[r$from]] else ""
        to   <- if (!is.null(r$to) && r$to != "") grp[[r$to]] else ""
        if (from == to) next  # reaction collapsed away

        new_rate <- .rewrite_rate(r$rate, grp, VKorig)
        L$addReaction(from = from, to = to, rate = new_rate)
    }

    for (d in M$doses) {
        d$target <- grp[[d$target]]
        L$addDosing(d)
    }

    L
}



#' Helper function to replace reaction rates during (un-)lumping by AST traversal
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

.get_refstate <- function(M) {
    src_cmt <- M$reactions |> 
        lapply(FUN = function(r) r$from) |> 
        unlist() |> 
        table() |> 
        which.max() |> 
        names()
    warning('Using default reference compartment: ', src_cmt)
    src_cmt
}

#' General symbolic lumping (experimental version)
#' 
#' @param M A CompartmentModel object
#' @param partitioning A list of character vectors (each a group of compartments)
#' @param ref A string specifying a reference compartment (defaults to the one with most outflows)
#' @returns A CompartmentModel object representing the lumped model
#' @export
symbolic_lumping <- function(M, partitioning = list(), ref = .get_refstate(M)) {

    # Check requirements
    if (!M$isLinear(M)) stop("Symbolic lumping only supports linear models.")
    # Currently, sink terms are not supported
    if (any(vapply(M$reactions, function(r) r$to == "", logical(1)))) {
        stop("Symbolic lumping currently does not support sink reactions.")
    }

    # --- validate partitioning ------------------------------------------------
    cmt <- M$getStateNames()
    flatpart <- unlist(partitioning)
    if (any(duplicated(flatpart))) {
        stop("Partitioning must not contain any duplicate values.")
    }
    if (length(setdiff(flatpart, cmt)) > 0) {
        stop("Some compartment name(s) in 'partitioning' were not found in the model.")
    }
    remaining <- setdiff(cmt, flatpart)

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

    # --- create graph structure of model topology -------------------------------
    nreact <- length(M$reactions)
    react_dist <- numeric(nreact)

}