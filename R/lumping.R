# generic lumping function

#' Lump a `CompartmentModel` object
#'
#' @param M A `CompartmentModel` object, the model to be lumped.
#' @param partitioning A list of character arrays, possibly named
#' @returns A `CompartmentModel` object representing the lumped model
#' @export
lump_model <- function(M, partitioning = list()) {

    # check partitioning argument
    cmt <- M$getStateNames()
    flatpart <- unlist(partitioning)
    if (any(duplicated(flatpart))) {
        stop("Partitioning must not contain any duplicate values.")
    }
    if (length(setdiff(flatpart,cmt))>0) {
        stop("Some compartment names(s) in partitioning were not found in the model.")
    }
    remaining <- setdiff(cmt,flatpart)

    # Name lumped compartments
    lumped_names <- vapply(partitioning,
                           function(p) paste(p, collapse = "+"),
                           character(1))
    nm_part <- names(partitioning)
    if (!is.null(nm_part)) {
        lumped_names[nm_part != ""] <- nm_part[nm_part != ""]
    }
    new_names <- c(lumped_names, remaining)

    # Mapping original compartment -> lumped group
    grp <- setNames(
        rep(new_names,
            times = c(lengths(partitioning), rep(1, length(remaining)))),
        c(flatpart, remaining)
    )

    # --- Lump initial conditions & VK ---
    X0orig <- M$getInitialState(named = TRUE)

    VKorig <- rep(1,length(X0orig)) # TODO! M$VK
    names(VKorig) <- names(X0orig)

    lump <- function(x) tapply(x, grp[names(x)], sum)
    unlump <- function(xl) {
        vapply(names(X0orig), function(nm) {
            lump_name <- grp[[nm]]
            vk_group <- sum(VKorig[names(grp)[grp == lump_name]])
            (VKorig[[nm]] / vk_group) * xl[[lump_name]]
        }, numeric(1))
    }

    X0lump <- lump(X0orig)
    VKlump <- lump(VKorig)

    # ---- Construct new model ----
    L <- CompartmentModel$new() #M$clone(deep = TRUE)

    L$compartments <- lapply(names(X0lump), function(nm) {
        Compartment$new(name = nm, initial = X0lump[[nm]])
    })
    for (r in M$reactions) {
        from <- grp[[r$from]]
        to   <- grp[[r$to]]
        if (from == to) next  # reaction collapsed away

        new_rate <- .rewrite_rate(r$rate, grp, VKorig)

        L$addReaction(from = from, to = to, rate = new_rate)
    }

    L
}


#' Helper function to replace reaction rates during (un-)lumping by AST traversal
#'
#' @param expr An expression
#' @param grp A mapping of lumped to non-lumped compartments
#' @param VKorig Normalizing variables for lumping condition
#' @returns The expression in which the original variables are replaced by
#'   lumped ones.
.rewrite_rate <- function(expr, grp, VKorig) {
    # recursive substitution
    substitute_symbols <- function(e) {
        if (is.symbol(e)) {
            nm <- as.character(e)
            if (nm %in% names(grp)) {
                lump_name <- grp[[nm]]
                vk_group <- sum(VKorig[names(grp)[grp == lump_name]])
                subst <- bquote((.(VKorig[[nm]]) / .(vk_group)) * .(as.symbol(lump_name)))
                return(subst)
            } else {
                return(e) # leave untouched (e.g. parameters)
            }
        } else if (is.call(e)) {
            as.call(lapply(e, substitute_symbols))
        } else {
            e
        }
    }
    substitute_symbols(expr)
}
