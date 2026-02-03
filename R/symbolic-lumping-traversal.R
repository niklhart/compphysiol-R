# Function to solve the symbolic linear system using topological traversal


#' Solve the symbolic linear system for the given condensation
#' @param cond A condensation graph
#' @param reactions A list of `Reaction` objects
#' @param simplify Method for simplifying the resulting expressions.
#' @returns A named list of solution expressions
#' @noRd
#' @export
.solve_model_symbolic <- function(cond, reactions, simplify) {

    adj <- .adjacency_list(cond$nodes, cond$edges)
    order <- .topo_order(cond$nodes, adj$incoming)

    ## refstate is the first state in the condensation graph
    refstate <- cond$sccs[[order[1]]]
    if (length(refstate) != 1) {
        stop("Reference state must be a singleton SCC")
    }

    ## seed with reference state
    known <- list()
    known[[refstate]] <- as.symbol(refstate)

    for (cid in order) {

        scc <- cond$sccs[[cid]]

        ## skip reference SCC
        if (length(scc) == 1 && scc == refstate) next

        ## assemble linear system for current SCC
        sys <- .assemble_linear_expr(scc, reactions)

        ## substitute already-known states (incl. refstate)
        A_sub <- sys$A
        for (i in seq_len(nrow(A_sub))) {
            for (j in seq_len(ncol(A_sub))) {
                A_sub[[i, j]] <- do.call(
                substitute,
                list(A_sub[[i, j]], known)
                )
            }
        }

        b_sub <- lapply(
            sys$b,
            function(expr) .substitute_expr(expr, known)
        )

        ## solve SCC system
        sol <- .solve_linear_expr(A_sub, b_sub)

        ## simplify solutions
        sol <- .simplify(sol, method = simplify)

        ## add new solutions to known
        known <- c(known, sol)
    }

    known
}

#' Substitution helper for symbolic expressions
#' @param expr An R expression
#' @param values A named list of substitutions
#' @returns The expression with substitutions applied
#' @noRd
.substitute_expr <- function(expr, values) {
    if (length(values) == 0) return(expr)
    do.call(substitute, list(expr, values))
}
