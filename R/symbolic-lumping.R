#' General symbolic lumping (experimental version)
#' 
#' @param M A CompartmentModel object
#' @param partitioning A list of character vectors (each a group of compartments)
#' @param refstate A string specifying a reference state (defaults to the one with most outflows)
#' @returns A CompartmentModel object representing the lumped model
#' @export
symbolic_lumping <- function(M, partitioning = list(), refstate = .get_default_refstate(M)) {

    # Check requirements
    if (!M$isLinear()) stop("Symbolic lumping only supports linear models.")
    # Currently, sink terms are not supported
    if (any(vapply(M$reactions, function(r) is.null(r$to), logical(1)))) {
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

#' Determine a default reference state if unspecified
#'
#' @param model A `CompartmentModel` object
#' @returns A string containing the compartment name with the most outgoing flows
#' @noRd
.get_default_refstate <- function(model) {
    refstate <- model$reactions |> 
        lapply(FUN = function(r) r$from) |> 
        unlist() |> 
        table() |> 
        which.max() |> 
        names()
    warning('Using default reference state: ', refstate)
    refstate
}

#' Get symbolic lumping conditions based on a reference state
#'
#' @param M A `CompartmentModel` object
#' @param refstate A string specifying the reference state
#' @param maxdegree An integer specifying the maximum degree for which to carry out symbolic substitution.
#'   Values > 2 are currently unsupported and will throw an error.
#' @param simplify Method for simplifying the resulting expressions.
#'   Install Ryacas package to use this feature.
#' @returns A named list of expressions representing the lumping conditions. Names are compartment names and
#'   expressions are in terms of the reference state and rate constants.
#' @noRd
get_lumping_conditions <- function(M, refstate, maxdegree = 2, simplify = c("none", "Ryacas")) {

    # graph-theoretical part
    graph <- .make_graph(M, refstate = refstate)
    adj <- .adjacency_list(graph$nodes, graph$edges)
    sccs <- .tarjan_scc(graph$nodes, adj$outgoing)
    cond <- .condense_graph(graph$nodes, graph$edges, sccs)

    # check maxdegree
    if (any(vapply(sccs,length,1L) > maxdegree)) {
        stop("Degree of at least one strongly connected component exceeds maxdegree.")
    }

    # algebraic part
    res <- .solve_model_symbolic(cond, M$reactions) |>
        .simplify(method = match.arg(simplify))

}

