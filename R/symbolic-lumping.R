



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

#' Derive lumping conditions based on a reference state
#'
#' @param M A `CompartmentModel` object
#' @param refstate A named vector mapping original compartments -> lumped compartments
#' @param maxdegree An integer specifying the maximum degree for which to carry out symbolic substitution
#' @returns A modified expression in which original variables are replaced by lumped ones
#' @noRd
.derive_lumping_conditions <- function(M, refstate, maxdegree = 1) {

    # create graph structure
    nodes <- M$getStateNames()
    edges <- M$reactions |>
        lapply(function(r) data.frame(
            from = r$from, 
            to = if (!is.null(r$to)) r$to else NA, 
            rate = r$rateConstant(nodes))) |>
        do.call(what=rbind)

    # remove edges towards refstate (break cycle)
    edges <- edges |>
        subset(is.na(to) | to != refstate)

    # cut elimination reactions (relevant for algebra only, not graph structure)
    graph_edges <- edges[!is.na(edges$to), ]

    # determine strongly connected components (SCCs) and their size
    incoming <- split(graph_edges$from, graph_edges$to)
    outgoing <- split(graph_edges$to, graph_edges$from)

    incoming[nodes[!nodes %in% names(incoming)]] <- list(character())
    outgoing[nodes[!nodes %in% names(outgoing)]] <- list(character())

    # walk through SCCs and derive lumping condition
    sccs <- .tarjan_scc(nodes, outgoing)

    # check maximum degree condition
    maxdegree_M <- max(vapply(sccs,length,1))
    if (maxdegree_M > maxdegree) {
        stop(paste0("Degree of a SCC is larger (",maxdegree_M,") than maximum degree allowed (",maxdegree,")."))
    }

    # Build condensation graph
    scc_id <- setNames(seq_along(sccs),
                   unlist(sccs))

    cond_edges <- unique(
        data.frame(
            from = scc_id[edges$from],
            to   = scc_id[edges$to]
        )
    )

    # Remove self-loops
    cond_edges <- cond_edges[cond_edges$from != cond_edges$to, ]

    # 

}




#' Build (coupled) lumping equations for a strongly connected component of a directed graph
#' 
#' @param scc A strongly connected component in a directed graph
#' @param edges A data frame encoding the connectivity of the directed graph
#' @returns A named list containing
#' * `states`, the SCC
#' * `size`, the number of nodes in the SCC
#' * `equations` .... TODO
.process_scc <- function(scc, edges) {
    # scc = vector of node names
    # extract internal edges
    internal <- edges[edges$from %in% scc & edges$to %in% scc, ]

    # build linear system
    # dX/dt = A X + b
    # under QSS: A X + b = 0

    list(
        states = scc,
        size = length(scc),
        equations = internal
    )
}



#' Derive lumped first pass effect formulas
#' 
#' 
.derive_first_pass <- function(M, partitioning) {

}