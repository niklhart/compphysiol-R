# Graph theoretical helpers for symbolic lumping

#' Create condensation graph from a `CompartmentModel` and reference state
#'
#' @param M A `CompartmentModel` object
#' @param refstate A string specifying the reference state
#' @returns A list with entries `nodes`, `edges`, `sccs`, `scc_id`
#' @noRd
.make_condensation_graph <- function(M, refstate) {
    graph <- .make_graph(M, refstate = refstate)
    adj <- .adjacency_list(graph$nodes, graph$edges)
    sccs <- .tarjan_scc(graph$nodes, adj$outgoing)
    .condense_graph(graph$nodes, graph$edges, sccs)
}


#' Construct a directed graph encoding the topology of a linear `CompartmentModel`.
#'
#' @param model A `CompartmentModel` object
#' @param refstate A string, corresponding to the state used as reference.
#' @returns A length two named list with entries `edges` (a data frame with
#'     columns `from` and `to`) and `nodes` (a character vector)
#' @export
.make_graph <- function(model, refstate) {
    nodes <- names(model$compartments)
    edges <- data.frame(from = model$flows$from, to = model$flows$to)

    # remove elimination edges and edges towards refstate (break cycle)
    edges <- edges[edges$to != refstate & !is.na(edges$to), , drop = FALSE]

    # check for missing nodes
    stopifnot(edges$from %in% nodes, edges$to %in% nodes)

    list(
        edges = edges,
        nodes = nodes
    )
}

#' Create an adjacency list for a directed graph
#'
#' @param nodes A character vector, the graph nodes
#' @param edges A data frame with columns `from`, `to` (and `rate`, unused)
#' @returns A named list with elements `incoming` and `outgoing`, each a named list of character vectors
#' @noRd
#' @export
.adjacency_list <- function(nodes, edges) {
    incoming <- split(edges$from, edges$to)
    outgoing <- split(edges$to, edges$from)

    incoming[nodes[!nodes %in% names(incoming)]] <- list(character())
    outgoing[nodes[!nodes %in% names(outgoing)]] <- list(character())

    list(
        incoming = incoming,
        outgoing = outgoing
    )
}

#' Tarjan's algorithm to derive strongly connected components (SCCs) of a directed graph
#'
#' @param nodes A character vector, the graph nodes
#' @param outgoing A list, outcoing connections per graph node
#' @returns A list of character vectors, each entry corresponding to an SCC
#' @noRd
#' @export
.tarjan_scc <- function(nodes, outgoing) {
    index <- 0
    stack <- character()
    onstack <- setNames(rep(FALSE, length(nodes)), nodes)
    indices <- lowlink <- setNames(rep(NA_integer_, length(nodes)), nodes)
    sccs <- list()

    strongconnect <- function(v) {
        index <<- index + 1
        indices[v] <<- index
        lowlink[v] <<- index
        stack <<- c(stack, v)
        onstack[v] <<- TRUE

        for (w in outgoing[[v]]) {
            if (is.na(indices[w])) {
                strongconnect(w)
                lowlink[v] <<- min(lowlink[v], lowlink[w])
            } else if (onstack[w]) {
                lowlink[v] <<- min(lowlink[v], indices[w])
            }
        }

        if (lowlink[v] == indices[v]) {
            scc <- character()
            repeat {
                w <- tail(stack, 1)
                stack <<- head(stack, -1)
                onstack[w] <<- FALSE
                scc <- c(scc, w)
                if (w == v) break
            }
            sccs[[length(sccs) + 1]] <<- scc
        }
    }

    for (v in nodes) {
        if (is.na(indices[v])) strongconnect(v)
    }

    sccs
}


#' Construct the condensation graph from SCCs
#'
#' @param nodes A character vector of original graph nodes
#' @param edges A data frame with columns `from`, `to`
#' @param sccs A list of character vectors (output of `.tarjan_scc()`)
#' @returns A list with entries `nodes`, `edges`, `sccs`, `scc_id`
#' @noRd
#' @export
.condense_graph <- function(nodes, edges, sccs) {
    # map each original node -> SCC index
    scc_id <- setNames(
        rep(seq_along(sccs), lengths(sccs)),
        unlist(sccs)
    )

    # lift edges to SCC level
    cond_edges <- data.frame(
        from = scc_id[edges$from],
        to = scc_id[edges$to],
        stringsAsFactors = FALSE
    )

    # remove self-loops and duplicates
    cond_edges <- unique(cond_edges[cond_edges$from != cond_edges$to, ])

    list(
        nodes = seq_along(sccs),
        edges = cond_edges,
        sccs = sccs,
        scc_id = scc_id
    )
}



#' Topological ordering of a directed acyclic graph (DAG).
#'
#' @param nodes Nodes of the DAG
#' @param incoming Named list of characters (same length as `nodes`), corresponding to incoming connections
#' @returns A vector of integers (same length as `nodes`), the order to visit states for a topological traversal of the DAG.
#' @noRd
#' @export
.topo_order <- function(nodes, incoming) {
    visited <- setNames(rep(FALSE, length(nodes)), nodes)
    order <- integer()

    repeat {
        ready <- nodes[
            !visited[nodes] &
                sapply(nodes, function(n) {
                    all(visited[incoming[[as.character(n)]]])
                })
        ]

        if (length(ready) == 0) {
            break
        }

        for (n in ready) {
            visited[as.character(n)] <- TRUE
            order <- c(order, n)
        }
    }

    if (!all(visited)) {
        stop("Cycle in condensation graph (should not happen)")
    }
    order
}

