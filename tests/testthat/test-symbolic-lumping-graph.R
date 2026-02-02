# Testing graph-theoretical part of symbolic lumping

process_graph <- function(M, refstate) {
    graph <- compphysiol:::.make_graph(M, refstate = refstate)
    adjacency <- do.call(compphysiol:::.adjacency_list, graph)
    sccs <- compphysiol:::.tarjan_scc(graph$nodes, adjacency$outgoing)
    cond <- compphysiol:::.condense_graph(graph$nodes, graph$edges, sccs)
    cond_adj <- compphysiol:::.adjacency_list(cond$nodes, cond$edges)

    list(
        graph = graph,
        adjacency = adjacency,
        sccs = sccs,
        cond = cond,
        cond_adj = cond_adj
    )
}

expect_topo_order <- function(cond, cond_adj) {
    order <- compphysiol:::.topo_order(cond$nodes, cond_adj$incoming)
    pos <- setNames(seq_along(order), order)

    apply(cond$edges, 1, function(e) {
        expect_lt(
            pos[as.character(e["from"])],
            pos[as.character(e["to"])]
        )
    })

    invisible(order)
}

expect_adjacency <- function(adj, incoming, outgoing) {
    expect_equal(adj$incoming, incoming)
    expect_equal(adj$outgoing, outgoing)
}


test_that("A->B->C graph processing works", {

    # Model definition
    M <- CompartmentModel$new()

    M$addCompartment("A", 0)
    M$addCompartment("B", 0)
    M$addCompartment("C", 0)

    M$addReaction("A","B","kAB*A")
    M$addReaction("B","C","kBC*B")

    # Graph processing
    res <- process_graph(M, refstate = "A")

    # Graph validation
    expect_equal(res$graph$nodes, c("A","B","C"))

    expect_adjacency(
        res$adjacency,
        incoming = list("B"="A","C"="B","A"=character(0)),
        outgoing = list("A"="B","B"="C","C"=character(0))
    )

    expect_equal(res$sccs, list("C","B","A"))
    expect_equal(length(res$cond$sccs), 3)
    expect_equal(nrow(res$cond$edges), 2)

    expect_topo_order(res$cond, res$cond_adj)

})


test_that("A->B<->C graph processing works", {

    # Model definition
    M <- CompartmentModel$new()

    M$addCompartment("A", 0)
    M$addCompartment("B", 0)
    M$addCompartment("C", 0)

    M$addReaction("A","B","kAB*A")
    M$addReaction("B","C","kBC*B")
    M$addReaction("C","B","kCB*C")

    # Graph processing
    res <- process_graph(M, refstate = "A")

    # Graph validation
    expect_adjacency(
        res$adjacency,
        incoming = list("B"=c("A","C"),"C"="B","A"=character(0)),
        outgoing = list("A"="B","B"="C","C"="B")
    )

    expect_equal(res$sccs, list(c("C","B"),"A"))
    expect_equal(length(res$cond$sccs), 2)
    expect_equal(nrow(res$cond$edges), 1)

    expect_topo_order(res$cond, res$cond_adj)
})

test_that("topo_order fails on cyclic graph", {
    incoming <- list(
        "1" = "3",
        "2" = "1",
        "3" = "2"
    )
    nodes <- c(1, 2, 3)

    expect_error(
        compphysiol:::.topo_order(nodes, incoming),
        "Cycle"
    )
})

test_that(" A->B->0 graph processing ignores elimination", {

    # Model definition
    M <- CompartmentModel$new()

    M$addCompartment("A", 0)
    M$addCompartment("B", 0)

    M$addReaction("A","B","kAB*A")
    M$addReaction("B","","kB0*B")

    # Graph processing
    res <- process_graph(M, refstate = "A")

    # Graph validation
    expect_equal(res$graph$nodes, c("A","B"))

    expect_adjacency(
        res$adjacency,
        incoming = list("B"="A","A"=character(0)),
        outgoing = list("A"="B","B"=character(0))
    )

    expect_equal(res$sccs, list("B","A"))
    expect_equal(length(res$cond$sccs), 2)
    expect_equal(nrow(res$cond$edges), 1)

    expect_topo_order(res$cond, res$cond_adj)

})
