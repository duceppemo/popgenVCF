test_that("provenance DAGs validate and trace lineage", {
  nodes <- list(
    new_provenance_node("vcf", kind = "input", digest = "abc"),
    new_provenance_node("prune", kind = "transformation"),
    new_provenance_node("pca", kind = "analysis"),
    new_provenance_node("figure", kind = "artifact")
  )
  edges <- list(
    new_provenance_edge("vcf", "prune", "consumes"),
    new_provenance_edge("prune", "pca", "derived_from"),
    new_provenance_edge("pca", "figure", "produces", "pca.pdf")
  )
  dag <- new_provenance_dag(nodes, edges)
  expect_s3_class(dag, "PopgenVCFProvenanceDAG")
  expect_equal(provenance_topological_order(dag), c("vcf", "prune", "pca", "figure"))
  expect_equal(provenance_ancestors(dag, "figure"), c("pca", "prune", "vcf"))
  expect_equal(provenance_descendants(dag, "vcf"), c("figure", "pca", "prune"))
  expect_equal(nrow(provenance_node_table(dag)), 4L)
  expect_equal(nrow(provenance_edge_table(dag)), 3L)
})

test_that("provenance DAGs reject invalid graph structure", {
  a <- new_provenance_node("a")
  b <- new_provenance_node("b")
  expect_error(new_provenance_edge("a", "a"), "self-referential")
  expect_error(
    new_provenance_dag(list(a), list(new_provenance_edge("a", "missing"))),
    "dangling"
  )
  expect_error(
    new_provenance_dag(
      list(a, b),
      list(new_provenance_edge("a", "b"), new_provenance_edge("b", "a"))
    ),
    "cycle"
  )
  expect_error(new_provenance_dag(list(a, a)), "unique")
})

test_that("nodes and edges can be registered incrementally", {
  dag <- new_provenance_dag()
  dag <- add_provenance_node(dag, new_provenance_node("input", kind = "input"))
  dag <- add_provenance_node(dag, new_provenance_node("analysis", kind = "analysis"))
  dag <- add_provenance_edge(dag, new_provenance_edge("input", "analysis"))
  expect_equal(provenance_topological_order(dag), c("input", "analysis"))
})
