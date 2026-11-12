
test_that("Move operation succeeds", {

  data <- data.frame(
    id        = c("A", "B", "C"),
    parent_id = c(NA, "A", "B"),
    lemma     = c("A", "B", "C")
  )

  moves <- data.frame(
    from_path = "A > B",
    to_path   = "X > Y > B"
  )

  result <- tree_move(data, moves)

  expected <- tibble::tribble(
    ~id,      ~norm_iri,  ~tree_path,      ~parent_id, ~lemma, ~from_path,
    "A",      NA,         "A",             NA,         "A",    "A",
    "tidy~x", "tidy~x",   "X",             NA,         "X",    "X",
    "tidy~x-y","tidy~x-y","X > Y",         "tidy~x",   "Y",    "X > Y",
    "B",      NA,         "X > Y > B",     "tidy~x-y", "B",    "A > B",
    "C",      NA,         "X > Y > B > C", "B",        "C",    "A > B > C"
  )
  testthat::expect_equal(result, expected)

})



test_that("Unused move operations are ignored", {

  data <- data.frame(
    id        = c("A", "B", "C"),
    parent_id = c(NA, "A", "B"),
    lemma     = c("A", "B", "C")
  )

  moves <- data.frame(
    from_path = c("A > B","F > G"),
    to_path   = c("X > Y > B", "F > H")
  )

  result <- tree_move(data, moves)

  expected <- tibble::tribble(
    ~id,      ~norm_iri,  ~tree_path,      ~parent_id, ~lemma, ~from_path,
    "A",      NA,         "A",             NA,         "A",    "A",
    "tidy~x", "tidy~x",   "X",             NA,         "X",    "X",
    "tidy~x-y","tidy~x-y","X > Y",         "tidy~x",   "Y",    "X > Y",
    "B",      NA,         "X > Y > B",     "tidy~x-y", "B",    "A > B",
    "C",      NA,         "X > Y > B > C", "B",        "C",    "A > B > C"
  )
  testthat::expect_equal(result, expected)

})



test_that("Existing nodes are preserved", {

  data <- tibble::tibble(
    id        = c("A", "B", "C", "X1", "X2"),
    parent_id = c(NA, "A", "B", NA, "B"),
    lemma     = c("A", "B", "C", "X", "X")
  )



  moves <- data.frame(
    from_path = "X",
    to_path   = "A > B > X"
  )

  result <- tree_move(data, moves)

  expected <- tibble::tribble(
    ~id,     ~parent_id, ~lemma, ~tree_path,       ~from_path,
    "A",               NA,         "A",  "A",      "A",
    "B",              "A",        "B",  "A > B",   "A > B",
    "C",         "B",        "C",   "A > B > C",   "A > B > C",
    "X1",        "B",        "X",   "A > B > X",   "X",
    "X2",        "B",        "X",   "A > B > X",   "A > B > X"
  )
  testthat::expect_equal(result, expected)

})


test_that("Type is preserved", {

  data <- data.frame(
    id        = c("A", "B", "C"),
    parent_id = c(NA, "A", "B"),
    lemma     = c("A", "B", "C"),
    type = "example"
  )

  moves <- data.frame(
    from_path = "A > B",
    to_path   = "X > Y > B"
  )

  result <- tree_move(data, moves)

  expected <- tibble::tribble(
    ~id,                          ~norm_iri,  ~tree_path,      ~parent_id,                   ~lemma, ~type,     ~from_path,
    "A",                          NA,         "A",             NA,                           "A",    "example", "A",
    "properties/example/tidy~x",  "tidy~x",   "X",             NA,                           "X",    "example", "X",
    "properties/example/tidy~x-y","tidy~x-y", "X > Y",         "properties/example/tidy~x",  "Y",    "example", "X > Y",
    "B",                          NA,         "X > Y > B",     "properties/example/tidy~x-y","B",    "example", "A > B",
    "C",                          NA,         "X > Y > B > C", "B",                          "C",    "example", "A > B > C"
  )
  testthat::expect_equal(result, expected)

})



test_that("Lemma is renamed", {

  data <- data.frame(
    id        = c("A", "B", "C"),
    parent_id = c(NA, "A", "B"),
    lemma     = c("A", "B", "C")
  )

  moves <- data.frame(
    from_path = "A > B",
    to_path   = "X > Y > Z"
  )

  result <- tree_move(data, moves)

  expected <- tibble::tribble(
    ~id,       ~norm_iri,  ~tree_path,      ~parent_id, ~lemma, ~from_path,
    "A",       NA,         "A",             NA,         "A",    "A",
    "tidy~x",  "tidy~x",   "X",             NA,         "X",    "X",
    "tidy~x-y","tidy~x-y", "X > Y",         "tidy~x",   "Y",    "X > Y",
    "B",       NA,         "X > Y > Z",     "tidy~x-y", "Z",    "A > B",
    "C",       NA,         "X > Y > Z > C", "B",        "C",    "A > B > C"
  )
  testthat::expect_equal(result, expected)

})
