
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

  tree_move(data, moves)

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

  tree_move(data, moves)

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

  tree_move(data, moves)

})
