
test_that("Dataframes with different column types are row binded", {

  df1 <- tibble::tribble(~x, ~y, ~z, ~a,
                 1L, "A", 1, "a",
                 2L, "B", 2, "b"
         )

  df2 <- tibble::tribble(~x, ~z, ~y, ~b,
                 "3", 3, "C", 11,
                 "X", 4, "D", 22
         )

  df <- bind_rows_char(list(df1, df2))

  expect_equal(sapply(df, class),c("x"="character","y"="character","z" = "numeric","a" = "character","b" = "numeric"))
})



test_that("Empty dataframes are row binded", {

  df1 <- tibble::tribble(~x, ~y, ~z, ~a,
                         1L, "A", 1, "a",
                         2L, "B", 2, "b"
  )

  df2 <- tibble::tibble()

  df <- bind_rows_char(list(df1, df2))

  expect_equal(sapply(df, class),c("x"="integer","y"="character","z" = "numeric","a" = "character"))
})



test_that("Wide data is extracted", {

  df_wide <- tibble::tribble(
    ~id, ~articles_id, ~sections_id, ~properties.id, ~properties.lemma,
    "items/categories/moview~001~categories~genre",
    "articles/default/movies~001","sections/categories/movies~001~categories",
    "properties/categories/fancygenre","Fancy Genre"
  )


  df_expected <- tibble::tribble(
    ~table, ~id, ~lemma, ~articles_id, ~sections_id, ~ properties_id,
    "properties","properties/categories/fancygenre", "Fancy Genre", NA, NA, NA,
    "items","items/categories/moview~001~categories~genre", NA, "articles/default/movies~001", "sections/categories/movies~001~categories", "properties/categories/fancygenre"
  )

  df_long <- epi_wide_to_long(df_wide)

  expect_equal(df_long, df_expected)
})



test_that("Numbers are converted to alphabet and back", {

  nums <- c(1, 2, 26, 27, 28, 52, 53, 676, 702, 703, 18278)
  abcs <- vapply(nums, num2abc, character(1))

  roundtrip <- vapply(abcs, abc2num, double(1))
  names(roundtrip) <- NULL

  expect_equal(nums, roundtrip)
})
