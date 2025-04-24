#' Add a row to the rows in the epi attribute
#'
#' @param df A tibble
#' @param rows Crafted rows
#' @param skip Whether to update the record or only use it as reference
#' @return Epigraf tibble
ram_add <- function(df, rows, skip=FALSE) {

  epi <- attr(df, "epi")
  if (is.null(epi)) {
    epi <- list()
  }

  if (is.null(epi$rows)) {
    epi$rows <- tibble::tibble()
  }

  rows <- dplyr::mutate(rows, dplyr::across(tidyselect::everything(), as.character))

  if (skip) {
    rows$`_action` <- "skip"
  }

  epi$rows <- dplyr::bind_rows(epi$rows, rows)
  attr(df, "epi") <- epi

  # Set class
  class(df) <- c("epi_tbl", setdiff(class(df),"epi_tbl"))
  df
}

#' Compile a crafted table with RAM rows ready to patch into the database
#'
#' RAM rows can be added with `ram_add()`.
#'
#' @param df A data frame with RAM rows in the epi attribute
#' @return The RAM rows data frame
#' @examples
#' library(tibble)
#' library(rpigraf)
#'
#' # Example data
#' df <- tribble(
#'   ~case, ~title, ~text,
#'   1, "Case 01", "Happy New Year!",
#'   2, "Case 02", "Happy Easter!",
#'   3, "Case 03", "Happy Birthday!"
#' )
#'
#' # Add RAM rows
#' df <- df |>
#'  df_to_ram(
#'    item.cols = c("content" = "text"),
#'    article.cols = c("id" = "case", "signature" = "case", "name" = "title")
#'  )
#'
#' ram_compile(df)
#'
#' @param df An Epigraf tibble with RAM rows in the epi attribute
#' @return A tibble with RAM rows
#' @export
ram_compile <- function(df) {
  rows <- attr(df, "epi")$rows
  if (is.null(rows)) {
    rows <- tibble::tibble()
  }

  # Arrange the rows so that articles stick together
  rows <- rows  |>
    dplyr::mutate(
      dplyr::across(tidyselect::any_of(c(".project",".article",".section",".item",".property")), forcats::as_factor)
    ) |>
    dplyr::arrange(dplyr::desc(
        dplyr::across(tidyselect::any_of(c(".project",".article",".section",".item",".property")))
      )
    )

  cols <- colnames(rows)
  cols_first <- c("table", "id")
  cols_last <- c("properties_id","sections_id","articles_id","projects_id","_fields")
  cols_mid <- setdiff(cols,c(cols_first,cols_last))

  rows <- dplyr::select(
    rows,
    tidyselect::any_of(c(cols_first,cols_mid,cols_last)),
    -tidyselect::starts_with(".")
  )


  tibble::as_tibble(apply(rows, 2, rev))
}

#' Remove all RAM rows from a tibble
#'
#' @param df A tibble with RAM rows in the epi attribute
#' @return Epigraf tibble without RAM rows
#' @examples
#' library(tibble)
#' library(rpigraf)
#'
#' # Example data
#' df <- tribble(
#'   ~case, ~title, ~text,
#'   1, "Case 01", "Happy New Year!",
#'   2, "Case 02", "Happy Easter!",
#'   3, "Case 03", "Happy Birthday!"
#' )
#'
#' # Add RAM rows
#' df <- df |>
#'  df_to_ram(
#'    item.cols = c("content" = "text"),
#'    article.cols = c("id" = "case", "signature" = "case", "name" = "title")
#'  )
#'
#' ram_compile(df)
#'
#' # Remove RAM rows
#' df <-ram_clear(df)
#' ram_compile(df)
#'
#' @export
ram_clear <- function(df) {

  epi <- attr(df, "epi")
  if (is.null(epi)) {
    epi <- list()
  }

  epi$rows <- tibble::tibble()
  attr(df, "epi") <- epi

  # Set class
  class(df) <- c("epi_tbl", setdiff(class(df),"epi_tbl"))
  df
}
