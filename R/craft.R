#' Map a data frame to the Relational Article Model (RAM)
#'
#' Calls craft methods for projects, articles, sections, items and properties.
#' Alternatively, you can use the craft methods directly.
#'
#' @param df A data frame with the source data.
#' @param project.cols A named list. Names are RAM columns, values source columns.
#' @param project.fill A named list with fixed values for the rows.
#' @param article.cols A named list. Names are RAM columns, values source columns.
#' @param article.fill A named list with fixed values for the rows.
#' @param section.cols A named list. Names are RAM columns, values source columns.
#' @param section.fill A named list with fixed values for the rows.
#' @param property.cols A named list. Names are RAM columns, values source columns.
#' @param property.fill A named list with fixed values for the rows.
#' @param item.cols A named list. Names are RAM columns, values source columns.
#' @param item.fill A named list with fixed values for the rows.
#' @param compile Whether to return the compiled RAM rows or a RAM enhanced data frame.
#' @return When compile is TRUE: a RAM data frame, ready for patching into the Epigraf database.
#'         When compile is FALSE: a RAM-enhanced data frame.
#'         In RAM-enhanced data frames, the RAM rows are stored in the epi attribute.
#'         Call `ram_compile()` to get a RAM data frame from a RAM-enhanced data frame.
#' @examples
#' library(tibble)
#' library(rpigraf)
#'
#' df <- tribble(
#'   ~case, ~title, ~text,
#'   1, "Case 01", "Happy New Year!",
#'   2, "Case 02", "Happy Easter!",
#'   3, "Case 03", "Happy Birthday!"
#' )
#'
#' df |>
#'  df_to_ram(
#'    project.fill = c("fragment" = "Example"),
#'    item.cols = c("content" = "text"),
#'    article.cols = c("id" = "case", "signature" = "case", "name" = "title"),
#'    compile = T
#'  )
#'
#' @export
df_to_ram <- function(
    df,
    project.cols = c(), project.fill = c(),
    article.cols = c(), article.fill = c(),
    section.cols = c(), section.fill = c(),
    property.cols = c(), property.fill = c(),
    item.cols = c(), item.fill = c(),
    compile = FALSE
) {

  if (length(c(property.cols,property.fill)) > 0) {
    df <- craft_properties(df, property.cols, property.fill)
  }

  if (length(c(project.cols,project.fill)) > 0) {
    df <- craft_projects(df, project.cols, project.fill)
  }

  if (length(c(article.cols,article.fill)) > 0) {
    df <- craft_articles(df, article.cols, article.fill)
  }
  if (length(c(section.cols,section.fill)) > 0) {
    df <- craft_sections(df, section.cols, section.fill)
  }
  if (length(c(item.cols,item.fill)) > 0) {
    df <- craft_items(df, item.cols, item.fill)
  }

  if (compile ) {
    return(ram_compile(df))
  }

  return(df)
}

#' Create RAM rows for project data
#'
#' @param df The source data frame
#' @param cols The mapping of source columns to RAM columns
#' @param fill Fixed values for the RAM
#' @return RAM rows
craft_projects <- function(df, cols = c(), fill = c()) {

  cols <- merge_vectors(cols, c("type" = "project.type", "fragment" = "project.fragment"))

  rows <- df
  rows <- default_values(rows, cols[["fragment"]], "default")
  rows <- default_values(rows, cols[["type"]], "default")

  rows <- as.data.frame(rows[, cols, drop = FALSE])
  names(rows) <- names(cols)

  for (name in names(fill)) {
    rows[[name]] <- fill[[name]]
  }

  rows$id <- epi_create_iri("projects", rows$type, rows$fragment)
  rows$type <- NULL
  rows$fragment <- NULL
  cols <- colnames(rows)

  df$.project <- rows$id
  rows$.project <- rows$id

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(c(cols,"type","norm_iri"), collapse=",")

  df <- ram_add(df, rows)
  df
}

#' Create RAM rows for article data
#'
#' @param df The source data frame
#' @param cols The mapping of source columns to RAM columns
#' @param fill Fixed values
#' @return A RAM-enhanced data frame
craft_articles <- function(df, cols = c(), fill = c()) {

  if (!(".project" %in% colnames(df))) {
    stop("Please, craft a project first")
  }

  cols <- merge_vectors(cols, c("fragment" = "article.fragment", "type" = "article.type", "projects_id" = ".project"))
  rows <- df
  rows <- default_values(rows, cols[["fragment"]], "default")
  rows <- default_values(rows, cols[["type"]], "default")

  rows <- as.data.frame(rows[, cols, drop = FALSE])
  names(rows) <- names(cols)

  for (name in names(fill)) {
    rows[[name]] <- fill[[name]]
  }

  rows$id <- epi_create_iri("articles", rows$type, rows$fragment)
  rows$type <- NULL
  rows$fragment <- NULL
  cols <- colnames(rows)

  df$.article <- rows$id
  rows$.article <- rows$id
  rows$.project <- df$.project

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(c(cols,"type","norm_iri"), collapse=",")

  df <- ram_add(df, rows)
  df
}

#' Create RAM rows for section data
#'
#' @param df The source data frame
#' @param cols The mapping of source columns to RAM columns
#' @param fill Fixed values
#' @return A RAM-enhanced data frame
#' @export
craft_sections <- function(df, cols = c(), fill = c()) {

  if (!(".project" %in% colnames(df))) {
    stop("Please, craft a project first")
  }

  if (!(".article" %in% colnames(df))) {
    stop("Please, craft an article first")
  }

  cols <- merge_vectors(cols, c("fragment" = "section.fragment", "type" = "section.type", "articles_id" = ".article", "projects_id" = ".project"))
  rows <- df
  rows <- default_values(rows, cols[["fragment"]], "default")
  rows <- default_values(rows, cols[["type"]], "default")

  rows <- as.data.frame(rows[, cols, drop = FALSE])
  names(rows) <- names(cols)

  for (name in names(fill)) {
    rows[[name]] <- fill[[name]]
  }

  rows$id <- epi_create_iri("sections", rows$type, paste0(epi_iri_parent(rows$articles_id),rows$fragment))
  rows$fragment <- NULL
  rows$type <- NULL

  cols <- colnames(rows)
  df$.section <- rows$id
  rows$.section <- rows$id
  rows$.project <- df$.project
  rows$.article <- df$.article

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(c(cols,"type","norm_iri"), collapse=",")

  df <- ram_add(df, rows)
  df
}

#' Create RAM rows for item data
#'
#' @param df The source data frame
#' @param cols The mapping of source columns to RAM columns
#' @param fill Fixed values
#' @return A RAM-enhanced data frame
#' @export
craft_items <- function(df, cols = c(), fill = c()) {

  if (!(".project" %in% colnames(df))) {
    stop("Please, map a project first")
  }

  if (!(".article" %in% colnames(df))) {
    stop("Please, map an article first")
  }

  if (!(".section" %in% colnames(df))) {
    stop("Please, map a section first")
  }

  cols <- merge_vectors(cols, c("fragment" = "item.fragment", "type" = "item.type", "sections_id" = ".section", "articles_id" = ".article","projects_id" = ".project"))
  rows <- df
  rows <- default_values(rows, cols[["fragment"]], "default")
  rows <- default_values(rows, cols[["type"]], "default")

  rows <- as.data.frame(rows[, cols, drop = FALSE])
  names(rows) <- names(cols)

  for (name in names(fill)) {
    rows[[name]] <- fill[[name]]
  }

  rows$id <- epi_create_iri("items", rows$type, paste0(epi_iri_parent(rows$sections_id),rows$fragment))
  rows$type <- NULL
  rows$fragment <- NULL
  cols <- colnames(rows)

  df$.item <- rows$id
  rows$.item <- rows$id

  rows$.project <- df$.project
  rows$.article <- df$.article
  rows$.section <- df$.section

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(c(cols,"type","norm_iri"), collapse=",")

  df <- ram_add(df, rows)
  df
}


#' Create RAM rows for property data
#'
#' @param df The source data frame
#' @param cols The mapping of source columns to RAM columns
#' @param fill Fixed values
#' @return A RAM-enhanced data frame
#' @export
craft_properties <- function(df, cols = c(), fill = c()) {

  cols <- merge_vectors(cols, c("fragment" = "property.id", "type" = "property.type"))
  rows <- df
  rows <- default_values(rows, cols[["fragment"]], "default")
  rows <- default_values(rows, cols[["type"]], "default")

  rows <- as.data.frame(rows[, cols, drop = FALSE])
  names(rows) <- names(cols)

  for (name in names(fill)) {
    rows[[name]] <- fill[[name]]
  }

  rows$id <- epi_create_iri("properties", rows$type, rows$fragment)
  rows$type <- NULL
  rows$fragment <- NULL
  cols <- colnames(rows)

  df$.property <- rows$id
  rows$.property <- rows$id

  rows <- dplyr::distinct(rows)
  rows$`_fields` <- paste0(c(cols,"type","norm_iri"), collapse=",")

  df <- ram_add(df, rows)
  df
}
