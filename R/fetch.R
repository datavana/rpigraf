#' Fetch entity data such as articles, projects or properties
#'
#' Returns all data belonging to all entities matched by the params.
#' The procedure corresponds to calling the index action
#' with the columns parameter set to 0 in the Epigraf interface.
#'
#' @param table The table name (e.g. "articles")
#' @param params A named list of query params
#' @param db The database name
#' @param maxpages Maximum number of pages to request.
#'                 Set to 1 for non-paginated tables.
#' @export
api_fetch <- function(table, params=c(), db = NA, maxpages=1) {

  params["columns"] <- "0"
  params["idents"] <- "id"
  df <- api_table(table, params, db, maxpages)
  df$table <- stringr::str_extract(df$id,"^[a-z]+")
  df

  # fetch_table(table, "id", params, db, maxpages) |>
  #  fetch_entity()
}

#' Fetch tables such as articles, projects or properties
#'
#' Returns a row with defined columns for each record matched by the params.
#' The procedure corresponds to calling the index action in the Epigraf interface.
#'
#' @param table The table name (e.g. "articles")
#' @param columns A vector of column names.
#' @param params A named list of query params
#' @param db The database name
#' @param maxpages Maximum number of pages to request.
#'                 Set to 1 for non-paginated tables.
#' @export
fetch_table <- function(table, columns=c(), params=c(), db = NA, maxpages=1) {

  columns <-unique(c("id",columns))
  columns <- paste0(columns, collapse = ",")
  params["columns"] = columns

  params["idents"] <- "id"

  api_table(table, params, db, maxpages)
}

#' Fetch entities such as single articles, projects or properties
#'
#' Returns all data belonging to the entity identified by ID.
#' The procedure corresponds to calling the view action in the Epigraf interface.
#'
#' @param ids A character vector with IDs as returned by fetch_table, e.g. articles-1.
#'            Alternatively, provide a dataframe containg the IDs in the id-column.
#'            So you can chain fetch_articles() and fetch_entity()
#' @param params A named list of query params
#' @param db The database name. Leave empty when providing a dataframe produced by fetch_table().
#'           In this case, the database name will be extracted from the dataframe.
#' @param silent Whether to output a progress bar
#' @export
fetch_entity <- function(ids, params = c(), db = NULL, silent = FALSE) {
  # Get the database name from a dataframe
  if (is.null(db) && ("epi_tbl" %in% class(ids))) {
    db <- attr(ids, "source")["db"]
  }

  if (!is.na(db)) {
    check_is_db(db)
  }

  # Get the ID vector from a dataframe
  if (is.data.frame(ids)) {
      ids <- ids[["id"]]
  }

  # Iterate all IDs
  if (length(ids) > 1) {
    if (!silent) {
      cli::cli_progress_bar("Fetching data", type="iterator", total = length(ids))
    }
    data <- tibble::tibble()

    for (id in ids) {
      data <- bind_rows_char(
        list(
          data,
          fetch_entity(id, params, db, silent = TRUE)
        )
      )

      if (!silent) {
        cli::cli_progress_update(status=id)
      }
    }

    if (!silent) {
      cli::cli_progress_done()
    }
    return (data)
  }

  if (length(ids) == 0) {
    data <- .to_epitable(tibble::tibble(), c("params" = params, "db"=db))
    return (data)
  }

  # Get data
  check_is_id(ids)
  id <- strsplit(ids,"-", TRUE)[[1]]
  table <- id[1]
  id <- id[2]

  data <- api_table(paste0(table,"/view/", id), params, db, 1, silent = silent)
  data <- tidyr::separate_wider_delim(data, id, delim="-", names=c("table","row"), cols_remove = F)
  .to_epitable(data)
}
