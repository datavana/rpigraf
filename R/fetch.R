#' Fetch entity data such as articles, projects or properties from the API
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
  df <- api_table(table, params, db, maxpages, compact = TRUE)
  df <- dplyr::distinct(df)
  df <- move_cols_to_front(df, c("database", "table", "type", "id"))
  df
}

#' Fetch entity data such as articles, projects or properties using direct database access
#'
#' Returns all data belonging to all entities matched by the params.
#'
#' @param table The table name (e.g. "articles").
#' @param params A named list of query conditions, passed to db_table.
#' @param db The database name (character).
#'           Provide a character vector of dababase names to get and row bind data from multiple databases.
#' @importFrom rlang .data
#' @export
db_fetch <- function(table, params=list(), db = NA) {

  # If db is a character vector of length > 1, iterate and bind
  if (is.character(db) && length(db) > 1) {
    return(
      dplyr::bind_rows(
        lapply(db, function(singledb) {
          db_fetch(
            table = table,
            params = params,
            db = singledb
          )
        })
      )
    )
  }

  df_root <- db_table(table, params, db = db, compact = TRUE)
  df <- df_root

  # Get contained article data
  # TODO: Use joins
  if ((table == "articles") && (nrow(df_root) > 0)) {
    df_root$project <- df_root$projects_id
    df_root$projects_id <- NULL

    df$project <- df$projects_id
    df$projects_id <- NULL

    df_sections <- db_table("sections", list("articles_id" = df_root$id), db = db, compact = TRUE)
    df <- bind_rows_char(list(df, df_sections))

    df_items <- db_table("items", list("articles_id" = df_root$id), db = db, compact = TRUE)
    df_items$property <- df_items$properties_id
    df_items$properties_id <- NULL
    df <- bind_rows_char(list(df, df_items))

    items_props <- df_items[!is.na(df_items$property),]$property
    if (length(items_props) > 0) {
      df_props <- db_table("properties", list("id" = items_props), db = db, compact = TRUE)
      df <- bind_rows_char(list(df, df_props))
    }

    df_footnotes <- db_table("footnotes", list("root_tab" = "articles", "root_id" = df_root$id), db = db, compact = TRUE)
    df <- bind_rows_char(list(df, df_footnotes))

    df_links <- db_table("links", list("root_tab" = "articles", "root_id" = df_root$id), db = db, compact = TRUE)
    df <- bind_rows_char(list(df, df_links))

    links_props <- dplyr::filter(df_links, .data$to_tab == "properties", !is.na(.data$to_id))
    if (nrow(links_props) > 0) {
      df_props <- db_table("properties", list("id" = links_props$to_id), db = db, compact = TRUE)
      df <- bind_rows_char(list(df, df_props))
    }

    df_projects <- db_table("projects", list("id" = df_root$project), db = db, compact = TRUE)
    df <- bind_rows_char(list(df, df_projects))

  }

  # Add property ancestors
  while (TRUE) {

    props_all <- df[df$table=="properties",]
    props_all <- dplyr::distinct(props_all)

    if(nrow(props_all) == 0) {
      break
    }

    props_missing <- props_all[!is.na(props_all$parent_id),, drop = FALSE]
    props_missing <- dplyr::anti_join(props_missing, props_all, by = c("parent_id" = "id"))
    props_missing <- unique(props_missing$parent_id)

    if (length(props_missing) == 0) {
      break
    }

    props_missing <- db_table("properties", list("id" = props_missing), db = db, compact = TRUE)

    if (nrow(props_missing) == 0) {
      break
    }

    df <- bind_rows_char(list(df, props_missing))
  }


  df <- drop_empty_columns(df)
  df <- move_cols_to_front(df, c("database", "table", "type", "id"))
  df

}
