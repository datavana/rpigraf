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
#' Params may contain sublists named by a table name to target conditions at a
#' specific table, e.g.
#' \code{params = list("properties" = list("propertytype" = "fonttypes"),
#'                     "articles"   = list("articletype"  = "epi-article"))}.
#' Any parameters not nested under one of the tables \code{articles},
#' \code{sections}, \code{items}, \code{properties}, \code{links},
#' \code{footnotes} or \code{projects} are passed to the first (root)
#' \code{db_table} call.
#'
#' @param table The table name (e.g. "articles").
#' @param params A named list of query conditions, passed to db_table.
#'              May contain sublists named by a table name to route conditions
#'              to that table.
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

  # Split params into table-specific sublists and root params.
  # Sublists named by one of the known tables are routed to that table's
  # db_table call; everything else goes to the first (root) call.
  known_tables <- c("articles", "sections", "items", "properties", "links", "footnotes", "projects")
  table_params <- list()
  root_params  <- list()
  for (nm in names(params)) {
    if (!is.null(nm) && nm %in% known_tables && is.list(params[[nm]])) {
      table_params[[nm]] <- params[[nm]]
    } else {
      root_params[[nm]] <- params[[nm]]
    }
  }

  # Merge base query conditions with any table-specific extra conditions.
  merge_params <- function(base, tbl) {
    if (is.null(table_params[[tbl]])) {
      return (base)
    }
    merge_lists(list(base, table_params[[tbl]]))
  }

  df_root <- db_table(table, merge_params(root_params, table), db = db, compact = TRUE)
  df <- df_root

  # Get contained article data
  # TODO: Use joins
  if ((table == "articles") && (nrow(df_root) > 0)) {
    df_root$project <- df_root$projects_id
    df_root$projects_id <- NULL

    df$project <- df$projects_id
    df$projects_id <- NULL

    df_sections <- db_table("sections", merge_params(list("articles_id" = df_root$id), "sections"), db = db, compact = TRUE)
    df <- bind_rows_char(list(df, df_sections))

    df_items <- db_table("items", merge_params(list("articles_id" = df_root$id), "items"), db = db, compact = TRUE)
    df_items$property <- df_items$properties_id
    df_items$properties_id <- NULL
    df <- bind_rows_char(list(df, df_items))

    items_props <- df_items[!is.na(df_items$property),]$property
    if (length(items_props) > 0) {
      df_props <- db_table("properties", merge_params(list("id" = items_props), "properties"), db = db, compact = TRUE)
      df <- bind_rows_char(list(df, df_props))
    }

    df_footnotes <- db_table("footnotes", merge_params(list("root_tab" = "articles", "root_id" = df_root$id), "footnotes"), db = db, compact = TRUE)
    df <- bind_rows_char(list(df, df_footnotes))

    df_links <- db_table("links", merge_params(list("root_tab" = "articles", "root_id" = df_root$id), "links"), db = db, compact = TRUE)
    df <- bind_rows_char(list(df, df_links))

    links_props <- dplyr::filter(df_links, .data$to_tab == "properties", !is.na(.data$to_id))
    if (nrow(links_props) > 0) {
      df_props <- db_table("properties", merge_params(list("id" = links_props$to_id), "properties"), db = db, compact = TRUE)
      df <- bind_rows_char(list(df, df_props))
    }

    df_projects <- db_table("projects", merge_params(list("id" = df_root$project), "projects"), db = db, compact = TRUE)
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
