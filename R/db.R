#
# Functions for direct database access
#

#' Save database connection settings to environment variables.
#' Environment variables are prefixed with "epi_" and used in db_connect()
#' to establish the connection.
#'
#' @param host host
#' @param port port
#' @param username username
#' @param password password
#' @param database database
#' @export
db_setup <- function(host="localhost", port=3306, username="root", password="root", database="") {
  settings <- as.list(environment())
  settings <- stats::setNames(settings, paste0("epi_",names(settings)))
  do.call(Sys.setenv, settings)
}

#' Get a connection to a database
#'
#' Before you can use this function, call db_setup once
#' to set the connection parameters.
#' All parameters are stored in the environment.
#'
#' @param db Name of the database as string.
#'           Leave empty to use the database
#'           name from the environment settings.
#' @export
db_connect <- function(db=NULL) {
  if (missing(db)) {
    db <- Sys.getenv("epi_dbname")
  }

  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = Sys.getenv("epi_host"),
    port = as.numeric(Sys.getenv("epi_port")),
    username = Sys.getenv("epi_username"),
    password = Sys.getenv("epi_password"),
    dbname = db
  )

  return(con)
}

#' Get the database name from a connection object
#'
#' @param con A connection object
#' @importFrom rlang .data
#' @export
db_name <- function(con) {
  na.omit(stringr::str_extract(utils::capture.output(summary(con)),"(?<=Dbname: )[^ ]+"))
}


#' Get list of all databases
#'
#' @param epi Only keep databases with the epi-prefix and join meta data from the main database
#' @importFrom rlang .data
#' @export
db_databases <- function(epi = TRUE) {
  con <- db_connect()
  dbs = DBI::dbGetQuery(con,"SHOW DATABASES;")
  DBI::dbDisconnect(con)
  rm(con)

  colnames(dbs) <- c("name")

  if (epi) {
    dbs <- dplyr::filter(dbs, stringr::str_starts(.data$name,"epi_"))
    meta <- db_table("databanks", db= DB_MAIN, deleted=T)
    dbs <- full_join(dbs, meta, by="name")
  }

  return(dbs)
}



#
#' Construct filter conditions for the db_table() function
#'
#' @param table Table name. Leave empty to omit the table prefix.
#' @param field Field name.
#' @param value A single value, a list of characters or a list of integers.
#' @export
db_condition <- function(table = NA, field, value) {

  # preprocess value(s) for sql

  # if items in list are numeric --> collapse without ''
  if (all(is.numeric(value))) {
    value = paste("(", paste(value, collapse = ","), ")",sep = "")
  }

  # if items in list are characters --> collapse  ''
  else if (all(is.character(value))) {
    value = paste("('", paste(value, collapse = "','"), "')",sep = "")
  }

  # create statement of type "col in list"
  if (!is.na(table)) {
    field <- paste0(table, ".", field)
  }
  statement = paste0(field, " in ", value)

  return (statement)
}


#
#' Get data from a database table
#'
#' @param table Table name.
#' @param cond Either a named list of conditions, or a full condition as character, or a character vector of conditions, e.g. `c("id = 10")`.
#' @param deleted Deleted records are skipped by default. Set to TRUE, to get all records.
#' @param compact Whether to rename type columns to `type` and to add a `table` and a `database` column.
#' @param db A connection object (object) or the database name (character).
#'           Provide a character vector of dababase names to get and row bind data from multiple databases.
#'           In this case, the compact parameter is automatically set to TRUE. Thus, a database name column is added.
#' @export
db_table <- function(table, cond=list(), deleted = FALSE, compact = FALSE, db){

  # If db is a character vector of length > 1, iterate and bind
  if (is.character(db) && length(db) > 1) {
    return(
      dplyr::bind_rows(
        lapply(db, function(singledb) {
          db_table(
            table = table,
            cond = cond,
            deleted = deleted,
            compact = TRUE,
            db = singledb
          )
        })
      )
    )
  }

  # Single DB handling
  if (is.character(db)) {
    con <- db_connect(db)
  } else {
    con <- db
  }

  # Construct SQL
  sql <- paste0("SELECT * FROM ", table)

  # Add deleted condition
  if (!deleted) {
    cond <- c("deleted = 0", cond)
  }

  # Convert condition list to character vector
  if (is.list(cond)) {
    cond <- lapply(seq_along(cond), function(i) {
      cond_name <- names(cond)[i]
      cond_value <- cond[[i]]
      if (!is.null(cond_name) && nzchar(cond_name)) {
        db_condition(NA, cond_name, cond_value)
      } else {
        cond_value
      }
    })
    cond <- as.vector(cond)
  }

  # Add all conditions to the query
  if (length(cond) > 0) {
    cond <- paste0("(", cond, ")")
    cond <- paste0(cond, collapse = " AND ")
    sql <- paste0(sql, " WHERE ", cond)
  }

  # Get data
  df <- dplyr::as_tibble(
    DBI::dbGetQuery(con, sql)
  )

  df <- dplyr::mutate_if(
    df,
    is.character,
    .funs = function(x) { return(`Encoding<-`(x, "UTF-8")) }
  )

  # Close connection
  if (is.character(db)){
    DBI::dbDisconnect(con)
  }

  if (compact) {
    df$table <- table
    df$database <- db

    typecol <- colnames(df)
    typecol <- typecol[grepl("^[a-z]+type$", typecol)]
    if (length(typecol) == 1) {
      df$type <- df[[typecol]]
      df[[typecol]] <- NULL
    }
  }

  return(df)
}

