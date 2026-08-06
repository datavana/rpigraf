#
# Functions for low level API access to Epigraf
#


#' Save API connection settings to environment variables
#'
#' @param apiserver URL of the Epigraf server (including https-protocol).
#' @param apitoken Access token. If NULL, you will be asked to enter the token.
#' @param verbose Show debug messages and the built URLs.
#' @export
api_setup <- function(apiserver, apitoken = NULL, verbose = FALSE) {
  if (missing(apitoken)) {
    apitoken <- readline(prompt="Please, enter your access token:")
  }
  settings <- as.list(environment())
  settings <- stats::setNames(settings, paste0("epi_",names(settings)))
  do.call(Sys.setenv, settings)
}

#' Set silent mode
#'
#' In silent mode, all user prompts are automatically confirmed.
#' Be careful, this will skip the prompt to confirm operations
#' on the live server.
#'
#' @param silent Boolen
#' @export
api_silent <- function(silent = F) {
  Sys.setenv("epi_silent" = silent)
}

#' Build base URL
#'
#' @param endpoint The endpoint, e.g. articles/import.
#' @param query Query parameters for the endpoint.
#' @param database The database name.
#' @param extension Extension added to the URL path, defaults to json.
#' @export
api_buildurl <- function(endpoint, query=NA, database=NA, extension="json") {

  # Get server and token from the global settings
  server <- Sys.getenv("epi_apiserver")
  token <- Sys.getenv("epi_apitoken")
  verbose <- Sys.getenv("epi_verbose") == "TRUE"
  silent <- Sys.getenv("epi_silent") == "TRUE"

  url <- httr::parse_url(server)
  url$query$token <- token

  # Add query parameters
  if (!all(is.na(query)))  {
    url$query <-  merge_lists(list(url$query,as.list(query)))
  }

  if (!stringr::str_starts(endpoint,"/")) {
    endpoint <-  paste0("/", endpoint)
  }

  # Merge endpoint URL (if it contains query params or an extension)
  parsed_endpoint <- httr::parse_url(endpoint)
  url$query = merge_lists(list(url$query, as.list(parsed_endpoint$query)))
  endpoint = parsed_endpoint$path
  endpoint_extension <- get_extension(endpoint)

  if (endpoint_extension != "") {
    extension <-  ""
  }
  else if (is.na(extension)) {
    extension <-  ""
  }
  else if (!is.na(extension) && !stringr::str_starts(extension,"\\.")) {
    extension <-  paste0(".", extension)
  }


  if (!is.na(database)) {
    url$path <- paste0("epi/",database,endpoint,extension)
  } else {
    url$path <- paste0(endpoint, extension)
  }

  url <- httr::build_url(url)
  if (verbose && !silent) {
    print(url)
  }

  return(url)

}


#' Create and execute a job
#'
#' @param endpoint The endpoint supporting job creation.
#' @param params Query parameters.
#' @param database The selected database.
#' @param payload The data posted to the job endpoint.
#' @return void
#' @export
api_job_create <- function(endpoint, params, database, payload=NULL) {
  verbose <- Sys.getenv("epi_verbose") == "TRUE"
  server <- Sys.getenv("epi_apiserver")

  silent <- Sys.getenv("epi_silent")
  if (silent != "TRUE") {
    print(paste0("Creating job on server ", server))
  }

  if (!isLocalServer(server)) {
    confirmAction()
  }

  # 1. Create job
  url <- api_buildurl(endpoint, params, database)

  if (verbose)  {
    resp <- httr::POST(url, body=payload, encode="json", httr::set_cookies(XDEBUG_SESSION=XDEBUG_COOKIE))
  } else {
    resp <- httr::POST(url, body=payload, encode="json")
  }


  body <- httr::content(resp)
  job_id <- purrr::pluck(body,"job_id",.default = NA)

  error <- F
  message <- NA

  # Request error
  if (resp$status_code != 200)
  {
    error <- T
    message <- purrr::pluck(body,"error","message",.default = NA)
  }

  # Job error
  else if (purrr::pluck(body,"success",.default = TRUE) != TRUE)
  {
    error <- T
    message <- purrr::pluck(body,"message",.default = NA)
  }

  # No job ID
  else if (is.na(job_id))
  {
    error <- T
    message <- "No job ID found."
  }


  if (error) {
    stop(paste0("Could not create job: ", message))
  }

  if (!is.na(message)){
    print(message)
  }

  # 2. Execute job
  api_job_execute(job_id)
}

#' Execute a job
#'
#' @param job_id The job ID
#' @return Whether the job was finished without error.
#' @export
api_job_execute <- function(job_id) {
  verbose <- Sys.getenv("epi_verbose") == "TRUE"
  cli::cli_alert_info("Starting job {job_id}.")

  url = api_buildurl(paste0("jobs/execute/", job_id))

  result = list()
  polling <- T
  error <- NA
  message <- NA
  pb <- cli::cli_progress_bar("Executing", total = NA, .auto_close = FALSE)

  while (polling) {

    if (verbose) {
      resp <- httr::POST(url, httr::set_cookies(XDEBUG_SESSION=XDEBUG_COOKIE))
    } else {
      resp <- httr::POST(url)
    }

    body <- httr::content(resp)
    newresult <- NA

    # Request error
    if (resp$status_code != 200)
    {
      polling <- F
      error <- T
      message <- purrr::pluck(body,"error","message",.default = NA)
    }

    # Job error
    else if (purrr::pluck(body,"job","error",.default = FALSE) != FALSE)
    {
      polling <- F
      error <- T
      message <- purrr::pluck(body,"job","error",.default = NA)
    }

    # Continue
    else if (!is.na(purrr::pluck(body,"job","nextUrl",.default = NA)))
    {
      polling <- T
      error <- F
      message <- purrr::pluck(body,"job","message",.default = NA)
      newresult <- purrr::pluck(body,"job","result",.default = NA)

      delay <- purrr::pluck(body,"job","delay",.default = 0)
      if (delay > 0) {
        Sys.sleep(1)
      }

      #url <- purrr::pluck(body,"job","nexturl",.default = NA)
      #api_buildurl(url, NA, "epi_all")

      progressCurrent <- purrr::pluck(body, "job", "progress",    .default = NA)
      progressMax     <- purrr::pluck(body, "job", "progressmax", .default = -1)

      # if (progressMax == -1) {
      #   print(paste0("Progress ", progressCurrent))
      # } else {
      #   print(paste0("Progress ", progressCurrent, " / ", progressMax))
      # }

      if (progressMax > 0 && !is.na(progressCurrent)) {
        cli::cli_progress_update(id = pb, total = progressMax, set = progressCurrent)
      } else {
        cli::cli_progress_update(id = pb, force = TRUE)
      }
    }

    # Finished
    else
    {
      polling <- F
      error <- F
      message <- purrr::pluck(body,"message",.default = NA)
      newresult <- purrr::pluck(body,"job","result",.default = NA)
    }

    if (!is.na(newresult)) {
      result = append(result, list(newresult))
    }

    # Output
    if (!is.na(message)) {
      cli::cli_progress_output(message, id = pb)
    }
  }

  cli::cli_progress_done(id = pb)

  # Extract solved IDs
  solved <- lapply(result, \(x) tibble::enframe(unlist(x$solved)))
  result <- lapply(result, \(x) {x$solved <- NULL;x })

  if (length(solved) > 0) {
    solved <- do.call(rbind, solved)
    solved <- dplyr::distinct(solved)
  }

  # Extract downloads
  downloads <- lapply(result, \(x) purrr::map_dfr(x$downloads, ~ as_tibble(.x)))
  result <- lapply(result, \(x) {x$downloads <- NULL;x })

  if (length(downloads) > 0) {
    downloads <- do.call(rbind, downloads)
    downloads <- dplyr::distinct(downloads)
  }

  result <- list(
    polling = polling,
    error = error,
    message = message,
    data = result,
    solved = solved,
    downloads = downloads
  )

  class(result) <- c("epi_job", setdiff(class(result), "epi_job"))

  return (invisible(result))
}


#' Download tables
#'
#' Fetches tables such as articles, projects or properties
#'
#' TODO: silent problem message (false positive, results from the last column being empty from case to case)
#' TODO: add progress bar
#'
#' @param endpoint The endpoint path (e.g. "articles/index" or "articles/view/1")
#' @param params A named list of query params
#' @param db The database name.
#'           Provide a character vector of dababase names to get and row bind data from multiple databases.
#'           In this case, the compact parameter is automatically set to TRUE. Thus, a database name column is added.
#' @param maxpages Maximum number of pages to request.
#'                 Set to 1 for non-paginated tables.
#' @param compact Whether to rename type columns to `type` and to add a `table` and a `database` column.
#' @param silent Whether to output status messages
#' @export
api_table <- function(endpoint, params=c(), db = NA, maxpages=1, compact = FALSE, silent=FALSE) {

  # If db is a character vector of length > 1, iterate and bind
  if (is.character(db) && length(db) > 1) {

      data <- dplyr::bind_rows(
        lapply(db, function(singledb) {
          api_table(
            endpoint = endpoint,
            params = params,
            db = singledb,
            maxpages = maxpages,
            compact = TRUE,
            silent = silent
          )
        })
      )

      return(.to_epitable(data, c("endpoint"=endpoint, "params"=params, "db"=db)))
  }

  verbose <- Sys.getenv("epi_verbose") == "TRUE"

  data = data.frame()
  page = 1

  fetchmore <- TRUE
  while (fetchmore) {
    params["page"] <- page
    url = api_buildurl(endpoint, params, db, "csv")

    if (!silent) {
      if (maxpages == 1) {
        message(paste0("Fetching data from ", endpoint,"."))
      } else {
        message(paste0("Fetching page ", page ," from ", endpoint,"."))
      }
    }
    message <- NA

    rows <- tryCatch(
      {

        if (verbose)  {
          resp <- httr::GET(url, httr::set_cookies(XDEBUG_SESSION=XDEBUG_COOKIE))
        } else {
          resp <- httr::GET(url)
        }

        if (resp$status_code == 200) {
          body <- httr::content(resp,as="text")
          rows <- suppressWarnings(readr::read_delim(I(body), delim=";", col_types = readr::cols(.default = readr::col_character())))
        }

        else if (resp$status_code == 404) {
          message <- "No more data found."
          rows <- data.frame()
        }

        else {
          rows <- data.frame()
          message <- paste0("Error ",resp$status_code,": ", httr::content(resp))
        }

        rows
      },
      error=function(msg) {
        message <- msg
        data.frame()
      }
    )

    if (!is.na(message)) {
      print(message)
    }

    if (nrow(rows) > 0) {
      data <- bind_rows_char(list(data, rows))
      fetchmore <- (page < maxpages)
      page <- page + 1
    } else {
      fetchmore = F
    }
  }


  if (compact && nrow(data) > 0) {

    data$database <- db
    table_default <- stringr::str_extract(endpoint,"^[a-z]+")
    table_id <- stringr::str_extract(data$id,"^[a-z]+")
    data$table <- dplyr::coalesce(data$table, table_id, table_default)

    typecol <- colnames(data)
    typecol <- typecol[grepl("^[a-z]+type$", typecol)]
    if (length(typecol) == 1) {
      data$type <- data[[typecol]]
      data[[typecol]] <- NULL
    }
  }

  if (!silent) {
    print (paste0("Fetched ", nrow(data) ," records from ", endpoint,"."))
  }

  if (nrow(data) > 0) {
    data <- suppressMessages(readr::type_convert(data))
  }

  .to_epitable(data, c("endpoint"=endpoint, "params"=params, "db"=db))
}


#' Fetch tables such as articles, projects or properties
#'
#' Returns a row with defined columns for each record matched by the params.
#' The procedure corresponds to calling the index action in the Epigraf interface.
#'
#' @param table The table name (e.g. "articles")
#' @param columns A vector of column names.
#' @param params A named list of query params
#' @param db The database name.
#'           Provide a character vector of dababase names to get and row bind data from multiple databases.
#' @param maxpages Maximum number of pages to request.
#'                 Set to 1 for non-paginated tables.
#' @export
api_fetch_table <- function(table, columns=c(), params=c(), db = NA, maxpages=1) {

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
#' @param ids A character vector with IDs as returned by api_fetch_table, e.g. articles-1.
#'            Alternatively, provide a dataframe containg the IDs in the id-column.
#'            So you can chain fetch_articles() and api_fetch_entity()
#' @param params A named list of query params
#' @param db The database name. Leave empty when providing a dataframe produced by api_fetch_table().
#'           In this case, the database name will be extracted from the dataframe.
#' @param silent Whether to output a progress bar
#' @export
api_fetch_entity <- function(ids, params = c(), db = NULL, silent = FALSE) {
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
          api_fetch_entity(id, params, db, silent = TRUE)
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


#' Post data to epigraf
#'
#' @param endpoint The endpoint path
#' @param params Query parameters
#' @param payload The data posted to the endpoint
#' @param database The selected database
#' @return void
#' @export
api_post <- function(endpoint, params=c(), payload=NULL, database = NA) {
  result <- .api_request(endpoint, params, payload, database, httr::POST)
  return (invisible(result))
}

#' Upload file to epigraf
#'
#' @param endpoint The endpoint path
#' @param params Query parameters
#' @param filepath A full path to the local file
#' @param mimetype The mime type fof the file. Will be guessed if empty.
#' @param overwrite Whether to overwrite existing files.
#' @param database The selected database
#' @return void
#' @export
api_upload <- function(endpoint, params=c(), filepath=NULL, mimetype = NULL, overwrite = FALSE, database = NA) {
  payload <- list("FileData[0]" = httr::upload_file(filepath, mimetype), "FileOverwrite" = ifelse(overwrite,"1","0"))
  result <- .api_request(endpoint, params, payload, database, httr::POST, encode = "multipart")
  return (invisible(result))
}


#' Download a file from Epigraf
#'
#' @param endpoint The endpoint path.
#' @param params Query parameters.
#' @param filename A file name or a full path to the local file.
#' @param filepath A target folder path
#' @param overwrite Whether to overwrite existing files.
#' @param database The selected database.
#' @return A list with error and data elements.
#' @export
api_download <- function(endpoint, params=c(), filename=NULL, filepath=NULL, overwrite = FALSE, database = NA) {

  verbose <- Sys.getenv("epi_verbose") == "TRUE"
  server <- Sys.getenv("epi_apiserver")

  silent <- Sys.getenv("epi_silent")
  if (silent != "TRUE") {
    print(paste0("Downloading file from ", server))
  }

  destfile <- join_path(filename, filepath)

  # 1. Post data
  url <- api_buildurl(endpoint, params, database, extension = NA)

  if (verbose)  {
    resp <- httr::GET(url, httr::write_disk(destfile, overwrite = overwrite),  httr::set_cookies(XDEBUG_SESSION=XDEBUG_COOKIE))
  } else {
    resp <- httr::GET(url, httr::write_disk(destfile, overwrite = overwrite))
  }


  error <- F
  message <- NA

  # Request error
  if (resp$status_code != 200)
  {
    error <- T
    body <- httr::content(resp)
    message <- purrr::pluck(body,"error","message",.default = NA)
  }

  if (!is.na(message)){
    print(message)
  } else {
    print(paste0("Downloaded file to ", destfile))
  }

  result <- list(
    error = error,
    data = destfile
  )

  return (invisible(result))
}

#' Delete epigraf data
#'
#' @param endpoint The endpoint path
#' @param params Query parameters
#' @param payload The data posted to the endpoint
#' @param database The selected database
#' @return void
#' @export
api_delete <- function(endpoint, params=c(), payload=NULL, database = NA) {
  result <- .api_request(endpoint, params, payload, database, httr::DELETE)
  return (invisible(result))
}

#' Patch data
#'
#' Update entities in the database using the API.
#' Existing entities will be updated, missing entities will be created.
#' The function supports uploading all data related to articles:
#' articles, sections, items, links, footnotes, properties, projects, users, types.
#' The IRI path in the ID column of the dataframe must contain the specific table name.
#'
#' @param data A dataframe with the column `id`.
#'             Additional columns such as `norm_data` will be written to the entity.
#'             The id must either be a valid IRI path (e.g. `properties/objecttypes/xxx`)
#'             or an id prefixed by the table name (e.g. `properties-12`).
#'             Patching properties with prefixed ids requires a `type` column
#'             that contains the property type.
#'             If wide is set to TRUE (default), column names prefixed with table names are extracted.
#' @param db The database name.
#' @param table Optional: Check that the data only contains rows for a specific table.
#' @param type Optional: Check that the data only contains rows with a specific type.
#' @param wide Convert wide format to long format.
#'             If TRUE, column names prefixed with "properties", "items", "sections", "articles"
#'             and "projects" followed by a dot (e.g. `properties.id`, `properties.lemma`)
#'             will be extracted and patched as additional entities.
#' @export
api_patch <- function(data, db, table=NA, type=NA, wide=T) {

  if (wide) {
    data <- epi_wide_to_long(data)
  }

  stopifnot(epi_is_iripath(data$id, table, type) | epi_is_id(data$id, table))

  data <- data |>

    # Reorder
    dplyr::select(tidyselect::all_of("id"), tidyselect::everything()) |>

    # Remove complete empty columns
    dplyr::select(tidyselect::where(~!all(is.na(.x)))) |>

    # Remove rows where all values are NA
    dplyr::filter(dplyr::if_any(tidyselect::everything(), ~ !is.na(.)))


  if ((nrow(data) == 0) || (ncol(data) == 0)) {
    stop("Data is empty or contains NA values.")
  }

  if ((ncol(data) == 1) && (colnames(data) == "id")) {
    stop("Skipped, the data only contains the ID column.")
  }

  print(paste0("Uploading ",nrow(data)," rows."))

  api_job_create("articles/import", NA, db, list(data=data))
}

#' Send request to epigraf
#'
#' @keywords internal
#' @param endpoint The endpoint supporting job creation
#' @param params Query parameters
#' @param payload The data posted to the endpoint
#' @param database The selected database
#' @param method One of the httr functions (httr::POST, httr::DELETE)
#' @param encode Payload encoding. Passed to the httr method function.
#'               See httr::POST for how to upload files with multipart encoding.
#' @return void
.api_request <- function(endpoint, params=c(), payload=NULL, database = NA, method = httr::POST, encode = "json") {
  verbose <- Sys.getenv("epi_verbose") == "TRUE"
  server <- Sys.getenv("epi_apiserver")

  silent <- Sys.getenv("epi_silent")
  if (silent != "TRUE") {
    print(paste0("Posting data to ", server))
  }

  if (!isLocalServer(server)) {
    confirmAction()
  }

  # 1. Post data
  url <- api_buildurl(endpoint, params, database)

  if (verbose)  {
    resp <- method(url, body=payload, encode=encode, httr::set_cookies(XDEBUG_SESSION=XDEBUG_COOKIE))
  } else {
    resp <- method(url, body=payload, encode=encode)
  }


  body <- httr::content(resp)

  error <- F
  message <- NA

  # Request error
  if (resp$status_code != 200)
  {
    error <- T
    message <- purrr::pluck(body,"error","message",.default = NA)
  }

  # Post error
  else if (purrr::pluck(body,"status","success",.default = TRUE) != TRUE)
  {
    error <- T
    message <- purrr::pluck(body,"status","message",.default = NA)
  }

  if (!is.na(message)){
    print(message)
  }

  result <- list(
    error = error,
    data = body
  )

  return (invisible(result))
}


#' Add the epi_tbl class and make it remember its source
#'
#' @param data A tibble
#' @param source A named vector of source parameters, containing endpount, parameters and database name
.to_epitable <- function(data, source=NULL) {
  if (!is.null(source)) {
    attr(data, "source") <- source
  }

  # Reorder columns
  id_cols <-  intersect(c("database", "table", "row", "type", "norm_iri"), names(data))
  belongsto_id_cols <- grep("id$", names(data), value = TRUE)
  belongsto_name_cols <- intersect(c("project","article","section","item","property","footnote"), names(data))
  state_cols <- grep("^(created)|(modified)", names(data), value = TRUE)
  content_cols <- setdiff(names(data), c(id_cols, belongsto_id_cols, belongsto_name_cols, state_cols))

  data <- dplyr::select(
    data,
    dplyr::all_of(id_cols),
    dplyr::all_of(content_cols),
    dplyr::all_of(belongsto_name_cols),
    dplyr::all_of(belongsto_id_cols),
    dplyr::all_of(state_cols)
  )


  class(data) <- c("epi_tbl", setdiff(class(data), "epi_tbl"))
  data
}
