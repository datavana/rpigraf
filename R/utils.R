#'Remove NA entries from a list
#'
#' @keywords internal
#'
#' @param ... Not used, for compatibility with na.omit()
#' @return A cleaned list
#' @importFrom stats na.omit
#' @export
na.omit.list <- function(object, ...) {
  return(object[!vapply(object, function(x) all(is.na(x)), logical(1))])
}

#' Ask user to confirm script execution
#'
#' @keywords internal
confirmAction <- function() {
  silent <- Sys.getenv("epi_silent")
  if (silent == "TRUE") {
    return (TRUE)
  }

  user_input <- readline("Are you sure you want to proceed? (y/n)  ")
  if(user_input != 'y')
    stop('Canceled')
}

#' Check whether the URL is on a local server
#'
#' @param server The server URL
#' @keywords internal
isLocalServer <- function(server) {
  return (
    startsWith(server,"https://127.0.0.1") ||
      startsWith(server,"http://127.0.0.1") ||
      startsWith(server,"https://localhost") ||
      startsWith(server,"http://localhost")
  )
}

#' Remove HTML entities
#'
#' @keywords internal
#'
#' @param str The input character value
#' @return The output character value
unescape_html <- function(str){
  if (is.na(str)) {
    return (str)
  } else {
    return (xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>"))))
  }
}

#' Remove empty columns
#'
#' @keywords internal
#'
#' @param df A data frame
#' @return A data frame without columns that only contain NA values
drop_empty_columns <- function(df) {
  dplyr::select_if(df, ~{any(!is.na(.))})
}


#' Add columns if they are missing from the data frame
#'
#' @keywords internal
#'
#' @param df A data frame
#' @param cols A character vector with column names
#' @param default A value to set as default for missing columns
#' @return A data frame with new columns if necessary
add_missing_columns <- function(df, cols, default = NA) {
  missing_cols <- setdiff(cols, names(df))
  for (col in missing_cols) {
    df[[col]] <- default
  }
  df
}

#' Shift selected columns to the front
#'
#' @param df A data frame.
#' @param cols A vector of column names
#' @return A dataframe starting with the selected cols if they are present
move_cols_to_front <- function(df, cols) {
  existing_cols <- intersect(cols, colnames(df))
  other_cols <- setdiff(colnames(df), existing_cols)
  df[, c(existing_cols, other_cols), drop = FALSE]
}

#' Shift selected columns to the end
#'
#' @param df A data frame.
#' @param cols A vector of column names
#' @return A dataframe ending with the selected cols if they are present
move_cols_to_end <- function(df, cols) {
  existing_cols <- intersect(cols, colnames(df))
  other_cols <- setdiff(colnames(df), existing_cols)
  df[, c(other_cols, existing_cols), drop = FALSE]
}

#' Parse JSON columns
#'
#' @param data A character vector that may contain NAs
#' @return A parsed vector
#' @export
parse_json <- function(data) {
  data[data == "[]"] = "{}"
  data[is.na(data)] = "{}"

  jsonlite::stream_in(textConnection(data))
}

#' Merge list elements by their name
#'
#'@param l A list of lists to merge
#'@return A merged list
#'@export
merge_lists <- function(l) {
  keys <- unique(unlist(lapply(l, names)))
  l <- stats::setNames(do.call(mapply, c(FUN=c, lapply(l, `[`, keys))), keys)
  as.list(l)
}

#' Convert a number to letters, e.g. 3 becomes c
#'
#' @param number The number to convert
#' @param base The number of letters to use
#' @export
num2abc <- function(number, base = 26) {
  n <- ceiling(log((1/(1 - base) - 1 - number) * (1 - base), base = base)) - 1
  digits <- encode(number - sum(base^seq(0, n-1)), rep(base, n))
  paste(letters[digits + 1], collapse = "")
}

#' Convert letters to a number, e.g. c becomes 3
#'
#' @param s The string to convert (lowercase letters)
#' @param base The number of letters to use (default 26 for a-z)
#' @export
abc2num <- function(s, base = 26) {
  s <- tolower(s)
  chars <- strsplit(s, NULL)[[1]]
  digits <- match(chars, letters) - 1  # 0-index
  n <- length(digits)
  offset <- if(n > 1) sum(base ^ seq(0, n-2)) else 0
  number <- sum(digits * base ^ rev(seq(0, n-1)))
  number + offset + 1
}


#' Converts "b" using the "base"
#'
#' APL-decode / APL-base "_|_", pw10/02
#' @param b The number to convert
#' @param base The base
#' @export
decode <- function(b, base) {
  b <- as.integer(b)
  if( length(base) == 1 ) base<-rep(base,length(b))
  base<-c(base,1)
  number<-as.vector( cumprod(rev(base)[ 1:length(b) ] ) %*% rev(b) )
  number
}

#' Converts numbers using the radix vector
#'
#' APL-encode / APL-representation "T", pw 10/02
#'
#' @param number The number to convert
#' @param base The base
#' @export
encode <- function(number, base) {
  "base"
  n.base <- length(base); result <- matrix(0, length(base),
                                           length(number))
  for(i in n.base:1){
    result[i,] <- if(base[i]>0) number %% base[i] else number
    number     <- ifelse(rep(base[i]>0,length(number)),
                         floor(number/base[i]), 0)
  }
  return( if(length(number)==1) result[,1] else result )
}

#' Create distinct pseudonyms
#'
#' @param n Number of pseudonyms. Leave empty when used inside mutate to create value for each row
#' @return Vector of pseudonyms
#' @export
pseudonyms <- function(n) {

  if (missing(n)) {
    n = dplyr::n()
  }

  letters.consonant <- c("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")
  letters.vocal <- c("a","e","i","o","u")

  candidates <- c()
  todo <- n
  iterations <- 0

  while (todo > 0) {

    if (iterations > 100) {
      stop("Could not create sufficient values in 100 iterations.")
    }

    candidates <- unique(
      c(
        candidates,
        paste0(
          sample(stringr::str_to_upper(letters.consonant), size=todo, replace=T),
          sample(letters.vocal, size=todo, replace=T),

          sample(letters.consonant, size=todo, replace=T),
          sample(letters.vocal, size=todo, replace=T),

          sample(letters.consonant, size=todo, replace=T),
          sample(letters.vocal, size=todo, replace=T)
        )
      )
    )

    todo <- n - length(candidates)
    iterations <- iterations + 1
  }

  candidates
}


#' Bind rows of dataframes even if column types differ
#'
#' Convert columns that differ in type to character,
#' then bind rows.
#'
#' @param dataframes A list of dataframes
#' @return A list of dataframes with common column types
bind_rows_char <- function(dataframes){

  cols_names <- unique(unlist(lapply(dataframes, colnames)))
  cols_classes <- lapply(cols_names, function(colname)
    unique(sapply(dataframes, function(df) ifelse(
      colname %in% colnames(df),class(df[[colname]]),
      "NULL"
    ))
  ))
  cols_tocharacter <- lapply(cols_classes, \(col) (length(col[col != "NULL"]) > 1))
  names(cols_tocharacter) <- cols_names


  if (length(cols_tocharacter) > 0) {
    dataframes <- lapply(dataframes, \(df) {
      for (colname in colnames(df)) {
        if (cols_tocharacter[[colname]]) {
          df[[colname]] <- as.character(df[[colname]])
        }
      }
      df

    })
  }

  return(dplyr::bind_rows(dataframes))
}

#' Merge vectors
#'
#' @param values A named vector
#' @param default A second named vector
#' @return The first vector containing all additional named values from the second vector
merge_vectors <- function(values, default) {
  default[names(values)] <- values
  default
}

#' Set default values
#'
#' @param df A data frame
#' @param colname A column name as character value
#' @param default If the column is not present in the data frame, add it with the default value
#' @return A data frame where the given column was added if necessary
default_values <- function(df, colname, default) {
  if (!(colname %in% colnames(df))) {
    df[[colname]] <- default
  }
  df
}

#' Get file extension from URL path component
#'
#' @keywords internal
#'
#' @param path A file path
#' @return The file extension, without dot
get_extension <- function(path) {

  filename <- basename(path)

  # Check if filename has a dot and something after it
  if (grepl("\\.", filename)) {
    ext <- sub(".*\\.(.*)$", "\\1", filename)

    # If the dot is at the start (hidden files like .bashrc), treat as no extension:
    if (startsWith(filename, ".") && !grepl("\\..+\\.", filename)) {
      return("")
    } else {
      return(ext)
    }
  } else {
    return("")
  }
}

#' Join folder and filename
#'
#' @keywords internal
#'
#' @param filename File name
#' @param filepath Target folder or NULL
#' @return Joined path.
join_path <- function(filename, filepath = NULL) {
  if (is.null(filepath) || filepath == "") {
    return(filename)
  } else {
    return(file.path(filepath, filename))
  }
}

#' Inject character offsets into id-bearing XML elements
#'
#' Walks the XML tree once in document order, accumulating a running character
#' offset over all text and CDATA nodes, and stamps `data-start` and `data-end`
#' attributes onto every element that carries an `id` attribute. Offsets are
#' 1-based and inclusive, and refer to positions in the *plain* (tag-stripped)
#' text, i.e. the concatenation of all text nodes in document order.
#'
#' The input is wrapped in a synthetic `<root>` element so that XML fragments
#' with multiple top-level nodes can be parsed. Mixed content (interleaved text
#' and elements) is handled correctly: a parent element's span covers both its
#' text runs and its child elements, while children span only their own content.
#' Comment and processing-instruction nodes are skipped, consistent with how
#' [xml2::xml_text()] and `//text()` treat them.
#'
#' @keywords internal
#'
#' @param xml Character value containing XML text. May be a fragment with
#'   several top-level nodes.
#' @return An [xml2::xml_document] with `data-start` and `data-end` integer-valued
#'   attributes injected onto all elements that have an `id`. Feed the result to
#'   [extract_positions()] for a tidy data frame of spans.
#' @seealso [extract_segments()]
annotate_offsets <- function(xml) {
  doc <- xml2::read_xml(paste0("<root>", xml, "</root>"))
  offset <- 0L

  walk <- function(node) {
    start <- offset + 1L                    # 1-based inclusive
    kids  <- xml2::xml_contents(node)
    types <- xml2::xml_type(kids)

    for (i in seq_along(kids)) {
      tp <- types[i]
      if (tp == "text" || tp == "cdata") {
        offset <<- offset + nchar(xml2::xml_text(kids[[i]]))
      } else if (tp == "element") {
        walk(kids[[i]])
      }
    }

    # only stamp nodes we care about, to cut down on writes
    if (!is.na(xml2::xml_attr(node, "id"))) {
      xml2::xml_set_attr(node, "data-start", start)
      xml2::xml_set_attr(node, "data-end",   offset)
    }
  }

  walk(xml2::xml_root(doc))
  doc
}


#' Total length covered by a set of integer intervals
#'
#' Merges overlapping/adjacent `[start, end]` ranges and sums their widths, so
#' overlapping annotations are counted once. Ranges are 1-based and inclusive.
#'
#' @keywords internal
#'
#' @param start Integer vector of range starts.
#' @param end Integer vector of range ends.
#' @return A single integer: the number of distinct positions covered.
covered_length <- function(start, end) {
  if (length(start) == 0) return(0L)
  ord <- order(start, end)
  start <- start[ord]
  end   <- end[ord]

  total  <- 0L
  cur_lo <- start[1]
  cur_hi <- end[1]
  for (i in seq_along(start)[-1]) {
    if (start[i] <= cur_hi + 1L) {          # overlapping or adjacent
      cur_hi <- max(cur_hi, end[i])
    } else {
      total  <- total + (cur_hi - cur_lo + 1L)
      cur_lo <- start[i]
      cur_hi <- end[i]
    }
  }
  total + (cur_hi - cur_lo + 1L)
}
