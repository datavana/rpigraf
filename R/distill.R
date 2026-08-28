#' Get articles
#'
#' Section, item, and property data can be joined.
#'
#' @param df A RAM data frame.
#' @param cols Article columns.
#' @param section.type Section types to join.
#'        The result contains only items within sections of the given type.
#'        Set to NULL to get all items.
#' @param section.cols Cols to join from the sections.
#' @param item.type Item types to join.
#' @param item.cols Cols to join from the items.
#' @param property.cols Cols to join from the property.
#' @return A tibble with articles.
#' @export
distill_articles <- function(df, cols = c(), section.type = NULL, section.cols =c(), item.type = NULL, item.cols = c(), property.cols = c()) {
  cases <- df[df$table == "articles", unique(c("id","type","norm_iri", cols))]
  cases <- dplyr::distinct(cases)

  extract.cols <- c()
  if (length(section.cols) > 0) {
    extract.cols <- c(extract.cols, c(paste0("sections.",section.cols)))
  }
  if (length(item.cols) > 0) {
    extract.cols <- c(extract.cols, c(paste0("items.",item.cols)))
  }
  if (length(property.cols) > 0) {
    extract.cols <- c(extract.cols, c(paste0("properties.",property.cols)))
  }

  if (length(extract.cols) > 0) {
    items <- epi_extract_long(df, "items", item.type)

    if (!missing(property.cols)) {
      props <- epi_extract_long(df, "properties")
      if ((nrow(props) > 0) && (nrow(items) > 0)) {
        items <- dplyr::left_join(items,props, by = c("items.property" = "properties.id"))
      }
    }

    if (!missing(section.cols)) {

      sections <- epi_extract_long(df, "sections", section.type)

      if ((nrow(sections) > 0) && (nrow(items) > 0)) {
        items <- dplyr::inner_join(sections, items, by = c("sections.id" = "items.sections_id"))
      }
    }

    items <- items[, c("items.articles_id", extract.cols),drop = FALSE]
    #colnames(items) <- c("items.articles_id","value")

    items <- items |>
      dplyr::mutate(dplyr::across(tidyselect::any_of(extract.cols), ~ stringr::str_replace_all(.x,"&amp;","&"))) |>
      dplyr::mutate(dplyr::across(tidyselect::any_of(extract.cols), ~ stringr::str_replace_all(.x,"&x2f;","&")))

    cases <- dplyr::full_join(cases, items, by=c("id" = "items.articles_id"))
    cases <- cases[,c(cols, extract.cols, "id", "type", "norm_iri")]
  }

  cases <- move_cols_to_end(cases, c("id", "type", "norm_iri"))
  cases
}

#' Get the property tree (including annotations)
#'
#' @param df A RAM data frame.
#' @param type The property type.
#' @param cols The property columns.
#' @param annos Whether to distill annotations.
#' @param levelup If set to a number, the tree will be simplified by replacing the path value
#'                on lower levels with the ancestor path from the given level.
#' @return A tibble containing the properties tree.
#' @export
distill_properties <- function(df, type = NULL, cols = c(), annos = FALSE, levelup = NULL) {
  props <- epi_extract_long(df, "properties", type, FALSE)

  if (nrow(props) == 0) {
    warning(paste0("No property data with type ", type, " found"), call. = F)
    return (props)
  }

  props <- add_missing_columns(props, c("parent_id", "articles_id"), NA_character_)
  props$id <- as.character(props$id)
  props$parent_id <- as.character(props$parent_id)
  props <- props[, unique(c("lemma","type","norm_iri", "level","lft","rght","id","parent_id", cols)), drop = FALSE]
  props <- dplyr::arrange(props, !!rlang::sym("lft"))
  props <- tree_add_path(props, !!rlang::sym("id"), !!rlang::sym("parent_id"), !!rlang::sym("lemma"))
  props <- drop_empty_columns(props)

  props <- dplyr::select(props, tidyselect::any_of(unique(c("tree_path", "id", "parent_id", cols,"type", "norm_iri"))))
  if (colnames(props)[1] == "tree_path") {
    colnames(props)[1] <- "path"
  }

  if (annos) {

    # Items
    items <- distill_items(df, NULL, cols = c("property"))

    items$items_id <- items$id
    items <- items[, c("property","articles_id", "sections_id", "items_id")]
    items <- stats::na.omit(items)
    items <- dplyr::inner_join(props, items, by=c("id"="property"))
    items <- drop_empty_columns(items)

    if (nrow(items) > 0) {
      props <- dplyr::anti_join(props, items, by="id")
    }


    # links
    links <- distill_links(df, properties.type = type, cols = c("segments", "offsets", "length", "coverage"), level = NULL)

    if (nrow(links) > 0) {
      links <- dplyr::inner_join(props, links, by=c("id"="to_id"))
    }

    if (nrow(links) > 0) {
      links <- drop_empty_columns(links)
      props <- dplyr::anti_join(props, links, by="id")
    }

    props <- dplyr::bind_rows(props, links, items)
  }

  if (!is.null(levelup)) {
    props <- add_missing_columns(props, c("parent_id", "path"), NA_character_)
    props <- tree_add_ancestor(props, level = levelup, !!rlang::sym("id"), !!rlang::sym("parent_id"), !!rlang::sym("path"))
  }

  props
}

#' Get articles (including selected item values)
#'
#' TODO: Implement article.cols parameter.
#'
#' @keywords internal
#'
#' @param df A RAM data frame.
#' @param type Item types to filter.
#' @param cols Cols returned from the items.
#' @param property.cols Property columns joined to the items.
#' @param article.cols Article columns joined to the items. Not implemented yet.
#' @return A tibble with items.
#' @importFrom rlang .data
#' @export
distill_items <- function(df, type = NULL, cols = c(), property.cols = c(), article.cols = c()) {
  items <- epi_extract_long(df, "items", type, prefix = FALSE)
  items$id <- as.character(items$id)

  extract.cols <- cols

  if (!missing(property.cols) && ("property" %in% colnames(items))) {
    props <- epi_extract_long(df, "properties")
    if (nrow(props) > 0) {
      items$property <- as.character(items$property)
      props$properties.id <- as.character(props$properties.id)
      items <- dplyr::left_join(items, props, by = c("property" = "properties.id"))
      if (length(property.cols) > 0) {
        extract.cols <- c(extract.cols, c(paste0("properties.",property.cols)))
      }
    }
  }

  if (!missing(article.cols)) {
    arts <- epi_extract_long(df, "articles")
    if (nrow(arts) > 0) {
      items$articles_id <- as.character(items$articles_id)
      arts$articles.id <- as.character(arts$articles.id)
      items <- dplyr::left_join(items, arts, by = c("articles_id" = "articles.id"))

      if (length(article.cols) > 0) {
        extract.cols <- c(extract.cols, c(paste0("articles.",article.cols)))
      }
    }
  }

  #cases <- dplyr::full_join(cases, items, by=c("id" = "items.articles_id"))
  #items <- items[, c(cols),drop = FALSE]
  items <- add_missing_columns(items, "norm_iri")
  extract.cols <- unique(c(extract.cols, "id", "type", "norm_iri", "articles_id", "sections_id"))
  items <- items[, extract.cols, drop = FALSE]

  items <- items |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(extract.cols), ~ stringr::str_replace_all(.x,"&amp;","&"))) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(extract.cols), ~ stringr::str_replace_all(.x,"&x2f;","&")))

  items
}

#' Get annotations for the articles
#'
#' @keywords internal
#'
#' @param df A RAM data frame.
#' @param items.type The type of items with annotations.
#' @param properties.type Keep only links that target the given property type.
#' @param cols A list of columns to add. Add `segments` to extract annotated text segments.
#'             Add `offsets` to return annotated offsets, add `length` to add a plain text size column.
#'             Add `coverage` to get the percentage of covered plain text.
#' @param article.cols A list of article columns to join.
#' @param level The aggregation level, beginning with 0. Set to NULL to get the lowest level.
#' @importFrom rlang .data
#' @return A tibble containing annotations.
distill_links <- function(df,  items.type = NULL, properties.type = NULL, cols = c("path", "segments", "offsets", "length", "coverage"), article.cols=c(), level = 0) {

  codes <- distill_properties(df, properties.type, cols = c("parent_id","level","norm_iri"))
  cases <- distill_articles(df, cols = article.cols)
  cases$id <- as.character(cases$id)

  cases <- dplyr::select(cases, -tidyselect::any_of(c("type","norm_iri")))
  codes <- add_missing_columns(codes, "parent_id", NA_character_)
  codes$id <- as.character(codes$id)

  ancestors <- codes |>
    dplyr::select(tidyselect::all_of(c("id", "parent_id"))) |>
    tree_stack_ancestors("id", "parent_id",  "anc_id") |>
    dplyr::distinct()


  if (is.null(level)) {
    codes_level <- codes
    level <- max(codes$level)
  } else {

    codes_level <- codes[codes$level == level,]
  }

  links <- epi_extract_long(df, "links", prefix = FALSE)
  if ("root_tab" %in% colnames(links)) {
    links <- links[links$root_tab == "articles",]
  }

  if ("from_tab" %in% colnames(links)) {
    links <- links[links$from_tab == "items",]
  }
  if ("to_tab" %in% colnames(links)) {
    links <- links[links$to_tab == "properties",]
  }

  if (nrow(links) == 0) {
    return (tibble::tibble())
  }

  codings <- links |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("root_id", "from_id", "from_tagid", "to_id")), as.character)) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("root_id", "from_id", "from_tagid", "to_id")))) |>
    dplyr::left_join(ancestors,by=c("to_id"="id"), relationship = "many-to-many")

  codings <- codings |>
    dplyr::inner_join(codes_level, by=c("anc_id"="id")) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("root_id", "from_id", "from_tagid", "to_id", "path")))) |>
    dplyr::left_join(cases, by=c("root_id"="id")) |>

    # TODO: Use tree_add_ancestor or the ancestors cols
    tidyr::separate_wider_delim(
      .data$path, delim=" / ",
      names = c(paste0("level_",0:level)),
      cols_remove = F,
      too_many="merge",
      too_few="align_start"
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::starts_with("level_"), ~ stringr::str_replace_all(., "&#47;","/"))) |>
    dplyr::mutate(dplyr::across(tidyselect::starts_with("level_"), ~ stringr::str_replace_all(., "&x2f;","&")))


  # Segments in items
  # Segments / offsets in items
  if (any(c("segments", "offsets", "length", "coverage") %in% cols)) {
    segments <- epi_extract_long(df, "items", items.type, prefix = FALSE)
    segments$items_id <- segments$id
    segments <- add_missing_columns(segments, c("items_id", "sections_id", "articles_id", "content", "norm_iri"), NA_character_)

    segments <- segments |>
      dplyr::select(tidyselect::all_of(c("items_id", "sections_id", "articles_id", "content", "norm_iri"))) |>
      dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) |>
      dplyr::inner_join(codings, by = c("items_id" = "from_id"), relationship = "many-to-many") |>
      dplyr::mutate(item_iri = .data$norm_iri) |>
      dplyr::select(tidyselect::all_of(c("items_id", "sections_id", "articles_id", "from_tagid", "content", "item_iri")))

    # Parse each distinct content only once, reuse the annotated doc per row
    docs <- tibble::tibble(content = unique(segments$content))
    docs$.doc <- lapply(docs$content, function(x) if (is.na(x)) NULL else annotate_offsets(x))
    segments <- dplyr::left_join(segments, docs, by = "content")

    # One extraction per row against the pre-parsed doc
    offsets <- Map(extract_segments, segments$.doc, segments$from_tagid)

    if ("segments" %in% cols) {
      segments$segments <- vapply(offsets, `[[`, character(1), "segments")
    }
    if ("offsets" %in% cols) {
      segments$offsets <- vapply(offsets, `[[`, character(1), "offsets")
    }
    if ("length" %in% cols) {
      segments$length <- vapply(offsets, `[[`, integer(1), "length")
    }

    if ("coverage" %in% cols) {
      segments$coverage <- vapply(offsets, `[[`, numeric(1), "coverage")
    }

    segments <- dplyr::select(
      segments,
      tidyselect::any_of(c(
        "items_id", "sections_id", "articles_id",
        "from_tagid", "content", "item_iri",
        "segments", "offsets","length","coverage"
      ))
    )

    # TODO: Segments in footnotes

    # Join
    codings <- dplyr::left_join(codings, segments, by = c("from_id" = "items_id", "from_tagid"))
  }

  codings$items_id <- codings$from_id

  codings <- dplyr::select(codings, tidyselect::any_of(c(article.cols, "articles_id","sections_id","items_id", "from_tagid", cols, "to_id")))
  codings <- add_missing_columns(codings, "to_id", NA)
  codings
}


#' Extract segment text and character offsets for a tag id
#'
#' Pulls both the plain text and the character-offset ranges for all elements
#' matching `tagid`. Reads the `data-start` / `data-end` attributes injected by
#' [annotate_offsets()] and works vectorized over the selected node set, so
#' repeated extraction from the same annotated document is cheap.
#'
#' Multiple matches (discontinuous annotations sharing an `id`) yield multiple
#' spans, returned in document order so that `segments` and `offsets` align
#' element-for-element.
#'
#' @keywords internal
#'
#' @param doc One of: an `xml2::xml_document` already processed by
#'   [annotate_offsets()] (whose id-bearing elements carry `data-start` /
#'   `data-end` attributes); a character value containing raw XML, which is
#'   annotated on the fly; or `NULL` (e.g. for missing content), in which case
#'   `NA` values are returned. Pass a pre-annotated document when extracting
#'   several tag ids from the same content, to avoid re-parsing.
#' @param tagid Optional character value giving the tag `id` to extract. If
#'   `NULL`, all offset-annotated elements are returned.
#' @return A list with elements:
#'   \describe{
#'     \item{segments}{Text pieces joined by `;`.}
#'     \item{offsets}{Ranges as `"start-end"` joined by `;`.}
#'     \item{ranges}{A one-element list holding a data frame with columns
#'       `id`, `tag`, `start`, `end`, `text` (one row per matching element).
#'       The `start`/`end` columns can be passed directly to
#'       `IRanges::IRanges()` for coverage and overlap analysis.}
#'   }
#' @seealso [annotate_offsets()]
extract_segments <- function(doc, tagid = NULL) {

  if (is.null(doc) || (is.character(doc) && (length(doc) == 0 || is.na(doc)))) {
    return(list(
      segments = NA_character_,
      offsets  = NA_character_,
      length   = NA_integer_,
      coverage = NA_real_,
      ranges   = list(NULL)
    ))
  }

  if (is.character(doc)) {
    doc <- annotate_offsets(doc)
  }

  full_length <- nchar(xml2::xml_text(xml2::xml_root(doc)))

  xpath <- if (is.null(tagid))
    "//*[@data-start]"
  else
    paste0('//*[@id="', tagid, '"][@data-start]')

  els <- xml2::xml_find_all(doc, xpath)

  pos <- data.frame(
    id    = xml2::xml_attr(els, "id"),
    tag   = xml2::xml_name(els),
    start = as.integer(xml2::xml_attr(els, "data-start")),
    end   = as.integer(xml2::xml_attr(els, "data-end")),
    text  = xml2::xml_text(els),
    length = full_length,
    stringsAsFactors = FALSE
  )

  if (nrow(pos) == 0) {
    return(list(
      segments = NA_character_,
      offsets  = NA_character_,
      length   = full_length,
      coverage = 0,
      ranges   = list(pos)
    ))
  }

  covered  <- covered_length(pos$start, pos$end)
  coverage <- if (full_length > 0) covered / full_length else NA_real_

  list(
    segments = paste0(pos$text, collapse = ";"),
    offsets  = paste0(pos$start, "-", pos$end, collapse = ";"),
    length   = full_length,
    coverage = coverage,
    ranges   = list(pos)
  )
}

#' Function to extract non-tagged text
#'
#' @keywords internal
#'
#' @param xml The XML as character value.
#' @return A character value where all text contained in tags was stripped.
extract_untagged <- function(xml) {
  xml <- paste0("<root>",xml,"</root>")
  xml = stringr::str_replace_all(xml, "&", "&#038;")
  xml_doc <- xml2::read_xml(xml)
  root_text <- xml2::xml_text(xml2::xml_find_all(xml_doc, "/root/text()"))
  #non_tagged_text <- xml2::xml_text(xml2::xml_find_all(xml_doc, "//text()[not(parent::*)]"))
  return(trimws(root_text))
}
