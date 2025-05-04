#' Get articles (including selected item values)
#'
#' @param df A RAM data frame
#' @param item.type Item types to join
#' @param item.cols Cols to join from the items
#' @return A tibble with articles
#' @export
distill_articles <- function(df, cols = c(), item.type = NULL, item.cols = c(), property.cols = c()) {
  cases <- df[df$table == "articles", unique(c("id","type","norm_iri", cols))]

  extract.cols <- c()
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
      if (nrow(props) > 0) {
        items <- dplyr::left_join(items, props, by = c("items.property" = "properties.id"))
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

  cases
}

#' Get the property tree (including annotations)
#'
#' @param df A RAM data frame
#' @return A tibble containing the properties tree
#' @export
distill_properties <- function(df, type = NULL, cols = c(), annos = FALSE) {
  props <- epi_extract_long(df, "properties", type, FALSE)
  if (!("parent_id" %in% colnames(props))) {
    props$parent_id <- NA_character_
  }
  props <- props[, unique(c("lemma","type","norm_iri", "level","lft","rght","id","parent_id", cols)), drop = FALSE]
  props <- dplyr::arrange(props, !!rlang::sym("lft"))
  props <- tree_add_path(props, !!rlang::sym("id"), !!rlang::sym("parent_id"), !!rlang::sym("lemma"))
  props <- drop_empty_columns(props)

  props <- dplyr::select(props, tidyselect::any_of(unique(c("tree_path", "id", cols,"type", "norm_iri"))))
  colnames(props)[1] <- "path"

  if (annos) {

    # links
    links <- distill_links(df, type, c("segment"))
    links <- dplyr::inner_join(props, links, by=c("id"="to_id"))
    links <- drop_empty_columns(links)

    # Items
    items <- distill_items(df, NULL, cols = c("articles_id", "property"))
    items$root_id <- items$articles_id
    items <- items[, c("property","root_id")]
    items <- stats::na.omit(items)
    items <- dplyr::inner_join(props, items, by=c("id"="property"))
    items <- drop_empty_columns(items)

    if (nrow(items) > 0) {
      props <- dplyr::anti_join(props, items, by="id")
    }


    if (nrow(links) > 0) {
      props <- dplyr::anti_join(props, links, by="id")
    }

    props <- dplyr::bind_rows(props, links, items)
  }

  props
}

#' Get articles (including selected item values)
#'
#' TODO: Implement article.cols parameter.
#'
#' @keywords internal
#'
#' @param df A RAM data frame
#' @param type Item types to filter
#' @param cols Cols returned from the items
#' @param property.cols Property columns joined to the items
#' @param article.cols Article columns joined to the items. Not implemented yet.
#' @return A tibble with items
#' @importFrom rlang .data
#' @export
distill_items <- function(df, type = NULL, cols = c(), property.cols = c(), article.cols = c()) {
  items <- epi_extract_long(df, "items", type, prefix = FALSE)

  extract.cols <- cols
  if (length(property.cols) > 0) {
    extract.cols <- c(extract.cols, c(paste0("properties.",property.cols)))
  }

  if (!missing(property.cols)) {
    props <- epi_extract_long(df, "properties")
    if (nrow(props) > 0) {
      items <- dplyr::left_join(items, props, by = c("property" = "properties.id"))
    }
  }

  #cases <- dplyr::full_join(cases, items, by=c("id" = "items.articles_id"))
  #items <- items[, c(cols),drop = FALSE]
  items <- items[, c(extract.cols, "id", "type", "norm_iri"), drop = FALSE]

  items <- items |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(extract.cols), ~ stringr::str_replace_all(.x,"&amp;","&"))) |>
    dplyr::mutate(dplyr::across(tidyselect::any_of(extract.cols), ~ stringr::str_replace_all(.x,"&x2f;","&")))

  items
}

#' Get annotations for the articles
#'
#' @keywords internal
#'
#' @param df A RAM data frame
#' @param item.type The type of items with annotations
#' @param article.cols A list of article columns to join
#' @param level The aggregation level, beginning with 0
#' @importFrom rlang .data
#' @return A tibble containing annotations
distill_links <- function(df,  type = NULL, cols = c("path", "segment"), article.cols=c(), level = 0) {

  codes <- distill_properties(df, cols = c("parent_id","level","norm_iri"))
  cases <- distill_articles(df, cols = article.cols)

  cases <- dplyr::select(cases, -tidyselect::any_of(c("type","norm_iri")))

  if (!("parent_id" %in% colnames(codes))) {
    codes$parent_id <- NA_character_
  }

  ancestors <- codes |>
    tidyselect::all_of(c("id", "parent_id"))|>
    tree_stack_ancestors(.data$id, .data$parent_id, .data$anc_id) |>
    dplyr::distinct()

  codes_level <- codes[codes$level == level,]

  links <- epi_extract_long(df, "links", prefix = FALSE)

  codings <- links |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("root_id", "from_id", "from_tagid", "to_id")))) |>
    dplyr::left_join(ancestors,by=c("to_id"="id"), relationship = "many-to-many") |>
    dplyr::inner_join(codes_level, by=c("anc_id"="id")) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of("root_id", "from_id", "from_tagid", "to_id", "path"))) |>
    dplyr::left_join(cases, by=c("root_id"="id")) |>

    tidyr::separate_wider_delim(
      .data$path, delim=" / ",
      names = c(paste0("level_",0:level)),
      cols_remove = F,
      too_many="merge",
      too_few="align_start"
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::starts_with("level_"), ~ stringr::str_replace_all(., "&#47;","/"))) |>
    dplyr::mutate(dplyr::across(tidyselect::starts_with("level_"), ~ stringr::str_replace_all(., "&x2f;","&")))

  segments <- epi_extract_long(df, "items", NULL, prefix = FALSE)  |>
    tidyselect::all_of("from_id"="id", "content", "norm_iri") |>
    dplyr::inner_join(codings, by=c("from_id"), relationship="many-to-many")  |>
    tidyselect::all_of("from_id", "from_tagid", "content", "item_iri"="norm_iri") |>
    dplyr::rowwise() |>
    dplyr::mutate(segment = paste0(extract_segment(.data$content, .data$from_tagid), collapse=";"))

  codings <- dplyr::left_join(codings, segments, by=c("from_id", "from_tagid"))

  codings <- dplyr::select(codings, tidyselect::any_of(c(article.cols, "root_id", "from_tagid", cols, "to_id")))
  codings
}

#' Function to extract segments based on ID attribute
#'
#' @keywords internal
#'
#' @param xml Character value containing XML text
#' @param tagid Character value containing the tag ID
#' @return A character value containing only the text of elements with the tag ID
extract_segment <- function(xml, tagid) {
  xml <- paste0("<root>",xml,"</root>")
  xml_doc <- xml2::read_xml(xml)
  segments <- xml2::xml_find_all(xml_doc, paste0('//*[@id="', tagid, '"]//text()'))
  segment_text <- trimws(xml2::xml_text(segments))
  return(segment_text)
}

#' Function to extract non-tagged text
#'
#' @keywords internal
#'
#' @param xml The XML as character value
#' @return A character value where all text contained in tags was stripped
extract_untagged <- function(xml) {
  xml <- paste0("<root>",xml,"</root>")
  xml = stringr::str_replace_all(xml, "&", "&#038;")
  xml_doc <- xml2::read_xml(xml)
  root_text <- xml2::xml_text(xml2::xml_find_all(xml_doc, "/root/text()"))
  #non_tagged_text <- xml2::xml_text(xml2::xml_find_all(xml_doc, "//text()[not(parent::*)]"))
  return(trimws(root_text))
}
