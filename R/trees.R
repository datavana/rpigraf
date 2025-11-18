#' Add level, thread and order
#'
#' TODO: fix tree_order ... should be related to the thread, not the parent
#'
#' @param data The dataframe containing hierarchical data
#' @param col_id The ID column of the node
#' @param col_parent The ID column of the parent node
#' @param col_sort Column for sorting the nodes inside each parent. Leave empty to use the ID column.
#' @return Data frame with the additional columns tree_thread, tree_order and tree_level
#' @export
#' @importFrom rlang .data

tree_add_level <- function(data, col_id, col_parent, col_sort=NULL) {
  # Quoting
  col_id <- rlang::enquo(col_id)
  col_parent <- rlang::enquo(col_parent)
  col_sort <- rlang::enquo(col_sort)

  if (rlang::quo_is_null(col_sort)) {
    col_sort <- col_id
  }

  # Prepare columns
  data <- dplyr::mutate(data,.tree_id=!!col_id)
  data <- dplyr::mutate(data,.tree_parent=!!col_parent)

  # Prepare roots
  roots <- data |>
    dplyr::anti_join(data,by=c(".tree_parent"=".tree_id")) |>
    dplyr::mutate(tree_thread=.data$.tree_id,tree_level=0,tree_order=0)

  # First level
  .level <- 1
  children <- data |>
    dplyr::inner_join(dplyr::select(roots, tidyselect::all_of(c(".tree_id","tree_thread"))),by=c(".tree_parent"=".tree_id")) |>
    dplyr::mutate(tree_level=.level) |>

    dplyr::group_by(dplyr::across(tidyselect::all_of(".tree_parent"))) |>
    dplyr::arrange(!!col_sort) |>
    dplyr::mutate(tree_order=dplyr::row_number()) |>
    dplyr::ungroup()

  cat("Level ",.level,". ",sep="")
  cat(nrow(children)," nodes addes.\n\n",sep="")

  while (TRUE) {

    .level <- .level + 1
    cat("Level ",.level,". ",sep="")

    children.next <- data |>
      dplyr::anti_join(children,by=c(".tree_id")) |>
      dplyr::mutate(.parent_order = .data$tree_order) |>
      dplyr::inner_join(dplyr::select(children, tidyselect::all_of(c("tree_thread",".tree_id",".parent_order"))),by=c(".tree_parent"=".tree_id")) |>
      dplyr::mutate(tree_level=.level) |>

      dplyr::group_by(dplyr::across(tidyselect::all_of(".tree_parent"))) |>
      dplyr::arrange(!!col_sort) |>
      dplyr::mutate(tree_order= dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::select(-tidyselect::all_of(".parent_order"))

    children <- dplyr::bind_rows(children,children.next)

    cat(nrow(children.next)," nodes addes.\n\n",sep="")

    if (!nrow(children.next))
      break
  }

  dplyr::bind_rows(roots,children) |>
    dplyr::arrange(.data$tree_thread,.data$tree_order) |>
    dplyr::select(-tidyselect::all_of(c(".tree_id",".tree_parent")))

}

#' Adds left and right values to the dataframe
#'
#' Left and right values are used to store hierarchical data
#' using the concept of modified preorder tree traversal.
#' See https://www.sitepoint.com/hierarchical-data-database-3/
#'
#' @param data Dataframe with the columns tree_id, tree_parent,
#'   tree_thread, tree_level, tree_order
#' @return Dataframe with lft and rght values
#' @export
#' @importFrom rlang .data
#' @importFrom rlang .env
tree_add_mptt <- function(data) {

  # Bind variables
  tree_id <- tree_parent <- tree_thread <- tree_level <-
    tree_order <- tree_descendants <- tree_tmp_descendants <-
    tree_no <- tree_rgt <- tree_lft <- tree_parent_lft <- level <- NULL

  # Progress
  maxlevel <- max(data$tree_level)
  minlevel <- min(data$tree_level)
  p <- progressr::progressor(steps = (maxlevel - minlevel + 1) * 2)

  # Add descendants
  data$tree_descendants <- 0
  for (level in maxlevel:minlevel) {

    p(message = paste0("Level ", level))

    descendants <- data %>%
      dplyr::filter(.data$tree_level == .env$level) %>%
      dplyr::mutate(tree_descendants = .data$tree_descendants + 1) %>%
      dplyr::group_by(.data$tree_thread,
                      tree_id = .data$tree_parent) %>%
      dplyr::summarise(tree_tmp_descendants = sum(.data$tree_descendants),
                       .groups = "keep") %>%
      dplyr::ungroup()

    data <- data %>%
      dplyr::left_join(descendants,
                       by = c("tree_thread", "tree_id")) %>%
      tidyr::replace_na(list(tree_tmp_descendants = 0)) %>%
      dplyr::mutate(tree_descendants =
                      .data$tree_descendants + .data$tree_tmp_descendants) %>%
      dplyr::select(-.data$tree_tmp_descendants)
  }

  # Add left / right values
  data <- data %>%
    dplyr::group_by(.data$tree_thread, .data$tree_parent) %>%
    dplyr::arrange(.data$tree_order) %>%
    dplyr::mutate(tree_no = dplyr::row_number()) %>%
    dplyr::mutate(
      tree_rgt = 1 +
        cumsum(.data$tree_descendants) * 2 +
        (2 * (.data$tree_no - 1)) + 1
    ) %>%
    dplyr::mutate(
      tree_lft = .data$tree_rgt - 2 * .data$tree_descendants - 1
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$tree_descendants)

  # Bubble parent offsets down the tree
  level <- minlevel
  parents <- data %>%
    dplyr::filter(.data$tree_level == .env$minlevel)

  while (nrow(parents) > 0) {

    p(message = paste0("Level ", level))
    level <- level + 1

    data <- data %>%
      dplyr::left_join(
        dplyr::select(
          parents,
          .data$tree_thread,
          .data$tree_id,
          tree_parent_lft = .data$tree_lft
        ),
        by = c("tree_thread", "tree_parent" = "tree_id")
      ) %>%
      tidyr::replace_na(list(tree_parent_lft = 0)) %>%
      dplyr::mutate(
        tree_lft = .data$tree_lft + .data$tree_parent_lft,
        tree_rgt = .data$tree_rgt + .data$tree_parent_lft
      ) %>%
      dplyr::select(-.data$tree_parent_lft)

    parents <- data %>%
      dplyr::semi_join(
        parents,
        by = c("tree_thread", "tree_parent" = "tree_id")
      )
  }

  data %>%
    dplyr::arrange(.data$tree_thread, .data$tree_lft) %>%
    dplyr::select(dplyr::starts_with("tree_"), dplyr::everything())
}



#' Add a column holding the path of each node.
#'
#' The path is created by concatenating all col_lemma values up to the root node.
#' Lemmata are concatenated using a slash - existing slashes are replaced by the entity &#47;.
#'
#' @param data Dataframe containing hierarchical data
#' @param col_id The ID column of the node
#' @param col_parent_id The ID column of the parent node
#' @param col_lemma The column holding the node name that will be used for the path
#' @param delim Character that glues together the path elements. Set to NULL to create a vector instead.
#' @return A data frame with the additional column tree_path
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
tree_add_path <- function(data, col_id, col_parent_id, col_lemma, delim="/")  {

  col_id <- rlang::enquo(col_id)
  col_parent_id <- rlang::enquo(col_parent_id)
  col_lemma <- rlang::enquo(col_lemma)
  join_by_parent = rlang::set_names(rlang::quo_name(col_id), rlang::quo_name(col_parent_id))


  # Escape slashes (or other characters used as delimiter) in lemmata
  delim_entity <- paste0("&x", charToRaw(delim),";")
  data <- data |>
    dplyr::mutate(!!col_lemma := stringr::str_replace_all(!!col_lemma, delim ,delim_entity))

  # Init path
  data <- data |>
    dplyr::mutate(tree_path=NA)

  # Root nodes
  current <- data |>
    dplyr::filter(is.na(!!col_parent_id)) |>
    dplyr::mutate(tree_path =!!col_lemma) |>
    dplyr::select(!!col_id, tidyselect::all_of(c("tree_path")))

  while(nrow(current) > 0) {
    print(paste0(nrow(current), " nodes processed."))

    # Update path of current batch
    data <- data |>
      dplyr::left_join(
        current |>
          dplyr::mutate(.tree_path_new = .data$tree_path) |>
          dplyr::select(!!col_id, tidyselect::all_of(c(".tree_path_new"))),
        by=rlang::quo_name(col_id)
      ) |>
      dplyr::mutate(tree_path = ifelse(is.na(.data$.tree_path_new), .data$tree_path, .data$.tree_path_new)) |>
      dplyr::select(-tidyselect::all_of(".tree_path_new"))

    # Get children of current batch and create path
    current <- data |>
      dplyr::inner_join(dplyr::select(current, !!col_id,tidyselect::all_of(c(".tree_parent_path" = "tree_path"))),by=join_by_parent) |>
      dplyr::mutate(tree_path = paste0(.data$.tree_parent_path, " ", delim, " ", !!col_lemma)) |>
      dplyr::select(!!col_id, tidyselect::all_of(c("tree_path")))
  }

  return(data)
}


#' Get all distinct nodes in an edge list
#'
#' @param edges An edge list
#' @param col_source Source column name
#' @param col_target Target column name
#' @return A tibble with one column `id` containing unique source and target values
#' @export
tree_get_nodes <- function(edges, col_source, col_target) {

  edges |>
    dplyr::select({{col_source}}, {{col_target}}) |>
    tidyr::pivot_longer(c({{col_source}},{{col_target}})) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("id"="value"))))
}

#' Row bind all ancestors of the selected nodes
#'
#' @param .data Data frame containing the selected nodes
#' @param .tree Data frame containing all nodes including all ancestors
#' @param id Column name of the id in .data and .tree
#' @param parent_id Column name of the parent id in .data and .tree
#' @return Data frame containing the nodes of .data and all ancestors
#' @export
tree_bind_ancestors <- function(.data, .tree, id, parent_id) {
  id <- rlang::enquo(id)
  parent_id <- rlang::enquo(parent_id)

  # Equavalent to c("id" = "parent_id"), note the changed field order
  join_semi = rlang::set_names(rlang::quo_name(parent_id), rlang::quo_name(id))

  selected = tibble::tibble()

  while (nrow(.data) > 0) {
    print(paste0(nrow(.data), " nodes added" ))
    selected <-  dplyr::bind_rows(selected, .data)
    .data <-  dplyr::semi_join(.tree, .data,by=join_semi)
    .data <- dplyr::anti_join(.data,selected,by=rlang::quo_name(id))
  }

  return (selected)
}


#' For each node, add each ancestor's id
#'
#' In the result, nodes will be duplicated for all their ancestors.
#' As an example: a node on level 2 will be present two times,
#'   1. the node containing the parent_id in the col_stack column
#'   2. the node containing the parents parent_id in the col_stack column
#'
#' @param data All nodes
#' @param col_id The column holding IDs of the nodes
#' @param col_parent The column holding IDs of the parent nodes
#' @param col_stack The column that will hold the ancestors IDs
#' @importFrom rlang :=
#' @export
tree_stack_ancestors <- function(data, col_id, col_parent, col_stack) {

  col_id <- rlang::ensym(col_id)
  col_parent <- rlang::ensym(col_parent)
  col_stack <- rlang::ensym(col_stack)


  # Prepare temporary columns (for easier joins)
  data <- dplyr::mutate(data,.tree_id = !!col_id)
  data <- dplyr::mutate(data,.tree_parent = !!col_parent)

  # Put items themselves on the stack
  data_stacked <- dplyr::mutate(data,.tree_main=.data$.tree_id)

  # Init parents (.tree_main is the parent id)
  data_parents <- data |>
    dplyr::filter(!is.na(.data$.tree_parent)) |>
    dplyr::mutate(.tree_main=.data$.tree_parent)

  while (TRUE) {

    if (nrow(data_parents) > 0) {
      cat("Adding ", nrow(data_parents)," rows. \n",sep = "")
      data_stacked <- dplyr::bind_rows(data_stacked, data_parents)
    } else {
      break
    }

    # Find parents
    data_parents <- data_parents |>
      dplyr::inner_join(
        dplyr::select(data, tidyselect::all_of(c(".tree_id", ".tree_main" = ".tree_parent"))),
        by=c(".tree_main"=".tree_id")
      ) |>
      dplyr::filter(!is.na(.data$.tree_main.y)) |>
      dplyr::mutate(.tree_main = .data$.tree_main.y) |>
      # TODO: Does this work?
      dplyr::select(-tidyselect::all_of(".tree_main.y"))

  }

  # Remove columns and return data
  data_stacked |>
    dplyr::select(-tidyselect::all_of(c(".tree_id",".tree_parent"))) |>
    dplyr::mutate(!!col_stack := .data$.tree_main) |>
    dplyr::select(-tidyselect::all_of(".tree_main"))
}
