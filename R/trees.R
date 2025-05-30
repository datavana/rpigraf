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
tree_add_level <- function(data, col_id, col_parent, col_sort=NULL) {
  # Quoting
  col_id <- enquo(col_id)
  col_parent <- enquo(col_parent)
  col_sort <- enquo(col_sort)

  if (quo_is_null(col_sort)) {
    col_sort <- col_id
  }

  # Prepare columns
  data <- mutate(data,.tree_id=!!col_id)
  data <- mutate(data,.tree_parent=!!col_parent)

  # Prepare roots
  roots <- data |>
    anti_join(data,by=c(".tree_parent"=".tree_id")) |>
    mutate(tree_thread=.tree_id,tree_level=0,tree_order=0)

  # First level
  .level <- 1
  children <- data |>
    inner_join(select(roots,.tree_id,tree_thread),by=c(".tree_parent"=".tree_id")) |>
    mutate(tree_level=.level) |>

    group_by(.tree_parent) |>
    arrange(!!col_sort) |>
    mutate(tree_order=row_number()) |>
    ungroup()

  cat("Level ",.level,". ",sep="")
  cat(nrow(children)," nodes addes.\n\n",sep="")

  while (TRUE) {

    .level <- .level + 1
    cat("Level ",.level,". ",sep="")

    children.next <- data |>
      anti_join(children,by=c(".tree_id")) |>
      inner_join(select(children,tree_thread,.tree_id,.parent_order=tree_order),by=c(".tree_parent"=".tree_id")) |>
      mutate(tree_level=.level) |>

      group_by(.tree_parent) |>
      arrange(!!col_sort) |>
      mutate(tree_order= row_number()) |>
      ungroup() |>
      select(-.parent_order)

    children <- dplyr::bind_rows(children,children.next)

    cat(nrow(children.next)," nodes addes.\n\n",sep="")

    if (!nrow(children.next))
      break
  }

  bind_rows(roots,children) |>
    arrange(tree_thread,tree_order) |>
    select(-.tree_id,-.tree_parent)

}


#' Adds left and right values to the dataframe
#'
#' Left and right values are used to store hierarchical data
#' using the concept of modified preorder tree traversal.
#' See https://www.sitepoint.com/hierarchical-data-database-3/
#'
#' @param data Dataframe with the columns tree_id, tree_parent, tree_thread, tree_level, tree_order
#' @return Dataframe with lft and rght values
#' @export
tree_add_mptt <- function(data) {
  # Progress
  .maxlevel = max(data$tree_level)
  .minlevel = min(data$tree_level)
  p <- progressr::progressor(steps = (.maxlevel-.minlevel+1) * 2)

  # Add descendants
  data$tree_descendants = 0
  for (.level in c(.maxlevel : .minlevel)) {
    p(message=paste0("Level ",.level))

    descendants <- data |>
      filter(tree_level == .level)  |>
      mutate(tree_descendants = tree_descendants + 1) |>
      group_by(tree_thread, tree_id=tree_parent) |>
      summarise(tree_tmp_descendants = sum(tree_descendants),.groups="keep") |>
      ungroup(tree_thread, tree_id)

    data <-  data |>
      left_join(descendants, by=c("tree_thread", "tree_id")) |>
      replace_na(list(tree_tmp_descendants=0)) |>
      mutate(tree_descendants = tree_descendants + tree_tmp_descendants) |>
      select(-tree_tmp_descendants)

  }


  # Add left
  data <- data |>
    group_by(tree_thread, tree_parent) |>
    arrange(tree_order) |>
    mutate(tree_no = row_number()) |>
    mutate(tree_rgt = 1 + cumsum(tree_descendants)*2 + (2 * (tree_no-1)) + 1) |>
    mutate(tree_lft = tree_rgt - 2*tree_descendants - 1) |>
    ungroup(tree_thread,tree_parent) |>
    select(-tree_descendants)


  # Bubble from parents to children
  .level <- .minlevel
  parents <- data |>
    filter(tree_level == .minlevel)

  while(nrow(parents) > 0) {

    p(message=paste0("Level ",.level))
    .level <- .level + 1

    data <- data |>
      left_join(select(parents,tree_thread,tree_id,tree_parent_lft=tree_lft),
                by=c("tree_thread","tree_parent"="tree_id"))|>
      replace_na(list(tree_parent_lft = 0)) |>
      mutate(tree_lft = tree_lft + tree_parent_lft) |>
      mutate(tree_rgt = tree_rgt + tree_parent_lft) |>
      select(-tree_parent_lft)

    parents <- data |>
      semi_join(parents,by=c("tree_thread","tree_parent"="tree_id"))

  }

  data <- data |>
    arrange(tree_thread, tree_lft) |>
    select(starts_with("tree_"), everything())

  return(data)
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
tree_add_path <- function(data, col_id, col_parent_id, col_lemma, delim="/")  {

  col_id <- enquo(col_id)
  col_parent_id <- enquo(col_parent_id)
  col_lemma <- enquo(col_lemma)
  join_by_parent = rlang::set_names(quo_name(col_id), quo_name(col_parent_id))


  # Escape slashes (or other characters used as delimiter) in lemmata
  delim_entity <- paste0("&x", charToRaw(delim),";")
  data <- data |>
    mutate(!!col_lemma := str_replace_all(!!col_lemma, delim ,delim_entity))

  # Init path
  data <- data |>
    mutate(tree_path=NA)

  # Root nodes
  current <- data |>
    filter(is.na(!!col_parent_id)) |>
    mutate(tree_path =!!col_lemma) |>
    select(!!col_id, tree_path)

  while(nrow(current) > 0) {
    print(paste0(nrow(current), " nodes processed."))

    # Update path of current batch
    data <- data |>
      left_join(select(current,!!col_id,.tree_path_new=tree_path),by=quo_name(col_id)) |>
      mutate(tree_path = ifelse(is.na(.tree_path_new), tree_path, .tree_path_new)) |>
      select(-.tree_path_new)

    # Get children of current batch and create path
    current <- data |>
      inner_join(select(current, !!col_id,.tree_parent_path = tree_path),by=join_by_parent) |>
      mutate(tree_path = paste0(.tree_parent_path, " ", delim, " ", !!col_lemma)) |>
      select(!!col_id, tree_path)
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
    dplyr::distinct(id=value)
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
  id <- enquo(id)
  parent_id <- enquo(parent_id)

  # Equavalent to c("id" = "parent_id"), note the changed field order
  join_semi = rlang::set_names(quo_name(parent_id), quo_name(id))

  selected = tibble::tibble()

  while (nrow(.data) > 0) {
    print(paste0(nrow(.data), " nodes added" ))
    selected <-  dplyr::bind_rows(selected, .data)
    .data <-  dplyr::semi_join(.tree, .data,by=join_semi)
    .data <- dplyr::anti_join(.data,selected,by=quo_name(id))
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
#' @export
#'
tree_stack_ancestors <- function(data, col_id, col_parent, col_stack) {

  # Quoting
  col_id <- enquo(col_id)
  col_parent <- enquo(col_parent)
  col_stack <- enquo(col_stack)

  # Prepare temporary columns (for easier joins)
  data <- mutate(data,.tree_id=!!col_id)
  data <- mutate(data,.tree_parent=!!col_parent)

  # Put items themselves on the stack
  data_stacked <- mutate(data,.tree_main=.tree_id)

  # Init parents (.tree_main is the parent id)
  data_parents <- data |>
    filter(!is.na(.tree_parent)) |>
    mutate(.tree_main=.tree_parent)

  while (TRUE) {

    if (nrow(data_parents) > 0) {
      cat("Adding ", nrow(data_parents)," rows. \n",sep = "")
      data_stacked <- bind_rows(data_stacked, data_parents)
    } else {
      break
    }

    # Find parents
    data_parents <- data_parents |>
      inner_join(
        select(data, .tree_id, .tree_main = .tree_parent),
        by=c(".tree_main"=".tree_id")
      ) |>
      filter(!is.na(.tree_main.y)) |>
      mutate(.tree_main = .tree_main.y) |>
      select(-.tree_main.y)

  }

  # Remove columns and return data
  data_stacked |>
    select(-.tree_id,-.tree_parent) |>
    rename(!!col_stack := .tree_main)
}
