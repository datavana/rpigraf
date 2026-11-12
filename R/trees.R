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

  verbose <- Sys.getenv("epi_verbose") == "TRUE"

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
    dplyr::mutate(tree_level= .env$.level) |>

    dplyr::group_by(dplyr::across(tidyselect::all_of(".tree_parent"))) |>
    dplyr::arrange(!!col_sort) |>
    dplyr::mutate(tree_order=dplyr::row_number()) |>
    dplyr::ungroup()

  if (verbose) {
    cat("Level ",.level,". ",sep="")
    cat(nrow(children)," nodes addes.\n\n",sep="")
  }

  while (TRUE) {

    .level <- .level + 1
    if (verbose) {
      cat("Level ",.level,". ",sep="")
    }

    children.next <- data |>
      dplyr::anti_join(children,by=c(".tree_id")) |>
      dplyr::inner_join(dplyr::select(children, tidyselect::all_of(c("tree_thread",".tree_id"))), by=c(".tree_parent"=".tree_id")) |>
      dplyr::mutate(tree_level = .env$.level) |>

      dplyr::group_by(dplyr::across(tidyselect::all_of(".tree_parent"))) |>
      dplyr::arrange(!!col_sort) |>
      dplyr::mutate(tree_order= dplyr::row_number()) |>
      dplyr::ungroup()

    children <- dplyr::bind_rows(children,children.next)

    if (verbose) {
      cat(nrow(children.next)," nodes addes.\n",sep="")
    }

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

    descendants <- data |>
      dplyr::filter(.data$tree_level == .env$level) |>
      dplyr::mutate(tree_descendants = .data$tree_descendants + 1) |>
      dplyr::group_by(.data$tree_thread,
                      tree_id = .data$tree_parent) |>
      dplyr::summarise(tree_tmp_descendants = sum(.data$tree_descendants),
                       .groups = "keep") |>
      dplyr::ungroup()

    data <- data |>
      dplyr::left_join(descendants,
                       by = c("tree_thread", "tree_id")) |>
      tidyr::replace_na(list(tree_tmp_descendants = 0)) |>
      dplyr::mutate(tree_descendants =
                      .data$tree_descendants + .data$tree_tmp_descendants) |>
      dplyr::select(-.data$tree_tmp_descendants)
  }

  # Add left / right values
  data <- data |>
    dplyr::group_by(.data$tree_thread, .data$tree_parent) |>
    dplyr::arrange(.data$tree_order) |>
    dplyr::mutate(tree_no = dplyr::row_number()) |>
    dplyr::mutate(
      tree_rgt = 1 +
        cumsum(.data$tree_descendants) * 2 +
        (2 * (.data$tree_no - 1)) + 1
    ) |>
    dplyr::mutate(
      tree_lft = .data$tree_rgt - 2 * .data$tree_descendants - 1
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-.data$tree_descendants)

  # Bubble parent offsets down the tree
  level <- minlevel
  parents <- data |>
    dplyr::filter(.data$tree_level == .env$minlevel)

  while (nrow(parents) > 0) {

    p(message = paste0("Level ", level))
    level <- level + 1

    data <- data |>
      dplyr::left_join(
        dplyr::select(
          parents,
          .data$tree_thread,
          .data$tree_id,
          tree_parent_lft = .data$tree_lft
        ),
        by = c("tree_thread", "tree_parent" = "tree_id")
      ) |>
      tidyr::replace_na(list(tree_parent_lft = 0)) |>
      dplyr::mutate(
        tree_lft = .data$tree_lft + .data$tree_parent_lft,
        tree_rgt = .data$tree_rgt + .data$tree_parent_lft
      ) |>
      dplyr::select(-.data$tree_parent_lft)

    parents <- data |>
      dplyr::semi_join(
        parents,
        by = c("tree_thread", "tree_parent" = "tree_id")
      )
  }

  data |>
    dplyr::arrange(.data$tree_thread, .data$tree_lft) |>
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
#' @param delim Character that glues together the path elements.
#'              In the lemma column, the delimiter will be replaced by a html entity to prevent conflicts.
#'              Set to NULL to create a vector of parent lemmas instead.
#' @return A data frame with the additional column tree_path.
#'        If it existed, it will be overwritten.
#'        Delimiters will be replaced in the lemma column.
#' @export
#' @importFrom rlang .data
#' @importFrom rlang :=
tree_add_path <- function(data, col_id, col_parent_id, col_lemma, delim="/")  {

  col_id <- rlang::enquo(col_id)
  col_parent_id <- rlang::enquo(col_parent_id)
  col_lemma <- rlang::enquo(col_lemma)
  join_by_parent = rlang::set_names(rlang::quo_name(col_id), rlang::quo_name(col_parent_id))


  # Escape characters used as delimiters in lemmata
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

  verbose <- Sys.getenv("epi_verbose") == "TRUE"

  while(nrow(current) > 0) {

    if (verbose) {
      cat(nrow(current)," nodes processed. \n",sep = "")
    }

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


#' Add parent IDs to a tree data frame
#'
#' Given a data frame containing tree nodes in depth-first (preorder) order,
#' with an ID column and a numeric level column, this function adds a
#' `parent_id` column that identifies the parent of each node.
#'
#' The algorithm uses a stack to keep track of ancestors. When the level
#' increases, the current parent is the previous item on the stack. When the
#' level decreases, the stack is popped until the appropriate ancestor is
#' found. This assumes the rows are in tree order (e.g., as produced by a
#' depth-first traversal).
#'
#' @param df A data frame containing tree nodes.
#' @param idcol Name of the column containing node IDs.
#' @param levelcol Name of the column containing numeric levels (depth).
#'
#' @return A data frame with an additional column `parent_id`.
#'
#' @examples
#' df <- data.frame(
#'   id    = c("A", "B", "C", "D", "E"),
#'   level = c(1, 2, 3, 2, 1),
#'   value = c(10, 20, 30, 40, 50)
#' )
#' tree_add_parents(df, "id", "level")
#'
#' @export
tree_add_parent <- function(df, idcol, levelcol) {
  ids <- df[[idcol]]
  lev <- df[[levelcol]]
  n <- nrow(df)

  # Make sure levels are numeric
  if (!is.numeric(lev)) {
    lev <- as.numeric(as.character(lev))
  }
  if (anyNA(lev)) {
    stop("`levelcol` must contain numeric levels with no missing values.")
  }

  # parent_id vector with the same type as ids
  parent_ids <- ids[rep(NA_integer_, n)]

  # Stack of row indices for the current path
  stack <- integer(n)
  top <- 0L

  for (i in seq_len(n)) {
    cur_level <- lev[i]

    # Pop ancestors that are not strictly above the current node
    while (top > 0L && lev[stack[top]] >= cur_level) {
      top <- top - 1L
    }

    # The top of the stack is the parent, if any
    if (top > 0L) {
      parent_ids[i] <- ids[stack[top]]
    } else {
      parent_ids[i] <- ids[NA_integer_]
    }

    # Push current row onto the stack
    top <- top + 1L
    stack[top] <- i
  }

  df[["parent_id"]] <- parent_ids
  df
}

#' Move nodes in a tree
#'
#' Restructures a tree by updating `parent_id` fields and adding missing nodes
#' based on move operations. Move operations are defined by a current path and
#' a target path.
#'
#' @param data A data frame with the current tree. The columns `id`, `parent_id`,
#'   and `lemma` are necessary. For move operations crossing tree types, provide
#'   a `type` column. Other columns are allowed and preserved in the output.
#' @param moves A data frame with the two columns `from_path` and `to_path`.
#'   Optionally, a `to_type` column can be provided to move nodes to a different
#'   tree type.
#' @param keep Whether to keep the columns `from_path` and `tree_path` in the
#'   resulting data frame (TRUE) or to drop them (FALSE).
#' @param delim The delimiter to use for the tree path (default is `">"`).
#'
#' @return A data frame with updated parent IDs, based on the move operations.
#'   Missing parent nodes are created.
#'
#' @examples
#' data <- data.frame(
#'   id        = c("A", "B", "C"),
#'   parent_id = c(NA, "A", "B"),
#'   lemma     = c("A", "B", "C"),
#'   type      = "example"
#' )
#' moves <- data.frame(
#'   from_path = "A > B",
#'   to_path   = "X > Y > B"
#' )
#' tree_move(data, moves)
#'
#' @importFrom rlang .data
#' @export
tree_move <- function(data, moves, keep = TRUE, delim = ">") {

  # Checks

  stopifnot(
    "Moves must contain at least the columns from_path and to_path" =
      all(c("from_path", "to_path") %in% names(moves))
  )

  stopifnot(
    "Data must contain at least the columns id, parent_id, and lemma" =
      all(c("id", "parent_id", "lemma") %in% names(data))
  )

  # Add type columns if necessary
  if (!"type" %in% names(data)) {
    data$type <- ""
  }

  stopifnot(
    "Data must only contain nodes of the same type" =
      length(unique(data$type)) == 1
  )

  if (!"to_type" %in% names(moves)) {
    moves$to_type <- data$type[1]
  }

  # Add tree_path to current data
  data <- tree_add_path(data, id, parent_id, lemma, delim)

  stopifnot(
    "Tree paths in the data must be unique" =
      length(unique(data$tree_path)) == nrow(data)
  )

  # Ignore moves whose source node does not exist in the data.
  # Moves without from_path (pure creation) are kept.
  unused <- dplyr::anti_join(moves, data, by = c("from_path" = "tree_path"))
  moves <- dplyr::semi_join(moves, data, by = c("from_path" = "tree_path"))

  if (length(unused) > 0) {
    message("Ignoring moves with unknown from_path: ", paste(unused$from_path, collapse = ", "))
  }

  # 2. Create missing ancestor nodes of the target paths

  # Paths that will be occupied by moved nodes, they must not be created
  targets <- moves %>%
    dplyr::filter(!is.na(.data$from_path), !is.na(.data$to_path)) %>%
    dplyr::select(tree_path = .data$to_path, type = .data$to_type) %>%
    dplyr::distinct()

  # Start from the parent of each target path.
  # Only if there is no from_path (pure creation) the target itself is created.
  to_create <- moves %>%
    tree_split_path(to_path, delim) %>%
    dplyr::mutate(
      tree_path = dplyr::if_else(is.na(.data$from_path), .data$to_path, .data$tree_parent)
    ) %>%
    dplyr::select(.data$tree_path, type = .data$to_type) %>%
    dplyr::filter(!is.na(.data$tree_path)) %>%
    dplyr::distinct() %>%
    dplyr::anti_join(data, by = c("tree_path", "type")) %>%
    dplyr::anti_join(targets, by = c("tree_path", "type"))

  new <- tibble::tibble()

  while (nrow(to_create) > 0) {

    # Go up one level
    to_create <- to_create %>%
      tree_split_path(tree_path, delim)

    # Resulting columns: tree_path, type, tree_lemma, tree_parent
    new <- dplyr::bind_rows(new, to_create)

    to_create <- to_create %>%
      dplyr::select(tree_path = .data$tree_parent, type = .data$type) %>%
      dplyr::filter(!is.na(.data$tree_path)) %>%
      dplyr::distinct() %>%
      dplyr::anti_join(data, by = c("tree_path", "type")) %>%
      dplyr::anti_join(targets, by = c("tree_path", "type")) %>%
      dplyr::anti_join(new, by = c("tree_path", "type"))
  }

  # 3. Join old and new

  new <- new %>%
    dplyr::mutate(
      norm_iri = paste0("tidy~", epi_clean_irifragment(.data$tree_path)),
      id = ifelse(.data$type == "", .data$norm_iri, paste0("properties/", .data$type, "/", .data$norm_iri))
    ) %>%
    dplyr::select(
      .data$id, .data$norm_iri, .data$tree_path,
      to_type = .data$type,
      to_parent = .data$tree_parent,
      to_lemma = .data$tree_lemma
    )

  # Prepare moves with to_parent and to_lemma
  moves <- moves %>%
    tree_split_path(to_path, delim) %>%
    dplyr::select(.data$from_path, .data$to_type, to_parent = .data$tree_parent, to_lemma = .data$tree_lemma)

  data <- data %>%
    dplyr::left_join(moves, by = c("tree_path" = "from_path"))

  data <- dplyr::bind_rows(new, data)

  # 4. Update fields

  data <- data %>%
    dplyr::mutate(
      lemma = dplyr::coalesce(.data$to_lemma, .data$lemma),
      type  = dplyr::coalesce(.data$to_type, .data$type)
    ) %>%
    dplyr::left_join(
        dplyr::select(data, to_parent_id = .data$id, tree_path = .data$tree_path),
      by = c("to_parent" = "tree_path")
    ) %>%
    dplyr::mutate(
      parent_id = dplyr::coalesce(.data$to_parent_id, .data$parent_id),
      from_path = .data$tree_path
    )

  # Drop move columns if keep = FALSE
  data <- dplyr::select(data, -dplyr::any_of(c("to_type","to_parent","to_lemma", "to_parent_id")))

  data$from_path <- data$tree_path
  data <- tree_add_path(data, id, parent_id, lemma, delim)
  data <- dplyr::arrange(data, .data$tree_path)

  if (!keep) {
    data$tree_path  <- NULL
    data$from_path  <- NULL
  }

  # Remove empty type
  if (all(data$type == "")) {
    data$type <- NULL
  }

  data
}


#' Add ancestor id and path from a specific level to all children
#'
#'@param data A data frame with properties
#'@param level The target level
#'@param col_id The column holding property ids
#'@param col_parent_id The column holding property parent ids
#'@param col_path The column holding a value (mostly a path or lemma) that will be added in addition to the id.
#'@return Data frame with the two columns ancestor_id and ancestor_path added
tree_add_ancestor <- function(data, level = 0, col_id, col_parent_id, col_path) {

  col_id <- rlang::enquo(col_id)
  col_parent_id <- rlang::enquo(col_parent_id)
  col_path <- rlang::enquo(col_path)

  target <- dplyr::distinct(data, !!col_id, !!col_parent_id, !!col_path)
  target <- tree_add_level(target, !!col_id, !!col_parent_id)
  target <- target[target$tree_level == level, c(rlang::as_name(col_id), rlang::as_name(col_path))]
  colnames(target) <- c("ancestor_id", "ancestor_path")

  data |>
    dplyr::distinct() |>
    tree_stack_ancestors(!!col_id, !!col_parent_id, "ancestor_id") |>
    dplyr::inner_join(target, by = "ancestor_id")
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
#' @param ancestor.cols Additional columns to join from the ancestors, a character vector.
#' @importFrom rlang :=
#' @export
tree_stack_ancestors <- function(data, col_id, col_parent, col_stack, ancestor.cols = c()) {

  col_id <- rlang::ensym(col_id)
  col_parent <- rlang::ensym(col_parent)
  col_stack <- rlang::ensym(col_stack)


  # Work on a distinct edge list so duplicate rows in `data`
  # don't multiply during the (many-to-many) joins below.
  tree <- data |>
    dplyr::transmute(
      .tree_id     = !!col_id,
      .tree_parent = !!col_parent
    ) |>
    dplyr::distinct()

  # Put items themselves on the stack
  data_stacked <- dplyr::mutate(tree, .tree_main=.data$.tree_id)

  # Init parents (.tree_main is the parent id)
  data_parents <- tree |>
    dplyr::filter(!is.na(.data$.tree_parent)) |>
    dplyr::mutate(.tree_main=.data$.tree_parent)

  verbose <- Sys.getenv("epi_verbose") == "TRUE"

  while (TRUE) {

    if (nrow(data_parents) > 0) {
      if (verbose) {
        cat(nrow(data_parents)," rows added. \n",sep = "")
      }
      data_stacked <- dplyr::bind_rows(data_stacked, data_parents)
    } else {
      break
    }

    # Find parents
    data_parents <- data_parents |>
      dplyr::inner_join(
        dplyr::select(tree, tidyselect::all_of(c(".tree_id", ".tree_main" = ".tree_parent"))),
        by=c(".tree_main"=".tree_id")
      ) |>
      dplyr::filter(!is.na(.data$.tree_main.y)) |>
      dplyr::mutate(.tree_main = .data$.tree_main.y) |>
      dplyr::select(-tidyselect::all_of(".tree_main.y"))

  }

  # By now, `data_stacked` holds the columns .tree_id, .tree_parent, ad .tree_main.
  # If requested, join more ancestor columns.

  if (length(ancestor.cols) > 0) {
    ancestors <- data |>
      dplyr::mutate(.tree_id = !!col_id) |>
      dplyr::select(tidyselect::all_of(c(".tree_id", ancestor.cols)))

    ancestors <- ancestors[!duplicated(ancestors$.tree_id), , drop =FALSE]
    colnames(ancestors) <- paste0("ancestor.", colnames(ancestors))
    data_stacked <- dplyr::left_join(data_stacked, ancestors, by = c(".tree_main" = "ancestor..tree_id"))
  }

  # Join the original data (duplicates included)
  data <- data |>
    dplyr::mutate(.tree_id = !!col_id) |>
    dplyr::left_join(data_stacked, by = ".tree_id", relationship = "many-to-many")

  # Remove columns and return data
  data <- data |>
    dplyr::select(-tidyselect::all_of(c(".tree_id",".tree_parent"))) |>
    dplyr::mutate(!!col_stack := .data$.tree_main) |>
    dplyr::select(-tidyselect::all_of(".tree_main"))

  data
}


#' Split a tree path column into lemma and parent_path
#'
#' Internal helper that splits a path column (e.g. `"A > B > C"`) into
#' the last element (`tree_lemma`) and the remainder (`tree_parent`).
#'
#' @keywords internal
#'
#' @param data A data frame.
#' @param path_col Column containing tree paths (quoted or unquoted).
#' @param delim Delimiter (default `">"`).
#'
#' @return A data frame with additional columns `tree_lemma` and `tree_parent`.
#'
#' @importFrom rlang .data
tree_split_path <- function(data, path_col, delim = ">") {
  path_col <- rlang::enquo(path_col)

  data %>%
    dplyr::mutate(
      parts  = strsplit(!!path_col, delim, fixed = TRUE),
      parts  = lapply(.data$parts, trimws),
      tree_lemma  = vapply(.data$parts, function(x) tail(x, 1L), character(1)),
      tree_parent = vapply(.data$parts, function(x) {
        if (length(x) == 1L) {
          NA_character_
        } else {
          paste(x[-length(x)], collapse = paste0(" ", delim, " "))
        }
      }, character(1))
    ) %>%
    dplyr::select(-.data$parts)
}
