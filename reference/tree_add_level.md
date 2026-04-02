# Add level, thread and order

TODO: fix tree_order ... should be related to the thread, not the parent

## Usage

``` r
tree_add_level(data, col_id, col_parent, col_sort = NULL)
```

## Arguments

- data:

  The dataframe containing hierarchical data

- col_id:

  The ID column of the node

- col_parent:

  The ID column of the parent node

- col_sort:

  Column for sorting the nodes inside each parent. Leave empty to use
  the ID column.

## Value

Data frame with the additional columns tree_thread, tree_order and
tree_level
