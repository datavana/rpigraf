# Row bind all ancestors of the selected nodes

Row bind all ancestors of the selected nodes

## Usage

``` r
tree_bind_ancestors(.data, .tree, id, parent_id)
```

## Arguments

- .data:

  Data frame containing the selected nodes

- .tree:

  Data frame containing all nodes including all ancestors

- id:

  Column name of the id in .data and .tree

- parent_id:

  Column name of the parent id in .data and .tree

## Value

Data frame containing the nodes of .data and all ancestors
