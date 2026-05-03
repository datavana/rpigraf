# For each node, add each ancestor's id

In the result, nodes will be duplicated for all their ancestors. As an
example: a node on level 2 will be present two times,

1.  the node containing the parent_id in the col_stack column

2.  the node containing the parents parent_id in the col_stack column

## Usage

``` r
tree_stack_ancestors(data, col_id, col_parent, col_stack, ancestor.cols = c())
```

## Arguments

- data:

  All nodes

- col_id:

  The column holding IDs of the nodes

- col_parent:

  The column holding IDs of the parent nodes

- col_stack:

  The column that will hold the ancestors IDs

- ancestor.cols:

  Additional columns to join from the ancestors, a character vector.
