# Add parent IDs to a tree data frame

Given a data frame containing tree nodes in depth-first (preorder)
order, with an ID column and a numeric level column, this function adds
a `parent_id` column that identifies the parent of each node.

## Usage

``` r
tree_add_parent(df, idcol, levelcol)
```

## Arguments

- df:

  A data frame containing tree nodes.

- idcol:

  Name of the column containing node IDs.

- levelcol:

  Name of the column containing numeric levels (depth).

## Value

A data frame with an additional column `parent_id`.

## Details

The algorithm uses a stack to keep track of ancestors. When the level
increases, the current parent is the previous item on the stack. When
the level decreases, the stack is popped until the appropriate ancestor
is found. This assumes the rows are in tree order (e.g., as produced by
a depth-first traversal).

## Examples

``` r
df <- data.frame(
  id    = c("A", "B", "C", "D", "E"),
  level = c(1, 2, 3, 2, 1),
  value = c(10, 20, 30, 40, 50)
)
tree_add_parents(df, "id", "level")
#> Error in tree_add_parents(df, "id", "level"): could not find function "tree_add_parents"
```
