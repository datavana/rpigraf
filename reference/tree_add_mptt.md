# Adds left and right values to the dataframe

Left and right values are used to store hierarchical data using the
concept of modified preorder tree traversal. See
https://www.sitepoint.com/hierarchical-data-database-3/

## Usage

``` r
tree_add_mptt(data)
```

## Arguments

- data:

  Dataframe with the columns tree_id, tree_parent, tree_thread,
  tree_level, tree_order

## Value

Dataframe with lft and rght values
