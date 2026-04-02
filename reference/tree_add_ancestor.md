# Add ancestor id and path from a specific level to all children

Add ancestor id and path from a specific level to all children

## Usage

``` r
tree_add_ancestor(data, level = 0, col_id, col_parent_id, col_path)
```

## Arguments

- data:

  A data frame with properties

- level:

  The target level

- col_id:

  The column holding property ids

- col_parent_id:

  The column holding property parent ids

- col_path:

  The column holding a value (mostly a path or lemma) that will be added
  in addition to the id.

## Value

Data frame with the two columns ancestor_id and ancestor_path added
