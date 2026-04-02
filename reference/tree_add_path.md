# Add a column holding the path of each node.

The path is created by concatenating all col_lemma values up to the root
node. Lemmata are concatenated using a slash - existing slashes are
replaced by the entity /.

## Usage

``` r
tree_add_path(data, col_id, col_parent_id, col_lemma, delim = "/")
```

## Arguments

- data:

  Dataframe containing hierarchical data

- col_id:

  The ID column of the node

- col_parent_id:

  The ID column of the parent node

- col_lemma:

  The column holding the node name that will be used for the path

- delim:

  Character that glues together the path elements. Set to NULL to create
  a vector instead.

## Value

A data frame with the additional column tree_path
