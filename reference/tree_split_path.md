# Split a tree path column into lemma and parent_path

Internal helper that splits a path column (e.g. `"A > B > C"`) into the
last element (`tree_lemma`) and the remainder (`tree_parent`).

## Usage

``` r
tree_split_path(data, path_col, delim = ">")
```

## Arguments

- data:

  A data frame.

- path_col:

  Column containing tree paths (quoted or unquoted).

- delim:

  Delimiter (default `">"`).

## Value

A data frame with additional columns `tree_lemma` and `tree_parent`.
