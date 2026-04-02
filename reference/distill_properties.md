# Get the property tree (including annotations)

Get the property tree (including annotations)

## Usage

``` r
distill_properties(df, type = NULL, cols = c(), annos = FALSE, levelup = NULL)
```

## Arguments

- df:

  A RAM data frame.

- type:

  The property type.

- cols:

  The property columns.

- annos:

  Whether to distill annotations.

- levelup:

  If set to a number, the tree will be simplified by replacing the path
  value on lower levels with the ancestor path from the given level.

## Value

A tibble containing the properties tree.
