# Get annotations for the articles

Get annotations for the articles

## Usage

``` r
distill_links(
  df,
  items.type = NULL,
  properties.type = NULL,
  cols = c("path", "segment"),
  article.cols = c(),
  level = 0
)
```

## Arguments

- df:

  A RAM data frame.

- items.type:

  The type of items with annotations.

- properties.type:

  Keep only links that target the given property type.

- article.cols:

  A list of article columns to join.

- level:

  The aggregation level, beginning with 0. Set to NULL to get the lowest
  level.

## Value

A tibble containing annotations.
