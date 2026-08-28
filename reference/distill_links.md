# Get annotations for the articles

Get annotations for the articles

## Usage

``` r
distill_links(
  df,
  items.type = NULL,
  properties.type = NULL,
  cols = c("path", "segments", "offsets", "length", "coverage"),
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

- cols:

  A list of columns to add. Add `segments` to extract annotated text
  segments. Add `offsets` to return annotated offsets, add `length` to
  add a plain text size column. Add `coverage` to get the percentage of
  covered plain text.

- article.cols:

  A list of article columns to join.

- level:

  The aggregation level, beginning with 0. Set to NULL to get the lowest
  level.

## Value

A tibble containing annotations.
