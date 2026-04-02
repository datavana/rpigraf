# Get articles (including selected item values)

TODO: Implement article.cols parameter.

## Usage

``` r
distill_items(
  df,
  type = NULL,
  cols = c(),
  property.cols = c(),
  article.cols = c()
)
```

## Arguments

- df:

  A RAM data frame.

- type:

  Item types to filter.

- cols:

  Cols returned from the items.

- property.cols:

  Property columns joined to the items.

- article.cols:

  Article columns joined to the items. Not implemented yet.

## Value

A tibble with items.
