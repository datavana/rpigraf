# Get articles

Section, item, and property data can be joined.

## Usage

``` r
distill_articles(
  df,
  cols = c(),
  section.type = NULL,
  section.cols = c(),
  item.type = NULL,
  item.cols = c(),
  property.cols = c()
)
```

## Arguments

- df:

  A RAM data frame.

- cols:

  Article columns.

- section.type:

  Section types to join. The result contains only items within sections
  of the given type. Set to NULL to get all items.

- section.cols:

  Cols to join from the sections.

- item.type:

  Item types to join.

- item.cols:

  Cols to join from the items.

- property.cols:

  Cols to join from the property.

## Value

A tibble with articles.
