# Convert wide to long format

Convert wide to long format

## Usage

``` r
epi_wide_to_long(data)
```

## Arguments

- data:

  A dataframe with the column id containing a valid IRI path. If
  additional column names are prefixed with "properties", "items",
  "sections", "articles" and "projects" followed by a dot (e.g.
  "properties.id", "properties.lemma"), they will be extracted and
  stacked to the dataframe.

## Value

A dataframe with all input rows and the nested entities stacked.
