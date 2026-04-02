# Convert wide to long format

Convert wide to long format

## Usage

``` r
epi_wide_to_long(data)
```

## Arguments

- data:

  A dataframe with the column id containing a valid IRI path. Additional
  columns may contain nested data in the following form:

              Column names prefixed with "properties", "items", "sections",
               "articles" and "projects" followed by a dot (e.g. "properties.id",
              "properties.lemma") will be extracted and stacked to the dataframe.

## Value

A dataframe with all input rows and the nested records stacked.
