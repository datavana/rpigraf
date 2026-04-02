# Get RAM rows by table name

Get RAM rows by table name

## Usage

``` r
epi_extract_long(df, table, type = NULL, prefix = TRUE)
```

## Arguments

- df:

  A RAM data frame.

- table:

  The table name.

- type:

  Filter by types: a single character value or a vector of multiple
  values.

- prefix:

  Whether to prefix the columns with the table name.

## Value

A data frame with the filtered rows and columns prefixed with the table
name.
