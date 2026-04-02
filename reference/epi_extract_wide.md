# Select nested data from prefixed columns

Select nested data from prefixed columns

## Usage

``` r
epi_extract_wide(data, cols_prefix, cols_keep = c())
```

## Arguments

- data:

  A data frame

- cols_prefix:

  All columns with the prefix will be selected, the prefix will be
  removed from the column name.

- cols_keep:

  Convert the provided column names to underscored columns

## Value

A dataframe containing all columns with the prefix without the prefix
