# Download tables

Fetches tables such as articles, projects or properties

## Usage

``` r
api_table(
  endpoint,
  params = c(),
  db = NA,
  maxpages = 1,
  compact = FALSE,
  silent = FALSE
)
```

## Arguments

- endpoint:

  The endpoint path (e.g. "articles/index" or "articles/view/1")

- params:

  A named list of query params

- db:

  The database name. Provide a character vector of dababase names to get
  and row bind data from multiple databases. In this case, the compact
  parameter is automatically set to TRUE. Thus, a database name column
  is added.

- maxpages:

  Maximum number of pages to request. Set to 1 for non-paginated tables.

- compact:

  Whether to rename type columns to `type` and to add a `table` and a
  `database` column.

- silent:

  Whether to output status messages

## Details

TODO: silent problem message (false positive, results from the last
column being empty from case to case) TODO: add progress bar
