# Fetch tables such as articles, projects or properties

Returns a row with defined columns for each record matched by the
params. The procedure corresponds to calling the index action in the
Epigraf interface.

## Usage

``` r
fetch_table(table, columns = c(), params = c(), db = NA, maxpages = 1)
```

## Arguments

- table:

  The table name (e.g. "articles")

- columns:

  A vector of column names.

- params:

  A named list of query params

- db:

  The database name

- maxpages:

  Maximum number of pages to request. Set to 1 for non-paginated tables.
