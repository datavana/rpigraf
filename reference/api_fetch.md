# Fetch entity data such as articles, projects or properties from the API

Returns all data belonging to all entities matched by the params. The
procedure corresponds to calling the index action with the columns
parameter set to 0 in the Epigraf interface.

## Usage

``` r
api_fetch(table, params = c(), db = NA, maxpages = 1)
```

## Arguments

- table:

  The table name (e.g. "articles")

- params:

  A named list of query params

- db:

  The database name

- maxpages:

  Maximum number of pages to request. Set to 1 for non-paginated tables.
