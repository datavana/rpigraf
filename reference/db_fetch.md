# Fetch entity data such as articles, projects or properties using direct database access

Returns all data belonging to all entities matched by the params.

## Usage

``` r
db_fetch(table, params = list(), db = NA)
```

## Arguments

- table:

  The table name (e.g. "articles").

- params:

  A named list of query conditions, passed to db_table.

- db:

  The database name (character). Provide a character vector of dababase
  names to get and row bind data from multiple databases.
