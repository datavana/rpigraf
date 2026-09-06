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

  A named list of query conditions, passed to db_table. May contain
  sublists named by a table name to route conditions to that table.

- db:

  The database name (character). Provide a character vector of dababase
  names to get and row bind data from multiple databases.

## Details

Params may contain sublists named by a table name to target conditions
at a specific table, e.g.
`params = list("properties" = list("propertytype" = "fonttypes"), "articles" = list("articletype" = "epi-article"))`.
Any parameters not nested under one of the tables `articles`,
`sections`, `items`, `properties`, `links`, `footnotes` or `projects`
are passed to the first (root) `db_table` call.
