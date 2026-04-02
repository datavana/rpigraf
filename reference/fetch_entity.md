# Fetch entities such as single articles, projects or properties

Returns all data belonging to the entity identified by ID. The procedure
corresponds to calling the view action in the Epigraf interface.

## Usage

``` r
fetch_entity(ids, params = c(), db = NULL, silent = FALSE)
```

## Arguments

- ids:

  A character vector with IDs as returned by fetch_table, e.g.
  articles-1. Alternatively, provide a dataframe containg the IDs in the
  id-column. So you can chain fetch_articles() and fetch_entity()

- params:

  A named list of query params

- db:

  The database name. Leave empty when providing a dataframe produced by
  fetch_table(). In this case, the database name will be extracted from
  the dataframe.

- silent:

  Whether to output a progress bar
