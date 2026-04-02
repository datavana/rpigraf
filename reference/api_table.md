# Download tables

Fetches tables such as articles, projects or properties

## Usage

``` r
api_table(endpoint, params = c(), db = NA, maxpages = 1, silent = FALSE)
```

## Arguments

- endpoint:

  The endpoint path (e.g. "articles/index" or "articles/view/1")

- params:

  A named list of query params

- db:

  The database name

- maxpages:

  Maximum number of pages to request. Set to 1 for non-paginated tables.

- silent:

  Whether to output status messages

## Details

TODO: silent problem message (false positive, results from the last
column being empty from case to case) TODO: add progress bar
