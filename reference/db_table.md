# Get data from a database table

Get data from a database table

## Usage

``` r
db_table(table, cond = list(), deleted = FALSE, compact = FALSE, db)
```

## Arguments

- table:

  Table name.

- cond:

  Either a named list of conditions, or a full condition as character,
  or a character vector of conditions, e.g. `c("id = 10")`.

- deleted:

  Deleted records are skipped by default. Set to TRUE, to get all
  records.

- compact:

  Whether to rename type columns to `type` and to add a `table` and a
  `database` column.

- db:

  A connection object (object) or the database name (character). Provide
  a character vector of dababase names to get and row bind data from
  multiple databases. In this case, the compact parameter is
  automatically set to TRUE. Thus, a database name column is added.
