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

  Whether rename types columns to `type` and to add a `table` and a
  `database` column.

- db:

  A connection object (object) or the database name (character).
