# Transfer datasets between different databases

Transfer datasets between different databases

## Usage

``` r
api_transfer(table, db_source, db_target, db_params = list())
```

## Arguments

- table:

  Table name, e.g. "types"

- db_source:

  Source database name

- db_target:

  Target database name

- db_params:

  A parameter list for selecting the appropriate rows. For example:
  params \<- list("scopes" = "properties")
