# Get a connection to a database

Before you can use this function, call db_setup once to set the
connection parameters. All parameters are stored in the environment.

## Usage

``` r
db_connect(db = NULL)
```

## Arguments

- db:

  Name of the database as string. Leave empty to use the database name
  from the environment settings.
