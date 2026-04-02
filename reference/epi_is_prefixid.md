# Check whether the provided vector contains valid IDs prefixed with table names and temporary prefixes. Example: articles-tmp123

Check whether the provided vector contains valid IDs prefixed with table
names and temporary prefixes. Example: articles-tmp123

## Usage

``` r
epi_is_prefixid(ids, table = NA, prefix = NA)
```

## Arguments

- ids:

  The vector that will be proofed

- table:

  Check whether the path contains the table. Leave empty to allow all
  tables.

- prefix:

  Check whether the ID contains the prefix, e.g. "tmp". Leave empty to
  allow all prefixes.
