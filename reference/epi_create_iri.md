# Create a clean IRI

Create a clean IRI

## Usage

``` r
epi_create_iri(table, type, fragment, split = F)
```

## Arguments

- table:

  The table name

- type:

  If NA, the type will be omitted.

- fragment:

  The IRI fragment that will be cleaned

- split:

  If TRUE and the fragment already contains a type, the fragment's type
  is used
