# Upload file to epigraf

Upload file to epigraf

## Usage

``` r
api_upload(
  endpoint,
  params = c(),
  filepath = NULL,
  mimetype = NULL,
  overwrite = FALSE,
  database = NA
)
```

## Arguments

- endpoint:

  The endpoint path

- params:

  Query parameters

- filepath:

  A full path to the local file

- mimetype:

  The mime type fof the file. Will be guessed if empty.

- overwrite:

  Whether to overwrite existing files.

- database:

  The selected database

## Value

void
