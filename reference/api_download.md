# Download a file from Epigraf

Download a file from Epigraf

## Usage

``` r
api_download(
  endpoint,
  params = c(),
  filename = NULL,
  filepath = NULL,
  overwrite = FALSE,
  database = NA
)
```

## Arguments

- endpoint:

  The endpoint path.

- params:

  Query parameters.

- filename:

  A file name or a full path to the local file.

- filepath:

  A target folder path

- overwrite:

  Whether to overwrite existing files.

- database:

  The selected database.

## Value

A list with error and data elements.
