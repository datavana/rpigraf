# Send request to epigraf

Send request to epigraf

## Usage

``` r
.api_request(
  endpoint,
  params = c(),
  payload = NULL,
  database = NA,
  method = httr::POST,
  encode = "json"
)
```

## Arguments

- endpoint:

  The endpoint supporting job creation

- params:

  Query parameters

- payload:

  The data posted to the endpoint

- database:

  The selected database

- method:

  One of the httr functions (httr::POST, httr::DELETE)

- encode:

  Payload encoding. Passed to the httr method function. See httr::POST
  for how to upload files with multipart encoding.

## Value

void
