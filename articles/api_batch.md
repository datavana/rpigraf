# Batch processing and export pipelines

## Example for downloading data from an export job

In this example, the pipeline (ID 471) needs to be finished with a
download task that contains two download files

``` r

job <- api_job_create(
  endpoint = "articles/export", 
  params = list(projects=20, pipeline=471, id=2508), 
  database = "epi_playground",
  payload = list(config=list(options=list(sort_signatures=0)))
)


downloads <- job$downloads

api_download(downloads$url[1], filename = downloads$name[1])
api_download(downloads$url[2], filename = downloads$name[2])
```
