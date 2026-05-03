# Manage Epigraf databases using the API

``` r


# Create a database
api_post("databanks/add", payload=list("name"="dio","category"="DIO"))

# Clear databases in a category
dbs <- fetch_table("databanks", params=c("category"="DIO"))

for (dbid in dbs$id) {
  api_post(paste0("databanks/drop/", dbid))
  api_post(paste0("databanks/create/", dbid))
}
```
