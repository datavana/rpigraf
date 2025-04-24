# Rpigraf Package 

The Rpigraf package makes data work with [Epigraf](https://digicademy.github.io/epigraf/) easier.
It provides functions for data transfer using the Epigraf APIs: Preparing data imports, e.g. from social media datasets, and preparing data analyses.

## Installation 

```
library(remotes)
install_github("datavana/rpigraf")
```

## Access the Epigraf API

Please be aware: The API is under development and responds at a cosy pace. Please don't stress the servers.

The endpoints for accessing article data are documented in the [Epigraf help](https://epigraf.inschriften.net/help/epiweb-api). 
To get an access token for nonpublic data access follow the instructions in the help. 
After loading the rpigraf package, you configure the connection to the API:

```
library(rpigraf)
api_setup("https://epigraf.inschriften.net/", "MYACCESSTOKEN")
```

The access token is like a password, don't show it to anyone and make sure it is not printed in any logs or outputs.

Note: If you are working as a developer in a local environment, use the URL https://127.0.0.1/. 
The api_setup()-function provides a third parameter for enabling debug output.

If you get an "Error 401" when using the following methods, check your permissions.

## Reading data 

To warm up, get some article data.

```
# Get an article list
articles <- fetch_table("articles", columns = c("name"), db = "epi_movies", maxpages = 2)

# Get a single article by its ID
article <- fetch_entity("articles-1", db = "epi_movies")

```

You can combine fetching an article list, and the full entity data in the next step:

```
# Get all data for the first articles page (limit each page to 5 articles)
articles <- api_fetch("articles", params=c(limit=5), db="epi_movies", maxpages=1)
```


The data come in the Relation-Article-Model-format. 
That means all pieces of an article are returned as rows.  

Use the distill-function to get some cozy data.
For example, extract all properties of type "categories" from the RAM data:

```
distill_properties(articles, "categories")
```

Or extract the list of articles:

```
distill_articles(articles, c("signature", "name"))
```

See the import vignette for further examples, e.g. on how to get annotations.

## Writing data

You can create or update data with api_patch(). The function expects data in the Relational Article Model-format.
The following command creates one categorie "Western" with the IRI "properties/categories/western" in the database epi_movies.

```
properties <- tibble(
  id = c("properties/categories/western"),
  lemma = c("Western")
)

api_patch(properties, "epi_movies")

```

If a property with the given IRI path already exists, it will not be created, but updated. This way you can change the labels.

The property types, "categories" in the example,  need to be configured in Epigraf. 
Thereafter, you can see the new properties in Epigraf by clicking the categories menu button. 

For more complex data, use the craft functions to map your data frames to the RAM.
See the import vignette for further examnples.

