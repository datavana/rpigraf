# Eprgraf Package 

The Epigraf package makes data work with [Epigraf](https://digicademy.github.io/epigraf/) easier.
It provides functions for data transfer using the Epigraf APIs: Preparing data imports, e.g. from social media datasets, and preparing data analyses.

## Installation 

```
library(remotes)
install_github("datavana/eprgraf")
```

## Access the Epigraf API

Please be aware: The API is under development and responds at a cosy pace. Please don't stress the servers.

The endpoints for accessing article data are documented in the [Epigraf help](https://epigraf.inschriften.net/help/epiweb-api). 
To get an access token for nonpublic data access follow the instructions in the help. 
After loading the Eprgraf package, you configure the connection to the API:

```
library(eprgraf)
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
articles <- fetch_table("articles", columns=c("name"), db="epi_playground", maxpages=2)

# Get a single article by its ID
article <- fetch_entity("articles-1", db="epi_playground")

```

You can combine fetching an article list, and the full entity data in the next step:

```
# Get all data for the first articles page (limit each page to 5 articles)
articles <- fetch_table("articles", params=c(limit=5), db="epi_playground", maxpages=1) |> 
  fetch_entity()
```


The data come in the Relation-Article-Model-format. 
That means all pieces of an article are returned as rows.  
Filter the rows to dive into a specific slice of the article data.
For example, extract the property tree from the articles:

```
props <- articles |> 
  filter(table=="properties") |> 
  select_if(~ ! all(is.na(.))) |> 
  distinct() |> 
  arrange(propertytype,lft) 
  select(lemma,level,lft,rght,id,parent_id) |> 
  tree_add_path(id, parent_id,id)

```


## Writing data

You can create or update data with api_patch(). The function expects data in the Relational Article Model-format.
The following command creates one categorie "Hansestädte" with the IRI "properties/topics/hanseatic" in the database epi_example.

```
properties <- tibble(
  id = c("properties/topics/hanseatic"),
  lemma = c("Hansestädte")
)

api_patch(properties, "epi_example")

```

If a property with the given IRI path already exists, it will not be created, but updated. This way you can change the labels.

The property types, "topics" in the example,  need to be configured in Epigraf. 
Thereafter, you can see the new properties in Epigraf by clicking the categories menu button. 

