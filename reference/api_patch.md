# Patch data

Update entities in the database using the API. Existing entities will be
updated, missing entities will be created. The function supports
uploading all data related to articles: articles, sections, items,
links, footnotes, properties, projects, users, types. The IRI path in
the ID column of the dataframe must contain the specific table name.

## Usage

``` r
api_patch(data, db, table = NA, type = NA, wide = T)
```

## Arguments

- data:

  A dataframe with the column `id`. Additional columns such as
  `norm_data` will be written to the entity. The id must either be a
  valid IRI path (e.g. `properties/objecttypes/xxx`) or an id prefixed
  by the table name (e.g. `properties-12`). Patching properties with
  prefixed ids requires a `type` column that contains the property type.
  If wide is set to TRUE (default), column names prefixed with table
  names are extracted.

- db:

  The database name.

- table:

  Optional: Check that the data only contains rows for a specific table.

- type:

  Optional: Check that the data only contains rows with a specific type.

- wide:

  Convert wide format to long format. If TRUE, column names prefixed
  with "properties", "items", "sections", "articles" and "projects"
  followed by a dot (e.g. `properties.id`, `properties.lemma`) will be
  extracted and patched as additional entities.
