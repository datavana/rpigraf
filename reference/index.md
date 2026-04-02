# Package index

## All functions

- [`abc2num()`](https://datavana.github.io/rpigraf/reference/abc2num.md)
  : Convert letters to a number, e.g. c becomes 3
- [`api_buildurl()`](https://datavana.github.io/rpigraf/reference/api_buildurl.md)
  : Build base URL
- [`api_delete()`](https://datavana.github.io/rpigraf/reference/api_delete.md)
  : Delete epigraf data
- [`api_download()`](https://datavana.github.io/rpigraf/reference/api_download.md)
  : Download a file from Epigraf
- [`api_fetch()`](https://datavana.github.io/rpigraf/reference/api_fetch.md)
  : Fetch entity data such as articles, projects or properties from the
  API
- [`api_job_create()`](https://datavana.github.io/rpigraf/reference/api_job_create.md)
  : Create and execute a job
- [`api_job_execute()`](https://datavana.github.io/rpigraf/reference/api_job_execute.md)
  : Execute a job
- [`api_patch()`](https://datavana.github.io/rpigraf/reference/api_patch.md)
  : Patch data
- [`api_post()`](https://datavana.github.io/rpigraf/reference/api_post.md)
  : Post data to epigraf
- [`api_setup()`](https://datavana.github.io/rpigraf/reference/api_setup.md)
  : Save API connection settings to environment variables.
- [`api_silent()`](https://datavana.github.io/rpigraf/reference/api_silent.md)
  : Set silent mode
- [`api_table()`](https://datavana.github.io/rpigraf/reference/api_table.md)
  : Download tables
- [`api_transfer()`](https://datavana.github.io/rpigraf/reference/api_transfer.md)
  : Transfer datasets between different databases
- [`api_upload()`](https://datavana.github.io/rpigraf/reference/api_upload.md)
  : Upload file to epigraf
- [`bind_rows_char()`](https://datavana.github.io/rpigraf/reference/bind_rows_char.md)
  : Bind rows of dataframes even if column types differ
- [`craft_articles()`](https://datavana.github.io/rpigraf/reference/craft_articles.md)
  : Create RAM rows for article data
- [`craft_items()`](https://datavana.github.io/rpigraf/reference/craft_items.md)
  : Create RAM rows for item data
- [`craft_projects()`](https://datavana.github.io/rpigraf/reference/craft_projects.md)
  : Create RAM rows for project data
- [`craft_properties()`](https://datavana.github.io/rpigraf/reference/craft_properties.md)
  : Create RAM rows for property data
- [`craft_sections()`](https://datavana.github.io/rpigraf/reference/craft_sections.md)
  : Create RAM rows for section data
- [`db_condition()`](https://datavana.github.io/rpigraf/reference/db_condition.md)
  : Construct filter conditions for the db_table() function
- [`db_connect()`](https://datavana.github.io/rpigraf/reference/db_connect.md)
  : Get a connection to a database
- [`db_databases()`](https://datavana.github.io/rpigraf/reference/db_databases.md)
  : Get list of all databases
- [`db_fetch()`](https://datavana.github.io/rpigraf/reference/db_fetch.md)
  : Fetch entity data such as articles, projects or properties using
  direct database access
- [`db_name()`](https://datavana.github.io/rpigraf/reference/db_name.md)
  : Get the database name from a connection object
- [`db_setup()`](https://datavana.github.io/rpigraf/reference/db_setup.md)
  : Save database connection settings to environment variables.
  Environment variables are prefixed with "epi\_" and used in
  db_connect() to establish the connection.
- [`db_table()`](https://datavana.github.io/rpigraf/reference/db_table.md)
  : Get data from a database table
- [`decode()`](https://datavana.github.io/rpigraf/reference/decode.md) :
  Converts "b" using the "base"
- [`default_values()`](https://datavana.github.io/rpigraf/reference/default_values.md)
  : Set default values
- [`df_to_ram()`](https://datavana.github.io/rpigraf/reference/df_to_ram.md)
  : Map a data frame to the Relational Article Model (RAM)
- [`distill_articles()`](https://datavana.github.io/rpigraf/reference/distill_articles.md)
  : Get articles
- [`distill_properties()`](https://datavana.github.io/rpigraf/reference/distill_properties.md)
  : Get the property tree (including annotations)
- [`.to_epitable()`](https://datavana.github.io/rpigraf/reference/dot-to_epitable.md)
  : Add the epi_tbl class and make it remember its source
- [`encode()`](https://datavana.github.io/rpigraf/reference/encode.md) :
  Converts numbers using the radix vector
- [`epi_clean_irifragment()`](https://datavana.github.io/rpigraf/reference/epi_clean_irifragment.md)
  : Create a clean IRI fragment
- [`epi_create_iri()`](https://datavana.github.io/rpigraf/reference/epi_create_iri.md)
  : Create a clean IRI
- [`epi_extract_long()`](https://datavana.github.io/rpigraf/reference/epi_extract_long.md)
  : Get RAM rows by table name
- [`epi_extract_wide()`](https://datavana.github.io/rpigraf/reference/epi_extract_wide.md)
  : Select nested data from prefixed columns
- [`epi_iri_parent()`](https://datavana.github.io/rpigraf/reference/epi_iri_parent.md)
  : Get the IRI fragment of an IRI path
- [`epi_is_id()`](https://datavana.github.io/rpigraf/reference/epi_is_id.md)
  : Check whether the provided vector contains valid IDs prefixed with
  table names. Example: articles-123
- [`epi_is_irifragment()`](https://datavana.github.io/rpigraf/reference/epi_is_irifragment.md)
  : Check whether the provided vector contains only valid IRI fragments
- [`epi_is_iripath()`](https://datavana.github.io/rpigraf/reference/epi_is_iripath.md)
  : Check whether the provided vector contains a valid IRI path
- [`epi_is_prefixid()`](https://datavana.github.io/rpigraf/reference/epi_is_prefixid.md)
  : Check whether the provided vector contains valid IDs prefixed with
  table names and temporary prefixes. Example: articles-tmp123
- [`epi_wide_to_long()`](https://datavana.github.io/rpigraf/reference/epi_wide_to_long.md)
  : Convert wide to long format
- [`fetch_entity()`](https://datavana.github.io/rpigraf/reference/fetch_entity.md)
  : Fetch entities such as single articles, projects or properties
- [`fetch_table()`](https://datavana.github.io/rpigraf/reference/fetch_table.md)
  : Fetch tables such as articles, projects or properties
- [`merge_lists()`](https://datavana.github.io/rpigraf/reference/merge_lists.md)
  : Merge list elements by their name
- [`merge_vectors()`](https://datavana.github.io/rpigraf/reference/merge_vectors.md)
  : Merge vectors
- [`move_cols_to_end()`](https://datavana.github.io/rpigraf/reference/move_cols_to_end.md)
  : Shift selected columns to the end
- [`move_cols_to_front()`](https://datavana.github.io/rpigraf/reference/move_cols_to_front.md)
  : Shift selected columns to the front
- [`num2abc()`](https://datavana.github.io/rpigraf/reference/num2abc.md)
  : Convert a number to letters, e.g. 3 becomes c
- [`parse_json()`](https://datavana.github.io/rpigraf/reference/parse_json.md)
  : Parse JSON columns
- [`pseudonyms()`](https://datavana.github.io/rpigraf/reference/pseudonyms.md)
  : Create distinct pseudonyms
- [`ram_add()`](https://datavana.github.io/rpigraf/reference/ram_add.md)
  : Add a row to the rows in the epi attribute
- [`ram_clear()`](https://datavana.github.io/rpigraf/reference/ram_clear.md)
  : Remove all RAM rows from a tibble
- [`ram_compile()`](https://datavana.github.io/rpigraf/reference/ram_compile.md)
  : Compile a crafted table with RAM rows ready to patch into the
  database
- [`tree_add_ancestor()`](https://datavana.github.io/rpigraf/reference/tree_add_ancestor.md)
  : Add ancestor id and path from a specific level to all children
- [`tree_add_level()`](https://datavana.github.io/rpigraf/reference/tree_add_level.md)
  : Add level, thread and order
- [`tree_add_mptt()`](https://datavana.github.io/rpigraf/reference/tree_add_mptt.md)
  : Adds left and right values to the dataframe
- [`tree_add_path()`](https://datavana.github.io/rpigraf/reference/tree_add_path.md)
  : Add a column holding the path of each node.
- [`tree_bind_ancestors()`](https://datavana.github.io/rpigraf/reference/tree_bind_ancestors.md)
  : Row bind all ancestors of the selected nodes
- [`tree_get_nodes()`](https://datavana.github.io/rpigraf/reference/tree_get_nodes.md)
  : Get all distinct nodes in an edge list
- [`tree_stack_ancestors()`](https://datavana.github.io/rpigraf/reference/tree_stack_ancestors.md)
  : For each node, add each ancestor's id
