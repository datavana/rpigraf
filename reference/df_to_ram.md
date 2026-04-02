# Map a data frame to the Relational Article Model (RAM)

Calls craft methods for projects, articles, sections, items and
properties. Alternatively, you can use the craft methods directly.

## Usage

``` r
df_to_ram(
  df,
  project.cols = c(),
  project.fill = c(),
  article.cols = c(),
  article.fill = c(),
  section.cols = c(),
  section.fill = c(),
  property.cols = c(),
  property.fill = c(),
  item.cols = c(),
  item.fill = c(),
  compile = FALSE
)
```

## Arguments

- df:

  A data frame with the source data.

- project.cols:

  A named list. Names are RAM columns, values source columns.

- project.fill:

  A named list with fixed values for the rows.

- article.cols:

  A named list. Names are RAM columns, values source columns.

- article.fill:

  A named list with fixed values for the rows.

- section.cols:

  A named list. Names are RAM columns, values source columns.

- section.fill:

  A named list with fixed values for the rows.

- property.cols:

  A named list. Names are RAM columns, values source columns.

- property.fill:

  A named list with fixed values for the rows.

- item.cols:

  A named list. Names are RAM columns, values source columns.

- item.fill:

  A named list with fixed values for the rows.

- compile:

  Whether to return the compiled RAM rows or a RAM enhanced data frame.

## Value

When compile is TRUE: a RAM data frame, ready for patching into the
Epigraf database. When compile is FALSE: a RAM-enhanced data frame. In
RAM-enhanced data frames, the RAM rows are stored in the epi attribute.
Call
[`ram_compile()`](https://datavana.github.io/rpigraf/reference/ram_compile.md)
to get a RAM data frame from a RAM-enhanced data frame.

## Examples

``` r
library(tibble)
library(rpigraf)

df <- tribble(
  ~case, ~title,     ~text,
  1,     "Case 01", "Happy New Year!",
  2,     "Case 02", "Happy Easter!",
  3,     "Case 03", "Happy Birthday!"
)

df |>
 df_to_ram(
   project.fill = c("fragment" = "Example"),
   article.cols = c("id" = "case", "signature" = "case", "name" = "title"),
   section.fill = c("fragment" = "text"),
   item.cols = c("content" = "text"),
   compile = TRUE
 )
#> # A tibble: 8 × 8
#>   id       signature name  content sections_id articles_id projects_id `_fields`
#>   <chr>    <chr>     <chr> <chr>   <chr>       <chr>       <chr>       <chr>    
#> 1 project… NA        NA    NA      NA          NA          NA          id,type,…
#> 2 article… 3         Case… NA      NA          NA          projects/d… projects…
#> 3 article… 2         Case… NA      NA          NA          projects/d… projects…
#> 4 article… 1         Case… NA      NA          NA          projects/d… projects…
#> 5 section… NA        NA    NA      NA          articles/d… projects/d… articles…
#> 6 items/d… NA        NA    Happy … sections/d… articles/d… projects/d… sections…
#> 7 items/d… NA        NA    Happy … sections/d… articles/d… projects/d… sections…
#> 8 items/d… NA        NA    Happy … sections/d… articles/d… projects/d… sections…
```
