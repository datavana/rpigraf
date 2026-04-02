# Remove all RAM rows from a tibble

Remove all RAM rows from a tibble

## Usage

``` r
ram_clear(df)
```

## Arguments

- df:

  A tibble with RAM rows in the epi attribute

## Value

Epigraf tibble without RAM rows

## Examples

``` r
library(tibble)
library(rpigraf)

# Example data
df <- tribble(
  ~case, ~title,     ~text,
  1,     "Case 01", "Happy New Year!",
  2,     "Case 02", "Happy Easter!",
  3,     "Case 03", "Happy Birthday!"
)

# Add RAM rows
df <- df |>
 df_to_ram(
   project.fill = c("fragment" = "Example"),
   article.cols = c("id" = "case", "signature" = "case", "name" = "title"),
   section.fill = c("fragment" = "text"),
   item.cols = c("content" = "text")
 )

ram_compile(df)
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

# Remove RAM rows
df <- ram_clear(df)
ram_compile(df)
#> # A tibble: 0 × 1
#> # ℹ 1 variable: value <lgl>
```
