# Move nodes in a tree

Restructures a tree by updating `parent_id` fields and adding missing
nodes based on move operations. Move operations are defined by a current
path and a target path.

## Usage

``` r
tree_move(data, moves, keep = TRUE, delim = ">")
```

## Arguments

- data:

  A data frame with the current tree. The columns `id`, `parent_id`, and
  `lemma` are necessary. For move operations crossing tree types,
  provide a `type` column. Other columns are allowed and preserved in
  the output.

- moves:

  A data frame with the two columns `from_path` and `to_path`.
  Optionally, a `to_type` column can be provided to move nodes to a
  different tree type.

- keep:

  Whether to keep the columns `from_path` and `tree_path` in the
  resulting data frame (TRUE) or to drop them (FALSE).

- delim:

  The delimiter to use for the tree path (default is `">"`).

## Value

A data frame with updated parent IDs, based on the move operations.
Missing parent nodes are created.

## Examples

``` r
data <- data.frame(
  id        = c("A", "B", "C"),
  parent_id = c(NA, "A", "B"),
  lemma     = c("A", "B", "C"),
  type      = "example"
)
moves <- data.frame(
  from_path = "A > B",
  to_path   = "X > Y > B"
)
tree_move(data, moves)
#> Ignoring moves with unknown from_path: 
#> # A tibble: 5 × 7
#>   id                          norm_iri tree_path parent_id lemma type  from_path
#>   <chr>                       <chr>    <chr>     <chr>     <chr> <chr> <chr>    
#> 1 A                           NA       A         NA        A     exam… A        
#> 2 properties/example/tidy~x   tidy~x   X         NA        X     exam… X        
#> 3 properties/example/tidy~x-y tidy~x-y X > Y     properti… Y     exam… X > Y    
#> 4 B                           NA       X > Y > B properti… B     exam… A > B    
#> 5 C                           NA       X > Y > … B         C     exam… A > B > C
```
