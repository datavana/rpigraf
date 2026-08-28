# Extract segment text and character offsets for a tag id

Pulls both the plain text and the character-offset ranges for all
elements matching `tagid`. Reads the `data-start` / `data-end`
attributes injected by
[`annotate_offsets()`](https://datavana.github.io/rpigraf/reference/annotate_offsets.md)
and works vectorized over the selected node set, so repeated extraction
from the same annotated document is cheap.

## Usage

``` r
extract_segments(doc, tagid = NULL)
```

## Arguments

- doc:

  One of: an `xml2::xml_document` already processed by
  [`annotate_offsets()`](https://datavana.github.io/rpigraf/reference/annotate_offsets.md)
  (whose id-bearing elements carry `data-start` / `data-end`
  attributes); a character value containing raw XML, which is annotated
  on the fly; or `NULL` (e.g. for missing content), in which case `NA`
  values are returned. Pass a pre-annotated document when extracting
  several tag ids from the same content, to avoid re-parsing.

- tagid:

  Optional character value giving the tag `id` to extract. If `NULL`,
  all offset-annotated elements are returned.

## Value

A list with elements:

- segments:

  Text pieces joined by `;`.

- offsets:

  Ranges as `"start-end"` joined by `;`.

- ranges:

  A one-element list holding a data frame with columns `id`, `tag`,
  `start`, `end`, `text` (one row per matching element). The
  `start`/`end` columns can be passed directly to `IRanges::IRanges()`
  for coverage and overlap analysis.

## Details

Multiple matches (discontinuous annotations sharing an `id`) yield
multiple spans, returned in document order so that `segments` and
`offsets` align element-for-element.

## See also

[`annotate_offsets()`](https://datavana.github.io/rpigraf/reference/annotate_offsets.md)
