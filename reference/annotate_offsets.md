# Inject character offsets into id-bearing XML elements

Walks the XML tree once in document order, accumulating a running
character offset over all text and CDATA nodes, and stamps `data-start`
and `data-end` attributes onto every element that carries an `id`
attribute. Offsets are 1-based and inclusive, and refer to positions in
the *plain* (tag-stripped) text, i.e. the concatenation of all text
nodes in document order.

## Usage

``` r
annotate_offsets(xml)
```

## Arguments

- xml:

  Character value containing XML text. May be a fragment with several
  top-level nodes.

## Value

An `xml2::xml_document` with `data-start` and `data-end` integer-valued
attributes injected onto all elements that have an `id`. Feed the result
to
[`extract_segments()`](https://datavana.github.io/rpigraf/reference/extract_segments.md)
for a tidy data frame of spans.

## Details

The input is wrapped in a synthetic `<root>` element so that XML
fragments with multiple top-level nodes can be parsed. Mixed content
(interleaved text and elements) is handled correctly: a parent element's
span covers both its text runs and its child elements, while children
span only their own content. Comment and processing-instruction nodes
are skipped, consistent with how
[`xml2::xml_text()`](http://xml2.r-lib.org/reference/xml_text.md) and
`//text()` treat them.

## See also

[`extract_segments()`](https://datavana.github.io/rpigraf/reference/extract_segments.md)
