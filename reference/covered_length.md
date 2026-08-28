# Total length covered by a set of integer intervals

Merges overlapping/adjacent `[start, end]` ranges and sums their widths,
so overlapping annotations are counted once. Ranges are 1-based and
inclusive.

## Usage

``` r
covered_length(start, end)
```

## Arguments

- start:

  Integer vector of range starts.

- end:

  Integer vector of range ends.

## Value

A single integer: the number of distinct positions covered.
