# Move row names to a column

A lightweight alternative to \`tibble::rownames_to_column()\` that does
not require the tibble package. Prepends the row names of a data.frame
as a new column.

## Usage

``` r
rowname_to_column(data, var)
```

## Arguments

- data:

  A data.frame.

- var:

  Character scalar. Name of the new column to hold row names.

## Value

A data.frame with the same columns as \`data\` plus the new column
\`var\` as the first column. Row names are removed.

## Examples

``` r
df <- data.frame(x = 1:3, row.names = c("a", "b", "c"))
rowname_to_column(df, "id")
#>   id x
#> 1  a 1
#> 2  b 2
#> 3  c 3
```
