# Non tidyverse alternative to tibble::rownames_to_column

Non tidyverse alternative to tibble::rownames_to_column

## Usage

``` r
rowname_to_column(data, var)
```

## Arguments

- data:

  Dataframe

- var:

  column name to store rownames

## Value

Dataframe with an additional column of rownames

## Examples

``` r
if (FALSE) { # \dontrun{
rowname_to_column(mtcars, "rn")
} # }
```
