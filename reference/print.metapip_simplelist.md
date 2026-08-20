# Pretty print for metapip_simplelist objects

S3 method that prints a named list of character vectors in a readable,
aligned format using the cli package. Default branches are shown in red;
custom branches are shown in blue.

## Usage

``` r
# S3 method for class 'metapip_simplelist'
print(x, ...)
```

## Arguments

- x:

  A named list of character vectors with class \`"metapip_simplelist"\`.
  Attributes: - \`title\`: cli-formatted header string. - \`to_red\`:
  character scalar to highlight in red.

- ...:

  Additional arguments passed to or from other methods (currently
  ignored).

## Value

\`x\` invisibly (for use in pipelines).
