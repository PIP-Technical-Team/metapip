# Set colorDF theme based on RStudio theme (internal)

Detects the current RStudio dark/light theme and sets the
\`colorDF_theme\` option accordingly (\`"wb"\` for dark, \`"bw"\` for
light).

## Usage

``` r
set_colorDF()
```

## Value

Invisible list. The RStudio theme information from \[rs_theme()\].
