# Path to the committed PIP_LOCK.csv manifest

Returns the path to the bundled \`PIP_LOCK.csv\` file that ships with
the installed metapip package. Returns \`""\` when the file is absent
(e.g., package installed without a lock). Callers should guard with
\`file.exists(p) && nzchar(p)\` before reading.

## Usage

``` r
pip_lock_path()
```

## Value

Character scalar. Path to \`PIP_LOCK.csv\`, or \`""\` if absent.

## See also

\[pip_snapshot()\], \[init_metapip()\]
