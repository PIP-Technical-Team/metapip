# Path to the committed PIP_LOCK.csv manifest

Path to the committed PIP_LOCK.csv manifest

## Usage

``` r
pip_lock_path()
```

## Value

path to the committed \`PIP_LOCK.csv\`, or \`""\` when the file is
absent (installed package without a lock). Read-only; callers should
guard with \`file.exists(p) && nzchar(p)\` before reading.
