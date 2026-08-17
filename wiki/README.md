# metapip

<!-- cg:auto:overview -->
`metapip` is a meta R package from the World Bank's PIP team (DECDG / GPID)
whose only objective is the proper management of all the other PIP packages.
It helps you install the latest branch of each PIP package, get information
about the packages, and much more — all enabled via the GitHub API. It provides
a set of functions for working efficiently with the entire PIP ecosystem:
`pipapi`, `pipaux`, `pipload`, `wbpip`, `pipfun`, `pipdata`, and `pipr`.

The package ships a committed team lock manifest (`inst/PIP_LOCK.csv`,
columns `package,branch,sha`) that pins the whole ecosystem to agreed
commits. `pip_snapshot()` writes or refreshes the lock, `init_metapip()`
installs from it (falling back to branch HEAD when the lock is absent), and
`update_pip_packages()` refreshes the lock as part of updating.
<!-- cg:auto:end -->

## Contents
- [Home](README.md)
- [API Reference](api-reference.md)
- [Vignettes](vignettes.md)
- [Changelog](changelog.md)

<!-- cg:auto:installation -->
Install the package directly from GitHub:

```r
devtools::install_github("PIP-Technical-Team/metapip")
```

Install functions require valid GitHub credentials — authentication is handled
via `{gitcreds}` and verified with `check_github_token()` (which returns a
redacted `metapip_token`; the live token never leaks). Read-only calls
(`get_branches()`, `get_branch_info()`, `get_latest_branch_update()`,
`core_metadata()`, `package_branches()`) work without a token against the
public `PIP-Technical-Team` org. The package's runtime dependencies `httr2`
(>= 1.0.0) and `data.table` are installed automatically. The package is
released under the MIT license.

A committed team lock manifest (`inst/PIP_LOCK.csv`) ships with the package.
`pip_snapshot()` writes or refreshes it; the write target is controlled by
`options(metapip.lock_path)`, defaulting to the installed package copy. Run
`pip_snapshot()` in a `devtools::load_all()` session to refresh the source-tree
`inst/PIP_LOCK.csv` and commit the change.
<!-- cg:auto:end -->

<!-- cg:auto:quick-start -->
Install all core PIP packages with:

```r
library(metapip)
install_pip_packages()
```

Update everything with `update_pip_packages()` or `metapip_update()`, and
inspect the ecosystem with `metapip_packages()`.

For team-consistent installs, generate (or refresh) the lock and install from
it:

```r
pip_snapshot()   # writes/refreshes PIP_LOCK.csv
init_metapip()   # installs every package at its recorded SHA
```

`init_metapip()` falls back to branch HEAD — and suggests `pip_snapshot()` —
when no `PIP_LOCK` is available.
<!-- cg:auto:end -->
