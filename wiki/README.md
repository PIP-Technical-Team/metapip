# metapip

<!-- cg:auto:overview -->
`metapip` is a meta R package from the World Bank's PIP team (DECDG / GPID)
whose only objective is the proper management of all the other PIP packages.
It helps you install the latest branch of each PIP package, get information
about the packages, and much more — all enabled via the GitHub API. It provides
a set of functions for working efficiently with the entire PIP ecosystem:
`pipapi`, `pipaux`, `pipload`, `wbpip`, `pipfun`, `pipdata`, and `pipr`.
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

`metapip` requires valid GitHub credentials. Authentication is handled via
`{gitcreds}` and verified with `check_github_token()`. The package is released
under the MIT license.
<!-- cg:auto:end -->

<!-- cg:auto:quick-start -->
Install all core PIP packages with:

```r
library(metapip)
install_pip_packages()
```

Update everything with `update_pip_packages()` or `metapip_update()`, and
inspect the ecosystem with `metapip_packages()`.
<!-- cg:auto:end -->
