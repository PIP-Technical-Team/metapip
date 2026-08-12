# AGENTS.md

## Project

`metapip` is an R package that manages and installs the PIP R packages (pipapi,
pipaux, pipload, wbpip, pipfun, pipdata, pipr) via the GitHub API. It is inspired
by the tidyverse meta-package.

## Commands

- Run tests: `Rscript -e "devtools::test()"`
- Run full check: `Rscript -e "rcmdcheck::rcmdcheck()"`
- Regenerate docs: `Rscript -e "devtools::document()"`
- Build pkgdown site: `Rscript -e "pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)"`

## Conventions

- R package conventions (roxygen2 for docs, `man/` and `NAMESPACE` are generated).
- If roxygen2 comments change, run `devtools::document()` and commit the regenerated `man/` and `NAMESPACE`.
- Update `NEWS.md` for user-visible changes.
- Version lives in `DESCRIPTION` (`Version:` field).
- Default branch for development is `master`; PRs target `master`.

## Testing

Tests live in `tests/testthat/` using the testthat 3rd edition. New functions
should have a corresponding `tests/testthat/test-<name>.R` file.
