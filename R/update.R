#' Check for CRAN updates to metapip and its dependencies
#'
#' @description
#' Checks whether `metapip` (and optionally its PIP package dependencies)
#' are up-to-date relative to the versions available on CRAN. Prints
#' the packages that need updating along with the R code to do so.
#'
#' @param pkg Character scalar. Package to check. Defaults to `"metapip"`.
#' @param recursive Logical. If `TRUE`, also checks all transitive
#'   dependencies of `pkg`. Default `FALSE`.
#' @param ... Additional arguments passed to [utils::install.packages()]
#'   in the generated install expression.
#'
#' @return `invisible()`. Prints a human-readable update report; does
#'   not modify any packages directly.
#'
#' @details
#' When `metapip` is not on CRAN (pre-release), a hardcoded dependency
#' list is used as fallback (see `pkg_deps()`).
#'
#' @seealso
#' [pkg_deps()]
#'
#' @examples
#' \dontrun{
#' metapip_update()
#' metapip_update(recursive = TRUE)
#' }
#'
#' @importFrom utils install.packages
#' @export
metapip_update <- function(pkg = "metapip", recursive = FALSE, ...) {
  deps <- pkg_deps(pkg, recursive)
  behind <- fsubset(deps, behind)

  if (nrow(behind) == 0) {
    cli::cat_line("All packages up-to-date")
    return(invisible())
  }

  cli::cat_line("The following packages are out of date:")
  cli::cat_line()
  cli::cat_bullet(
    format(behind$package), " (",
    behind$local, " -> ", behind$cran, ")"
  )

  cli::cat_line()
  cli::cat_line("Start a clean R session then run:")

  install_opt <- rlang::quos(...)
  install_pkg <- behind$package
  inst_expr <- rlang::quo(
    install.packages(c(!!!install_pkg), !!!install_opt)
  )
  pkg_str <- deparse(rlang::quo_squash(inst_expr))
  cli::cat_line(pkg_str)

  invisible()
}


#' Compare current package versions against CRAN
#'
#' @description
#' Queries CRAN for the latest versions of the specified packages and
#' their (optionally recursive) dependencies, then compares with locally
#' installed versions.
#'
#' @param x Character scalar. Package name to check. Defaults to
#'   `"metapip"`.
#' @param recursive Logical. If `TRUE`, includes all transitive
#'   dependencies. Default `FALSE`.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{package}{Character. Package name.}
#'     \item{cran}{Character. CRAN version.}
#'     \item{local}{Character. Locally installed version.}
#'     \item{behind}{Logical. `TRUE` if the CRAN version is newer.}
#'   }
#'   Base R packages are excluded.
#'
#' @details
#' When `"metapip"` is in `x` and is not yet on CRAN, a hardcoded set
#' of dependencies is returned as fallback.
#'
#' @seealso
#' [metapip_update()]
#'
#' @examples
#' \dontrun{
#' pkg_deps()
#' pkg_deps("metapip", recursive = TRUE)
#' }
#'
#' @export
pkg_deps <- function(x = "metapip", recursive = FALSE) {
  pkgs <- utils::available.packages()
  deps <- tools::package_dependencies(x, pkgs, recursive = recursive)

  # NULL before package is on CRAN
  if ("metapip" %in% x && is.null(deps$metapip)) {
    deps$metapip <- c("pipapi", "pipaux", "pipload", "wbpip",
                       "pipfun", "pipdata", "pipr", "cli",
                       "rstudioapi")
  }

  pkg_deps <- unique(sort(c(names(deps), unlist(deps))))
  pkg_deps <- pkg_deps[pkg_deps %in% pkgs]

  base_pkgs <- c("base", "compiler", "datasets", "graphics",
                 "grDevices", "grid", "methods", "parallel",
                 "splines", "stats", "stats4", "tools",
                 "tcltk", "utils")
  pkg_deps <- setdiff(pkg_deps, base_pkgs)

  cran_version <- lapply(pkgs[pkg_deps, "Version"],
                         base::package_version)
  local_version <- lapply(pkg_deps, utils::packageVersion)
  behind <- mapply(`>`, cran_version, local_version)

  data.frame(
    package = pkg_deps,
    cran = cran_version |> sapply(as.character),
    local = local_version |> sapply(as.character),
    behind = behind
  )
}