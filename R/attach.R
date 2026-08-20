# Internal vector of all core PIP ecosystem package names.
# Dropping pipr -- it is not available under PIP-Technical-Team.
core <- c("pipapi", "pipload", "wbpip", "pipfun", "pipdata",
           "pipster", "pipaux", "pipfaker")

pkg_loaded <- function(pkg = NULL) {
  if (is.null(pkg)) {
    pkg <- core
  }
  search <- paste0("package:", pkg)
  pkg[search %in% search()]
}

pkg_unloaded <- function(pkg = NULL) {
  if (is.null(pkg)) {
    pkg <- core
  }
  search <- paste0("package:", pkg)
  pkg[!search %in% search()]
}

metapip_attach <- function(pkg = NULL) {
  to_load <- pkg_unloaded(pkg = pkg)
  if (length(to_load) == 0) {
    return(invisible())
  }

  msg(
    cli::rule(
      left = cli::style_bold("Attaching packages"),
      right = paste0("metapip ", package_version("metapip"))
    ),
    startup = TRUE
  )
  # Check if the requested core packages are installed; if not, warn and skip
  # them, but never expand the attach set beyond what was requested.
  # O(1) per-package namespace checks instead of a full library scan
  not_installed_core_packages <-
    to_load[!vapply(to_load, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(not_installed_core_packages) > 0L) {
    to_load <- setdiff(to_load, not_installed_core_packages)

    if (length(to_load) == 0) {
      return(invisible())
    }

    to_install <- paste0("c(",
                         shQuote(not_installed_core_packages) |>
                           paste(collapse = ", ") ,")")
    cli::cli_warn(c(
      "Package{?s} {not_installed_core_packages} {?is/are} not installed.",
      "i" = "you may try {.run metapip::install_pip_packages({to_install})}"
    ))
  }

  versions <- vapply(to_load, package_version, character(1L))
  branch_name <- vapply(to_load,
                        \(x) {
                          y <- utils::packageDescription(x, fields = "GithubRef")
                          if (is.na(y)) {
                            y <- "local"
                          }
                          y
                        },
                        character(1L))

  clean_versions <- gsub(cli::ansi_regex(), "", versions, perl = TRUE)
  packages <- paste0(
    cli::col_green(cli::symbol$tick), " ",
    cli::col_blue(format(to_load)), " ",
    cli::ansi_align(versions, max(nchar(clean_versions))), " ",
    cli::col_blue("(", branch_name, ")")
  )

  msg(paste(packages, collapse = "\n"), startup = TRUE)

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  invisible()
}

package_version <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- cli::col_red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}
