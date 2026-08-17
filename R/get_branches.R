#' Get available branches for a package
#'
#' @description
#' Queries the GitHub API for all branches of a PIP package in the
#' `PIP-Technical-Team` organisation. Paginates automatically so repos with
#' more than 30 branches are fully returned.
#'
#' @param package Character. Single core PIP package name. Defaults to
#'   `"pipapi"`.
#' @param display Logical. If `TRUE` (default), branch names are printed to
#'   the console with bullet markers. Set to `FALSE` for silent use in
#'   scripts.
#'
#' @return Character vector of branch names (returned invisibly). Always
#'   returns the full set regardless of the `display` setting.
#'
#' @section Side effects:
#' When `display = TRUE`, prints a header and bullet list to the console
#' via [cli::cli_h3()] and [cli::cat_bullet()].
#'
#' @section Authentication:
#' Read-only function: works without a GitHub PAT against the public
#' `PIP-Technical-Team` org. Supplying a PAT via [gh_token()] increases
#' the API rate limit from 60 to 5000 requests/hour.
#'
#' @seealso
#' [get_branch_info()], [get_latest_branch_update()],
#' [install_branch()]
#'
#' @examples
#' \dontrun{
#' # Interactive use: displays branches in console
#' branches <- get_branches("wbpip")
#'
#' # Silent use: suppresses display
#' branches <- get_branches("wbpip", display = FALSE)
#' }
#'
#' @export
get_branches <- function(package = "pipapi", display = TRUE) {
  check_package_condition(package)
  out <- gh::gh(
    "GET /repos/PIP-Technical-Team/{repo}/branches",
    repo = package,
    .token = gh_token(),
    .limit = Inf
  )
  branches <- vapply(out, `[[`, "", "name")
  if (isTRUE(display)) {
    cli::cli_h3("These are available branches for {package} package: ")
    cli::cat_bullet(glue::glue("{branches}"))
  }
  return(invisible(branches))
}


#' Get last commit metadata for specific branches
#'
#' @description
#' For each requested branch of a PIP package, returns a data.frame with the
#' package name, branch name, last commit author name, and timestamp of the
#' last commit. Results are displayed in a colourised table via
#' [colorDF::colorDF()].
#'
#' @param package Character. Single core PIP package name. Defaults to
#'   `"pipapi"`.
#' @param branch Character vector. Branch name(s) to inspect. When `NULL`
#'   (default), uses the package's current branch assignment (via
#'   [get_package_current_branch()]).
#' @param display Logical. If `TRUE` (default), prints a colourised table
#'   to the console.
#'
#' @return A `data.frame` (returned invisibly) with columns:
#'   \describe{
#'     \item{package}{Character. Package name.}
#'     \item{branch_name}{Character. Branch name.}
#'     \item{last_commit_author_name}{Character. Name of the last committer.}
#'     \item{last_update_time}{Character. ISO 8601 timestamp of the last
#'       commit (e.g., `"2023-09-04T13:06:47Z"`).}
#'   }
#'
#' @section Errors:
#' Aborts if any requested branch name is not found in the package's branch
#' list.
#'
#' @seealso
#' [get_branches()], [get_latest_branch_update()],
#' [latest_commit_for_branch()]
#'
#' @examples
#' \dontrun{
#' # Single branch
#' get_branch_info(package = "pipr", branch = "DEV")
#'
#' # Multiple branches
#' get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
#'
#' # Use current branch (default)
#' get_branch_info(package = "wbpip")
#' }
#'
#' @export
get_branch_info <- function(package = "pipapi", branch = NULL,
                             display = TRUE) {
  is_core(package)
  check_package_condition(package)

  branches <- get_branches(package, display = FALSE)
  if (!is.null(branch)) {
    if (!all(branch %in% branches)) {
      cli::cli_abort(
        "{branch} is not a correct branch name. Please use one of
        {toString(branches)}."
      )
    }
    branches <- branch
  } else {
    branches <- get_package_current_branch(package)
  }
  out <- lapply(cli::cli_progress_along(branches), \(x) {
    dat <- latest_commit_for_branch(package, branches[x])
    data.frame(dat$commit$author)
  }) |> rowbind()

  res <- add_vars(out, package = rep(package, nrow(out)),
                  branch_name = branches, pos = "front") |>
    frename(last_update_time = "date",
            last_commit_author_name = "name") |>
    fselect(-email)

  if (isTRUE(display)) print(colorDF::colorDF(res))
  return(invisible(res))
}


#' Get details of the most recently updated branch
#'
#' @description
#' Inspects all branches of a core PIP package (excluding `gh-pages`) and
#' returns the one with the most recent commit. Useful for identifying which
#' branch has the latest development activity.
#'
#' If `package` has only `gh-pages` branches, a warning is issued and a
#' single-row data.frame of `NA` values is returned.
#'
#' @param package Character. Single core PIP package name. Defaults to
#'   `"pipapi"`.
#' @param display Logical. If `TRUE` (default), prints a colourised table
#'   to the console.
#'
#' @return A single-row `data.frame` (returned invisibly) with columns:
#'   \describe{
#'     \item{package}{Character. Package name.}
#'     \item{branch_name}{Character. Name of the most recently updated branch,
#'       or `NA` when only `gh-pages` exists.}
#'     \item{last_commit_author_name}{Character. Author of the latest commit.}
#'     \item{last_update_time}{POSIXct (UTC). Timestamp of the latest commit,
#'       or `NA` when only `gh-pages` exists.}
#'   }
#'
#' @section Edge cases:
#' - Packages with only `gh-pages` branches: returns `NA` values with a
#'   warning.
#' - Packages whose branches all fail to resolve (network errors): returns
#'   `NA` values without aborting.
#'
#' @seealso
#' [get_branch_info()], [get_branches()]
#'
#' @examples
#' \dontrun{
#' get_latest_branch_update()
#' get_latest_branch_update("wbpip", display = FALSE)
#' }
#'
#' @export
get_latest_branch_update <- function(package = "pipapi", display = TRUE) {
  is_core(package)
  check_package_condition(package)
  out <- get_branch_info(package, display = FALSE)
  res <- out |>
    fsubset(branch_name != "gh-pages")
  if (nrow(res) == 0L) {
    cli::cli_warn("No non-gh-pages branches found for {.pkg {package}}")
    return(invisible(data.frame(
      package = package,
      branch_name = NA_character_,
      last_commit_author_name = NA_character_,
      last_update_time = as.POSIXct(NA, tz = "UTC")
    )))
  }
  res <- res |>
    fmutate(last_update_time = as.POSIXct(
      last_update_time,
      format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
    )) |>
    roworder(-last_update_time) |>
    ss(1L)

  if (isTRUE(display)) print(colorDF::colorDF(res))
  return(invisible(res))
}


#' Get or set the global default branch
#'
#' @description
#' `get_default_branch()` returns the value of the `metapip.default_branch`
#' option, which controls the default branch for all core PIP packages.
#' `set_default_branch()` modifies it.
#'
#' @details
#' The global default branch is the fallback used by all functions that
#' accept a `branch` argument when no explicit branch is provided. Per-package
#' overrides (set via [set_custom_branch()]) take precedence over this
#' global default.
#'
#' @param branch Character. Name of the branch to set as the global default.
#'
#' @return `get_default_branch()`: character string of the current default
#'   branch.
#' @return `set_default_branch()`: the new default branch value (returned
#'   invisibly).
#'
#' @family branch configuration
#'
#' @examples
#' # View current default
#' get_default_branch()
#'
#' \dontrun{
#' # Change global default (persists for session)
#' set_default_branch("DEV")
#' }
#'
#' @rdname get_default_branch
#' @export
get_default_branch <- function() {
  getOption("metapip.default_branch")
}

#' @rdname get_default_branch
#' @export
set_default_branch <- function(branch) {
  options("metapip.default_branch" = branch)
  getOption("metapip.default_branch")
}


#' View current branch assignments for all (or selected) packages
#'
#' @description
#' Returns the branch each core PIP package is *configured* to use, not
#' necessarily the branch currently installed. Use [init_metapip()] to
#' detect and close the gap between configured and installed branches.
#'
#' Branch resolution order:
#' 1. Custom per-package overrides (from [set_custom_branch()]).
#' 2. Global default (from [get_default_branch()]).
#'
#' @param package Character vector. Restricts output to the specified
#'   packages. Defaults to `NULL` (all packages).
#' @param verbose Logical. If `TRUE` (default), prints the result via the
#'   [print.metapip_simplelist()] method and returns it visibly. If `FALSE`,
#'   returns a named list invisibly.
#'
#' @return A named list of class `"metapip_simplelist"` where names are
#'   package names and values are the configured branch (character scalar).
#'   When `verbose = FALSE`, returned invisibly.
#'
#' @section Errors:
#' Aborts if any `package` is not in the set of core packages.
#'
#' @family branch configuration
#'
#' @seealso
#' [get_package_current_branch()], [set_custom_branch()],
#' [init_metapip()]
#'
#' @examples
#' # Print all current branch assignments
#' get_current_branches()
#'
#' # Silent retrieval for scripting
#' branches <- get_current_branches(verbose = FALSE)
#'
#' \dontrun{
#' # Restrict to specific packages
#' get_current_branches(package = c("pipdata", "pipfaker"))
#' }
#'
#' @export
get_current_branches <- function(package = NULL, verbose = TRUE) {
  custom_branches <- getOption("metapip.custom_branch")
  names(custom_branches) <- gsub("_branch", "", names(custom_branches))

  default_branches <- getOption("metapip.default_branch") |>
    list() |>
    rep(length(core)) |>
    stats::setNames(core)

  if (length(custom_branches) > 0) {
    default_branches <- utils::modifyList(default_branches,
                                          custom_branches)
  }

  if (!is.null(package)) {
    default_branches <- default_branches[
      names(default_branches) %in% package]
    if (length(default_branches) == 0) {
      cli::cli_abort(c(
        x = "package{?s} {.pkg {package}} {?is/are} not available",
        i = "Available package{?s} {?is/are} {.pkg {core}}"
      ))
    }
  }

  attr(default_branches, "title") <-
    "{.pkg metapip} current branches (default in {cli::col_red('red')}):"
  attr(default_branches, "to_red") <-
    getOption("metapip.default_branch")
  class(default_branches) <- "metapip_simplelist"
  if (verbose) return(default_branches)
  invisible(default_branches)
}


#' Get the current branch for a single package
#'
#' @description
#' Convenience wrapper around [get_current_branches()] that returns the
#' branch name(s) as a plain character vector (not a `metapip_simplelist`).
#'
#' @param package Character vector. One or more core PIP package names.
#'
#' @return Named character vector. Names are package names, values are
#'   branch names.
#'
#' @family branch configuration
#'
#' @examples
#' get_package_current_branch("pipdata")
#' get_package_current_branch(c("pipdata", "pipfaker"))
#'
#' @rdname get_current_branches
#' @export
get_package_current_branch <- function(package) {
  get_current_branches(package = package,
                       verbose = FALSE) |>
    unlist()
}


#' Set or query custom per-package branch assignments
#'
#' @description
#' `set_custom_branch()` overrides the global default branch for specific
#' packages, persisting the change as the `metapip.custom_branch` R option.
#' `get_custom_branch()` retrieves the current custom branch assignments.
#'
#' @details
#' Custom branches take precedence over the global default (from
#' [get_default_branch()]) in all functions that resolve a package's branch.
#' The convention is `{package}_branch` in the options list, but you pass
#' package names directly to `set_custom_branch()`.
#'
#' @param ... For `set_custom_branch()`: named character scalars where
#'   each name is a core PIP package and the value is the branch to use.
#'   For `get_custom_branch()`: not used.
#' @param package Character vector. For `get_custom_branch()`: filter to
#'   specific packages. Defaults to `NULL` (all custom branches).
#'
#' @return `set_custom_branch()`: the updated custom branch list (returned
#'   invisibly).
#' @return `get_custom_branch()`: a named list of class
#'   `"metapip_simplelist"` with custom branch assignments, printed to the
#'   console.
#'
#' @section Errors:
#' `get_custom_branch()` aborts if the requested packages have no custom
#' branch assignments.
#'
#' @family branch configuration
#'
#' @seealso
#' [get_default_branch()], [get_current_branches()]
#'
#' @examples
#' \dontrun{
#' # Set per-package overrides
#' set_custom_branch(pipr = "main", pipapi = "DEV_v3")
#'
#' # View all custom branches
#' get_custom_branch()
#' }
#'
#' @rdname set_custom_branch
#' @export
set_custom_branch <- function(...) {
  new_entries <- list(...)
  if (length(new_entries) == 0) {
    cli::cli_alert_danger("no changes made to custom branches")
    return(get_custom_branch())
  }
  names(new_entries) <- paste0(names(new_entries), "_branch")
  existing_options <- getOption("metapip.custom_branch", list())
  merged <- utils::modifyList(existing_options, new_entries)
  options("metapip.custom_branch" = merged)

  get_custom_branch()
}


#' @rdname set_custom_branch
#' @export
get_custom_branch <- function(package = NULL) {
  existing_branches <- getOption("metapip.custom_branch", list())
  names(existing_branches) <- gsub("_branch", "", names(existing_branches))
  neb <- names(existing_branches)

  if (!is.null(package)) {
    existing_branches <- existing_branches[
      names(existing_branches) %in% package]
  }

  if (length(existing_branches) == 0) {
    cli::cli_abort(c(
      x = "package{?e/s} {.field {package}} {?is/are} not available.",
      i = "package{?s} {?is/are} available: {.emph {neb}}"
    ))
  }

  attr(existing_branches, "title") <-
    "{.pkg metapip} custom branches:"
  class(existing_branches) <- "metapip_simplelist"
  print(existing_branches)
}