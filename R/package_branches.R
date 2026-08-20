#' Compare package versions across branches and local installations
#'
#' @description
#' For each requested core PIP package, retrieves the DESCRIPTION version
#' from every branch on GitHub and compares it with the locally installed
#' version. Returns a list of tables showing version information, common
#' branch versions, and a local status indicator (ahead/behind/up-to-date
#' relative to a comparison branch).
#'
#' @param package Character vector. One or more core PIP package names.
#'   If `NULL` (default), all core packages are included.
#' @param branch_to_compare Character scalar. Branch to use as the
#'   comparison baseline for the local status column. Defaults to
#'   `getOption("metapip.default_branch")`.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{common}{`data.frame` pivoted wide with columns for each of
#'       the common branches (`PROD`, `DEV_v2`, `QA`) and the package
#'       version on that branch.}
#'     \item{local}{`data.frame` with columns: `package`, `local_branch`,
#'       `local_version`, and `local_status` (one of `"behind <branch>"`,
#'       `"ahead of <branch>"`, `"up-to-date"`, `"Not in local"`,
#'       `"<branch> not in repo"`, `"<branch> version unknown"`).}
#'     \item{*}{One additional `data.frame` per package listing its
#'       non-common branches and their versions.}
#'   }
#'
#' @section Progress:
#' Shows a progress bar while fetching DESCRIPTION files from GitHub for
#' every branch of every package.
#'
#' @seealso
#' [core_metadata()], [get_branches()]
#'
#' @examples
#' \dontrun{
#' package_branches()
#' package_branches(c("pipapi", "wbpip"))
#' package_branches(branch_to_compare = "QA")
#' }
#'
#' @export
package_branches <- function(
    package = NULL,
    branch_to_compare = getOption("metapip.default_branch")
) {
  if (!is.null(package)) {
    is_core(package)
  } else {
    package <- core
  }
  all_package_version <- get_package_version(package)
  complete_data <- get_complete_data(all_package_version)
  common <- common_data(complete_data)
  result <- split_packages_into_list(complete_data)

  local <- lapply(cli::cli_progress_along(package), \(.x) {
    out <- tryCatch(
      expr = {
        utils::packageDescription(
          package[.x],
          fields = c("GithubRef", "Version")
        )
      },
      error = function(e) {
        list(GithubRef = NA_character_, Version = NA_character_)
      },
      warning = function(w) {
        list(GithubRef = NA_character_, Version = NA_character_)
      }
    )
    data.frame(
      package = package[.x],
      local_branch = out$GithubRef,
      local_version = out$Version
    )
  }) |>
    rowbind()

  dev <- complete_data |>
    fsubset(branch %in% branch_to_compare)
  local <- join_and_get_status(local, dev, branch_to_compare)

  return(c(list(common = common, local = local), result))
}


#' Fetch DESCRIPTION version for a GitHub raw URL (internal)
#'
#' @param u Character. URL to a DESCRIPTION file on GitHub.
#'
#' @return Character scalar: the version string, or `NA_character_` on
#'   error or non-200 response.
#'
#' @keywords internal
get_version_for_url <- function(u) {
  tryCatch(
    {
      resp <- httr2::request(u) |>
        httr2::req_timeout(seconds = 10) |>
        httr2::req_perform()
      if (httr2::resp_status(resp) != 200L) {
        return(NA_character_)
      }
      tc <- suppressWarnings(
        textConnection(httr2::resp_body_string(resp))
      )
      on.exit(close(tc), add = TRUE)
      mat <- suppressWarnings(read.dcf(tc))
      if (!"Version" %in% colnames(mat)) {
        return(NA_character_)
      }
      unname(mat[, "Version"])
    },
    error = function(e) NA_character_
  )
}


#' Get DESCRIPTION versions for all branches of each package (internal)
#'
#' @param package Character vector of core PIP package names.
#'
#' @return Named list where each element corresponds to a package and
#'   contains a named character vector of branch -> version mappings.
#'
#' @keywords internal
get_package_version <- function(package) {
  lp <- length(package)
  cli::cli_progress_bar(
    "Getting versions for all branches of",
    total = lp,
    format = "{cli::col_green(cli::symbol$play)} {cli::pb_name}{.pkg {x}}"
  )
  lr <- vector("list", length = lp)
  names(lr) <- package
  for (x in package) {
    cli::cli_progress_update()
    br <- get_branches(x, display = FALSE)
    br <- br[br != "gh-pages"]
    urls <- glue::glue("https://raw.githubusercontent.com/PIP-Technical-Team/{x}/{br}/DESCRIPTION")
    versions <- vapply(urls, get_version_for_url, character(1L))
    names(versions) <- br
    lr[[x]] <- versions
  }
  lr
}


#' Flatten per-package version lists into a single data.frame (internal)
#'
#' @param all_package_version Named list from [get_package_version()].
#'
#' @return `data.frame` with columns: `package`, `branch`, `version`.
#'
#' @keywords internal
get_complete_data <- function(all_package_version) {
  branch <- unlist(lapply(all_package_version, names))
  package <- rep(
    names(all_package_version), lengths(all_package_version)
  )
  version <- unlist(all_package_version, use.names = FALSE)
  data.frame(package = package, branch = branch, version = version)
}


#' Pivot common branches to wide format (internal)
#'
#' @param complete_data `data.frame` from [get_complete_data()].
#'
#' @return Wide `data.frame` with one row per package and columns for
#'   `PROD`, `DEV_v2`, `QA` branch versions.
#'
#' @keywords internal
common_data <- function(complete_data) {
  complete_data |>
    fsubset(branch %in% c("PROD", "DEV_v2", "QA")) |>
    pivot(names = "branch", values = "version", how = "wider") |>
    colorder(package, PROD)
}


#' Split non-common branches into per-package list (internal)
#'
#' @param complete_data `data.frame` from [get_complete_data()].
#'
#' @return Named list of `data.frame`s, one per package, each with
#'   columns `branch` and `version`.
#'
#' @keywords internal
split_packages_into_list <- function(complete_data) {
  complete_data |>
    fsubset(!branch %in% c("PROD", "DEV_v2", "QA")) |>
    split(~package) |>
    lapply(\(x) x |> fselect(-package))
}


#' Join local install info with remote and compute status (internal)
#'
#' @param local `data.frame` with columns `package`, `local_branch`,
#'   `local_version`.
#' @param dev `data.frame` with columns `package`, `branch`, `version`
#'   for the comparison branch.
#' @param branch_to_compare Character scalar. Name of the comparison
#'   branch (for labelling).
#'
#' @return `data.frame` with columns: `package`, `local_branch`,
#'   `local_version`, `local_status`.
#'
#' @keywords internal
join_and_get_status <- function(local, dev, branch_to_compare) {
  # Remote DESCRIPTION Version fields are untrusted: an empty or malformed
  # value must degrade to a defined status instead of crashing the whole
  # report (compareVersion errors on non-parseable input).
  safe_cmp <- function(a, b) {
    if (is.na(a) || is.na(b)) return(0L)
    if (!nzchar(trimws(a)) || !nzchar(trimws(b))) return(NA_integer_)
    tryCatch(
      utils::compareVersion(trimws(a), trimws(b)),
      error = function(e) NA_integer_
    )
  }
  join(local, dev, "package", how = "full") |>
    fmutate(
      cmp = mapply(
        function(a, b) safe_cmp(a, b),
        local_version, version,
        SIMPLIFY = TRUE
      )
    ) |>
    fmutate(local_status = fcase(
      is.na(local_version), "Not in local",
      is.na(branch), paste(branch_to_compare, "not in repo"),
      is.na(version), paste(branch_to_compare, "version unknown"),
      is.na(cmp), "unknown",
      cmp < 0, paste("behind", branch_to_compare),
      cmp > 0, paste("ahead of", branch_to_compare),
      default = "up-to-date"
    )) |>
    fselect(-branch, -version, -cmp)
}
