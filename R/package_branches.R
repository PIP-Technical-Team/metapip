#' Status tables of package versions in different branches along with local
#' installations. For local installation,a status column is returned which
#' indicates if the local version is ahead or behind the PROD branch.
#'
#' @param package One (or more) of the PIP core packages. Default NULL will
#'   include all the packages
#' @param branch_to_compare character: names of branch to compare to. Default is
#'   "PROD".
#'
#' @return table of pip packages and the corresponding package versions of
#'   branch
#' @examples
#' \dontrun{
#' package_branches()
#' package_branches(c("pipapi", "wbpip"))
#' package_branches(branch_to_compare = "QA")
#' }
#'
#' @export
#'
package_branches <- function(
  package = NULL,
  branch_to_compare = getOption("metapip.default_branch")
) {
  check_github_token()
  if (!is.null(package)) {
    is_core(package)
  } else {
    package <- core
  }
  # For each of the core packages, get all the branches
  # From every branch, get the version of the package
  all_package_version <- get_package_version(package)
  complete_data <- get_complete_data(all_package_version)
  common <- common_data(complete_data)
  result <- split_packages_into_list(complete_data)
  # Get local installation
  local <- lapply(cli::cli_progress_along(package), \(.x) {
    out <- tryCatch(
      expr = {
        utils::packageDescription(package[.x], fields = c("GithubRef", "Version"))
      }, # end of expr section

      error = function(e) {
        list(GithubRef = NA_character_, Version = NA_character_)
      }, # end of error section

      warning = function(w) {
        list(GithubRef = NA_character_, Version = NA_character_)
      }
    ) # End of trycatch

    data.frame(
      package = package[.x],
      local_branch = out$GithubRef,
      local_version = out$Version
    )
  }) |>
    rowbind()

  # PROD data
  dev <- complete_data |>
    fsubset(branch %in% branch_to_compare)
  local <- join_and_get_status(local, dev, branch_to_compare)

  return(c(list(common = common, local = local), result))
}

#' @noRd
get_package_version <- function(package) {
  lp <- length(package)
  cli::cli_progress_bar("Getting versions for all branches of",
                        total = lp,
                        format = "{cli::col_green(cli::symbol$play)} {cli::pb_name}{.pkg {x}}")
  lr <- vector("list", length = lp)
  names(lr) <- package
  for (x in package) {
    cli::cli_progress_update()
    br <- get_branches(x, display = FALSE)
    br <- br[br != "gh-pages"]
    urls <- glue::glue("https://raw.githubusercontent.com/PIP-Technical-Team/{x}/{br}/DESCRIPTION")
    versions <- sapply(urls, \(y) {
      mat <- read.dcf(url(y))
      mat[, "Version"]
    })
    names(versions) <- br
    lr[[x]] <- versions
  }
  lr
}


#' @noRd
get_complete_data <- function(all_package_version) {
  branch  <- unlist(lapply(all_package_version, names))
  package <- rep(names(all_package_version), lengths(all_package_version))
  version <- unlist(all_package_version, use.names = FALSE)
  data.frame(package = package, branch = branch, version = version)
}

#' @noRd
common_data <- function(complete_data) {
  complete_data |>
    fsubset(branch %in% c("PROD", "DEV_v2", "QA")) |>
    pivot(names = "branch", values = "version", how = "wider") |>
    colorder(package, PROD)
}

#' @noRd
split_packages_into_list <- function(complete_data) {
  complete_data |>
    fsubset(!branch %in% c("PROD", "DEV_v2", "QA")) |>
    split(~package) |>
    lapply(\(x) x |> fselect(-package))
}

#' @noRd
join_and_get_status <- function(local, dev, branch_to_compare) {
  # Join dev data with local data to create status column
  join(local, dev, "package", how = "full") |>
    fmutate(local_status = fcase(
      local_status == 1, paste("behind", branch_to_compare),
      local_status == -1, paste("ahead", branch_to_compare),
      is.na(local_version), "Not in local",
      is.na(branch), paste(branch_to_compare, "not in repo"),
      default = "up-to-date"
    )) |>
    fselect(-branch, -version)
}


