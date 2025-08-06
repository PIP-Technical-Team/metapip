#'Initializes and updates the pip core packages
#'
#'@param exclude character: packages to exclude from attaching. if [getwd] is
#'  one of the core PIP packages, that package will be excluded be default. To
#'  avoid that, set exclude to `NULL`.
#' @param ask logical. Ask the user if she wants to install outdated packages. Default TRUE
#'
#'@description Based on options() settings provides an option to download latest
#'package versions from the branch
#'
#'@returns `init_metapip()` return invisible() output
#' @examples
#' \dontrun{
#'   init_metapip()
#'}
#'
#'@export
init_metapip <- function(exclude = NA,
                         ask     = TRUE,
                         answer  = 1) {
  update_pip_packages(exclude = exclude,
                      ask = ask,
                      answer  = 1)
  # Finally load all the packages once it is installed.
  metapip_attach()
}




#' Update PIP package
#'
#' @param answer numeric: Developers  argument. Only works for demonstration
#'   purposes.
#' @returns `update_pip_packages()` return logical vector. TRUE if missing package
#'   were update. FALSE if all packages are up to date of the user selects not
#'   to update.
#' @export
#' @rdname init_metapip
#'
#' @examples
#' update_pip_packages(ask = FALSE,
#' answer = 2) # this is to make it work in examples and vignettes.
update_pip_packages <- \(exclude = NA,
                         ask = TRUE,
                         answer = 1) {
  # Based on options settings check if the latest version of that branch is
  # installed for every pip core package If there is an updated commit, give an
  # option to install the latest version of those branches

  pkgs <- get_core_pagkages(exclude = exclude)
  default_branch <- get_package_current_branch(package = pkgs)

  pkgs_vec <- mapply(compare_sha, pkgs, default_branch[pkgs])

  # get those pkgs for which branch does not exist
  null_vec    <- Filter(is.null, pkgs_vec) |>
    names()

  null_branch <- default_branch[null_vec]

  if (length(null_vec) > 0) {
    # Find the maximum width of left column for padding
    max_left_width <- max(nchar(null_vec))
    cli::cli_alert_danger("The following packages do not have the corresponding
                          branch available on GitHub:")

    for (i in seq_along(null_vec)) {
      null_vec_padded <- sprintf("%-*s", max_left_width, null_vec[i])
      cli::cli_alert("{.pkg {null_vec_padded}} -->  {.val {null_branch[i]}}")
    }
  }

  # just the ones with branches available, By unlisting we not only convert it
  # to atomic vector but remove as well the NULL values.
  logical_vec <- unlist(pkgs_vec)

  missing_pkgs <- logical_vec[logical_vec == FALSE] |>
    names()

  if (length(missing_pkgs) > 0) {
    cli::cli_alert_warning(
      "The following packages do not have the updated version of default branch
      installed: {cli::qty(length(missing_pkgs))}{.pkg {missing_pkgs}}"
    )
    answer <- 1
    if (ask) {
      answer <- utils::menu(
        choices = c("Yes", "No"),
        title = "Do you want to install them now?"
      )
    }

    if (answer == 1) {
      cli::cli_alert_info("Installing missing packages...")
      Map(install_branch, missing_pkgs, default_branch[missing_pkgs])
      return(invisible(TRUE))
    } else {
      cli::cli_alert_danger("Skipping installation.")
      return(invisible(FALSE))
    }
  }

  cli::cli_inform("All packages are up-to-date")
  return(invisible(FALSE))

}



compare_sha <- function(package, branch) {

  # Get GH sha
  out <- latest_commit_for_branch(package, branch)
  gh_sha <- out$sha

  if (is.null(gh_sha)) {
    return(NULL)
  }

  # Get locally installed sha
  local_sha <- package |>
    utils::packageDescription(fields = "RemoteSha") |>
    suppressWarnings()

  # If the package is not installed return FALSE
  if (is.na(local_sha)) return(FALSE)

  # finally, compare
  local_sha == gh_sha

}


#' Get core PIP ecosystem package
#'
#' @inheritParams init_metapip
#'
#' @returns character vector with names of PIP packages
#' @export
#'
#' @examples
#' get_core_pagkages()
#' get_core_pagkages(exclude = "pipdata")
get_core_pagkages <- \(exclude = NULL) {

  if (is.null(exclude)) return(core)

  if (is.na(exclude)) {
    current_project <- getwd() |>
      fs::path_file()
    if (current_project %in% core) {
      return(core[!(core %in% current_project)])
    } else {
      return(core)
    }
  }

  if (all(exclude %in% core)) {
    return(core[!(core %in% exclude)])
  } else {
    wrong_exclude <- exclude[!exclude %in% core]
    cli::cli_abort(c(x = "package{?s} {.pkg {wrong_exclude}} {?is/are} not part of PIP ecosystem",
                     i = "available packages are {.pkg {core}}"))
  }

}
