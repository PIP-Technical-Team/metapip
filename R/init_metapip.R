#' Initializes the pip core packages
#' @description
#' Based on options() settings provides an option to download latest package versions from the branch
#'
#' @returns invisible() output
#' @export
#'
init_metapip <- function() {
  # Based on options settings check if the latest version of that branch is installed for every pip core package
  # If there is an updated commit, give an option to install the latest version of those branches
  default_branch <- get_current_branches() |>
    unlist()

  pkgs_vec <- mapply(compare_sha, core, default_branch[core])

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

    answer <- utils::menu(
      choices = c("Yes", "No"),
      title = "Do you want to install them now?"
    )

    if (answer == 1) {
      cli::cli_alert_info("Installing missing packages...")
      Map(install_branch, missing_pkgs, default_branch[missing_pkgs])
    } else {
      cli::cli_alert_danger("Skipping installation.")
    }
  } else {
    cli::cli_inform("All packages are up-to-date")
  }
  # Finally load all the packages once it is installed.
  metapip_attach()
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

#' Set default custom branching options
#'
#' @param ... Named elements to be added or updated in the custom default list.
#'
#' @returns invisible NULL
#' @export
#'
#' @examples {
#' set_custom_default_branch(pipr = 'main', 'pipapi' = 'DEV_v3')
#' }
#'
set_custom_default_branch <- \(...) {
  new_entries <- list(...)
  names(new_entries) <- paste0(names(new_entries), '_branch')
  existing_options <- getOption("metapip.custom_default_branch", list())
  merged <- modifyList(existing_options, new_entries)
  options("metapip.custom_default_branch" = merged)
}
