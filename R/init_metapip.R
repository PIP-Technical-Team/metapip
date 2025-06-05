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
  default_branch <- sapply(core, get_default_branch)

  logical_vec <- mapply(compare_sha, core, default_branch)
  missing_pkgs <- core[!logical_vec]
  if (length(missing_pkgs) > 0) {
    cli::cli_alert_warning(
      "The following packages do not have the updated version of default branch installed: {cli::qty(length(missing_pkgs))}{.pkg {missing_pkgs}}"
    )

    answer <- utils::menu(
      choices = c("Yes", "No"),
      title = "Do you want to install them now?"
    )

    if (answer == 1) {
      cli::cli_alert_info("Installing missing packages...")
      Map(install_branch, missing_pkgs, default_branch[!logical_vec])
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
  # Get locally installed sha
  desc <- utils::packageDescription(package) |> suppressWarnings()
  # If the package is not installed return FALSE
  if (length(desc) == 1 && is.na(desc)) return(FALSE)
  local_sha <- desc$RemoteSha

  # Get GH sha
  out <- latest_commit_for_branch(package, branch)
  gh_sha <- out$sha

  local_sha == gh_sha

}
