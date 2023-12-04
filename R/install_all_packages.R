#' Install one (or more) pip core packages from a specific branch.
#' This includes packages like pipapi, pipaux, pipload, wbpip, pipfun, pipdata and pipr
#'
#' @param package one (or more) of the core package name, if NULL all the core packages are installed from the branch
#' @param branch valid branch name (default "PROD")
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' install_all_packages(branch = "test")
#' }
#'
#' @export
#'
install_all_packages <- function(package = NULL, branch = "PROD") {
  check_github_token()
  if(is.null(package)) {
    package = core
  } else {
    is_core(package)
  }
  lapply(package, install_branch, branch)
  return(invisible(NULL))
}
