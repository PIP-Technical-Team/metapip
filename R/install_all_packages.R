#' Install all pip core packages from a specific branch.
#' This includes packages like pipapi, pipaux, pipload, wbpip, pipfun, pipdata and pipr
#'
#' @param package one of the core package name (default "pipapi")
#' @param branch valid branch name (default "PROD")
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' install_all_packages("test")
#' }
#'
#' @export
#'
install_all_packages <- function(package = NULL, branch = "PROD") {
  check_github_token()
  if(is.null(NULL))
  lapply(core, install_branch, branch)
  return(invisible(NULL))
}
