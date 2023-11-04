#' Install latest branch from core packages
#'
#' @param package one (or more) of core packages. default NULL would install latest branch for all packages
#'
#' @return NULL
#' @examples
#' \dontrun{
#'   install_latest_branch()
#'   install_latest_branch(c("pipfun", "pipapi"))
#'}
#' @export
#'
install_latest_branch <- function(package = NULL) {
  check_github_token()
  if(!is.null(package)) is_core(package)
  else package <- core
  dat <- purrr::map_df(package, get_latest_branch_update)
  Map(\(x, y) install_branch(x, y), dat$package, dat$branch_name)
  NULL
}
