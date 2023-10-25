#' Return meta information about core packages
#'
#' @param package One (or more) core packages, if NULL shows information about all of them.
#' @return a knitr::kable output with details about the core packages
#' @export
#'
core_metadata <- function(package = NULL) {
  if(is.null(package)) {
    package <- core
  } else {
    is_core(package)
  }
  branches <- lapply(package, get_branches, display = FALSE)
  no_of_branches <- lengths(branches)
  out <- data.frame(package, no_of_branches)
  knitr::kable(out)
}
