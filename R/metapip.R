#' metapip: PIP packages manager
#'
#' The goal of metapip is to provide the user with a set of functions that
#' allow him/her to work efficiently with all the PIP R packages. The technical
#' world of PIP consist of several packages that interact to each to load,
#' format, modify, and estimate data for the PIP system. Given that the number
#' of packages has increased over time, it was necessary to create a meta
#' package whose only objective is the proper management of all the other PIP
#' packages.
#'
#' @section metapip functions: The metapip functions ...
#'
#' @docType package
#' @name metapip
#' @importFrom glue glue


# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":="
    ),
    package = utils::packageName()
  )
}

# Suppress R CMD check note
# Namespace in Imports field not imported from: PKG
#   All declared Imports should be used.
ignore_unused_imports <- function() {
  pipapi::pip
  #pipaux::load_aux
  pipload::pip_load_aux
  pipdata::dlw_clean
  pipfun::load_from_gh
  pipr::get_aux
  wbpip::get_bins
}


NULL
