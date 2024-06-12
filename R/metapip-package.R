#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @name metapip
#' @rawNamespace import(collapse, except = fdroplevels)
# @rawNamespace import(data.table, except = fdroplevels)
## usethis namespace: end
##

# Prevent R CMD check from complaining about the use of pipe expressions
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":=",
      "branch",
      "ind",
      "local_status",
      "values"
    ),
    package = utils::packageName()
  )
}

NULL
