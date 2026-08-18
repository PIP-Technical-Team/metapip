# This file intentionally left minimal.
# The package-level documentation lives in metapip-package.R.

# Prevent R CMD check from complaining about the use of pipe expressions
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      "!!",
      ":="
    ),
    package = utils::packageName()
  )
}

NULL