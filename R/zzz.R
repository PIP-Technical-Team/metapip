.onAttach <- function(...) {
  # set display options
  set_colorDF()

  # Load the core packages
  needed <- core[!is_attached(core)]
  if (length(needed) > 0) {
    suppressWarnings(metapip_attach(needed))
  }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

metapip_default_options <- list(
  metapip.default_branch = "PROD",
  metapip.custom_branch = list(
    pipapi_branch = "DEV",
    pipfaker_branch = "main",
    wbpip_branch = "DEV",
    pipster_branch = "DEV"
  )
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(metapip_default_options) %in% names(op))
  if (any(toset)) options(metapip_default_options[toset])

  # Session-scoped memoization cache for GitHub API calls. Cleared on package
  # reload; restarting R is the intended cache invalidation mechanism.
  .metapip_cache <<- new.env(parent = emptyenv())

  invisible()
}
