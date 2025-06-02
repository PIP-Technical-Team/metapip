.onAttach <- function(...) {

  # set options to display tables
  set_colorDF()

  # Load the core packages --------
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  metapip_attach()


}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

metapip_default_options <- list(
  metapip.default_branch = "DEV_v2"
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(metapip_default_options ) %in% names(op))
  if (any(toset)) options(metapip_default_options [toset])
  invisible()
}
