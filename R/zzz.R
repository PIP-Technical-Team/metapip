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

# Set default branch .onLoad
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.metapip <- list(
    metapip.default_branch = "DEV_v2"
  )
  toset <- !(names(op.metapip) %in% names(op))
  if (any(toset)) options(op.metapip[toset])
  invisible()
}

