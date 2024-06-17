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
