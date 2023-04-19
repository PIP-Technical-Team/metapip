.onAttach <- function(...) {
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  metapip_attach()

  # if (!"package:conflicted" %in% search()) {
  #   x <- metapip_conflicts()
  #   msg(metapip_conflict_message(x), startup = TRUE)
  # }
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
