# Session-scoped memoization cache -----------------------------------------
#
# The GitHub API functions in this package make many sequential calls with no
# caching (core_metadata() fires ~80 calls). This package-level environment
# provides per-session memoization: results are reused within an R session and
# cleared on package reload or restart.
#
# The cache is created in .onLoad(). It is intentionally not locked at the
# binding level so entries can be added and removed at runtime.

.metapip_cache <- NULL

cache_get <- function(key) {
  if (is.null(.metapip_cache)) {
    return(NULL)
  }
  get0(key, envir = .metapip_cache, inherits = FALSE)
}

cache_set <- function(key, value) {
  if (is.null(.metapip_cache)) {
    .metapip_cache <<- new.env(parent = emptyenv())
  }
  assign(key, value, envir = .metapip_cache)
  invisible(value)
}

cache_clear <- function(pattern) {
  if (is.null(.metapip_cache)) {
    return(invisible())
  }
  keys <- ls(envir = .metapip_cache, all.names = TRUE)
  to_remove <- keys[grepl(pattern, keys)]
  if (length(to_remove) > 0) {
    rm(list = to_remove, envir = .metapip_cache)
  }
  invisible()
}

# Invalidate every memoized API result for a single package (branch list,
# commit SHAs, and release data) after a side-effecting install.
cache_invalidate <- function(package) {
  cache_clear(paste0("(^|:)", package, "(:|$)"))
}
