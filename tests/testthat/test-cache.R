test_that("cache miss returns NULL", {
  # Clear any ambient state so the test is order-independent
  cache_clear(".*")
  expect_null(cache_get("x"))
  cache_clear(".*")
})

test_that("cache set then get returns stored value", {
  cache_clear(".*")
  cache_set("x", 1)
  expect_equal(cache_get("x"), 1)
  cache_clear(".*")
})

test_that("cache survives multiple calls in same session", {
  cache_clear(".*")
  cache_set("key", "value")
  expect_equal(cache_get("key"), "value")
  expect_equal(cache_get("key"), "value")
  cache_clear(".*")
})

test_that("cache_clear removes keys matching a pattern", {
  cache_clear(".*")
  cache_set("branches:pipapi", list(a = 1))
  cache_set("commit:pipapi:PROD", list(b = 2))
  cache_set("other", list(c = 3))

  cache_clear("^branches:")
  expect_null(cache_get("branches:pipapi"))
  expect_false(is.null(cache_get("commit:pipapi:PROD")))
  expect_false(is.null(cache_get("other")))

  cache_clear(":")
  expect_null(cache_get("commit:pipapi:PROD"))
  expect_false(is.null(cache_get("other")))
  cache_clear(".*")
})

test_that("cache_clear is a no-op on empty cache", {
  cache_clear(".*")
  # Also clear the missing-key path: a pattern matching nothing on a clean
  # cache must not throw.
  expect_no_error(cache_clear("NO_MATCH_xyz"))
  expect_null(cache_get("does-not-exist"))
})

test_that("cache key isolation between packages", {
  cache_clear(".*")
  cache_set("branches:pipapi", c("a", "b"))
  cache_set("branches:wbpip", c("x", "y"))
  expect_equal(cache_get("branches:pipapi"), c("a", "b"))
  expect_equal(cache_get("branches:wbpip"), c("x", "y"))

  # Cleanup so cached values do not leak into other test files
  cache_clear("branches:")
})

test_that("cache functions guard against a NULL cache binding", {
  # Temporarily simulate the pre-.onLoad state
  ns <- asNamespace("metapip")
  original <- get(".metapip_cache", envir = ns)
  unlockBinding(".metapip_cache", ns)
  on.exit({
    assign(".metapip_cache", original, envir = ns)
    lockBinding(".metapip_cache", ns)
  }, add = TRUE)
  assign(".metapip_cache", NULL, envir = ns)

  expect_null(cache_get("any"))
  expect_silent(cache_clear("any"))

  # cache_set lazily recreates the cache environment
  cache_set("k", "v")
  expect_equal(cache_get("k"), "v")
})
