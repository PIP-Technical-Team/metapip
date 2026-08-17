test_that("pip_snapshot writes a PIP_LOCK.csv manifest for core packages", {
  mockery::stub(pip_snapshot, "get_core_pagkages", function(...) c("pipapi", "wbpip"))
  mockery::stub(pip_snapshot, "get_package_current_branch", function(package) {
    c(pipapi = "PROD", wbpip = "DEV")
  })
  mockery::stub(pip_snapshot, "latest_commit_for_branch", function(pkg, brn) {
    list(sha = paste0("sha-", pkg))
  })

  tf <- tempfile(fileext = ".csv")
  res <- pip_snapshot(path = tf)

  expect_true(file.exists(tf))
  expect_equal(res, tf)
  lock <- utils::read.csv(tf, stringsAsFactors = FALSE)
  expect_equal(names(lock), c("package", "branch", "sha"))
  expect_equal(nrow(lock), 2)
  expect_equal(lock$sha, c("sha-pipapi", "sha-wbpip"))
})

test_that("pip_snapshot skips packages whose SHA cannot be resolved", {
  mockery::stub(pip_snapshot, "get_core_pagkages", function(...) c("pipapi", "wbpip"))
  mockery::stub(pip_snapshot, "get_package_current_branch", function(package) {
    c(pipapi = "PROD", wbpip = "PROD")
  })
  mockery::stub(pip_snapshot, "latest_commit_for_branch", function(pkg, brn) {
    if (pkg == "wbpip") return(list(sha = NULL))
    list(sha = "abc123")
  })

  tf <- tempfile(fileext = ".csv")
  expect_message(pip_snapshot(path = tf), "Could not resolve SHA")

  lock <- utils::read.csv(tf, stringsAsFactors = FALSE)
  expect_equal(lock$package, "pipapi")
  expect_equal(nrow(lock), 1)
})

test_that("pip_snapshot propagates write errors", {
  mockery::stub(pip_snapshot, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(pip_snapshot, "get_package_current_branch", function(package) {
    c(pipapi = "PROD")
  })
  mockery::stub(pip_snapshot, "latest_commit_for_branch", function(pkg, brn) {
    list(sha = "abc")
  })
  expect_error(
    suppressWarnings(pip_snapshot(path = file.path(tempdir(), "no-such-dir", "lock.csv"))),
    class = "simpleError"
  )
})

test_that("pip_snapshot survives when no SHA can be resolved", {
  mockery::stub(pip_snapshot, "get_core_pagkages", function(...) c("pipapi", "wbpip"))
  mockery::stub(pip_snapshot, "get_package_current_branch", function(package) {
    c(pipapi = "PROD", wbpip = "PROD")
  })
  mockery::stub(pip_snapshot, "latest_commit_for_branch", function(pkg, brn) {
    list(sha = NULL)
  })

  tf <- tempfile(fileext = ".csv")
  expect_message(pip_snapshot(path = tf), "Could not resolve any SHA")
  expect_false(file.exists(tf))
})

test_that("pip_lock_path returns a path or empty string when lock absent", {
  p <- pip_lock_path()
  expect_type(p, "character")
  expect_true(p == "" || file.exists(p))
})
