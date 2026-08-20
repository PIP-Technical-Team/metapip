test_that("core_metadata works as expected (mocked)", {
  pkg <- c("pipapi", "wbpip")
  mockery::stub(core_metadata, "check_github_token", function(...) list(password = "x"))
  mockery::stub(core_metadata, "is_core", function(...) TRUE)
  mockery::stub(core_metadata, "get_branches", function(package, display) {
    c("PROD", "DEV", "QA")
  })
  mockery::stub(core_metadata, "gh::gh", function(...) {
    list(tag_name = "1.0.0", published_at = "2026-01-01T00:00:00Z")
  })
  mockery::stub(core_metadata, "get_latest_branch_update", function(package, display) {
    data.frame(
      package = package,
      branch_name = "PROD",
      last_commit_author_name = "user",
      last_update_time = as.POSIXct("2026-01-01 00:00:00"),
      stringsAsFactors = FALSE
    )
  })

  expect_warning(out <- core_metadata(pkg), NA)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 2)
  expect_equal(out$package, pkg)
  expect_equal(out$no_of_branches, c(3, 3))
  expect_equal(out$latest_release_tag, rep("1.0.0", 2))
})

test_that("core_metadata handles missing releases gracefully (mocked)", {
  cache_clear("release:")
  mockery::stub(core_metadata, "check_github_token", function(...) list(password = "x"))
  mockery::stub(core_metadata, "is_core", function(...) TRUE)
  mockery::stub(core_metadata, "get_branches", function(package, display) {
    c("PROD")
  })
  mockery::stub(core_metadata, "gh::gh", function(...) {
    stop("404 Not Found")
  })
  mockery::stub(core_metadata, "get_latest_branch_update", function(package, display) {
    data.frame(
      package = package,
      branch_name = "PROD",
      last_commit_author_name = "user",
      last_update_time = as.POSIXct("2026-01-01 00:00:00"),
      stringsAsFactors = FALSE
    )
  })

  expect_warning(out <- core_metadata("pipapi"), NA)
  expect_true(is.na(out$latest_release_tag))
  expect_s3_class(out$latest_release_time, "POSIXct")
  expect_true(all(is.na(out$latest_release_time)))
})

test_that("core_metadata memoizes the release API call (V1)", {
  call_count <- 0L
  mockery::stub(core_metadata, "check_github_token", function(...) list(password = "x"))
  mockery::stub(core_metadata, "is_core", function(...) TRUE)
  mockery::stub(core_metadata, "get_branches", function(package, display) {
    c("PROD")
  })
  mockery::stub(core_metadata, "gh::gh", function(...) {
    call_count <<- call_count + 1L
    list(tag_name = "1.0.0", published_at = "2026-01-01T00:00:00Z")
  })
  mockery::stub(core_metadata, "get_latest_branch_update", function(package, display) {
    data.frame(
      package = package,
      branch_name = "PROD",
      last_commit_author_name = "user",
      last_update_time = as.POSIXct("2026-01-01 00:00:00"),
      stringsAsFactors = FALSE
    )
  })

  cache_clear("release:")
  expect_warning(core_metadata("pipapi"), NA)
  expect_warning(core_metadata("pipapi"), NA)
  expect_equal(call_count, 1L)

  # Different package -> separate cache key
  expect_warning(core_metadata("wbpip"), NA)
  expect_equal(call_count, 2L)
  cache_clear("release:")
})

test_that("latest_commit_for_branch memoizes the commits API call (V1)", {
  call_count <- 0L
  mockery::stub(latest_commit_for_branch, "gh::gh", function(...) {
    call_count <<- call_count + 1L
    list(sha = "abc123", commit = list(author = list(date = "2026-01-01", name = "user")))
  })

  cache_clear("commit:")
  out1 <- latest_commit_for_branch("pipapi", "PROD")
  out2 <- latest_commit_for_branch("pipapi", "PROD")
  expect_equal(call_count, 1L)
  expect_identical(out1, out2)
  expect_equal(out1$sha, "abc123")

  # Same package, different branch -> different key
  out3 <- latest_commit_for_branch("pipapi", "DEV")
  expect_equal(call_count, 2L)

  cache_clear("commit:")
})

test_that("install_branch clears the memoization cache after install", {
  cache_set("branches:pipapi", list(list(name = "x")))
  cache_set("commit:pipapi:PROD", list(sha = "y"))
  mockery::stub(install_branch, "check_github_token", function(...) list(password = "x"))
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) NULL)
  mockery::stub(install_branch, "get_branches", function(...) c("PROD"))
  mockery::stub(install_branch, "remotes::install_github", function(...) NULL)
  mockery::stub(install_branch, "cli::cli_alert_info", function(msg) NULL)

  install_branch("pipapi", "PROD")

  expect_null(cache_get("branches:pipapi"))
  expect_null(cache_get("commit:pipapi:PROD"))
})

gh_404 <- function() {
  structure(
    list(
      message = "Not Found (HTTP 404)",
      call = NULL,
      status_code = 404L
    ),
    class = c("gh_error", "error", "condition")
  )
}

test_that("core_metadata caches a 404 gh_error release lookup", {
  cache_clear("release:")
  mockery::stub(core_metadata, "check_github_token", function(...) list(password = "x"))
  mockery::stub(core_metadata, "is_core", function(...) TRUE)
  mockery::stub(core_metadata, "get_branches", function(package, display) c("PROD"))
  mockery::stub(core_metadata, "gh::gh", function(...) stop(gh_404()))
  mockery::stub(core_metadata, "get_latest_branch_update", function(package, display) {
    data.frame(
      package = package,
      branch_name = "PROD",
      last_commit_author_name = "user",
      last_update_time = as.POSIXct("2026-01-01 00:00:00"),
      stringsAsFactors = FALSE
    )
  })

  expect_warning(out <- core_metadata("pipapi"), NA)
  expect_true(is.na(out$latest_release_tag))

  # 404 is a genuine "no release" state: it is cached, so a second run must
  # not re-invoke gh::gh. Verify by replacing the stub with a failure trap.
  trap <- FALSE
  mockery::stub(core_metadata, "gh::gh", function(...) {
    trap <<- TRUE
    stop(gh_404())
  })
  expect_warning(core_metadata("pipapi"), NA)
  expect_false(trap)
  cache_clear("release:")
})

test_that("latest_commit_for_branch caches a 404 gh_error commit lookup", {
  cache_clear("commit:")
  mockery::stub(latest_commit_for_branch, "gh::gh", function(...) stop(gh_404()))

  out1 <- latest_commit_for_branch("pipapi", "gh-pages")
  expect_true(is.na(out1$commit$author$date))

  trap <- FALSE
  mockery::stub(latest_commit_for_branch, "gh::gh", function(...) {
    trap <<- TRUE
    stop(gh_404())
  })
  out2 <- latest_commit_for_branch("pipapi", "gh-pages")
  expect_false(trap)
  expect_identical(out1, out2)
  cache_clear("commit:")
})

test_that("core_metadata latest_commit_time is UTC", {
  withr::local_envvar(TZ = "America/New_York")
  mockery::stub(core_metadata, "check_github_token",
                function() invisible(list(username = "x", password = "x")))
  mockery::stub(core_metadata, "get_branches",
                function(package, display = FALSE) c("PROD", "DEV"))
  mockery::stub(core_metadata, "gh::gh",
                function(...) list(tag_name = "v1.0", published_at = "2026-01-01T00:00:00Z"))
  mockery::stub(core_metadata, "get_latest_branch_update", function(package, display = FALSE) {
    data.frame(
      package = package,
      branch_name = "PROD",
      last_commit_author_name = "Test Author",
      last_update_time = as.POSIXct("2026-01-15 12:30:00", format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
      stringsAsFactors = FALSE
    )
  })
  res <- core_metadata("pipapi")
  expect_true(inherits(res$latest_commit_time, "POSIXct"))
  expect_identical(attr(res$latest_commit_time, "tzone"), "UTC")
  expect_equal(
    as.numeric(res$latest_commit_time),
    as.numeric(as.POSIXct("2026-01-15 12:30:00", tz = "UTC"))
  )
})
