test_that("core_metadata function works as expected", {
  out1 <- core_metadata()
  out2 <- core_metadata("pipapi")
  expect_s3_class(out1, "data.frame")
  expect_s3_class(out2, "data.frame")
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
