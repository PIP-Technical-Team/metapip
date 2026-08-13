test_that("install_latest_branch skips packages already at HEAD", {
  install_calls <- character(0)
  info_messages <- character(0)

  mockery::stub(install_latest_branch, "check_github_token", function(...) NULL)
  mockery::stub(install_latest_branch, "is_core", function(...) TRUE)
  mockery::stub(install_latest_branch, "get_latest_branch_update", function(pkg, display) {
    data.frame(package = pkg, branch_name = "PROD", name = "user", last_update_time = Sys.time(), stringsAsFactors = FALSE)
  })
  mockery::stub(install_latest_branch, "compare_sha", function(pkg, branch) TRUE)
  mockery::stub(install_latest_branch, "install_branch", function(pkg, branch) {
    install_calls <<- c(install_calls, pkg)
  })
  mockery::stub(install_latest_branch, "cli::cli_alert_info", function(msg) {
    info_messages <<- c(info_messages, msg)
  })

  result <- install_latest_branch("pipapi")

  expect_null(result)
  expect_length(install_calls, 0)
  expect_true(any(grepl("already at HEAD", info_messages)))
})

test_that("install_latest_branch installs packages not at HEAD", {
  install_calls <- character(0)

  mockery::stub(install_latest_branch, "check_github_token", function(...) NULL)
  mockery::stub(install_latest_branch, "is_core", function(...) TRUE)
  mockery::stub(install_latest_branch, "get_latest_branch_update", function(pkg, display) {
    data.frame(package = pkg, branch_name = "PROD", name = "user", last_update_time = Sys.time(), stringsAsFactors = FALSE)
  })
  mockery::stub(install_latest_branch, "compare_sha", function(pkg, branch) FALSE)
  mockery::stub(install_latest_branch, "install_branch", function(pkg, branch) {
    install_calls <<- c(install_calls, pkg)
  })

  result <- install_latest_branch("pipapi")

  expect_null(result)
  expect_equal(install_calls, "pipapi")
})

test_that("install_latest_branch proceeds when compare_sha returns unknown", {
  install_calls <- character(0)

  mockery::stub(install_latest_branch, "check_github_token", function(...) NULL)
  mockery::stub(install_latest_branch, "is_core", function(...) TRUE)
  mockery::stub(install_latest_branch, "get_latest_branch_update", function(pkg, display) {
    data.frame(package = pkg, branch_name = "PROD", name = "user", last_update_time = Sys.time(), stringsAsFactors = FALSE)
  })
  mockery::stub(install_latest_branch, "compare_sha", function(pkg, branch) "unknown")
  mockery::stub(install_latest_branch, "install_branch", function(pkg, branch) {
    install_calls <<- c(install_calls, pkg)
  })

  result <- install_latest_branch("pipapi")

  expect_null(result)
  expect_equal(install_calls, "pipapi")
})
