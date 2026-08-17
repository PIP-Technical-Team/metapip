test_that("check_package_condition works correctly", {
    #expect_error(check_package_condition("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
    #expect_error(check_package_condition(c("abc", "def")), "Please enter a single package name.")
    expect_true(check_package_condition("pipapi"))
    expect_true(check_package_condition("wbpip"))
})


test_that("get_branches works correctly", {
  mockery::stub(get_branches, "gh_token", function() NULL)
  mockery::stub(get_branches, "gh::gh", function(...) {
    return(list(list(name = "abc", num = 1), list(name = "def", num = 5)))
  })
  res <- get_branches("pipapi")
  expect_equal(res, c("abc", "def"))
  expect_message(get_branches("wbpip"), "These are available branches for wbpip package")
})

test_that("get_branches returns all branches with full pagination (45 branches)", {
  captured_args <- NULL
  mockery::stub(get_branches, "gh_token", function() NULL)
  mockery::stub(get_branches, "check_package_condition", \(...) TRUE)
  mockery::stub(get_branches, "gh::gh", function(...) {
    captured_args <<- list(...)
    lapply(seq_len(45), \(i) list(name = sprintf("branch_%02d", i)))
  })

  res <- get_branches("pipapi", display = FALSE)

  expect_length(res, 45)
  expect_true(identical(captured_args$.limit, Inf))
  expect_true(identical(captured_args$repo, "pipapi"))
})

test_that("get_branches returns exactly 30 branches when repo has 30", {
  mockery::stub(get_branches, "gh_token", function() NULL)
  mockery::stub(get_branches, "check_package_condition", \(...) TRUE)
  mockery::stub(get_branches, "gh::gh", function(...) {
    lapply(seq_len(30), \(i) list(name = sprintf("branch_%02d", i)))
  })

  res <- get_branches("pipapi", display = FALSE)

  expect_length(res, 30)
})

test_that("get_branches propagates gh errors", {
  mockery::stub(get_branches, "gh_token", function() NULL)
  mockery::stub(get_branches, "check_package_condition", \(...) TRUE)
  mockery::stub(get_branches, "gh::gh", \(...) stop("GitHub API error"))

  expect_error(get_branches("pipapi", display = FALSE), "GitHub API error")
})

test_that("get_branches works without GITHUB_PAT", {
  gh_called <- FALSE
  token_seen <- "unset"
  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = ""), {
    mockery::stub(get_branches, "gh_token", function() NULL)
    mockery::stub(get_branches, "check_package_condition", \(...) TRUE)
    mockery::stub(get_branches, "gh::gh", function(..., .token = NULL) {
      gh_called <<- TRUE
      token_seen <<- .token
      list(list(name = "abc", num = 1))
    })

    res <- get_branches("pipapi", display = FALSE)
  })

  expect_true(gh_called)
  expect_null(token_seen)
  expect_equal(res, "abc")
})

test_that("install_branch works correctly", {
  install_refs <- character(0)
  mockery::stub(install_branch, "check_github_token", function() NULL)
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) invisible())
  mockery::stub(install_branch, "get_branches", function(...) {
    c("abc", "def")
  })
  mockery::stub(install_branch, "latest_commit_for_branch", function(...) {
    list(sha = "abc123")
  })
  mockery::stub(install_branch, "utils::packageDescription", function(...) {
    NA_character_
  })
  mockery::stub(install_branch, "remotes::install_github", function(x) {
    install_refs <<- c(install_refs, x)
    TRUE
  })

  expect_true(install_branch(branch = "abc"))
  expect_equal(install_refs, "PIP-Technical-Team/pipapi@abc123")
  expect_error(install_branch(branch = c("abc", "def")), "Please enter a single branch name.")
  expect_message(install_branch(branch = "abc"), "Installing branch abc from package pipapi")
})

test_that("install_branch(force=TRUE) installs live branch HEAD with a bypass warning", {
  install_refs <- character(0)
  sha_resolved <- FALSE
  mockery::stub(install_branch, "check_github_token", function() NULL)
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) invisible())
  mockery::stub(install_branch, "get_branches", function(...) c("abc", "def"))
  mockery::stub(install_branch, "latest_commit_for_branch", function(...) {
    sha_resolved <<- TRUE
    list(sha = "abc123")
  })
  mockery::stub(install_branch, "remotes::install_github", function(x) {
    install_refs <<- c(install_refs, x)
    TRUE
  })

  expect_message(install_branch(branch = "abc", force = TRUE), "force = TRUE bypasses the team lock")
  expect_equal(install_refs, "PIP-Technical-Team/pipapi@abc")
  expect_false(sha_resolved)
})

test_that("install_branch skips install when installed RemoteSha matches target SHA", {
  installed <- FALSE
  info_messages <- character(0)
  mockery::stub(install_branch, "check_github_token", function() NULL)
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) invisible())
  mockery::stub(install_branch, "get_branches", function(...) c("abc", "def"))
  mockery::stub(install_branch, "latest_commit_for_branch", function(...) {
    list(sha = "abc123")
  })
  mockery::stub(install_branch, "utils::packageDescription", function(...) {
    "abc123"
  })
  mockery::stub(install_branch, "remotes::install_github", function(x) {
    installed <<- TRUE
    TRUE
  })
  mockery::stub(install_branch, "cli::cli_alert_info", function(msg) {
    info_messages <<- c(info_messages, msg)
  })

  res <- install_branch(branch = "abc")

  expect_false(installed)
  expect_null(res)
  expect_true(any(grepl("already at SHA", info_messages)))
})

test_that("install_branch proceeds when installed RemoteSha is NA", {
  installed <- FALSE
  mockery::stub(install_branch, "check_github_token", function() NULL)
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) invisible())
  mockery::stub(install_branch, "get_branches", function(...) c("abc", "def"))
  mockery::stub(install_branch, "latest_commit_for_branch", function(...) {
    list(sha = "abc123")
  })
  mockery::stub(install_branch, "utils::packageDescription", function(...) {
    NA_character_
  })
  mockery::stub(install_branch, "remotes::install_github", function(x) {
    installed <<- TRUE
    TRUE
  })

  res <- install_branch(branch = "abc")

  expect_true(installed)
})

test_that("install_branch aborts cleanly when the target SHA cannot be resolved", {
  installed <- FALSE
  mockery::stub(install_branch, "check_github_token", function() NULL)
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) invisible())
  mockery::stub(install_branch, "get_branches", function(...) c("abc", "def"))
  mockery::stub(install_branch, "latest_commit_for_branch", function(...) {
    list(sha = NULL)
  })
  mockery::stub(install_branch, "remotes::install_github", function(x) {
    installed <<- TRUE
    TRUE
  })

  expect_error(install_branch(branch = "abc"), "Could not resolve SHA")
  expect_false(installed)
})

test_that("install_branch honours an explicit sha override", {
  install_refs <- character(0)
  mockery::stub(install_branch, "check_github_token", function() NULL)
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) invisible())
  mockery::stub(install_branch, "get_branches", function(...) c("abc", "def"))
  mockery::stub(install_branch, "latest_commit_for_branch", function(...) {
    list(sha = "branchhead")
  })
  mockery::stub(install_branch, "utils::packageDescription", function(...) {
    NA_character_
  })
  mockery::stub(install_branch, "remotes::install_github", function(x) {
    install_refs <<- c(install_refs, x)
    TRUE
  })

  install_branch(branch = "abc", sha = "deadbeef")

  expect_equal(install_refs, "PIP-Technical-Team/pipapi@deadbeef")
})

test_that("install_branch aborts for an invalid branch name", {
  mockery::stub(install_branch, "check_github_token", function() NULL)
  mockery::stub(install_branch, "check_package_condition", function(...) TRUE)
  mockery::stub(install_branch, "detach_package", function(...) invisible())
  mockery::stub(install_branch, "get_branches", function(...) c("abc", "def"))

  expect_error(install_branch(branch = "nope"), "Not a valid branch name")
})

test_that("is_core works as expected", {
  expect_true(is_core("pipapi"))
  expect_true(is_core(c("pipapi", "wbpip")))
  #expect_error(is_core("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
})


test_that("get_branch_info works as expected", {
  skip("avoid live network")
  out1 <- get_branch_info()
  out2 <- get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
  expect_s3_class(out1, "data.frame")
  expect_s3_class(out2, "data.frame")
  expect_length(out1, 4)
  expect_length(out2, 4)
})


test_that("get_branch_info returns an error", {
  #expect_error(get_branch_info("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
  #expect_error(get_branch_info(c("pipr", "pipapi")), "Please enter a single package name.")
})


test_that("get_latest_branch_update works as expected", {
  skip("avoid live network")
  out1 <- get_latest_branch_update()
  out2 <- get_latest_branch_update(package = "wbpip")
  expect_s3_class(out1, "data.frame")
  expect_s3_class(out2, "data.frame")
  expect_equal(dim(out1), c(1, 4))
  expect_equal(dim(out2), c(1, 4))
})


test_that("get_latest_branch_update parses timestamps as UTC", {
  withr::local_envvar(TZ = "America/New_York")
  mockery::stub(get_latest_branch_update, "check_github_token",
                function() invisible(list(username = "x", password = "x")))
  mockery::stub(get_latest_branch_update, "get_branch_info", function(...) {
    data.frame(
      package = "pipapi",
      branch_name = "PROD",
      last_commit_author_name = "Test Author",
      last_update_time = "2026-01-15T12:30:00Z",
      stringsAsFactors = FALSE
    )
  })
  res <- get_latest_branch_update("pipapi", display = FALSE)
  expect_true(inherits(res$last_update_time, "POSIXct"))
  expect_identical(attr(res$last_update_time, "tzone"), "UTC")
  expect_equal(
    as.numeric(res$last_update_time),
    as.numeric(as.POSIXct("2026-01-15 12:30:00", tz = "UTC"))
  )
})

test_that("get_latest_branch_update parses fractional-second timestamps as UTC", {
  withr::local_envvar(TZ = "America/New_York")
  mockery::stub(get_latest_branch_update, "check_github_token",
                function() invisible(list(username = "x", password = "x")))
  mockery::stub(get_latest_branch_update, "get_branch_info", function(...) {
    data.frame(
      package = "pipapi",
      branch_name = "PROD",
      last_commit_author_name = "Test Author",
      last_update_time = "2026-01-15T12:30:00.500Z",
      stringsAsFactors = FALSE
    )
  })
  res <- get_latest_branch_update("pipapi", display = FALSE)
  expect_true(inherits(res$last_update_time, "POSIXct"))
  expect_identical(attr(res$last_update_time, "tzone"), "UTC")
  expect_equal(
    as.numeric(res$last_update_time),
    as.numeric(as.POSIXct("2026-01-15 12:30:00.500", tz = "UTC")),
    tolerance = 1e-3
  )
})

test_that("get_latest_branch_update handles only-gh-pages branch set", {
  withr::local_envvar(TZ = "America/New_York")
  mockery::stub(get_latest_branch_update, "check_github_token",
                function() invisible(list(username = "x", password = "x")))
  mockery::stub(get_latest_branch_update, "get_branch_info", function(...) {
    data.frame(
      package = "pipapi",
      branch_name = "gh-pages",
      last_commit_author_name = "Author",
      last_update_time = "2026-01-15T12:30:00Z",
      stringsAsFactors = FALSE
    )
  })
  expect_warning(
    res <- get_latest_branch_update("pipapi", display = FALSE),
    "No non-gh-pages branches found"
  )
  expect_equal(nrow(res), 1L)
  expect_equal(colnames(res), c("package", "branch_name", "last_commit_author_name", "last_update_time"))
  expect_identical(res$package, "pipapi")
  expect_true(is.na(res$branch_name))
  expect_true(is.na(res$last_commit_author_name))
  expect_true(is.na(res$last_update_time))
  expect_identical(attr(res$last_update_time, "tzone"), "UTC")
})

test_that("get_latest_branch_update selects latest across multiple branches", {
  mockery::stub(get_latest_branch_update, "check_github_token",
                function() invisible(list(username = "x", password = "x")))
  mockery::stub(get_latest_branch_update, "get_branch_info", function(...) {
    data.frame(
      package = "pipapi",
      branch_name = c("DEV", "PROD", "QA"),
      last_commit_author_name = c("A", "B", "C"),
      last_update_time = c("2026-01-15T12:30:00Z", "2026-01-16T12:30:00Z", "2026-01-14T12:30:00Z"),
      stringsAsFactors = FALSE
    )
  })
  res <- get_latest_branch_update("pipapi", display = FALSE)
  expect_equal(nrow(res), 1L)
  expect_identical(res$branch_name, "PROD")
})

test_that("get_latest_branch_update handles single non-gh-pages branch", {
  mockery::stub(get_latest_branch_update, "check_github_token",
                function() invisible(list(username = "x", password = "x")))
  mockery::stub(get_latest_branch_update, "get_branch_info", function(...) {
    data.frame(
      package = "pipapi",
      branch_name = "DEV",
      last_commit_author_name = "A",
      last_update_time = "2026-01-15T12:30:00Z",
      stringsAsFactors = FALSE
    )
  })
  res <- get_latest_branch_update("pipapi", display = FALSE)
  expect_equal(nrow(res), 1L)
  expect_identical(res$branch_name, "DEV")
})

test_that("get_branch_info returns an error", {
  #expect_error(get_latest_branch_update("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
  #expect_error(get_latest_branch_update(c("pipr", "pipapi")), "Please enter a single package name.")
})
