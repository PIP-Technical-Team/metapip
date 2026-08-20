test_that("check_package_condition works correctly", {
    #expect_error(check_package_condition("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
    #expect_error(check_package_condition(c("abc", "def")), "Please enter a single package name.")
    expect_true(check_package_condition("pipapi"))
    expect_true(check_package_condition("wbpip"))
})


test_that("get_branches works correctly", {
  with_mocked_bindings(
    gh = function(...) list(list(name = "abc", num = 1), list(name = "def", num = 5)),
    .package = "gh",
    code = with_mocked_bindings(
      gh_token = function() NULL,
      check_package_condition = function(...) TRUE,
      .package = "metapip",
      code = {
        res <- get_branches("pipapi")
        expect_equal(res, c("abc", "def"))
        expect_message(get_branches("wbpip"), "These are available branches for wbpip package")
      }
    )
  )
})

test_that("get_branches memoizes gh::gh per package (V1)", {
  call_count <- 0L
  with_mocked_bindings(
    gh = function(...) {
      call_count <<- call_count + 1L
      list(list(name = "abc"), list(name = "def"))
    },
    .package = "gh",
    code = with_mocked_bindings(
      gh_token = function() NULL,
      check_package_condition = function(...) TRUE,
      .package = "metapip",
      code = {
        cache_clear("branches:")
        res1 <- get_branches("pipapi", display = FALSE)
        res2 <- get_branches("pipapi", display = FALSE)
        expect_equal(call_count, 1L)
        expect_equal(res1, c("abc", "def"))
        expect_equal(res2, c("abc", "def"))

        # A different package uses a different cache key
        res3 <- get_branches("wbpip", display = FALSE)
        expect_equal(call_count, 2L)
        expect_equal(res3, c("abc", "def"))

        cache_clear("branches:")
      }
    )
  )
})

test_that("get_branches display logic still runs on cache hit", {
  messages <- character(0)
  with_mocked_bindings(
    gh = function(...) list(list(name = "abc")),
    .package = "gh",
    code = with_mocked_bindings(
      gh_token = function() NULL,
      check_package_condition = function(...) TRUE,
      .package = "metapip",
      code = with_mocked_bindings(
        cli_h3 = function(msg) NULL,
        # cli::cat_bullet interpolates {branches} internally; simulate that
        # here so the test observes the rendered branch names.
        cat_bullet = function(msg) {
          messages <<- c(messages, glue::glue(msg, .envir = parent.frame()))
        },
        .package = "cli",
        code = {
          cache_clear("branches:")
          invisible(get_branches("pipapi", display = TRUE))
          invisible(get_branches("pipapi", display = TRUE))
          expect_length(messages, 2)
          cache_clear("branches:")
        }
      )
    )
  )
})

test_that("get_branches returns all branches with full pagination (45 branches)", {
  captured_args <- NULL
  cache_clear("branches:")
  with_mocked_bindings(
    gh = function(...) {
      captured_args <<- list(...)
      lapply(seq_len(45), \(i) list(name = sprintf("branch_%02d", i)))
    },
    .package = "gh",
    code = with_mocked_bindings(
      gh_token = function() NULL,
      check_package_condition = function(...) TRUE,
      .package = "metapip",
      code = {
        res <- get_branches("pipapi", display = FALSE)
        expect_length(res, 45)
        expect_true(identical(captured_args$.limit, Inf))
        expect_true(identical(captured_args$repo, "pipapi"))
      }
    )
  )
})

test_that("get_branches returns exactly 30 branches when repo has 30", {
  cache_clear("branches:")
  with_mocked_bindings(
    gh = function(...) {
      lapply(seq_len(30), \(i) list(name = sprintf("branch_%02d", i)))
    },
    .package = "gh",
    code = with_mocked_bindings(
      gh_token = function() NULL,
      check_package_condition = function(...) TRUE,
      .package = "metapip",
      code = {
        res <- get_branches("pipapi", display = FALSE)
        expect_length(res, 30)
      }
    )
  )
})

test_that("get_branches propagates gh errors", {
  cache_clear("branches:")
  with_mocked_bindings(
    gh = function(...) stop("GitHub API error"),
    .package = "gh",
    code = with_mocked_bindings(
      gh_token = function() NULL,
      check_package_condition = function(...) TRUE,
      .package = "metapip",
      code = {
        expect_error(get_branches("pipapi", display = FALSE), "GitHub API error")
      }
    )
  )
})

test_that("get_branches works without GITHUB_PAT", {
  gh_called <- FALSE
  token_seen <- "unset"
  cache_clear("branches:")
  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = ""), {
    with_mocked_bindings(
      gh = function(..., .token = NULL) {
        gh_called <<- TRUE
        token_seen <<- .token
        list(list(name = "abc", num = 1))
      },
      .package = "gh",
      code = with_mocked_bindings(
        gh_token = function() NULL,
        check_package_condition = function(...) TRUE,
        .package = "metapip",
        code = {
          res <- get_branches("pipapi", display = FALSE)
        }
      )
    )
  })

  expect_true(gh_called)
  expect_null(token_seen)
  expect_equal(res, "abc")
})

test_that("install_branch works correctly", {
  install_refs <- character(0)
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    detach_package = function(...) invisible(),
    get_branches = function(...) c("abc", "def"),
    latest_commit_for_branch = function(...) list(sha = "abc123"),
    .package = "metapip",
    code = with_mocked_bindings(
      packageDescription = function(...) NA_character_,
      .package = "utils",
      code = with_mocked_bindings(
        install_github = function(x) {
          install_refs <<- c(install_refs, x)
          TRUE
        },
        .package = "remotes",
        code = {
          cache_clear("branches:")
          cache_clear("commit:")
          expect_true(install_branch(branch = "abc"))
          expect_equal(install_refs, "PIP-Technical-Team/pipapi@abc123")
          expect_message(install_branch(branch = "abc"), "Installing branch abc from package pipapi")
        }
      )
    )
  )

  # The validation error is intentionally outside the packageDescription mock.
  # cli/rlang formats the error through utils::packageVersion("cli").
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    .package = "metapip",
    code = expect_error(
      install_branch(branch = c("abc", "def")),
      "Please enter a single branch name."
    )
  )
})

test_that("install_branch(force=TRUE) installs live branch HEAD with a bypass warning", {
  install_refs <- character(0)
  sha_resolved <- FALSE
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    detach_package = function(...) invisible(),
    get_branches = function(...) c("abc", "def"),
    latest_commit_for_branch = function(...) {
      sha_resolved <<- TRUE
      list(sha = "abc123")
    },
    .package = "metapip",
    code = with_mocked_bindings(
      install_github = function(x) {
        install_refs <<- c(install_refs, x)
        TRUE
      },
      .package = "remotes",
      code = {
        cache_clear("branches:")
        expect_message(install_branch(branch = "abc", force = TRUE), "force = TRUE bypasses the team lock")
        expect_equal(install_refs, "PIP-Technical-Team/pipapi@abc")
        expect_false(sha_resolved)
      }
    )
  )
})

test_that("install_branch skips install when installed RemoteSha matches target SHA", {
  installed <- FALSE
  info_messages <- character(0)
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    detach_package = function(...) invisible(),
    get_branches = function(...) c("abc", "def"),
    latest_commit_for_branch = function(...) list(sha = "abc123"),
    .package = "metapip",
    code = with_mocked_bindings(
      packageDescription = function(...) "abc123",
      .package = "utils",
      code = with_mocked_bindings(
        install_github = function(x) {
          installed <<- TRUE
          TRUE
        },
        .package = "remotes",
        code = with_mocked_bindings(
          cli_alert_info = function(msg) {
            info_messages <<- c(info_messages, msg)
          },
          .package = "cli",
          code = {
            res <- install_branch(branch = "abc")
            expect_false(installed)
            expect_null(res)
            expect_true(any(grepl("already at SHA", info_messages)))
          }
        )
      )
    )
  )
})

test_that("install_branch proceeds when installed RemoteSha is NA", {
  installed <- FALSE
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    detach_package = function(...) invisible(),
    get_branches = function(...) c("abc", "def"),
    latest_commit_for_branch = function(...) list(sha = "abc123"),
    .package = "metapip",
    code = with_mocked_bindings(
      packageDescription = function(...) NA_character_,
      .package = "utils",
      code = with_mocked_bindings(
        install_github = function(x) {
          installed <<- TRUE
          TRUE
        },
        .package = "remotes",
        code = {
          res <- install_branch(branch = "abc")
          expect_true(installed)
        }
      )
    )
  )
})

test_that("install_branch aborts cleanly when the target SHA cannot be resolved", {
  installed <- FALSE
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    detach_package = function(...) invisible(),
    get_branches = function(...) c("abc", "def"),
    latest_commit_for_branch = function(...) list(sha = NULL),
    .package = "metapip",
    code = with_mocked_bindings(
      install_github = function(x) {
        installed <<- TRUE
        TRUE
      },
      .package = "remotes",
      code = {
        expect_error(install_branch(branch = "abc"), "Could not resolve SHA")
        expect_false(installed)
      }
    )
  )
})

test_that("install_branch honours an explicit sha override", {
  install_refs <- character(0)
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    detach_package = function(...) invisible(),
    get_branches = function(...) c("abc", "def"),
    latest_commit_for_branch = function(...) list(sha = "branchhead"),
    .package = "metapip",
    code = with_mocked_bindings(
      packageDescription = function(...) NA_character_,
      .package = "utils",
      code = with_mocked_bindings(
        install_github = function(x) {
          install_refs <<- c(install_refs, x)
          TRUE
        },
        .package = "remotes",
        code = {
          cache_clear("branches:")
          install_branch(branch = "abc", sha = "deadbeef")
          expect_equal(install_refs, "PIP-Technical-Team/pipapi@deadbeef")
        }
      )
    )
  )
})

test_that("install_branch aborts for an invalid branch name", {
  with_mocked_bindings(
    check_github_token = function(...) NULL,
    check_package_condition = function(...) TRUE,
    detach_package = function(...) invisible(),
    get_branches = function(...) c("abc", "def"),
    .package = "metapip",
    code = {
      expect_error(install_branch(branch = "nope"), "Not a valid branch name")
    }
  )
})

test_that("is_core works as expected", {
  expect_true(is_core("pipapi"))
  expect_true(is_core(c("pipapi", "wbpip")))
  #expect_error(is_core("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
})


test_that("get_branch_info works as expected", {
  skip_if_offline()
  skip_on_cran()
  out1 <- get_branch_info()
  out2 <- get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
  expect_s3_class(out1, "data.frame")
  expect_s3_class(out2, "data.frame")
  expect_length(out1, 4)
  expect_length(out2, 4)
})

test_that("get_latest_branch_update works as expected", {
  skip_if_offline()
  skip_on_cran()
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
