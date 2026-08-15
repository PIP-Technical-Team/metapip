# rowname_to_column --------
test_that("rowname_to_column works as expected", {
  out <- rowname_to_column(mtcars, "rn")
  expect_equal(dim(out), c(32, 12))
  expect_equal(names(out)[1], "rn")
})

# detach_package --------
test_that("detach_package warns and continues when unloadNamespace fails", {
  mockery::stub(detach_package, "unloadNamespace", function(pkg) {
    stop("namespace 'testpkg' is imported by 'otherpkg'")
  })
  expect_warning(
    result <- detach_package("testpkg"),
    "Could not unload namespace"
  )
  expect_null(result)
})

test_that("detach_package succeeds silently when unloadNamespace works", {
  mock_unload <- mockery::mock(TRUE)
  mockery::stub(detach_package, "unloadNamespace", mock_unload)
  expect_silent(result <- detach_package("testpkg"))
  expect_null(result)
  mockery::expect_called(mock_unload, 1)
})


# rs_theme -----------

# Assuming rs_theme() and set_colorDF() are defined and work as expected

# Correct usage of local_mocked_bindings() and with_mocked_bindings()
test_that("rs_theme returns correct theme information", {

  withr::with_envvar(c(RSTUDIO = "0"), {
     rs_theme()$dark |> expect_false()
  })

  # Mock RStudio not being present
  withr::with_envvar(c(RSTUDIO = "0"), {
    expect_equal(invisible(rs_theme()), list(editor = "", global = "", dark = FALSE, foreground = "", background = ""))
  })

  withr::with_envvar(c(RSTUDIO = "0"), {
    set_colorDF()
    expect_equal(getOption("colorDF_theme"), "bw")
  })
})

# gh_token --------
test_that("gh_token returns the token from gitcreds when available", {
  mockery::stub(gh_token, "gitcreds::gitcreds_get", function() {
    list(username = "x", password = "abc", protocol = "https")
  })
  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = ""), {
    expect_equal(gh_token(), "abc")
  })
})

test_that("gh_token honors GITHUB_PAT env var first", {
  mockery::stub(gh_token, "gitcreds::gitcreds_get", function() {
    list(username = "x", password = "from-gitcreds", protocol = "https")
  })
  withr::with_envvar(c(GITHUB_PAT = "env-pat", GITHUB_TOKEN = ""), {
    expect_equal(gh_token(), "env-pat")
  })
})

test_that("gh_token honors GITHUB_TOKEN env var second", {
  mockery::stub(gh_token, "gitcreds::gitcreds_get", function() {
    list(username = "x", password = "from-gitcreds", protocol = "https")
  })
  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = "env-token"), {
    expect_equal(gh_token(), "env-token")
  })
})

test_that("gh_token returns NULL when no creds and does not abort", {
  mockery::stub(gh_token, "gitcreds::gitcreds_get", function() {
    stop("no credentials")
  })
  withr::with_envvar(c(GITHUB_PAT = "", GITHUB_TOKEN = ""), {
    expect_null(gh_token())
  })
})

# check_github_token redaction --------
test_that("check_github_token redacts the PAT on print", {
  mockery::stub(check_github_token, "gitcreds::gitcreds_get", function() {
    list(name = "x", password = "secret", protocol = "https", host = "github.com")
  })
  out <- capture.output(print(check_github_token()))
  expect_false(any(grepl("secret", out)))
  expect_true(any(grepl('""', out)))
})

test_that("check_github_token returns a redacted list (never the password)", {
  mockery::stub(check_github_token, "gitcreds::gitcreds_get", function() {
    list(name = "x", password = "secret", protocol = "https")
  })
  res <- check_github_token()
  expect_s3_class(res, "metapip_token")
  expect_false("secret" %in% unlist(res))
  expect_equal(res$password, "")
})

test_that("check_github_token still aborts when no credentials are available", {
  mockery::stub(check_github_token, "gitcreds::gitcreds_get", function() {
    stop("no credentials")
  })
  expect_error(check_github_token())
})
