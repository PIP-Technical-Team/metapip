test_that("package_branches works correctly", {
  skip("avoid live network")
  mockery::stub(package_branches, "get_branches", function(...) c("PROD", "DEV"))
  out <- package_branches(c("pipapi", "wbpip"))
  expect_length(out, 4)
  expect_length(out$local, 4)
  expect_length(out$common, 4)
})

test_that("get_version_for_url returns version on 200", {
  ok_body <- "Package: pipapi\nVersion: 1.2.3\n"
  httr2::with_mocked_responses(function(req) {
    httr2::response(status_code = 200L, body = charToRaw(ok_body))
  }, {
    res <- get_version_for_url("https://example.com/DESCRIPTION")
    expect_identical(res, "1.2.3")
  })
})

test_that("get_version_for_url returns NA_character_ on 404", {
  httr2::with_mocked_responses(function(req) {
    httr2::response(status_code = 404L)
  }, {
    expect_identical(
      get_version_for_url("https://example.com/DESCRIPTION"),
      NA_character_
    )
  })
})

test_that("get_version_for_url returns NA_character_ on network error", {
  httr2::with_mocked_responses(function(req) {
    stop("Connection timed out")
  }, {
    expect_identical(
      get_version_for_url("https://example.com/DESCRIPTION"),
      NA_character_
    )
  })
})

test_that("get_version_for_url returns NA_character_ for malformed DESCRIPTION", {
  httr2::with_mocked_responses(function(req) {
    httr2::response(status_code = 200L, body = charToRaw("Key: no version here\n"))
  }, {
    expect_identical(
      get_version_for_url("https://example.com/DESCRIPTION"),
      NA_character_
    )
  })
})

test_that("get_package_version handles branches whose DESCRIPTION 404s", {
  mockery::stub(get_package_version, "get_branches",
                function(...) c("PROD", "deleted-branch"))
  httr2::with_mocked_responses(function(req) {
    if (grepl("deleted-branch", req$url)) {
      httr2::response(status_code = 404L)
    } else {
      httr2::response(
        status_code = 200L,
        body = charToRaw("Package: pipapi\nVersion: 9.9.9\n")
      )
    }
  }, {
    res <- get_package_version("pipapi")
    expect_named(res, "pipapi")
    expect_identical(
      unname(res$pipapi),
      c(PROD = "9.9.9", `deleted-branch` = NA_character_)[
        c("PROD", "deleted-branch")
      ] |> unname()
    )
  })
})

test_that("join_and_get_status computes ahead/behind/up-to-date", {
  local <- data.frame(
    package = c("pipapi", "wbpip", "pipfun"),
    local_branch = c("DEV", "PROD", "DEV"),
    local_version = c("1.0.0", "0.9.0", "2.0.0"),
    stringsAsFactors = FALSE
  )
  dev <- data.frame(
    package = c("pipapi", "wbpip", "pipfun"),
    branch = c("PROD", "PROD", "PROD"),
    version = c("1.0.1", "0.9.0", "1.5.0"),
    stringsAsFactors = FALSE
  )
  res <- join_and_get_status(local, dev, "PROD")
  res <- res[order(res$package), ]
  expect_identical(
    res$local_status,
    c("behind PROD", "ahead of PROD", "up-to-date")
  )
})

test_that("join_and_get_status reports unknown compared version as NA", {
  local <- data.frame(
    package = "pipapi",
    local_branch = "DEV",
    local_version = "1.0.0",
    stringsAsFactors = FALSE
  )
  dev <- data.frame(
    package = "pipapi",
    branch = "PROD",
    version = NA_character_,
    stringsAsFactors = FALSE
  )
  res <- join_and_get_status(local, dev, "PROD")
  expect_identical(res$local_status, "PROD version unknown")
})

test_that("join_and_get_status reports packages not in local", {
  local <- data.frame(
    package = "pipapi",
    local_branch = NA_character_,
    local_version = NA_character_,
    stringsAsFactors = FALSE
  )
  dev <- data.frame(
    package = "pipapi",
    branch = "PROD",
    version = "1.0.1",
    stringsAsFactors = FALSE
  )
  res <- join_and_get_status(local, dev, "PROD")
  expect_identical(res$local_status, "Not in local")
})
