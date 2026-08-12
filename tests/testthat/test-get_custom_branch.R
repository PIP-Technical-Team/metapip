test_that("get_custom_branch returns branch for a package", {
  withr::with_options(list(metapip.custom_branch = list(pipapi_branch = "DEV")), {
    expect_equal(get_custom_branch(package = "pipapi"), list(pipapi = "DEV"), ignore_attr = TRUE)
  })
})

test_that("get_custom_branch returns all when package is NULL", {
  withr::with_options(list(metapip.custom_branch = list(pipapi_branch = "DEV", wbpip_branch = "PROD")), {
    out <- get_custom_branch()
    expect_named(out, c("pipapi", "wbpip"))
    expect_equal(out$pipapi, "DEV")
    expect_equal(out$wbpip, "PROD")
  })
})

test_that("get_custom_branch errors on unknown package", {
  withr::with_options(list(metapip.custom_branch = list(pipapi_branch = "DEV")), {
    expect_error(get_custom_branch(package = "unknown"), "not available")
  })
})

test_that("get_custom_branch errors when custom branch option is empty", {
  withr::with_options(list(metapip.custom_branch = list()), {
    expect_error(get_custom_branch(), "not available")
  })
})
