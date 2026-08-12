test_that("get_complete_data returns correct package/branch/version", {
  input <- list(pipapi = c(PROD = "0.1.0", DEV = "0.1.1"), wbpip = c(PROD = "0.2.0"))
  out <- metapip:::get_complete_data(input)

  expect_equal(out$package, c("pipapi", "pipapi", "wbpip"))
  expect_equal(out$branch, c("PROD", "DEV", "PROD"))
  expect_equal(out$version, c("0.1.0", "0.1.1", "0.2.0"))
})
