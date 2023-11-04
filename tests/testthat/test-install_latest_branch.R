test_that("install_latest_branch works as expected", {
  expect_null(install_latest_branch())
  expect_null(install_latest_branch(c("pipfun", "pipapi")))
})


test_that("install_latest_branch gives error", {
  expect_error(install_latest_branch("abc"),"The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
})
