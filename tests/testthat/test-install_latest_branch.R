skip("Avoid testing installation for now")
test_that("install_latest_branch works as expected", {
  expect_null(install_latest_branch())
  expect_null(install_latest_branch(c("pipfun", "pipapi")))
})
