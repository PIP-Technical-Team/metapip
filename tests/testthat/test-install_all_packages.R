skip("Avoid testing installation for now")
test_that("install_all_packages works correctly", {
  expect_null(install_all_packages(branch = "test"))
})
