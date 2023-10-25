test_that("core_metadata function works as expected", {
  out1 <- core_metadata()
  out2 <- core_metadata("pipapi")
  expect_s3_class(out1, "knitr_kable")
  expect_s3_class(out2, "knitr_kable")
  expect_error(core_metadata("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
})
