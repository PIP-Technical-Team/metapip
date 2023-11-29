test_that("core_metadata function works as expected", {
  out1 <- core_metadata()
  out2 <- core_metadata("pipapi")
  expect_s3_class(out1, "data.frame")
  expect_s3_class(out2, "data.frame")
})
