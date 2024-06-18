test_that("rowname_to_column works as expected", {
  out <- rowname_to_column(mtcars, "rn")
  expect_equal(dim(out), c(32, 12))
  expect_equal(names(out)[1], "rn")
})
