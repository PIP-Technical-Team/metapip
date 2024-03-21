test_that("status_table works correctly", {
 mockery::stub(status_table, "get_branches", function(...) c("PROD", "DEV"))
 out <- status_table(c("pipapi", "wbpip"))
 expect_equal(dim(out), c(2, 3))
})
