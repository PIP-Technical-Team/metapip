test_that("package_branches works correctly", {
 mockery::stub(package_branches, "get_branches", function(...) c("PROD", "DEV"))
 out <- package_branches(c("pipapi", "wbpip"))
 expect_length(out, 4)
 expect_length(out$local, 4)
 expect_length(out$common, 3)
})
