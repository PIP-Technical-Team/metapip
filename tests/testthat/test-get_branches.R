test_that("check_package_condition works correctly", {
    expect_error(check_package_condition("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
    expect_error(check_package_condition(c("abc", "def")), "Please enter a single package name.")
    expect_true(check_package_condition("pipapi"))
    expect_true(check_package_condition("pipr"))
})


test_that("get_branches works correctly", {
  mockery::stub(get_branches, "gh::gh", function(...) {
    return(list(list(name = "abc", num = 1), list(name = "def", num = 5)))
  })
  res <- get_branches("pipapi")
  expect_equal(res, c("abc", "def"))
  expect_message(get_branches("pipr"), "These are available branches for pipr package")
})

test_that("install_branch works correctly", {
  mockery::stub(install_branch, "get_branches", function(...) {
    c("abc", "def")
  })

  mockery::stub(install_branch, "remotes::install_github", function(...) {
    TRUE
  })
  expect_true(install_branch(branch = "abc"))
  expect_error(install_branch(branch = c("abc", "def")), "Please enter a single branch name.")
  expect_message(install_branch(branch = "abc"), "Installing abc from package pipapi")
})

test_that("is_core works as expected", {
  expect_true(is_core("pipapi"))
  expect_true(is_core(c("pipapi", "pipr")))
  expect_error(is_core("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
})


test_that("get_branch_info works as expected", {
  out1 <- get_branch_info()
  out2 <- get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
  expect_s3_class(out1, "data.frame")
  expect_s3_class(out2, "data.frame")
  expect_length(out1, 4)
  expect_length(out2, 4)
})


test_that("get_branch_info returns an error", {
  expect_error(get_branch_info("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
  expect_error(get_branch_info(c("pipr", "pipapi")), "Please enter a single package name.")
})


test_that("get_latest_branch_update works as expected", {
  out1 <- get_latest_branch_update()
  out2 <- get_latest_branch_update(package = "wbpip")
  expect_s3_class(out1, "data.frame")
  expect_s3_class(out2, "data.frame")
  expect_equal(dim(out1), c(1, 4))
  expect_equal(dim(out2), c(1, 4))
})


test_that("get_branch_info returns an error", {
  expect_error(get_latest_branch_update("abc"), "The package is not one of pipapi, pipload, wbpip, pipfun, pipdata, pipr.")
  expect_error(get_latest_branch_update(c("pipr", "pipapi")), "Please enter a single package name.")
})
