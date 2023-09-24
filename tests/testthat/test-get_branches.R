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
