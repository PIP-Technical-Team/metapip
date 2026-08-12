test_that("install_pip_packages calls install_branch once per core package", {
  calls <- list()
  core_pkgs <- metapip:::core
  mockery::stub(install_pip_packages, "check_github_token", function() list(password = "x"))
  mockery::stub(install_pip_packages, "install_branch", function(package, branch) {
    calls[[length(calls) + 1]] <<- list(package = package, branch = branch)
  })

  install_pip_packages(branch = "test")

  expect_length(calls, length(core_pkgs))
  for (i in seq_along(calls)) {
    expect_equal(calls[[i]]$branch, "test")
    expect_equal(calls[[i]]$package, core_pkgs[i])
  }
})

test_that("install_pip_packages honors named branch vector", {
  branch_vec <- c(pipapi = "DEV", wbpip = "PROD")
  pkgs <- character()
  brnchs <- character()
  mockery::stub(install_pip_packages, "check_github_token", function() list(password = "x"))
  mockery::stub(install_pip_packages, "install_branch", function(package, branch) {
    pkgs <<- c(pkgs, package)
    brnchs <<- c(brnchs, branch)
  })

  install_pip_packages(branch = branch_vec)

  expect_equal(pkgs[which(pkgs == "pipapi")], "pipapi")
  expect_equal(brnchs[which(pkgs == "pipapi")], "DEV")
  expect_equal(pkgs[which(pkgs == "wbpip")], "wbpip")
  expect_equal(brnchs[which(pkgs == "wbpip")], "PROD")
})

test_that("install_pip_packages error messages name the package", {
  core_pkgs <- metapip:::core
  mockery::stub(install_pip_packages, "check_github_token", function() list(password = "x"))
  mockery::stub(install_pip_packages, "install_branch", function(package, branch) {
    stop("simulated install failure")
  })

  expect_message(
    install_pip_packages(branch = "test"),
    core_pkgs[[1]]
  )
  expect_equal(invisible(install_pip_packages(branch = "test")), invisible(NULL))
})
