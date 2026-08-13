test_that("compare_sha works as expected", {
  hash_val <- "a051498f183e24afdc468ab167306f94a80e57f4"
  mockery::stub(compare_sha, "latest_commit_for_branch", \(...) list(sha = hash_val))
  mockery::stub(compare_sha, "utils::packageDescription", function(...) hash_val)
  expect_true(compare_sha("pipfun", "test"))

  mockery::stub(compare_sha, "latest_commit_for_branch", \(...) list(sha = "abc"))
  mockery::stub(compare_sha, "utils::packageDescription", function(...) hash_val)
  expect_false(compare_sha("pipfun", "test"))
})

test_that("compare_sha returns unknown when RemoteSha is NA", {
  mockery::stub(compare_sha, "latest_commit_for_branch", \(...) list(sha = "abc"))
  mockery::stub(compare_sha, "utils::packageDescription", function(...) NA_character_)
  expect_equal(compare_sha("pipfun", "test"), "unknown")
})

test_that("compare_sha returns NULL when gh_sha is NULL", {
  mockery::stub(compare_sha, "latest_commit_for_branch", \(...) list(sha = NULL))
  expect_null(compare_sha("pipfun", "test"))
})

test_that("set_custom_branch works correctly", {
  original <- getOption("metapip.custom_branch")
  on.exit(options("metapip.custom_branch" = original), add = TRUE)

  options("metapip.custom_branch" = list(pkgA_branch = "dev"))

  set_custom_branch(pkgB = "main", pkgA = "release")

  result <- getOption("metapip.custom_branch")

  expect_named(result, c("pkgA_branch", "pkgB_branch"))
  expect_equal(result$pkgA_branch, "release")
  expect_equal(result$pkgB_branch, "main")
})

test_that("init_metapip forwards its answer argument", {
  called_args <- list()
  attach_called <- FALSE
  mockery::stub(init_metapip, "update_pip_packages", function(exclude, ask, answer) {
    called_args <<- list(exclude = exclude, ask = ask, answer = answer)
  })
  mockery::stub(init_metapip, "metapip_attach", function(...) {
    attach_called <<- TRUE
  })

  init_metapip(exclude = "pipdata", ask = FALSE, answer = 2)
  expect_equal(called_args$exclude, "pipdata")
  expect_equal(called_args$ask, FALSE)
  expect_equal(called_args$answer, 2)
  expect_true(attach_called)
})

test_that("update_pip_packages isolates per-package failures", {
  install_calls <- character(0)
  install_branch_mock <- function(pkg, branch) {
    install_calls <<- c(install_calls, pkg)
    if (pkg == "wbpip") stop("install error for wbpip")
  }

  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi", "wbpip", "pipfun"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) {
    c(pipapi = "PROD", wbpip = "PROD", pipfun = "PROD")
  })
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "install_branch", install_branch_mock)

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_true(result)
  expect_true("wbpip" %in% install_calls)
  expect_true("pipapi" %in% install_calls)
  expect_true("pipfun" %in% install_calls)
})

test_that("update_pip_packages skips utils::menu in non-interactive session", {
  menu_called <- FALSE
  mockery::stub(update_pip_packages, "interactive", function() FALSE)
  mockery::stub(update_pip_packages, "utils::menu", function(...) {
    menu_called <<- TRUE
    1
  })
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)

  result <- update_pip_packages(ask = TRUE, answer = 1)

  expect_false(menu_called)
  expect_true(result)
})

test_that("update_pip_packages calls utils::menu in interactive session", {
  menu_called <- FALSE
  mockery::stub(update_pip_packages, "interactive", function() TRUE)
  mockery::stub(update_pip_packages, "utils::menu", function(...) {
    menu_called <<- TRUE
    1
  })
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)

  result <- update_pip_packages(ask = TRUE, answer = 1)

  expect_true(menu_called)
  expect_true(result)
})

test_that("update_pip_packages skips unknown packages with warning", {
  install_called <- character(0)
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(pkg, branch) "unknown")
  mockery::stub(update_pip_packages, "install_branch", function(pkg, branch) {
    install_called <<- c(install_called, pkg)
  })

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_false(result)
  expect_length(install_called, 0)
})

test_that("update_pip_packages emits CRAN gap info when installs occur", {
  info_messages <- character(0)
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)
  mockery::stub(update_pip_packages, "cli::cli_alert_info", function(msg) {
    info_messages <<- c(info_messages, msg)
  })

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_true(result)
  expect_true(any(grepl("renv", info_messages)))
})

test_that("init_metapip completes with interdependent mocked packages", {
  attach_called <- FALSE

  mockery::stub(init_metapip, "update_pip_packages", function(exclude, ask, answer) {
    invisible(TRUE)
  })
  mockery::stub(init_metapip, "metapip_attach", function(...) {
    attach_called <<- TRUE
  })

  result <- init_metapip(exclude = NA, ask = FALSE, answer = 1)

  expect_true(attach_called)
})

test_that("update_pip_packages returns FALSE when answer=2 in non-interactive", {
  mockery::stub(update_pip_packages, "interactive", function() FALSE)
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)

  result <- update_pip_packages(ask = FALSE, answer = 2)

  expect_false(result)
})

test_that("update_pip_packages returns FALSE when all packages are up-to-date", {
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) TRUE)

  result <- update_pip_packages(ask = FALSE)

  expect_false(result)
})
