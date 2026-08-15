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

test_that("init_metapip installs from a mock PIP_LOCK and attaches", {
  lock_csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      package = c("pipapi", "wbpip"),
      branch = c("PROD", "DEV"),
      sha = c("sha1", "sha2"),
      stringsAsFactors = FALSE
    ),
    lock_csv,
    row.names = FALSE
  )

  install_args <- list()
  attach_called <- FALSE
  mockery::stub(init_metapip, "pip_lock_path", function() lock_csv)
  mockery::stub(init_metapip, "get_core_pagkages", function(...) c("pipapi", "wbpip"))
  mockery::stub(init_metapip, "install_branch", function(package, branch, force, sha) {
    install_args[[package]] <<- list(branch = branch, sha = sha, force = force)
    invisible()
  })
  mockery::stub(init_metapip, "metapip_attach", function(...) attach_called <<- TRUE)

  init_metapip(ask = FALSE)

  expect_true(attach_called)
  expect_equal(install_args$pipapi$branch, "PROD")
  expect_equal(install_args$pipapi$sha, "sha1")
  expect_equal(install_args$wbpip$branch, "DEV")
  expect_equal(install_args$wbpip$sha, "sha2")
  expect_false(install_args$pipapi$force)
})

test_that("init_metapip falls back to branch HEAD when the lock is absent", {
  install_args <- list()
  info_messages <- character(0)
  attach_called <- FALSE
  mockery::stub(init_metapip, "pip_lock_path", function() "")
  mockery::stub(init_metapip, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(init_metapip, "get_package_current_branch", function(package) {
    c(pipapi = "PROD")
  })
  mockery::stub(init_metapip, "install_branch", function(package, branch, ...) {
    install_args[[package]] <<- list(branch = branch)
    invisible()
  })
  mockery::stub(init_metapip, "cli::cli_alert_info", function(msg) {
    info_messages <<- c(info_messages, msg)
  })
  mockery::stub(init_metapip, "metapip_attach", function(...) attach_called <<- TRUE)

  init_metapip(ask = FALSE)

  expect_true(attach_called)
  expect_equal(install_args$pipapi$branch, "PROD")
  expect_true(any(grepl("No PIP_LOCK found", info_messages)))
})

test_that("update_pip_packages reports packages without a corresponding branch", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  danger_messages <- character(0)
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) {
    c(pipapi = "PROD")
  })
  mockery::stub(update_pip_packages, "compare_sha", function(pkg, branch) NULL)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) {
    list(sha = "head")
  })
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)
  mockery::stub(update_pip_packages, "cli::cli_alert_danger", function(msg) {
    danger_messages <<- c(danger_messages, msg)
  })

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_false(result)
  expect_true(any(grepl("do not have the corresponding", danger_messages)))
})

test_that("update_pip_packages isolates per-package failures", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  install_calls <- character(0)
  install_branch_mock <- function(pkg, branch, ...) {
    install_calls <<- c(install_calls, pkg)
    if (pkg == "wbpip") stop("install error for wbpip")
  }

  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi", "wbpip", "pipfun"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) {
    c(pipapi = "PROD", wbpip = "PROD", pipfun = "PROD")
  })
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) {
    list(sha = paste0("head-", pkg))
  })
  mockery::stub(update_pip_packages, "install_branch", install_branch_mock)

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_true(result)
  expect_true("wbpip" %in% install_calls)
  expect_true("pipapi" %in% install_calls)
  expect_true("pipfun" %in% install_calls)
})

test_that("update_pip_packages skips utils::menu in non-interactive session", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  menu_called <- FALSE
  mockery::stub(update_pip_packages, "interactive", function() FALSE)
  mockery::stub(update_pip_packages, "utils::menu", function(...) {
    menu_called <<- TRUE
    1
  })
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) list(sha = "head"))
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)

  result <- update_pip_packages(ask = TRUE, answer = 1)

  expect_false(menu_called)
  expect_true(result)
})

test_that("update_pip_packages calls utils::menu in interactive session", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  menu_called <- FALSE
  mockery::stub(update_pip_packages, "interactive", function() TRUE)
  mockery::stub(update_pip_packages, "utils::menu", function(...) {
    menu_called <<- TRUE
    1
  })
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) list(sha = "head"))
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)

  result <- update_pip_packages(ask = TRUE, answer = 1)

  expect_true(menu_called)
  expect_true(result)
})

test_that("update_pip_packages skips unknown packages with warning", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  install_called <- character(0)
  warn_messages <- character(0)
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(pkg, branch) "unknown")
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) list(sha = "head"))
  mockery::stub(update_pip_packages, "install_branch", function(pkg, branch, ...) {
    install_called <<- c(install_called, pkg)
  })
  mockery::stub(update_pip_packages, "cli::cli_alert_warning", function(msg) {
    warn_messages <<- c(warn_messages, msg)
  })

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_false(result)
  expect_length(install_called, 0)
  expect_true(any(grepl("Cannot verify SHA", warn_messages)))
})

test_that("update_pip_packages emits CRAN gap info when installs occur", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  info_messages <- character(0)
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) list(sha = "head"))
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)
  mockery::stub(update_pip_packages, "cli::cli_alert_info", function(msg) {
    info_messages <<- c(info_messages, msg)
  })

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_true(result)
  expect_true(any(grepl("renv", info_messages)))
})

test_that("update_pip_packages writes PIP_LOCK and installs outdated at new SHAs", {
  lock_file <- tempfile(fileext = ".csv")
  original <- getOption("metapip.lock_path")
  on.exit(options("metapip.lock_path" = original), add = TRUE)
  options("metapip.lock_path" = lock_file)

  install_args <- list()
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi", "wbpip"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) {
    c(pipapi = "PROD", wbpip = "PROD")
  })
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) {
    list(sha = paste0("head-", pkg))
  })
  mockery::stub(update_pip_packages, "install_branch", function(pkg, branch, sha) {
    install_args[[pkg]] <<- list(branch = branch, sha = sha)
    invisible()
  })

  result <- update_pip_packages(ask = FALSE, answer = 1)

  expect_true(result)
  expect_true(file.exists(lock_file))
  lock <- utils::read.csv(lock_file, stringsAsFactors = FALSE)
  expect_equal(nrow(lock), 2)
  expect_equal(lock$sha, c("head-pipapi", "head-wbpip"))
  expect_equal(install_args$pipapi$sha, "head-pipapi")
  expect_equal(install_args$wbpip$sha, "head-wbpip")
})

test_that("update_pip_packages returns FALSE when answer=2 in non-interactive", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  mockery::stub(update_pip_packages, "interactive", function() FALSE)
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) list(sha = "head"))
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)

  result <- update_pip_packages(ask = FALSE, answer = 2)

  expect_false(result)
})

test_that("update_pip_packages returns FALSE when all packages are up-to-date", {
  withr::local_options(metapip.lock_path = tempfile(fileext = ".csv"))
  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) c(pipapi = "PROD"))
  mockery::stub(update_pip_packages, "compare_sha", function(...) TRUE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) list(sha = "head"))

  result <- update_pip_packages(ask = FALSE)

  expect_false(result)
})

test_that("update_pip_packages survives when no SHA can be resolved", {
  lock_file <- tempfile(fileext = ".csv")
  original <- getOption("metapip.lock_path")
  on.exit(options("metapip.lock_path" = original), add = TRUE)
  options("metapip.lock_path" = lock_file)

  mockery::stub(update_pip_packages, "get_core_pagkages", function(...) c("pipapi", "wbpip"))
  mockery::stub(update_pip_packages, "get_package_current_branch", function(...) {
    c(pipapi = "PROD", wbpip = "PROD")
  })
  mockery::stub(update_pip_packages, "compare_sha", function(...) FALSE)
  mockery::stub(update_pip_packages, "latest_commit_for_branch", function(pkg, brn) {
    list(sha = NULL)
  })
  mockery::stub(update_pip_packages, "install_branch", function(...) NULL)

  expect_no_error(result <- update_pip_packages(ask = FALSE, answer = 1))
  expect_false(file.exists(lock_file))
})

test_that("init_metapip respects the interactive answer=2 decline when a lock is present", {
  lock_csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      package = c("pipapi"),
      branch = c("PROD"),
      sha = c("sha1"),
      stringsAsFactors = FALSE
    ),
    lock_csv,
    row.names = FALSE
  )

  install_called <- FALSE
  attach_called <- FALSE
  mockery::stub(init_metapip, "pip_lock_path", function() lock_csv)
  mockery::stub(init_metapip, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(init_metapip, "interactive", function() TRUE)
  mockery::stub(init_metapip, "utils::menu", function(...) 2)
  mockery::stub(init_metapip, "install_branch", function(...) {
    install_called <<- TRUE
  })
  mockery::stub(init_metapip, "metapip_attach", function(...) attach_called <<- TRUE)

  init_metapip(ask = TRUE, answer = 1)

  expect_false(install_called)
  expect_true(attach_called)
})

test_that("init_metapip skips lock installs when ask=FALSE and answer=2", {
  lock_csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      package = c("pipapi"),
      branch = c("PROD"),
      sha = c("sha1"),
      stringsAsFactors = FALSE
    ),
    lock_csv,
    row.names = FALSE
  )

  install_called <- FALSE
  attach_called <- FALSE
  mockery::stub(init_metapip, "pip_lock_path", function() lock_csv)
  mockery::stub(init_metapip, "get_core_pagkages", function(...) c("pipapi"))
  mockery::stub(init_metapip, "install_branch", function(...) {
    install_called <<- TRUE
  })
  mockery::stub(init_metapip, "metapip_attach", function(...) attach_called <<- TRUE)

  init_metapip(ask = FALSE, answer = 2)

  expect_false(install_called)
  expect_true(attach_called)
})

test_that("init_metapip isolates per-package install failures from the lock", {
  lock_csv <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      package = c("pipapi", "wbpip"),
      branch = c("PROD", "DEV"),
      sha = c("sha1", "sha2"),
      stringsAsFactors = FALSE
    ),
    lock_csv,
    row.names = FALSE
  )

  install_calls <- character(0)
  attach_called <- FALSE
  mockery::stub(init_metapip, "pip_lock_path", function() lock_csv)
  mockery::stub(init_metapip, "get_core_pagkages", function(...) c("pipapi", "wbpip"))
  mockery::stub(init_metapip, "install_branch", function(package, branch, force, sha) {
    install_calls <<- c(install_calls, package)
    if (package == "wbpip") stop("install error")
  })
  mockery::stub(init_metapip, "metapip_attach", function(...) attach_called <<- TRUE)

  expect_no_error(init_metapip(ask = FALSE))

  expect_true(all(c("pipapi", "wbpip") %in% install_calls))
  expect_true(attach_called)
})
