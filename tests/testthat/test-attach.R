test_that(".onAttach() calls metapip_attach when core packages are not attached", {
  called <- FALSE
  mockery::stub(.onAttach, "is_attached", function(x) rep(FALSE, length(x)))
  mockery::stub(.onAttach, "set_colorDF", function() NULL)
  mockery::stub(.onAttach, "metapip_attach", function(...) {
    called <<- TRUE
  })
  mockery::stub(.onAttach, "is_attached", function(x) FALSE)

  .onAttach()
  expect_true(called)
})

test_that(".onAttach() does not error when core packages are not attached", {
  called <- FALSE
  mockery::stub(.onAttach, "is_attached", function(x) rep(FALSE, length(x)))
  mockery::stub(.onAttach, "set_colorDF", function() NULL)
  mockery::stub(.onAttach, "metapip_attach", function(...) {
    called <<- TRUE
  })
  mockery::stub(.onAttach, "is_attached", function(x) FALSE)

  expect_error(.onAttach(), NA)
})

test_that("pkg_loaded returns attached packages", {
  # stats is always on the search path in test sessions
  expect_true("stats" %in% pkg_loaded("stats"))
  expect_length(pkg_loaded("definitely-not-a-package"), 0)
})

test_that("pkg_unloaded returns packages not on the search path", {
  expect_true("definitely-not-a-package" %in% pkg_unloaded("definitely-not-a-package"))
  expect_false("stats" %in% pkg_unloaded("stats"))
})

test_that("package_version returns a dotted version string", {
  ver <- package_version("metapip")
  expect_type(ver, "character")
  expect_match(ver, "^[0-9.]+")
})

test_that("metapip_attach does not call installed.packages()", {
  # Regression guard for V2: namespaces must be checked with requireNamespace()
  mockery::stub(metapip_attach, "utils::installed.packages",
                function() stop("installed.packages() should no longer be called"))
  mockery::stub(metapip_attach, "pkg_unloaded", function(pkg = NULL) "wbpip")
  mockery::stub(metapip_attach, "requireNamespace", function(pkg, quietly) TRUE)
  mockery::stub(metapip_attach, "utils::packageDescription", function(x, fields) "DEV")
  mockery::stub(metapip_attach, "package_version", function(x) "0.0.2")
  mockery::stub(metapip_attach, "library", function(...) NULL)

  # Warnings are not the target here; the missing-package warning path has its
  # own dedicated test. suppressWarnings keeps the suite output clean.
  expect_error(suppressWarnings(metapip_attach("wbpip")), NA)
})

test_that("metapip_attach warns when core packages are not installed", {
  mockery::stub(metapip_attach, "utils::installed.packages",
                function() stop("installed.packages() should no longer be called"))
  mockery::stub(metapip_attach, "pkg_unloaded", function(pkg = NULL) c("pipapi", "wbpip"))
  mockery::stub(metapip_attach, "requireNamespace", function(pkg, quietly) pkg == "wbpip")
  mockery::stub(metapip_attach, "utils::packageDescription", function(x, fields) "DEV")
  mockery::stub(metapip_attach, "package_version", function(x) "0.0.2")
  mockery::stub(metapip_attach, "library", function(...) NULL)

  expect_warning(metapip_attach("wbpip"), "not installed")
})

test_that("metapip_attach display includes version and branch info", {
  messages <- character(0)
  collect <- function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  }
  mockery::stub(metapip_attach, "pkg_unloaded", function(pkg = NULL) "wbpip")
  mockery::stub(metapip_attach, "requireNamespace", function(pkg, quietly) TRUE)
  mockery::stub(metapip_attach, "utils::packageDescription", function(x, fields) "DEV")
  mockery::stub(metapip_attach, "package_version", function(x) "0.0.2")
  mockery::stub(metapip_attach, "library", function(...) NULL)

  withCallingHandlers(
    metapip_attach("wbpip"),
    packageStartupMessage = collect
  )

  expect_true(any(grepl("wbpip", messages)))
  expect_true(any(grepl("DEV", messages)))
})
