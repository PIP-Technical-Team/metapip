# rowname_to_column --------
test_that("rowname_to_column works as expected", {
  out <- rowname_to_column(mtcars, "rn")
  expect_equal(dim(out), c(32, 12))
  expect_equal(names(out)[1], "rn")
})

# detach_package --------
test_that("detach_package warns and continues when unloadNamespace fails", {
  mockery::stub(detach_package, "unloadNamespace", function(pkg) {
    stop("namespace 'testpkg' is imported by 'otherpkg'")
  })
  expect_warning(
    result <- detach_package("testpkg"),
    "Could not unload namespace"
  )
  expect_null(result)
})

test_that("detach_package succeeds silently when unloadNamespace works", {
  mock_unload <- mockery::mock(TRUE)
  mockery::stub(detach_package, "unloadNamespace", mock_unload)
  expect_silent(result <- detach_package("testpkg"))
  expect_null(result)
  mockery::expect_called(mock_unload, 1)
})


# rs_theme -----------

# Assuming rs_theme() and set_colorDF() are defined and work as expected

# Correct usage of local_mocked_bindings() and with_mocked_bindings()
test_that("rs_theme returns correct theme information", {

  withr::with_envvar(c(RSTUDIO = "0"), {
     rs_theme()$dark |> expect_false()
  })

  # Mock RStudio not being present
  withr::with_envvar(c(RSTUDIO = "0"), {
    expect_equal(invisible(rs_theme()), list(editor = "", global = "", dark = FALSE, foreground = "", background = ""))
  })

  withr::with_envvar(c(RSTUDIO = "0"), {
    set_colorDF()
    expect_equal(getOption("colorDF_theme"), "bw")
  })
})
