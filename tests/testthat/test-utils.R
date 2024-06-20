# rowname_to_column --------
test_that("rowname_to_column works as expected", {
  out <- rowname_to_column(mtcars, "rn")
  expect_equal(dim(out), c(32, 12))
  expect_equal(names(out)[1], "rn")
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
