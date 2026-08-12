test_that(".onAttach() calls metapip_attach when core packages are not attached", {
  called <- FALSE
  mockery::stub(.onAttach, "metapip_attach", function(...) {
    called <<- TRUE
  })

  .onAttach()
  expect_true(called)
})

test_that(".onAttach() does not error when core packages are not attached", {
  called <- FALSE
  mockery::stub(.onAttach, "metapip_attach", function(...) {
    called <<- TRUE
  })

  expect_error(.onAttach(), NA)
})
