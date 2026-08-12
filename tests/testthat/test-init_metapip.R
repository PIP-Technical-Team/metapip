test_that("compare_sha works as expected", {
  hash_val <- "a051498f183e24afdc468ab167306f94a80e57f4"
  mockery::stub(compare_sha, "latest_commit_for_branch", \(...) list(sha = hash_val))
  mockery::stub(compare_sha, "utils::packageDescription", function(...) hash_val)
  expect_true(compare_sha("pipfun", "test"))

  mockery::stub(compare_sha, "latest_commit_for_branch", \(...) list(sha = "abc"))
  mockery::stub(compare_sha, "utils::packageDescription", function(...) hash_val)
  expect_false(compare_sha("pipfun", "test"))
})

test_that("set_custom_branch works correctly", {
  # Save original option to restore later
  original <- getOption("metapip.custom_branch")
  on.exit(options("metapip.custom_branch" = original), add = TRUE)

  # Reset to known state
  options("metapip.custom_branch" = list(pkgA_branch = "dev"))

  # Update with new values
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
