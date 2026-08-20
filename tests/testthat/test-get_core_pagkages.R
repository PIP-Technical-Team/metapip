test_that("get_core_pagkages excludes cwd when it is a core package", {
  mockery::stub(get_core_pagkages, "getwd", function() file.path("some", "path", "pipapi"))

  expect_false("pipapi" %in% get_core_pagkages(exclude = NA))
})

test_that("get_core_pagkages returns all when cwd is not a core package", {
  mockery::stub(get_core_pagkages, "getwd", function() file.path("some", "path", "other"))

  out <- get_core_pagkages(exclude = NA)
  expect_true(all(core %in% out))
})

test_that("get_core_packages alias behaves identically to get_core_pagkages", {
  mockery::stub(get_core_packages, "get_core_pagkages", function(exclude) core)

  expect_equal(get_core_packages(), core)
})

test_that("get_core_packages honors exclude", {
  mockery::stub(get_core_packages, "get_core_pagkages", function(exclude) {
    core[!(core %in% exclude)]
  })

  expect_false("pipapi" %in% get_core_packages(exclude = "pipapi"))
})

test_that("get_core_pagkages errors on non-core exclude", {
  expect_error(get_core_pagkages(exclude = "notacorepkg"), "not part of PIP ecosystem")
})

test_that("get_core_pagkages returns all core when exclude is NULL", {
  out <- get_core_pagkages()
  expect_identical(out, core)
})
