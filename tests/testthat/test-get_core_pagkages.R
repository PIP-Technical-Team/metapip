test_that("get_core_pagkages excludes cwd when it is a core package", {
  mockery::stub(get_core_pagkages, "getwd", function() file.path("some", "path", "pipapi"))

  expect_false("pipapi" %in% get_core_pagkages(exclude = NA))
})

test_that("get_core_pagkages returns all when cwd is not a core package", {
  mockery::stub(get_core_pagkages, "getwd", function() file.path("some", "path", "other"))

  out <- get_core_pagkages(exclude = NA)
  expect_true(all(core %in% out))
})
