options(metapip.custom_branch = list(
  pipapi_branch = "DEV", pipfaker_branch = "main", wbpip_branch = "DEV", pipdata_branch = "DEV",
  pipr_branch = "DEV", pipster_branch = "DEV", pipfaker_branch = "main"
))

# Ensure each test file starts with a clean memoization cache
if (exists(".metapip_cache", envir = asNamespace("metapip"), inherits = FALSE)) {
  rm(list = ls(metapip:::.metapip_cache), envir = metapip:::.metapip_cache)
}
