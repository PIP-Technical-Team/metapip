msg <- function(..., startup = FALSE) {
  if (startup) {
    if (!isTRUE(getOption("metapip.quiet"))) {
      packageStartupMessage(text_col(...))
    }
  } else {
    message(text_col(...))
  }
}

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) cli::col_white(x) else cli::col_black(x)
}

#' List all packages in metapip
#'
#' @param include_self Include metapip in the list?
#' @export
#' @examples
#' metapip_packages()
metapip_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("metapip")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1))

  if (include_self) {
    names <- c(names, "metapip")
  }

  names
}

invert <- function(x) {
  if (length(x) == 0) {
    return()
  }
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}

# release_bullets <- function() {
#   c(
#     'Check what `usethis::use_latest_dependencies(TRUE, "CRAN")` might update',
#     "Use `metapip_dependency_dissuade()` to send emails"
#   )
# }

choose_startup_tip <- function(vec) {
  ind <- as.numeric(format(Sys.time(), "%S")) %% length(vec) + 1
  vec[ind]
}


check_github_token <- function() {
  assertthat::assert_that(Sys.getenv("GITHUB_PAT") != "",
                          msg = "Enviroment variable `GITHUB_PAT` is empty. Please set it up using Sys.setenv(GITHUB_PAT = 'code')")
}
