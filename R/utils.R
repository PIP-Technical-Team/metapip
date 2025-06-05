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


# This function is a modified version from  https://gitcreds.r-lib.org/
gitcreds_msg <- function(wh) {
  msgs <- c(
    no_git = paste0(
      "No git installation found. You need to install git and set up ",
      "your GitHub Personal Access token using {.fn gitcreds::gitcreds_set}."),
    no_creds = paste0(
      "No git credentials found. Please set up your GitHub Personal Access ",
      "token using {.fn gitcreds::gitcreds_set}.",
      "Or, follow the instruction here: {.url https://happygitwithr.com/https-pat#tldr}")
  )
  cli::format_inline(msgs[wh])
}



#' make sure your GITHUB credentials are properly setup
#'
#' @return invisible TRUE if credentials are perfectly set
#' @export
#'
#' @examples
#' \dontrun{
#' check_github_token()
#' }
check_github_token <- function() {
  # Check that either GITHUB_PAT is set or credentials have been stored using gitcreds
  # If not, abort with a message

  tryCatch(
    expr = {
      creds <- gitcreds::gitcreds_get()
    },
    gitcreds_nogit_error = function(e) cli::cli_abort("{gitcreds_msg(\"no_git\")}"),
    gitcreds_no_credentials = function(e) cli::cli_abort("{gitcreds_msg(\"no_creds\")}")
  )
  invisible(creds)

  # if (Sys.getenv("GITHUB_PAT") == "")
  #   cli::cli_abort("Enviroment variable `GITHUB_PAT` is empty. Please set it up using Sys.setenv(GITHUB_PAT = 'code')")
}

check_package_condition <- function(package) {
  if(length(package) != 1L) cli::cli_abort("Please enter a single package name.")
  is_core(package)
}

is_core <- function(package) {
  if(!all(package %in% core))
    cli::cli_abort("The package is not one of {toString(core)}")
  return(TRUE)
}

detach_package <- function(package) {
  unloadNamespace(package)
}


#' Non tidyverse alternative to tibble::rownames_to_column
#'
#' @param data Dataframe
#' @param var column name to store rownames
#'
#' @return Dataframe with an additional column of rownames
#' @export
#'
#' @examples
#' \dontrun{
#' rowname_to_column(mtcars, "rn")
#' }
rowname_to_column <- function(data, var) {
  rn <- rownames(data)
  out <- add_vars(data, rn = rn, pos = "front")
  names(out)[1] <- var
  rownames(out) <- NULL
  out
}

#' Set theme for colorDF
#'
#' @return invisible RStudio theme
#' @keywords internal
rs_theme <- function() {
  # set display options ------
  # Check if running in RStudio
  rstudio_theme <- template <-
    list(editor     = "",
         global     = "",
         dark       = FALSE,
         foreground = "",
         background = "")

  if (Sys.getenv("RSTUDIO") == "1") {
    # Attempt to infer theme or notify the user to set the theme if using a
    # newer RStudio version without `rstudioapi` support
    # If possible, use `rstudioapi` to get theme information (works only in certain versions)

    if ("rstudioapi" %in% rownames(utils::installed.packages())) {
      rstudio_theme <- tryCatch(rstudioapi::getThemeInfo(),
                                error = \(e) template,
                                silent = TRUE)
    }
  }
  # return
  invisible(rstudio_theme)
}


#' identify RStudio theme
#'
#' @return invisible RStudio theme
#' @keywords internal

set_colorDF <- function() {
  # set display options ------
  rstudio_theme <- rs_theme()
  if (rstudio_theme$dark) {
    options(colorDF_theme = "wb")
  } else {
    options(colorDF_theme = "bw")
  }

  invisible(rstudio_theme)
}

get_default_branch <- \(pkg) {
  # https://app.clickup.com/t/868e3vhk2?comment=90110143651180
  # Checking for option 2 and 3 here. For option 1, it should never come in this function
  default_branches <- getOption("metapip.custom_default_branch")
  branch_name <- default_branches[[paste0(pkg, "_branch")]]
  if (is.null(branch_name)) {
    return(getOption("metapip.default_branch"))
  } else {
    return(branch_name)
  }
}
