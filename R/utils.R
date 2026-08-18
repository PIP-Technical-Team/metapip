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
  if (!rstudioapi::isAvailable()) {
    return(x)
  }
  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }
  theme <- rstudioapi::getThemeInfo()
  if (isTRUE(theme$dark)) cli::col_white(x) else cli::col_black(x)
}


#' List all metapip package dependencies
#'
#' @description
#' Returns the names of all packages listed in the `Imports` field of
#' metapip's DESCRIPTION. Optionally includes `"metapip"` itself.
#'
#' @param include_self Logical. If `TRUE` (default), includes
#'   `"metapip"` in the returned vector.
#'
#' @return Character vector of package names.
#'
#' @examples
#' metapip_packages()
#' metapip_packages(include_self = FALSE)
#'
#' @export
metapip_packages <- function(include_self = TRUE) {
  raw <- utils::packageDescription("metapip")$Imports
  imports <- strsplit(raw, ",")[[1]]
  parsed <- gsub("^\\s+|\\s+$", "", imports)
  names <- vapply(
    strsplit(parsed, "\\s+"), "[[", 1, FUN.VALUE = character(1)
  )

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

choose_startup_tip <- function(vec) {
  ind <- as.numeric(format(Sys.time(), "%S")) %% length(vec) + 1
  vec[ind]
}


# This function is a modified version from  https://gitcreds.r-lib.org/
gitcreds_msg <- function(wh) {
  msgs <- c(
    no_git = paste0(
      "No git installation found. You need to install git and set up ",
      "your GitHub Personal Access token using ",
      "{.fn gitcreds::gitcreds_set}."
    ),
    no_creds = paste0(
      "No git credentials found. Please set up your GitHub Personal ",
      "Access token using {.fn gitcreds::gitcreds_set}.",
      "Or, follow the instruction here: ",
      "{.url https://happygitwithr.com/https-pat#tldr}"
    )
  )
  cli::format_inline(msgs[wh])
}


#' Return the GitHub token when available, or NULL
#'
#' @description
#' Resolves a GitHub token by checking (in order):
#' 1. `GITHUB_PAT` environment variable.
#' 2. `GITHUB_TOKEN` environment variable.
#' 3. `gitcreds::gitcreds_get()` (stored credentials).
#'
#' Returns `NULL` when no credentials are available, allowing read-only
#' `gh::gh()` calls to proceed unauthenticated against public repos.
#'
#' @return Character string (the token) or `NULL` when no credentials
#'   are available.
#'
#' @seealso
#' [check_github_token()]
#'
#' @keywords internal
gh_token <- function() {
  env_token <- Sys.getenv(c("GITHUB_PAT", "GITHUB_TOKEN"))
  env_token <- env_token[nzchar(env_token)]
  if (length(env_token) > 0) {
    return(unname(env_token[1]))
  }

  creds <- tryCatch(
    gitcreds::gitcreds_get(),
    error = function(e) NULL
  )

  if (is.null(creds)) {
    return(NULL)
  }

  creds$password
}


#' Validate GitHub credentials
#'
#' @description
#' Checks that a GitHub Personal Access Token (PAT) is available and
#' returns a redacted copy. This is a rate-limit guard: authenticated
#' requests have 5000 req/hr vs 60 for unauthenticated.
#'
#' The `PIP-Technical-Team` org is public, so this is not a security
#' requirement; it is a reliability requirement for installation
#' functions.
#'
#' @return An invisible list of class `"metapip_token"` with all
#'   credential fields blanked. The `print.metapip_token()` method
#'   shows `""` in place of the actual token -- the real value is never
#'   exposed.
#'
#' @note Install functions ([install_branch()], [install_pip_packages()])
#'   resolve credentials via `gitcreds` independently; this function is
#'   a validation gate, not a token carrier.
#'
#' @section Errors:
#' Aborts with an instructional message if no git installation is found
#' or no credentials are stored.
#'
#' @seealso
#' [gh_token()]
#'
#' @examples
#' \dontrun{
#' check_github_token() |> print()
#' }
#'
#' @export
check_github_token <- function() {
  token <- gh_token()
  if (!is.null(token)) {
    redacted <- list(
      username = "", password = "", protocol = "https"
    )
    class(redacted) <- c("metapip_token", "list")
    return(invisible(redacted))
  }

  tryCatch(
    expr = {
      creds <- gitcreds::gitcreds_get()
    },
    gitcreds_nogit_error = function(e) {
      cli::cli_abort("{gitcreds_msg(\"no_git\")}")
    },
    gitcreds_no_credentials = function(e) {
      cli::cli_abort("{gitcreds_msg(\"no_creds\")}")
    }
  )

  redacted <- lapply(creds, function(x) {
    if (is.character(x)) "" else x
  })
  class(redacted) <- c("metapip_token", "list")
  invisible(redacted)
}


#' @export
print.metapip_token <- function(x, ...) {
  cli::cat_line("<metapip_token (redacted)>")
  fields <- vapply(names(x), function(nm) {
    val <- x[[nm]]
    if (is.character(val) && nzchar(val)) val <- ""
    if (is.character(val)) val <- dQuote(val)
    sprintf("%s: %s", nm, paste(val, collapse = ", "))
  }, character(1))
  cli::cat_bullet(fields)
  invisible(x)
}


check_package_condition <- function(package) {
  if (length(package) != 1L) {
    cli::cli_abort("Please enter a single package name.")
  }
  is_core(package)
}

is_core <- function(package) {
  if (!all(package %in% core)) {
    cli::cli_abort(
      "The package is not one of {toString(core)}"
    )
  }
  return(TRUE)
}

detach_package <- function(package) {
  tryCatch(
    unloadNamespace(package),
    error = function(e) {
      cli::cli_warn(
        "Could not unload namespace for {.pkg {package}}:
        {conditionMessage(e)}.
        Restart R after installation to guarantee the new code is active."
      )
    }
  )
  invisible()
}


#' Move row names to a column
#'
#' @description
#' A lightweight alternative to `tibble::rownames_to_column()` that does
#' not require the tibble package. Prepends the row names of a data.frame
#' as a new column.
#'
#' @param data A data.frame.
#' @param var Character scalar. Name of the new column to hold row names.
#'
#' @return A data.frame with the same columns as `data` plus the new
#'   column `var` as the first column. Row names are removed.
#'
#' @examples
#' df <- data.frame(x = 1:3, row.names = c("a", "b", "c"))
#' rowname_to_column(df, "id")
#'
#' @export
rowname_to_column <- function(data, var) {
  rn <- rownames(data)
  out <- add_vars(data, rn = rn, pos = "front")
  names(out)[1] <- var
  rownames(out) <- NULL
  out
}


#' Detect RStudio theme for colorDF display settings (internal)
#'
#' @return A list with elements `editor`, `global`, `dark`,
#'   `foreground`, `background`. Returns an empty template when
#'   RStudio is not available.
#'
#' @keywords internal
rs_theme <- function() {
  rstudio_theme <- template <-
    list(editor     = "",
         global     = "",
         dark       = FALSE,
         foreground = "",
         background = "")

  if (Sys.getenv("RSTUDIO") == "1") {
    if ("rstudioapi" %in% rownames(utils::installed.packages())) {
      rstudio_theme <- tryCatch(
        rstudioapi::getThemeInfo(),
        error = \(e) template,
        silent = TRUE
      )
    }
  }
  invisible(rstudio_theme)
}


#' Set colorDF theme based on RStudio theme (internal)
#'
#' @description
#' Detects the current RStudio dark/light theme and sets the
#' `colorDF_theme` option accordingly (`"wb"` for dark, `"bw"` for
#' light).
#'
#' @return Invisible list. The RStudio theme information from
#'   [rs_theme()].
#'
#' @keywords internal
set_colorDF <- function() {
  rstudio_theme <- rs_theme()
  if (rstudio_theme$dark) {
    options(colorDF_theme = "wb")
  } else {
    options(colorDF_theme = "bw")
  }
  invisible(rstudio_theme)
}