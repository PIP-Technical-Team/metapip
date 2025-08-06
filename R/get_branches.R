#' Get available branches for a package
#' @description
#' Returns a vector of branch names for the package specified. If display is set to TRUE,
#' the branch names are printed in the console.
#'
#' @param package one of the core package name (default "pipapi")
#' @param display (default TRUE) do you want to display the name of the branches in the console?
#'
#' @return an invisible character vector of branches
#'
#' @examples
#' \dontrun{
#'   get_branches()
#'   get_branches("wbpip", display = FALSE)
#'}
#'
#' @export
#'
get_branches <- function(package = "pipapi", display = TRUE) {
  creds <- check_github_token()
  check_package_condition(package)
  out <- gh::gh("GET /repos/PIP-Technical-Team/{repo}/branches",
                repo = package,
                .token = creds$password)
  branches <- vapply(out, `[[`, "", "name")
  if (isTRUE(display)) {
    cli::cli_h3("These are available branches for {package} package: ")
    cli::cat_bullet(glue::glue("{branches}"))
  }
  return(invisible(branches))
}


#' Get last update time of branches in a package
#' @description
#' Returns a dataframe with information about package name, branch name, author of the last commit and
#' when was the last commit made to the branch
#'
#' @param package one of the core package (default "pipapi")
#' @param branch valid branch name for the package
#' @param display (default TRUE) do you want to display the branches in the console?
#' @return a colorDF::colorDF output with an invisible dataframe
#'
#' @examples
#' \dontrun{
#' get_branch_info()
#' get_branch_info(branch = c("PROD", "QA"), display = FALSE)
#' get_branch_info(package = "wbpip", branch = c("PROD", "QA"))
#' }
#' @export
get_branch_info <- function(package = "pipapi", branch = NULL, display = TRUE) {
  check_github_token()
  is_core(package)
  check_package_condition(package)

  branches <- get_branches(package, display = FALSE)
  if(!is.null(branch)) {
    if(!all(branch %in% branches)) {
      cli::cli_abort("{branch} is not a correct branch name. Please use one of {toString(branches)}.")
    }
    branches <- branch
  } else {
    branches <- get_package_current_branch(package)
  }
  out <- lapply(cli::cli_progress_along(branches), \(x) {
    dat <- latest_commit_for_branch(package, branches[x])
    data.frame(dat$commit$author)
  }) |> rowbind()

  res <- add_vars(out, package = rep(package, nrow(out)), branch_name = branches, pos = "front") |>
    frename(last_update_time = "date", last_commit_author_name = "name") |>
    fselect(-email)

  if(isTRUE(display)) print(colorDF::colorDF(res))
  return(invisible(res))
}


#' Get details of the branch which was last updated
#' @description
#' This function is useful to get latest branch name, author of latest commit and time it was last updated.
#'
#' @inheritParams get_branch_info
#'
#' @return colorDF::colorDF output along with an invisible single row dataframe
#'
#' @examples
#' \dontrun{
#' get_latest_branch_update()
#' get_latest_branch_update("wbpip", display = FALSE)
#' }
#' @export
#'
get_latest_branch_update <- function(package = "pipapi", display = TRUE) {
  check_github_token()
  is_core(package)
  check_package_condition(package)
  # Get info about all the branches
  out <- get_branch_info(package, display = FALSE)
  # Return only the latest information
  res <- out |>
    fsubset(branch_name != "gh-pages") |>
    fmutate(last_update_time = as.POSIXct(last_update_time, format = "%Y-%m-%dT%T")) |>
    # arrange data in descending order
    roworder(-last_update_time) |>
    # Get the 1st row (latest)
    ss(1L)

  if(isTRUE(display)) print(colorDF::colorDF(res))
  return(invisible(res))
}


#' Get default branch in PIP ecosystem
#'
#' @returns name of branch
#' @export
#'
#' @examples
#' get_default_branch()
get_default_branch <- \() {
  getOption("metapip.default_branch")
}


#' get the current branches that are meant to be used
#'
#' It does not necessarily mean that these are the branches currently installed.
#' [init_metapip] will notify you if that is the case.
#'
#' @param verbose logical: whether to display all current branches. Default is
#' @param package character: vector with name of branches. E.g., c("pipdata",
#'   "pipfaker").
#'   TRUE
#' @returns list with names of packages and branches
#' @export
#'
#' @examples
#' get_current_branches()
get_current_branches <- \(package = NULL, verbose = TRUE) {

  custom_branches <- getOption("metapip.custom_branch")
  names(custom_branches) <- gsub("_branch", "", names(custom_branches))

  default_branches <- getOption("metapip.default_branch") |>
    list() |>
    rep(length(core)) |>
    stats::setNames(core)

  if (length(custom_branches) > 0) {
    default_branches <- utils::modifyList(default_branches, custom_branches)
  }

  if (!is.null(package)) {
    default_branches <- default_branches[names(default_branches) %in% package]
    if (length(default_branches) == 0) {
      cli::cli_abort(c(x = "package{?s} {.pkg {package}} {?is/are} not available",
                       i = "Available package{?s} {?is/are} {.pkg {core}}"))
    }
  }

  attr(default_branches, "title") <- "{.pkg metapip} current branches (default in {cli::col_red('red')}):"
  attr(default_branches, "to_red") <- getOption("metapip.default_branch")
  class(default_branches) <- "metapip_simplelist"
  if (verbose) return(default_branches)
  invisible(default_branches)
}



#' Get package  current branch
#'
#' @returns named character vector with branches of package
#' @export
#' @rdname get_current_branches
#'
#' @examples
#' get_package_current_branch(c("pipdata", "pipfaker"))
get_package_current_branch <- \(package) {
  get_current_branches(package = package,
                       verbose = FALSE) |>
    unlist()
}


#' Set default custom branching options
#'
#' @param ... Named elements to be added or updated in the custom default list.
#'
#' @returns invisible custom branches
#' @export
#'
#' @examples
#' \dontrun{
#' set_custom_branch(pipr = 'main', 'pipapi' = 'DEV_v3')
#' }
set_custom_branch <- \(...) {
  new_entries <- list(...)
  if (length(new_entries) == 0) {
    cli::cli_alert_danger("no changes made to custom branches")
    return(get_custom_branch())
  }
  names(new_entries) <- paste0(names(new_entries), '_branch')
  existing_options <- getOption("metapip.custom_branch", list())
  merged <- utils::modifyList(existing_options, new_entries)
  options("metapip.custom_branch" = merged)

  get_custom_branch()
}


#' Get Custom branches
#'
#' @param package character: vector with name of branches. E.g., c("pipdata",
#'   "pipfaker"). Default return all packages whose default  branches have been
#'   customed.
#'
#' @returns Names list of packages and their corresponding branch
#' @export
#' @rdname set_custom_branch
#'
#'
#' @examples
#' get_custom_branch()
get_custom_branch <- \(package = NULL) {

  existing_branches        <- getOption("metapip.custom_branch", list())
  names(existing_branches) <-  gsub("_branch", "", names(existing_branches))
  # names of existing branches
  neb <- names(existing_branches)

  if (!is.null(package)) {
    existing_branches <- existing_branches[names(existing_branches) %in% branch]
  }

  if (length(existing_branches) == 0) {
    cli::cli_abort(c(x = "package{?e/s} {.field {package}} {?is/are} not available.",
                     i = "package{?s} available {?is/are} {.emph {neb}}"))
  }

  attr(existing_branches, "title") <- "{.pkg metapip} custom branches:"
  class(existing_branches) <- "metapip_simplelist"
  print(existing_branches)

}
