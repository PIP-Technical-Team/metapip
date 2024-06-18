#' Install latest branch from core packages
#'
#' @param package one (or more) of core packages. default NULL would install latest branch for all packages
#'
#' @return NULL
#' @examples
#' \dontrun{
#'   install_latest_branch()
#'   install_latest_branch(c("pipfun", "pipapi"))
#'}
#' @export
#'
install_latest_branch <- function(package = NULL) {
  check_github_token()
  if(!is.null(package)) is_core(package)
  else package <- core
  dat <- lapply(cli::cli_progress_along(package), get_latest_branch_update) |>
          rowbind()
  Map(\(x, y) install_branch(x, y), dat$package, dat$branch_name)
  NULL
}


#' Install one (or more) pip core packages from a specific branch.
#'
#' @description
#' This includes packages like pipapi, pipaux, pipload, wbpip, pipfun, pipdata and pipr
#'
#' @param package one (or more) of the core package name, if NULL all the core packages are installed from the branch
#' @param branch valid branch name (default "PROD")
#'
#' @return invisible NULL
#'
#' @examples
#' \dontrun{
#' install_pip_packages(branch = "test")
#' }
#'
#' @export
#'
install_pip_packages <- function(package = NULL, branch = "PROD") {
  check_github_token()
  if (is.null(package)) {
    package = core
  } else {
    is_core(package)
  }
  lapply(cli::cli_progress_along(package),
         \(x) {
           tryCatch(
             expr = {
               # Your code...
               install_branch(package = x, branch)
             },
             # end of expr section

             error = function(e) {
               cli::cli_alert_danger("package {.pkg {x}} could not be installed")
             },
             # end of error section

             warning = function(w) {
               cli::cli_alert_warning("package {.pkg {x}} produces warnings during installation")
             }
           ) # End of trycatch
         })
  return(invisible(NULL))
}


#' Install branch from a package
#'
#' @param package one of the core package name (default "pipapi")
#' @param branch valid branch name (default "DEV")
#'
#' @examples
#' \dontrun{
#'   install_branch()
#'   install_branch("pipfun", "ongoing")
#'}
#'
#' @export
#'
install_branch <- function(package = "pipapi", branch = "DEV") {
  check_github_token()
  check_package_condition(package)
  if(length(branch) != 1L) cli::cli_abort("Please enter a single branch name.")
  detach_package(package)

  br <- get_branches(package, display = FALSE)

  if(!branch %in% br) cli::cli_abort("Not a valid branch name for the package {package}. Select one of {toString(br)}")
  cli::cli_alert_info(glue::glue("Installing branch {branch} from package {package}"))
  remotes::install_github(glue::glue("PIP-Technical-Team/{package}@{branch}"))
}


