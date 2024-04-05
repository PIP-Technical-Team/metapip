#' Status tables of package versions in different branches along with local installations.
#' For local installation,a status column is returned which indicates if the local version is ahead or behind the DEV branch.
#'
#' @param package One (or more) of the PIP core packages. Default NULL will include all the packages
#'
#' @return tibble of pip packages and the corresponding package versions of branch
#' @examples
#' \dontrun{
#' package_branches()
#' package_branches(c("pipapi", "wbpip"))
#' }
#'
#' @export
#'
package_branches  <- function(package = NULL,
                              branch_to_compare = "DEV") {
  check_github_token()
  if(!is.null(package)) is_core(package)
  else package <- core
  # For each of the core packages, get all the branches
  # From every branch, get the version of the package
  all_package_version <- get_package_version(package)
  complete_data <- get_complete_data(all_package_version)
  common <- common_data(complete_data)
  result <- split_packages_into_list(complete_data)
  # Get local installation
  local <-  purrr::map_df(package, \(.x) {
    out <- tryCatch(
      expr = {
        utils::packageDescription(.x, fields = c("GithubRef", "Version"))
      }, # end of expr section

      error = function(e) {
        list(GithubRef = NA_character_,
             Version   = NA_character_)
      }, # end of error section

      warning = function(w) {
        list(GithubRef = NA_character_,
             Version   = NA_character_)
      }
    ) # End of trycatch


    tibble::tibble(package = .x, local_branch = out$GithubRef, local_version = out$Version)
  })

  # DEV data
  dev <- complete_data %>% dplyr::filter(.data$branch %in% branch_to_compare)
  local <- join_and_get_status(local, dev, branch_to_compare)

  return(c(list(common = common, local = local), result))
}

#' @noRd
get_package_version <- function(package) {
  lp <- length(package)
  cli::cli_progress_bar("Getting versions for all branches of",
                        total = lp,
                        format = "{cli::col_green(cli::symbol$play)} {cli::pb_name}{.pkg {x}}")
  lr <- vector("list", length = lp)
  names(lr) <- package
  for (x in package) {
    cli::cli_progress_update()
    br = get_branches(x, display = FALSE)
    br = br[br != "gh-pages"]
    urls <- glue::glue("https://raw.githubusercontent.com/PIP-Technical-Team/{x}/{br}/DESCRIPTION")
    lr[[x]] <- sapply(urls, \(y) {
      mat <- read.dcf(url(y))
      mat[, "Version"]
    })
  }
  lr
}


#' @noRd
get_complete_data <- function(all_package_version) {
  all_package_version %>%
    utils::stack() %>%
    tibble::rownames_to_column(var = "branch") %>%
    dplyr::rename(version = .data$values, package = .data$ind) %>%
    dplyr::mutate(branch = stringr::str_extract(.data$branch, "([0-9A-Za-z-_]+)/DESCRIPTION\\.Version", group = 1))
}

#' @noRd
common_data <- function(complete_data) {
  complete_data %>%
    dplyr::filter(.data$branch %in% c("PROD", "DEV", "QA")) %>%
    tidyr::pivot_wider(names_from = "branch", values_from = "version") %>%
    dplyr::relocate("package", "PROD")
}

#' @noRd
split_packages_into_list <- function(complete_data) {
  complete_data %>%
    dplyr::filter(!.data$branch %in% c("PROD", "DEV", "QA")) %>%
    split(.$package) %>%
    purrr::map(., ~.x %>% dplyr::select(-"package"))
}

#' @noRd
join_and_get_status <- function(local, dev, branch_to_compare) {
  # Join dev data with local data to create status column
  dplyr::full_join(local, dev, dplyr::join_by("package")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(local_status = utils::compareVersion(.data$version, .data$local_version)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(local_status =
                    dplyr::case_when(local_status == 1 ~  paste("behind",branch_to_compare),
                                     local_status == -1 ~ paste("ahead", branch_to_compare),
                                     TRUE ~ "up-to-date")) %>%
    dplyr::mutate(local_status = ifelse(is.na(.data$local_version),
                                        "Not in local",
                                        local_status)) %>%
    dplyr::mutate(local_status = ifelse(is.na(.data$branch ),
                                        paste(branch_to_compare, "not in repo"),
                                        local_status)) %>%
    dplyr::select(-"branch", -"version")

}


