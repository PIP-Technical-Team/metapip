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
package_branches  <- function(package = NULL) {
  check_github_token()
  if(!is.null(package)) is_core(package)
  else package <- core
  # For each of the core packages, get all the branches
  # From every branch, get the version of the package
  all_package_version <- sapply(package, \(x) {
    cli::cli_alert_info("Getting package versions for all branches of {x} package")
    br = get_branches(x, display = FALSE)
    br = br[br != "gh-pages"]
    urls <- glue::glue("https://raw.githubusercontent.com/PIP-Technical-Team/{x}/{br}/DESCRIPTION")
    sapply(urls, \(y) {
      mat <- read.dcf(url(y))
      mat[, "Version"]
    })
  }, simplify = FALSE)

  complete_data <- all_package_version %>%
    utils::stack() %>%
    tibble::rownames_to_column(var = "branch") %>%
    dplyr::rename(version = .data$values, package = .data$ind) %>%
    dplyr::mutate(branch = stringr::str_extract(.data$branch, "([0-9A-Za-z-_]+)/DESCRIPTION\\.Version", group = 1))

    common <- complete_data %>%
      dplyr::filter(.data$branch %in% c("PROD", "DEV", "QA")) %>%
      tidyr::pivot_wider(names_from = "branch", values_from = "version") %>%
      dplyr::relocate("package", "PROD")

    result <- complete_data %>%
      dplyr::filter(!.data$branch %in% c("PROD", "DEV", "QA")) %>%
      split(.$package) %>%
      purrr::map(., ~.x %>% dplyr::select(-"package"))

    # Show local installation
    local <-  purrr::map_df(package, \(.x) {
      out <- utils::packageDescription(.x, fields = c("GithubRef", "Version"))
      tibble::tibble(package = .x, local_branch = out$GithubRef, local_version = out$Version)
    })
    # DEV data
    dev <- complete_data %>% dplyr::filter(.data$branch %in% "DEV")
    # Join dev data with local data to create status column
    local <- dplyr::full_join(local, dev, dplyr::join_by(package)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(status = utils::compareVersion(.data$version, .data$local_version)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(status = dplyr::case_when(status == 1 ~ "behind",
                                              status == -1 ~ "ahead",
                                              TRUE ~ "up-to-date")) %>%
      dplyr::select(-"branch", -"version")
    c(list(common = common, local = local), result)
}


