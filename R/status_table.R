#' Status tables of package versions in different branches
#' Apart from PROD and DEV, we are only showing those branches whose version is higher than DEV.
#'
#' @param package One (or more) of the PIP core packages. Default NULL will include all the packages
#'
#' @return tibble of pipr packages and the corresponding package versions of branch
#' @examples
#' \dontrun{
#' status_table()
#' status_table(c("pipapi", "wbpip"))
#' }
#'
#' @export
#'
status_table <- function(package = NULL) {
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
  # Keep only package version above DEV version along with master and reshape the data in wider format
  all_package_version %>%
    utils::stack() %>%
    tibble::rownames_to_column(var = "branch") %>%
    dplyr::rename(version = .data$values, package = .data$ind) %>%
    dplyr::mutate(branch = stringr::str_extract(.data$branch, "([0-9A-Za-z-_]+)/DESCRIPTION\\.Version", group = 1)) %>%
    dplyr::group_by(.data$package) %>%
    dplyr::filter(.data$branch %in% c("PROD", "DEV") |
                    purrr::map_lgl(version, ~utils::compareVersion(.x, .data$version[match("DEV", .data$branch)]) == 1)) %>%
    tidyr::pivot_wider(names_from = .data$branch, values_from = .data$version) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(.data$package, .data$PROD)
}


