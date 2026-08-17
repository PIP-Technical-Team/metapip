#' Pretty print for metapip_simplelist objects
#'
#' S3 method that prints a named list of character vectors in a readable,
#' aligned format using the cli package. Default branches are shown in
#' red; custom branches are shown in blue.
#'
#' @param x A named list of character vectors with class
#'   `"metapip_simplelist"`. Attributes:
#'   - `title`: cli-formatted header string.
#'   - `to_red`: character scalar to highlight in red.
#'
#' @param ... Additional arguments passed to or from other methods
#'   (currently ignored).
#'
#' @return `x` invisibly (for use in pipelines).
#'
#' @export
print.metapip_simplelist <- function(x, ...) {
  title <- attr(x, "title", exact = TRUE)
  to_red <- attr(x, "to_red", exact = TRUE)
  if (is.null(to_red)) to_red <- ""

  if (is.null(title)) title <- "metapip simple list:"
  cli::cli_h2(title)

  if (length(x) == 0) {
    cli::cli_alert_info("(empty list)")
    return(invisible(x))
  }

  labels <- names(x)
  values <- vapply(seq_along(x), \(i) {
    val <- x[[i]]
    if (length(val) == 0) {
      "[empty]"
    } else if (val == to_red) {
      paste(cli::col_red(val), collapse = ", ")
    } else {
      paste(cli::col_blue(val), collapse = ", ")
    }
  }, character(1))

  clean_values <- gsub(cli::ansi_regex(), "", values, perl = TRUE)

  aligned <- paste0(
    cli::col_green(cli::symbol$circle_dotted), " ",
    cli::col_yellow(format(labels)), ": ",
    cli::ansi_align(values, max(nchar(clean_values)))
  ) |>
    paste(collapse = "\n")

  msg(aligned)
  invisible(x)
}