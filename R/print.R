#' Pretty print for metapip_simplelist objects
#'
#' Prints a named list of character vectors (possibly empty) in a readable way using the cli package.
#'
#' @param x A named list of character vectors (class 'metapip_simplelist').
#' @param ... Further arguments passed to or from other methods (ignored).
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
  # Build label and value vectors
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
  # Align values using cli::ansi_align, following attach.R style

  clean_values <- gsub(cli::ansi_regex(), "", values, perl = TRUE)

  aligned <- paste0(
    cli::col_green(cli::symbol$circle_dotted), " ", cli::col_yellow(format(labels)), ": ",
    cli::ansi_align(values, max(nchar(clean_values)))) |>
    paste(collapse = "\n")

  msg(aligned)
  invisible(x)
}
