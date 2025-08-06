#' Pretty print for metapip_simplelist objects
#'
#' Prints a named list of character vectors (possibly empty) in a readable way using the cli package.
#'
#' @param x A named list of character vectors (class 'metapip_simplelist').
#' @param ... Further arguments passed to or from other methods (ignored).
#' @export
print.metapip_simplelist <- function(x, ...) {

  title <- attr(x, "title", exact = TRUE)

  if (is.null(title)) title <- "metapip simple list:"
  cli::cli_h2(title)

  if (length(x) == 0) {
    cli::cli_alert_info("(empty list)")
    return(invisible(x))
  }
  for (nm in names(x)) {
    val <- x[[nm]]
    if (length(val) == 0) {
      cli::cli_text(paste0("$", nm, ": [empty]"))
    } else {
      cli::cli_text(paste0("$", nm, ": ", paste(cli::col_blue(val), collapse = ", ")))
    }
  }
  invisible(x)
}
