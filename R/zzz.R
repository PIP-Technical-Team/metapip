.onAttach <- function(...) {

  # set display options ------
  # Check if running in RStudio
  if (Sys.getenv("RSTUDIO") == "1") {
    # Attempt to infer theme or notify the user to set the theme if using a
    # newer RStudio version without `rstudioapi` support

    # If possible, use `rstudioapi` to get theme information (works only in certain versions)
    if ("rstudioapi" %in% rownames(installed.packages())) {
      try({
        theme <- rstudioapi::getThemeInfo()
        if (theme$dark) {
          options(colorDF_theme = "wb")
        } else {
          options(colorDF_theme = "bw")
        }
      },
      silent = TRUE)
    }
  } else {
    # For non-RStudio environments, assume light theme
    options(colorDF_theme = "bw")
  }

  # Notify user of the theme setting
  message("Using ",
          getOption("colorDF_theme"),
          " theme for console output.")



  # Load the core packages --------
  needed <- core[!is_attached(core)]
  if (length(needed) == 0) {
    return()
  }

  metapip_attach()


}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}
