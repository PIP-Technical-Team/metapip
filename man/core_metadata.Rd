% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core_metadata.R
\name{core_metadata}
\alias{core_metadata}
\title{Return meta information about core packages}
\usage{
core_metadata(package = NULL)
}
\arguments{
\item{package}{One (or more) core packages, if NULL shows information about all of them.}
}
\value{
a knitr::kable output with details about the core packages
}
\description{
Return meta information about core packages
}
\examples{
\dontrun{
core_metadata()
core_metadata(c("pipapi", "wbpip"))
}
}
