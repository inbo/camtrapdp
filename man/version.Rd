% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/version.R
\name{version}
\alias{version}
\title{Get Camtrap DP version}
\usage{
version(x)
}
\arguments{
\item{x}{Camera Trap Data Package object, as returned by
\code{\link[=read_camtrapdp]{read_camtrapdp()}}.
Also works on a Frictionless Data Package, as returned by
\code{\link[frictionless:read_package]{frictionless::read_package()}}.}
}
\value{
Camtrap DP version number (e.g. \code{1.0}).
}
\description{
Extracts the version number used by a Camera Trap Data Package object.
This version number indicates what version of the \href{https://camtrap-dp.tdwg.org}{Camtrap DP standard} was used.
}
\details{
The version number is derived as follows:
\enumerate{
\item The \code{version} attribute, if defined.
\item A version number contained in \code{x$profile}, which is expected to
contain the URL to the used Camtrap DP standard.
\item \code{x$profile} in its entirety (can be \code{NULL}).
}
}
\examples{
x <- example_dataset()
version(x)
}
\concept{misc functions}
