% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contributors.R
\name{contributors}
\alias{contributors}
\alias{contributors<-}
\title{Get or set contributors}
\usage{
contributors(x)

contributors(x) <- value
}
\arguments{
\item{x}{Camera Trap Data Package object, as returned by \code{\link[=read_camtrapdp]{read_camtrapdp()}}.}

\item{value}{A data frame to assign as contributors.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble()}} data frame with the contributors, containing the
following columns (columns absent in \code{x$contributors} will be created):
\itemize{
\item \code{title}
\item \code{firstName}: if absent, this will be set to the first word in \code{title},
except if it is a single word or the \code{role} is \code{rightsHolder} or
\code{publisher}.
\item \code{lastName}: if absent, this will be set to the remaining words in
\code{title}, with the same exceptions as \code{firstName}.
\item \code{email}
\item \code{path}
\item \code{role}
\item \code{organization}
}
}
\description{
\code{contributors()} gets contributors from the \code{x$contributors} property in a
Camera Trap Data Package object and returns it as a tibble data frame.

\verb{contributors()<-} is the assignment equivalent.
}
\examples{
x <- example_dataset()
# Get contributors
contributors(x)

# Set contributors
contributors(x) <- head(contributors(x), 1)
}
\seealso{
Other accessor functions: 
\code{\link{deployments}()},
\code{\link{events}()},
\code{\link{individuals}()},
\code{\link{locations}()},
\code{\link{media}()},
\code{\link{observations}()},
\code{\link{taxa}()}
}
\concept{accessor functions}
