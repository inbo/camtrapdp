% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_observations.R
\name{filter_observations}
\alias{filter_observations}
\title{Filter observations}
\usage{
filter_observations(x, ...)
}
\arguments{
\item{x}{Camera Trap Data Package object, as returned by \code{\link[=read_camtrapdp]{read_camtrapdp()}}.}

\item{...}{Filtering conditions, see \code{dplyr::filter()}.}
}
\value{
\code{x} filtered.
}
\description{
Subsets observations in a Camera Trap Data Package object, retaining all rows
that satisfy the conditions.
\itemize{
\item Deployments are not filtered.
\item Media are filtered on associated \code{mediaID} (for media-based observations)
and \code{eventID} (for event-based observations).
Filter on \code{observationLevel == "media"} to only retain directly linked media.
\item Metadata (\code{x$taxonomic}) are updated to match the filtered observations.
}
}
\examples{
x <- example_dataset()

# Filtering returns x, so pipe with observations() to see the result
x \%>\%
  filter_observations(observationType == "animal") \%>\%
  observations()

# Filtering on observations also affects associated media, but not deployments
x \%>\%
  filter_observations(
    scientificName == "Vulpes vulpes",
    observationLevel == "event"
  ) \%>\%
  media()
x \%>\%
  filter_observations(
    scientificName == "Vulpes vulpes",
    observationLevel == "media"
  ) \%>\%
  media()

# Filtering on multiple conditions (combined with &)
x \%>\%
  filter_observations(
    deploymentID == "577b543a",
    scientificName \%in\% c("Martes foina", "Mustela putorius")
  ) \%>\%
  observations()

# Filtering on datetimes is easiest with lubridate
library(lubridate, warn.conflicts = FALSE)
x \%>\%
  filter_observations(
    eventStart >= lubridate::as_datetime("2020-06-19 22:00:00"),
    eventEnd <= lubridate::as_datetime("2020-06-19 22:10:00")
  ) \%>\%
  observations()
}
\seealso{
Other filter functions: 
\code{\link{filter_deployments}()},
\code{\link{filter_media}()}
}
\concept{filter functions}
