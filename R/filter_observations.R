#' Filter based on observations
#'
#' Filter a Camera Trap Data Package object based on observations. Same syntax
#' and behavior as in dplyr's `filter()` function. This function does not filter
#' deployments, so absences are retained. It does not filter media either
#' since a media file can be used by more than one observation and observations
#' are not always hard linked to media.
#'
#' @inheritParams version
#'
#' @return Camera Trap Data Package object.
#' @family filter functions
#' @export
#' @examples
#' library(dplyr)
#' dataset <- example_dataset()
#' filter_observations(dataset, observationType == "animal")
#' filter_observations(
#'   dataset,
#'   scientificName %in% c("Anas platyrhynchos", "Ardea cinerea")
#' )
#' # piping is allowed
#' dataset %>%
#'   filter_observations(
#'   observationLevel == "event", observationType == "animal"
#' )
filter_observations <- function(x, ...) {

  # Filter observations
  obs <- observations(x)
  obs <- dplyr::filter(obs, ...)

  # Assign filtered data
  x$data$observations <- obs

  return(x)
}
