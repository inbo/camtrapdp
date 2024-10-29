#' Add a prefix to duplicates
#'
#' Compares two character vectors and adds a prefix to any values in `a` that
#' also occur in `b`.
#'
#' @param a,b Character vectors.
#' @param prefix Prefix to add, as `<prefix>_<value>`.
#' @return `a` with updated values.
#' @family helper functions
#' @noRd
#' @examples
#' a <- c("a", "a", "b", "c", 1, 1, NA)
#' b <- c(          "b",      1, 1, "e", NA)
#' prefix_duplicates(a, b, "prefix")
prefix_duplicates <- function(a, b, prefix) {
  duplicates <- a %in% b
  a_updated <- purrr::map2_chr(
    a,
    duplicates,
    ~ if(.y && !is.na(.x)) paste(prefix, .x, sep = "_") else .x
  )
  return(a_updated)
}

#' Add a prefix to identifiers in Camera Trap Data Package that occur in another
#'
#' Compares two Camera Trap Data Package objects (`x` and `y`) and adds a prefix
#' to any relevant identifiers in `x` that also occur in `y`:
#' - `deploymentID` (in deployments, media and observations)
#' - `mediaID` (in media and observations)
#' - `observationID` (in observations)
#' - `eventID` (in media and observations)
#'
#' @param x,y Camera Trap Data Package objects to compare
#' @param prefix Prefix to add, as `<prefix>_<identifier>`.
#' @return `x` with updated identifiers.
#' @family helper functions
#' @noRd
prefix_identifiers <- function(x, y, prefix) {
  y_deployment_ids <- purrr::pluck(deployments(y), "deploymentID")
  y_media_ids <- purrr::pluck(media(y), "mediaID")
  y_observation_ids <- purrr::pluck(observations(y), "observationID")
  y_event_ids <- purrr::pluck(observations(y), "eventID")

  deployments(x) <-
    deployments(x) %>%
    dplyr::mutate(
      deploymentID = prefix_duplicates(deploymentID, y_deployment_ids, prefix)
    )
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      deploymentID = prefix_duplicates(deploymentID, y_deployment_ids, prefix),
      mediaID = prefix_duplicates(mediaID, y_media_ids, prefix),
      eventID = prefix_duplicates(eventID, y_event_ids, prefix)
    )
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      deploymentID = prefix_duplicates(deploymentID, y_deployment_ids, prefix),
      observationID = prefix_duplicates(observationID, y_observation_ids, prefix),
      mediaID = prefix_duplicates(mediaID, y_media_ids, prefix),
      eventID = prefix_duplicates(eventID, y_event_ids, prefix)
    )

  return(x)
}
