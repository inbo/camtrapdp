#' Merge two character vectors and prefix duplicates
#'
#' Merges two character vectors and adds a prefix to any values in `a` that
#' occur in `b` and vice versa.
#'
#' @param a,b Character vectors.
#' @param prefixes Prefixes (e.g. `c("a", "b")`) to add to `a` and `b` values
#'   respectively, as `<prefix>_<value>`.
#' @return `c(a, b)` with prefixed duplicates.
#' @family helper functions
#' @noRd
#' @examples
#' a <- c("k", "l", "m", "n", 1, 1, NA)
#' b <- c(          "m",      1, 1, "o", NA)
#' merge_vectors(a, b, prefixes = c("a", "b"))
merge_vectors <- function(a, b, prefixes) {
  a_prefix <- prefixes[1]
  b_prefix <- prefixes[2]
  a_prefixed <- purrr:::map2_chr(a, a %in% b,
    ~ if (!is.na(.x) && .y) { paste(a_prefix, .x, sep = "_") } else { .x }
  )
  b_prefixed <- purrr:::map2_chr(b, b %in% a,
    ~ if (!is.na(.x) && .y) { paste(b_prefix, .x, sep = "_") } else { .x }
  )
  c(a_prefixed, b_prefixed)
}

#' Merge resources
#'
#' Merges the resources of Camera Trap Data Package `x` with the additional
#' resources of `y`.
#' Resource names that occur in both `x` and `y` get a prefix.
#' @inheritParams merge_camtrapdp
#' @param prefixes Prefixes (e.g. `c("x", "y")`) to add to duplicate values.
#' @return A list with resources.
#' @family helper functions
#' @noRd
merge_resources <- function(x, y, prefixes) {
  # Combine all resources of x with the additional resources of y
  x_resources <- x$resources
  y_resources <- purrr::keep(y$resources, ~ .x$name %in% additional_resources(y))
  resources <- c(x_resources, y_resources)

  # Prefix duplicate resource names
  resource_names <- merge_vectors(
    purrr::map_chr(x_resources, "name"),
    purrr::map_chr(y_resources, "name"),
    prefixes
  )
  update_name <- function(resource, name) {
    resource$name <- name
    return(resource)
  }
  purrr::map2(resources, resource_names, update_name)
}

#' Merge deployments
#'
#' Merges the deployments of Camera Trap Data Packages `x` and `y`.
#' Values in `deploymentID` that occur in both `x` and `y` get a prefix.
#' @inheritParams merge_camtrapdp
#' @param prefixes Prefixes (e.g. `c("x", "y")`) to add to duplicate values.
#' @return A [tibble::tibble()] data frame with deployments.
#' @family helper functions
#' @noRd
merge_deployments <- function(x, y, prefixes) {
  x_deployments <- deployments(x)
  y_deployments <- deployments(y)
  dplyr::bind_rows(x_deployments, y_deployments) %>%
    dplyr::mutate(
      deploymentID = merge_vectors(
        purrr::chuck(x_deployments, "deploymentID"),
        purrr::chuck(y_deployments, "deploymentID"),
        prefixes
      )
    )
}

#' Merge media
#'
#' Merges the media of Camera Trap Data Packages `x` and `y`.
#' Values in `mediaID`, `deploymentID` or `eventID` that occur in both `x` and
#' `y` get a prefix.
#' @inheritParams merge_camtrapdp
#' @param prefixes Prefixes (e.g. `c("x", "y")`) to add to duplicate values.
#' @return A [tibble::tibble()] data frame with media.
#' @family helper functions
#' @noRd
merge_media <- function(x, y, prefixes) {
  x_media <- media(x)
  y_media <- media(y)
  dplyr::bind_rows(x_media, y_media) %>%
    dplyr::mutate(
      mediaID = merge_vectors(
        purrr::chuck(x_media, "mediaID"),
        purrr::chuck(y_media, "mediaID"),
        prefixes
      ),
      deploymentID = merge_vectors(
        purrr::chuck(x_media, "deploymentID"),
        purrr::chuck(y_media, "deploymentID"),
        prefixes
      ),
      eventID = merge_vectors(
        purrr::pluck(x_media, "eventID"),
        purrr::pluck(y_media, "eventID"),
        prefixes
      )
    )
}

#' Merge observations
#'
#' Merges the observations of Camera Trap Data Packages `x` and `y`.
#' Values in `observationID`, `deploymentID`, `mediaID` or `eventID` that occur
#' in both `x` and `y` get a prefix.
#' @inheritParams merge_camtrapdp
#' @param prefixes Prefixes (e.g. `c("x", "y")`) to add to duplicate values.
#' @return A [tibble::tibble()] data frame with observations.
#' @family helper functions
#' @noRd
merge_observations <- function(x, y, prefixes) {
  x_observations <- observations(x)
  y_observations <- observations(y)
  dplyr::bind_rows(x_observations, y_observations) %>%
    dplyr::mutate(
      observationID = merge_vectors(
        purrr::chuck(x_observations, "observationID"),
        purrr::chuck(y_observations, "observationID"),
        prefixes
      ),
      deploymentID = merge_vectors(
        purrr::chuck(x_observations, "deploymentID"),
        purrr::chuck(y_observations, "deploymentID"),
        prefixes
      ),
      mediaID = merge_vectors(
        purrr::pluck(x_observations, "mediaID"),
        purrr::pluck(y_observations, "mediaID"),
        prefixes
      ),
      eventID = merge_vectors(
        purrr::pluck(x_observations, "eventID"),
        purrr::pluck(y_observations, "eventID"),
        prefixes
      )
    )
}
