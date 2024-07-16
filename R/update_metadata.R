#' Updates spatial metadata, that is, the bounding box of polygon coordinates.
#'
#' @inheritParams check_camtrapdp
#' @return `x` with updated spatial metadata
#' @family helper functions
#' @noRd
update_spatial <- function(x) {
  if (nrow(deployments(x)) == 0) {
    x$spatial <- NULL
  } else { # otherwise get bounding box data
    long_max <- max(deployments(x)$longitude)
    long_min <- min(deployments(x)$longitude)
    lat_max <- max(deployments(x)$latitude)
    lat_min <- min(deployments(x)$latitude)

    x$spatial$bbox <- c(long_min, lat_min, long_max, lat_max)

    x$spatial$coordinates <-
      array(
        c(
          long_min, long_max, long_max, long_min, long_min,
          lat_min, lat_min, lat_max, lat_max, lat_min
        ),
        dim = c(1, 5, 2)
      )
  }
  return(x)
}

#' Updates temporal metadata, that is, the start and end date.
#'
#' @inheritParams check_camtrapdp
#' @return `x` with updated temporal metadata
#' @family helper functions
#' @noRd
update_temporal <- function(x) {
  if (nrow(deployments(x)) == 0) {
    x$temporal <- NULL
  } else {
    x$temporal$start <-
      deployments(x) %>%
      dplyr::pull(.data$deploymentStart) %>%
      min() %>%
      format(format = "%Y-%m-%d")
    x$temporal$end <-
      deployments(x) %>%
      dplyr::pull(.data$deploymentEnd) %>%
      max() %>%
      format(format = "%Y-%m-%d")
  }
  return(x)
}

#' Updates taxonomic metadata, that is, the list of species observed.
#'
#' @inheritParams check_camtrapdp
#' @return `x` with updated taxonomic metadata
#' @family helper functions
#' @noRd
update_taxonomic <- function(x) {
  remaining_taxa <-
    observations(x) %>%
    dplyr::filter(!is.na(.data$scientificName)) %>%
    purrr::pluck("scientificName") %>%
    unique()

  if (is.null(x$taxonomic)) {
    x$taxonomic <- purrr::map(remaining_taxa, ~ list(scientificName = .x))
  } else {
    x$taxonomic <-
      purrr::keep(
        x$taxonomic, ~ purrr::pluck(.x, "scientificName") %in% remaining_taxa
      )
  }

  return(x)
}
