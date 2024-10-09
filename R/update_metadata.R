#' Update spatial metadata
#'
#' Sets `x$spatial` to a bounding box (expressed as geojson) that encompasses
#' the deployment coordinates or `NULL` if there are no deployments.
#'
#' @inheritParams print.camtrapdp
#' @return `x` with updated spatial metadata.
#' @family helper functions
#' @noRd
update_spatial <- function(x) {
  if (nrow(deployments(x)) == 0) {
    x$spatial <- NULL
    return(x)
  }

  lat_min <- min(deployments(x)$latitude)
  lat_max <- max(deployments(x)$latitude)
  long_min <- min(deployments(x)$longitude)
  long_max <- max(deployments(x)$longitude)

  x$spatial$coordinates <- array(
    c(
      long_min, long_max, long_max, long_min, long_min,
      lat_min, lat_min, lat_max, lat_max, lat_min
    ),
    dim = c(1, 5, 2)
  )
  return(x)
}

#' Update temporal metadata
#'
#' Sets `x$temporal$start` to the earliest deployment start date,
#' `x$temporal$end` to the latest deployment end date.
#' Sets `x$temporal` to `NULL` if there are no deployments.
#'
#' @inheritParams print.camtrapdp
#' @return `x` with updated temporal metadata.
#' @family helper functions
#' @noRd
update_temporal <- function(x) {
  if (nrow(deployments(x)) == 0) {
    x$temporal <- NULL
    return(x)
  }

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
  return(x)
}

#' Update taxonomic metadata
#'
#' Filters existing taxa in `x$taxonomic` on the scientific names found in the
#' observations or creates a `x$taxonomic` with a unique list of those
#' scientific names.
#' Sets `x$taxonomic` to `NULL` if there are no observations.
#'
#' @inheritParams print.camtrapdp
#' @return `x` with updated taxonomic metadata.
#' @family helper functions
#' @noRd
update_taxonomic <- function(x) {
  if (nrow(observations(x)) == 0) {
    x$taxonomic <- NULL
    return(x)
  }

  current_taxa <- purrr::pluck(taxa(x), "scientificName") %>% sort()
  # Set taxonomic
  if (is.null(x$taxonomic)) {
    x$taxonomic <- purrr::map(current_taxa, ~ list(scientificName = .x))

  # Update taxonomic
  } else {
    x$taxonomic <- purrr::keep(
      x$taxonomic, ~ purrr::pluck(.x, "scientificName") %in% current_taxa
    )
  }
  return(x)
}
