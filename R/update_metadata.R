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
  deployments <- deployments(x)

  if (nrow(deployments) == 0) {
    x$spatial <- NULL
    return(x)
  }

  x$spatial$type <- "Polygon"

  lat_min <- min(deployments$latitude)
  lat_max <- max(deployments$latitude)
  long_min <- min(deployments$longitude)
  long_max <- max(deployments$longitude)

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
  deployments <- deployments(x)

  if (nrow(deployments) == 0) {
    x$temporal <- NULL
    return(x)
  }

  x$temporal$start <-
    deployments %>%
    dplyr::pull(.data$deploymentStart) %>%
    min() %>%
    format(format = "%Y-%m-%d")
  x$temporal$end <-
    deployments %>%
    dplyr::pull(.data$deploymentEnd) %>%
    max() %>%
    format(format = "%Y-%m-%d")
  return(x)
}

#' Update taxonomic metadata
#'
#' Sets `x$taxonomic` to unique `taxa()` found in the observations.
#' Sets `x$taxonomic` to `NULL` if there are no taxa/observations.
#'
#' @inheritParams print.camtrapdp
#' @return `x` with updated taxonomic metadata.
#' @family helper functions
#' @noRd
update_taxonomic <- function(x) {
  taxa <- taxa(x)

  if (nrow(taxa) == 0) {
    x$taxonomic <- NULL
    return(x)
  }

  # Set taxonomic
  x$taxonomic <- purrr::map(1:nrow(taxa), function(i) {
    current_row <- taxa[i, ]

    # Create taxonomic list without vernacular names
    taxonomic_list <-
      current_row %>%
      dplyr::select(-dplyr::starts_with("vernacularNames")) %>%
      as.list()

    if (any(startsWith(names(current_row), "vernacularNames"))) {
      # Group vernacular names
      vernacularNames <-
        current_row %>%
        dplyr::select(dplyr::starts_with("vernacularNames")) %>%
        dplyr::rename_with(~ stringr::str_remove(.x, "^vernacularNames\\.")) %>%
        as.list()

      # Append to taxonomic list
      taxonomic_list <- append(
        taxonomic_list,
        list("vernacularNames" = vernacularNames)
      )
    }

    return(taxonomic_list)
  })

  return(x)
}
