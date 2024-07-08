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
