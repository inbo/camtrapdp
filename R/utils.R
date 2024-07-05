# HELPER FUNCTIONS

#' Updates spatial metadata, that is, the bounding box of polygon coordinates.
#'
#' @inheritParams check_camtrapdp
#' @return `x` with updated spatial metadata
#' @family helper functions
#' @noRd
build_spatial <- function(x) {
  # get bounding box data
  x_east <- max(deployments(x)$longitude)
  x_west <- min(deployments(x)$longitude)
  y_north <- max(deployments(x)$latitude)
  y_south <- min(deployments(x)$latitude)

  x$bbox <- c(x_west, y_south, x_east, y_north)

  x$coordinates <-
    array(
      c(
        x_west, x_east, x_east, x_west, x_west,
        y_south, y_south, y_north, y_north, y_south
      ),
      dim = c(1, 5, 2)
    )

  return(x)
}
