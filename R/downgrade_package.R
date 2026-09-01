#' Downgrade a Camera Trap Data Package object from v2 to v1
#'
#' Downgrade the descriptor and the associated resources.
#'
#' @param x Camera Trap Data Package object, as returned by [read_camtrapdp()].
#' @returns Downgraded `x`.
#' @family downgrade functions
#' @noRd
downgrade_package <- function(x) {
  frictionless::check_package(x)

  # No quick return for v2 package
  # Packages could be mixed versions

  # Downgrade descriptor, by removing $schema property
  x$`$schema` <- NULL

  # Downgrade resources
  for (resource_name in frictionless::resource_names(x)) {
    resource <- downgrade_resource(frictionless::resource(x, resource_name))
    frictionless::resource(x, resource_name) <- resource
  }

  return(x)
}

#' Downgrade a Data Resource object from v2 to v1
#'
#' @param resource List describing a Data Resource, as returned by [frictionless::resource()].
#' @returns Downgraded `resource`.
#' @family downgrade functions
#' @noRd
downgrade_resource <- function(resource) {
  version <- frictionless::version(resource)

  # Leave resource as is if data package version 1.0
  if (version == "1.0") {
    return(resource)
  }

  # Set profile back to "tabular-data-resource"
  resource$profile <- "tabular-data-resource"

  # Remove type and $schema
  resource$type <- NULL
  resource$`$schema` <- NULL

  return(resource)
}
