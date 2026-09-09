#' Downgrade a Camera Trap Data Package to Data Package v1
#'
#' Packages, schemas and resources created with frictionless >=2.0.0 follow the
#' v2 specification of Data Package, while Camtrap DP follows v1.
#' This function downgrades a package and its resources to v1.
#'
#' @inheritParams print.camtrapdp
#' @returns Downgraded `x`.
#' @family helper functions
#' @noRd
downgrade_package <- function(x) {
  # Downgrade descriptor
  x$`$schema` <- NULL # Profile expected to be present

  # Downgrade resources
  for (resource_name in frictionless::resource_names(x)) {
    resource <- frictionless::resource(x, resource_name)
    if (frictionless::version(resource) != "1.0") {
      # Set profile, remove $schema and type
      resource$profile <- "tabular-data-resource"
      resource$`$schema` <- NULL
      resource$type <- NULL
      frictionless::resource(x, resource_name) <- resource
    }
  }

  return(x)
}
