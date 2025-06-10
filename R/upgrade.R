#' Upgrade a Camera Trap Data Package
#'
#' Upgrades a Camera Trap Data Package object that uses an older version of
#' the Camtrap DP standard to a newer version.
#'
#' @inheritParams print.camtrapdp
#' @param upgrade_to Version to upgrade to.
#' @return `x` upgraded.
#' @family upgrade functions
#' @noRd
upgrade <- function(x, upgrade_to = "1.0.2") {
  # Upgrade until the version number matches the expected version
  while (version(x) != upgrade_to) {
    x <- switch(version(x),
      "1.0" = upgrade_1.0_to_1.0.1(x),
      "1.0.1" = upgrade_1.0.1_to_1.0.2(x),
      "1.0.2" = x
    )
  }
  return(x)
}

#' Upgrade a Camtrap DP 1.0 to 1.0.1
#'
#' This is a patch version, no changes are made to the standard.
#' @noRd
upgrade_1.0_to_1.0.1 <- function(x) {
  version(x) <- "1.0.1"
  return(x)
}

#' Upgrade a Camtrap DP 1.0.1 to 1.0.2
#'
#' This is a patch version, no changes are made to the standard.
#' @noRd
upgrade_1.0.1_to_1.0.2 <- function(x) {
  version(x) <- "1.0.2"
  return(x)
}
