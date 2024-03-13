#' Convert a Camera Trap Data Package
#'
#' Converts a Camera Trap Data Package object that uses an older version of
#' the Camtrap DP standard to a newer version (currently `1.0`).
#'
#' @inheritParams version
#' @param convert_to Version to convert to.
#' @return Converted Camera Trap Data Package object.
#' @noRd
convert <- function(camtrapdp, convert_to = "1.0") {
  # Convert until the version number matches the expected version
  while(version(camtrapdp) != convert_to) {
    camtrapdp <- switch(
      version(camtrapdp),
      "0.1.6" = convert_0.1.6_to_1.0(camtrapdp),
      "1.0" = camtrapdp
    )
  }
  camtrapdp
}

#' Convert a Camtrap DP 0.1.6 to 1.0
#' @noRd
convert_0.1.6_to_1.0 <- function(camtrapdp) {
  # TODO: conversion steps for 0.1.6
  attr(camtrapdp, "version") <- "1.0"
  camtrapdp
}
