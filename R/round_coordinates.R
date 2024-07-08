#' Round coordinates to generalize camera trap locations
#'
#' Rounds deployment coordinates to a certain number of digits to
#' fuzzy/generalize camera trap locations.
#' This function can be used before publishing data in order to protect
#' sensitive species and/or prevent theft of active cameras.
#'
#' @inheritParams check_camtrapdp
#' @param digits Number of decimal places to round coordinates to (`1`,
#'   `2` or `3`).
#' @return `x` with rounded coordinates, updated `coordinatePrecision` in
#'   metadata and updated `coordinateUncertainty` in deployments.
#' @family transformation functions
#' @export
#' @section Details:
#' Rounding coordinates is a recommended method to generalize sensitive
#' biodiversity information (see [Section 4.2](
#' https://doi.org/10.15468/doc-5jp4-5g10#s-generalization) in Chapman 2020).
#' Use this function to do so for your data.
#' Determine the category of sensitivity (see [Section 2.2](
#' https://docs.gbif.org/sensitive-species-best-practices/master/en/#table-06)
#' in Chapman 2020) and choose the associated number of digits :
#'
#' category | sensitivity | digits
#' --- | --- | ---
#' category 1 | extreme | (do not publish)
#' category 2 | high | 1
#' category 3 | medium | 2
#' category 4 | low | 3 | 0.001
#' not sensitive | not sensitive | all (do not use this function)
#'
#' The function will then:
#'
#' 1. Round all coordinates in the deployments to the selected number of digits.
#'
#' 2. Set the `coordinatePrecision` in the metadata (original values will be
#'     overwritten):
#'
#'     digits | coordinatePrecision
#'     --- | ---
#'     1 | 0.1
#'     2 | 0.01
#'     3 | 0.001
#'
#' 3. Update the `coordinateUncertainy` (in meters) in the deployments.
#'     This uncertainty is based on the number of digits and the latitude,
#'     following [Table 3](
#'     https://doi.org/10.15468/doc-gg7h-s853#table-uncertainty) in Chapman &
#'     Wieczorek 2020:
#'
#'     digits | 0° latitude | 30° latitude | 60° latitude | 85° latitude
#'     --- | --- | --- | --- | ---
#'     1 | 15691 m | 14697 m | 12461 m | 11211 m
#'     2 | 1570 m | 1470 m | 1246 m | 1121 m
#'     3 | 157 m | 147 m | 125 m | 112 m
#'
#'     If a `coordinatePrecision` is already present, the function will subtract
#'     the `coordinateUncertainty` associated with it before setting a new
#'     uncertainty (e.g. `0.001` to `0.01` = `original value - 157 + 1570 m`).
#'     If `original value` is `NA`, the function will assume the coordinates
#'     were obtained by GPS and set `original value = 30`.
#'
#' @examples
#' x <- example_dataset()
#'
#' # Original precision
#' x$coordinatePrecision
#'
#' # Original coordinates and uncertainty
#' deployments(x)[c("latitude","longitude","coordinateUncertainty")]
#'
#' # Round coordinates to 1 digit
#' x_rounded <- round_coordinates(x, 1)
#'
#' # Updated coordinatePrecision
#' x_rounded$coordinatePrecision
#'
#' # Updated coordinates and uncertainty (original 187 - 147 + 14697 = 14737)
#' deployments(x_rounded)[c("latitude","longitude","coordinateUncertainty")]
round_coordinates <- function(x, digits = 3) {
  if (is.null(digits) || !(digits %in% c(1, 2, 3))) {
    cli::cli_abort(
      c("{.arg digits} must be {.val 1}, {.val 2} or {.val 3}."),
      class = "camtrapdp_error_digits_invalid"
    )
  }

  # Detect original number of digits from coordinatePrecision or data
  original_precision <- x$coordinatePrecision
  if (!is.null(original_precision)) {
    original_digits <- -log10(original_precision) # 0.001 -> 3
    if (digits >= original_digits) {
      cli::cli_abort(
        c(
          "Can't round from {original_digits} to {digits} digits. ",
          "`{original_digits}` is derived from the ",
          "`x$coordinatePrecision={original_precision}`."
        ),
        class = "camtrapdp_error_precision"
      )
    }
  } else {
    original_digits <-
      deployments(x) %>%
      dplyr::mutate(
        lat_digits = nchar(gsub("\\..*", "", .data$latitude))
      ) %>%
      dplyr::summarize(max(.data$lat_digits)) %>%
      dplyr::pull()
    if (digits >= original_digits) {
      cli::cli_abort(
        c(
          "Can't round from {original_digits} to {digits} digits. ",
          "`{original_digits}` is the maximum number of decimals for latitude ",
          "in the data."
        ),
        class = "camtrapdp_error_precision_max"
      )
    }
  }

  # Set uncertainties per latitude and number of digits
  uncertainty <- list( # 0.1 degree, 0.01 degree, 0.001 degree
    "lat_0" = c(15691, 1570, 157),
    "lat_30" = c(14697, 1470, 147),
    "lat_60" = c(12461, 1246, 125),
    "lat_85" = c(11211, 1121, 112)
  )

  # Update longitude, latitude and coordinateUncertainty
  x$data$deployments <-
    deployments(x) %>%
    dplyr::mutate(
      latitudeGroup = dplyr::case_when(
        abs(latitude) >= 85 ~ "lat_85",
        abs(latitude) >= 60 ~ "lat_60",
        abs(latitude) >= 30 ~ "lat_30",
        .default = "lat_0"
      ),
      roundingUncertainty = purrr::map_dbl(
        .data$latitudeGroup, ~ uncertainty[[.]][digits]
      ),
      longitude = round(.data$longitude, digits),
      latitude = round(.data$latitude, digits),
      coordinateUncertainty =
        dplyr::case_when(
          # No uncertainty in data: assume 30, add rounding uncertainty
          is.na(.data$coordinateUncertainty) ~ 30 + roundingUncertainty,
          is.null(original_precision) ~
            .data$coordinateUncertainty + .data$roundingUncertainty,
          # Otherwise: subtract old rounding uncertainty, add new rounding
          # uncertainty
          TRUE ~ .data$coordinateUncertainty
            - purrr::map_dbl(.data$latitudeGroup, ~ uncertainty[[.]][original_digits])
            + .data$roundingUncertainty
        )
    ) %>%
    dplyr::select(-"latitudeGroup", -"roundingUncertainty")

  # Update coordinatePrecision
  x$coordinatePrecision <- 1 / 10^digits

  return(x)
}
