#' Round coordinates to generalize camera trap locations
#'
#' Rounds deployment coordinates to a certain number of digits to
#' fuzzy/generalize camera trap locations.
#' This function can be used before publishing data in order to protect
#' sensitive species and/or prevent theft of active cameras.
#'
#' @inheritParams check_camtrapdp
#' @param digits Number of decimal places to round coordinates to (`1`,
#' `2` or `3`).
#' @return `x` with rounded coordinates as well as updated
#'   `coordinateUncertainty` (in deployments) and `coordinatePrecision` (in
#'   metadata).
#' @family transformation functions
#' @export
#' @section Details:
#' Rounding coordinates is a recommended method to generalize sensitive
#' biodiversity information (see
#' [Section 4.2](https://doi.org/10.15468/doc-5jp4-5g10#s-generalization)
#' in Chapman 2020).
#' Choose a number of digits that aligns with the sensitivity of the data and
#' notice the effect on precision and uncertainty.
#' Publish the coordinates as is (i.e. do not use this function) if the data
#' are not sensitive.
#'
#' sensitivity | digits | coordinatePrecision
#' --- | --- | ---
#' high | 1 | 0.1
#' medium | 2 | 0.01
#' low | 3 | 0.001
#'
#' For records with `coordinateUncertainty = NA` the function will assume the
#' coordinates were obtained by GPS and use `30 m` as original uncertainty,
#' before adding uncertainty caused by rounding.
#' The added rounding uncertainty is dependent on latitude and follows the
#' categories in
#' [Table 3](https://doi.org/10.15468/doc-gg7h-s853#table-uncertainty) in
#' Chapman & Wieczorek 2020.
#'
#' latitude | 0.1 degree | 0.01 degree | 0.001 degree
#' --- | --- | --- | ---
#' 0° | 15691 m | 1570 m | 157 m
#' 30° | 14697 m | 1470 m | 147 m
#' 60° | 12461 m | 1246 m | 125 m
#' 85° | 11211 m | 1121 m | 112 m
#'
#' @examples
#' # Round coordinates of example package to 3 digits
#' x <- example_dataset()
#' x_rounded2 <- round_coordinates(x, 2)
#'
#' # coordinatePrecision is set in metadata
#' x_rounded2$coordinatePrecision
#'
#' # coordinateUncertainty is set in data: original uncertainty (or 30) + 1470 m
#' x_rounded2$data$deployments$coordinateUncertainty
round_coordinates <- function(x, digits = 3) {
  if (!(digits %in% c(1, 2, 3))) {
    cli::cli_abort(
      c("`digits` must be NULL, 1, 2 or 3."),
      class = "camtrapdp_error_digits"
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
        lat_digits = nchar(stringr::str_extract(.data$latitude, "\\d+$"))
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
        .data$latitudeGroup, ~uncertainty[[.]][digits]
      ),
      longitude = round(.data$longitude, digits),
      latitude = round(.data$latitude, digits),
      coordinateUncertainty =
        dplyr::case_when(
          # No uncertainty in data: assume 30, add rounding uncertainty
          is.na(.data$coordinateUncertainty) ~ 30 + roundingUncertainty,
          is.null(original_precision) ~
            .data$coordinateUncertainty + .data$roundingUncertainty,
          # Otherwise: subtract old rounding uncertainty, add new rounding uncertainty
          TRUE ~ .data$coordinateUncertainty
          - purrr::map_dbl(.data$latitudeGroup, ~uncertainty[[.]][original_digits])
          + .data$roundingUncertainty
        )
    ) %>%
    dplyr::select(-"latitudeGroup", -"roundingUncertainty")

  # Update coordinatePrecision
  x$coordinatePrecision <- 1 / 10^digits

  return(x)
}
