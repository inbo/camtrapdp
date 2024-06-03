#' Round coordinates to generalize camera trap locations
#'
#' Rounds deployment coordinates to a certain number of digits to
#' fuzzy/generalize camera trap locations.
#' This function can be used before publishing data in order to protect
#' sensitive species and/or prevent theft of active cameras.
#'
#' @param package A Camtrap DP, as read by [read_camtrap_dp()].
#' @param digits Number of decimal places to round coordinates to (`1`, `2` or
#'   `3`).
#' @return `package` with rounded coordinates as well as updated
#'   `coordinateUncertainty`.(in deployments) and `coordinatePrecision` (in
#'   metadata).
#' @family publication functions
#' @export
#' @importFrom dplyr %>% .data
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
#' sensitivity | digits | coordinatePrecision | coordinateUncertainty
#' --- | --- | --- | ---
#' high | 1 | 0.1 | original uncertainty + 15691 m
#' medium | 2 | 0.01 | original uncertainty + 1570 m
#' low | 3 | 0.001 | original uncertainty + 157 m
#'
#' For records with `coordinateUncertainty = NA` the function will assume the
#' coordinates were obtained by GPS and use `30 m` as original uncertainty,
#' before adding uncertainty caused by rounding.
#' The added uncertainty is the largest possible value caused by rounding (see
#' [Table 3](https://doi.org/10.15468/doc-gg7h-s853#table-uncertainty) in
#' Chapman & Wieczorek 2020).
#' @examples
#' # Round coordinates of example package to 3 digits
#' mica <- round_coordinates(mica, 3)
#'
#' # coordinatePrecision is set in metadata
#' mica$coordinatePrecision
#'
#' # coordinateUncertainty is set in data: original uncertainty (or 30) + 157 m
#' mica$data$deployments$coordinateUncertainty
round_coordinates <- function(x, digits = 3) {
  assertthat::assert_that(
    digits %in% c(1, 2, 3),
    msg = "`digits` must be 1, 2 or 3."
  )

  deployments <- x$data$deployments

  # Detect original number of digits from coordinatePrecision or data
  original_precision <- x$coordinatePrecision
  if (!is.null(original_precision)) {
    original_digits <- -log10(original_precision) # 0.001 -> 3
    assertthat::assert_that(
      digits <= original_digits, # 0.1 > 0.01
      msg = glue::glue(
        "Can't round from {original_digits} to {digits} digits. ",
        "`{original_digits}` is derived from the ",
        "`x$coordinatePrecision={original_precision}`."
      )
    )
  } else {
    original_digits <-
      deployments %>%
      dplyr::mutate(
        lat_digits = nchar(stringr::str_extract(.data$latitude, "\\d+$"))
      ) %>%
      dplyr::summarize(max(.data$lat_digits)) %>%
      dplyr::pull()
    assertthat::assert_that(
      digits <= original_digits, # 0.1 > 0.01
      msg = glue::glue(
        "Can't round from {original_digits} to {digits} digits. ",
        "`{original_digits}` is the maximum number of decimals for latitude ",
        "in the data."
      )
    )
  }

  # Set uncertainties
  uncertainty <- c(15691, 1570, 157) # In order for 1, 2, 3, digits

  # Update longitude, latitude and coordinateUncertainty
  x$data$deployments <-
    dplyr::mutate(deployments,
                  longitude = round(.data$longitude, digits),
                  latitude = round(.data$latitude, digits),
                  coordinateUncertainty = dplyr::case_when(
                    # No uncertainty in data: assume 30, add rounding uncertainty
                    is.na(.data$coordinateUncertainty) ~ 30 + uncertainty[digits],
                    # No precision in metadata: original uncertainty, add rounding uncertainty
                    is.null(original_precision) ~ .data$coordinateUncertainty + uncertainty[digits],
                    # Otherwise: subtract old rounding uncertainty, add new rounding uncertainty
                    TRUE ~ .data$coordinateUncertainty - uncertainty[original_digits] + uncertainty[digits]
                  )
    )

  # Update coordinatePrecision
  x$coordinatePrecision <- 1 / 10^digits

  x
}
