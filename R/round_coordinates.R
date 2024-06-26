#' Round coordinates to generalize camera trap locations
#'
#' Rounds deployment coordinates to a certain number of digits to
#' fuzzy/generalize camera trap locations.
#' This function can be used before publishing data in order to protect
#' sensitive species and/or prevent theft of active cameras.
#'
#' @inheritParams check_camtrapdp
#' @param digits NULL or number of decimal places to round coordinates to (`1`,
#' `2` or `3`). When `digits` = NULL, `coordinateUncertainty` is set to 30 m
#' and `coordinatePrecicion` remains unchanged.
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
#' x <- example_dataset()
#' x_rounded2 <- round_coordinates(x, 2)
#'
#' # coordinatePrecision is set in metadata
#' x_rounded2$coordinatePrecision
#'
#' # coordinateUncertainty is set in data: original uncertainty (or 30) + 157 m
#' x_rounded2$data$deployments$coordinateUncertainty
#'
#' # Set coordinateUncertainty to 30 m
#' x_30m <- round_coordinates(x_, NULL)
#'
#' x_30m$data$deployments$coordinateUncertainty
round_coordinates <- function(x, digits = 3) {

  if (is.null(digits)) {
    x$data$deployments <-
      deployments(x) %>%
      dplyr::mutate(coordinateUncertainty = 30)

  } else {

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
          c("Can't round from {original_digits} to {digits} digits. ",
            "`{original_digits}` is derived from the ",
            "`x$coordinatePrecision={original_precision}`."),
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
          c("Can't round from {original_digits} to {digits} digits. ",
            "`{original_digits}` is the maximum number of decimals for latitude ",
            "in the data."),
          class = "camtrapdp_error_precision_max"
        )
      }
    }

    # Set uncertainties
    uncertainty <- c(15691, 1570, 157) # In order for 1, 2, 3, digits

    # Update longitude, latitude and coordinateUncertainty
    x$data$deployments <-
      dplyr::mutate(
        deployments(x),
        longitude = round(.data$longitude, digits),
        latitude = round(.data$latitude, digits),
        coordinateUncertainty =
          dplyr::case_when(
            # No uncertainty in data: assume 30, add rounding uncertainty
            is.na(.data$coordinateUncertainty) ~ 30 + uncertainty[digits],
            is.null(original_precision) ~ .data$coordinateUncertainty + uncertainty[digits],
            # Otherwise: subtract old rounding uncertainty, add new rounding uncertainty
            TRUE ~ .data$coordinateUncertainty - uncertainty[original_digits] + uncertainty[digits]
            )
        )

    # Update coordinatePrecision
    x$coordinatePrecision <- 1 / 10^digits
  }

  return(x)
}
