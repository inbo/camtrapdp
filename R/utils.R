#' Create columns, but only if they are missing
#'
#' Adds columns using [dplyr::mutate()], but only if they are absent in the
#' provided data frame.
#'
#' @inherit dplyr::mutate
#' @family helper functions
#' @noRd
#' @examples
#' # Column "space" is not present yet, so it is added
#' mutate_when_missing(cars, space = "The final frontier")
#' # Column "speed" is present, so it is not added
#' mutate_when_missing(cars, speed = "warp 9")
mutate_when_missing <- function(.data, ...) {
  dots <- substitute(list(...))[-1]
  cols_to_check <- names(sapply(dots, deparse))
  columns_to_add <- cols_to_check[!cols_to_check %in% colnames(.data)]
  if (!rlang::is_empty(columns_to_add)) {
    .data <- dplyr::mutate(.data, ...)
  }
  return(.data)
}

#' Replace duplicated deploymentID
#'
#' Replaces duplicated deploymentIDs with unique values in all resources, using
#' `vdigest_algo_crc32` to generate hashes.
#'
#' @inheritParams print.camtrapdp
#' @param duplicated_deploymentID DeploymentID's to be replaced.
#' @return `x` with unique deploymenID's
#' @family helper functions
#' @noRd
#' @examples
#' replace_duplicated_deploymentID(example_dataset())
replace_duplicated_deploymentID <- function(x, duplicated_deploymentID) {

  # set a vectorised function for creating hash function digests
  vdigest_algo_crc32 <- digest::getVDigest(algo = "crc32")

  # unique deploymentIDs to deployments
  deployments(x) <-
    deployments(x) %>%
    dplyr::mutate(
      deploymentID =
        dplyr::if_else(
          .data$deploymentID %in% duplicated_deploymentID,
          vdigest_algo_crc32(.data$deploymentID),
          .data$deploymentID
        )
    )

  # unique deploymentIDs in observations
  observations(x) <-
    observations(x) %>%
    dplyr::mutate(
      deploymentID =
        dplyr::if_else(
          .data$deploymentID %in% duplicated_deploymentID,
          vdigest_algo_crc32(.data$deploymentID),
          .data$deploymentID
        )
    )

  # unique deploymentIDs in media
  media(x) <-
    media(x) %>%
    dplyr::mutate(
      deploymentID =
        dplyr::if_else(
          .data$deploymentID %in% duplicated_deploymentID,
          vdigest_algo_crc32(.data$deploymentID),
          .data$deploymentID
        )
    )

  return(x)
}
