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

#' Expand columns
#'
#' Expands a data frame with columns. Added columns will have `NA_character_`
#' values, existing columns of the same name will not be overwritten.
#'
#' @param df A data frame.
#' @param colnames A character vector of column names.
#' @return Data frame expanded with columns that were not yet present.
#' @family helper functions
#' @noRd
expand_cols <- function(df, colnames) {
  cols_to_add <- setdiff(colnames, colnames(df))
  df[, cols_to_add] <- NA_character_
  return(df)
}

#' Lists the names of additional resources in a Camera Trap Data Package
#'
#' @inheritParams print.camtrapdp
#' @return Character vector with the additional resource names.
#' @family helper functions
#' @noRd
additional_resources <- function(x) {
  camtrapdp_resource_names <- c("deployments", "media", "observations")
  resource_names <- frictionless::resources(x)
  resource_names[!resource_names %in% camtrapdp_resource_names]
}

#' Create list of contributors in EML format
#'
#' @param contributors A data frame with the contributors
#' @return List of contributors as emld responsibleParty objects.
#' @family helper functions
#' @noRd
create_eml_contributors <- function(contributors) {
  contributor_list <- purrr::transpose(contributors)
  purrr::map(contributor_list, ~ EML::set_responsibleParty(
    givenName = .$first_name,
    surName = .$last_name,
    organizationName = .$organization, # Discouraged by EML, but used by IPT
    email = .$email,
    userId = if (!is.na(.$orcid)) {
      list(directory = "https://orcid.org/", .$orcid)
    } else {
      NULL
    },
    onlineUrl = .$path
  ))
}

#' Replace NULL values recursively
#'
#' Replaces `NULL` values with `NA` by recursively iterating through each
#' element of the input list.
#'
#' @param list A nested list.
#' @return `x`, but with all `NULL` values replaced.
#' `NA`.
#' @family helper functions
#' @noRd
replace_null_recursive <- function(list) {
  purrr::map(list, function(element) {
    if (is.list(element) && !is.null(element)) {
      replace_null_recursive(element)
    } else {
      ifelse(is.null(element), NA, element)
    }
  })
}
