#' Creates columns, but only if they don't exist yet
#'
#' Using dplyr::mutate(), add a new column, but only if it's missing
#'
#' @inherit dplyr::mutate
#' @family helper functions
#' @noRd
#' @examples
#' \dontrun{
#' # doesn't add a column when it already exists
#' mutate_when_missing(cars, speed = "warp 9")
#' # but does add a column when it doesn't exist yet
#' mutate_when_missing(cars, space = "The final frontier")
#' }
mutate_when_missing <- function(.data,...){
  dots <- substitute(list(...))[-1]
  cols_to_check <- names(sapply(dots, deparse))
  columns_to_add <- cols_to_check[!cols_to_check %in% colnames(.data)]
  if(!rlang::is_empty(columns_to_add)){.data <- dplyr::mutate(.data,...)}
  return(.data)
}
    .data <- dplyr::mutate(.data, ...)
  }
}
