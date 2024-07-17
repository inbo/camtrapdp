#' Creates columns, but only if they don't exist yet
#'
#' Using dplyr::mutate(), add a new column, but only if it's missing
#'
#' @inherit dplyr::mutate
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
#' Splits at the first space
#'
#' @param string Character vector to be split.
#' @param part Which part to return (`1`or `2`)
#' @return Part of the string.
#' @noRd
#'
#' \dontrun{
#' split_first_space("Danny Van der beeck", 1)
#' split_first_space("Danny Van der beeck", 2)
split_first_space <- function(string, part) {
  parts <- strsplit(string, " ", fixed = TRUE)[[1]]
  part1 <- parts[1]
  part2 <- paste(parts[-1], collapse = " ")

  if (part == 1) {
    return(part1)
  }
  if (part == 2) {
    return(part2)
  }
}
