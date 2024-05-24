#' Get or set deployments
#'
#' @description
#' `deployments()` gets the deployments from a Camera Trap Data Package object.
#'
#' `deployments<-()` is the assignment equivalent.
#'   It should only be used within other functions, where the expected data
#'   structure can be guaranteed.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with deployments.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Get deployments
#' deployments(x)
#'
#' # Set deployments (not recommended outside a function)
#' deployments(x) <- head(deployments(x), 1)
deployments <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "deployments")
}

#' @rdname deployments
#' @param value A data frame to assign as deployments.
#' @export
'deployments<-' <- function(x, value) {
  if (!is.data.frame(value)) {
    cli::cli_abort("{.arg value} is a {.obj_type_friendly {value}} but needs to
                    be a {.code data.frame}",
                   class = "camtrapdp_error_assignment_wrong_class")
  }

  purrr::assign_in(x, list("data", "deployments"), value = value)
}
