#' Get deployments
#'
#' Gets the deployments from a Camera Trap Data Package object.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with deployments.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' deployments(x)
deployments <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "deployments")
}

#' @rdname deployments
#' @inheritParams version
#' @param value data.frame to assign to the deployments of a Camera Trap Data Package Object.
'deployments<-' <- function(x, value){
  if(!is.data.frame(value)){
    cli::cli_abort("{.arg value} is a {.obj_type_friendly {value}} but needs to
                    be a {.code data.frame}")
  }

  purrr::assign_in(x, "data", "deployments", value)
}
