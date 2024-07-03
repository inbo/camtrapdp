#' Print a Camera Trap Data Package
#'
#' Prints a human-readable summary of a Camera Trap Data Package, as an
#' extension of the printing provided by the frictionless package. This summary
#' provides the most important aspects of a Camera Trap Data Package.
#'
#' @param x Camera Trap Data Package object, as returned by
#'   `read_camtrapdp()`.
#' @param ... Further arguments, they are ignored by this function.
#'
#' @return [print()] with a summary of the Camera Trap Data Package object.
#' @export
#'
#' @examples
#' dp <- example_dataset()
#' print(example_dataset)
print.camtrapdp <- function(x, ...) {
  # Check if the Camera Trap Data Package object is valid
  check_camtrapdp(x)

  # List the resources

  # List resources
  resources <- frictionless::resources(x)
  cli::cat_line(
    cli::format_inline(
      "A Data Package with {length(resources)} resource{?s}{?./:/:}"
    )
  )

  # Calculate the number of rows of every resource in `x$data`
  nrow_summary <-
    purrr::pluck(x, "data") %>%
    purrr::map(nrow)

  # Print out a summary of the Camera Trap Data Package object.
  purrr::walk2(
    names(nrow_summary), # The names of the resources
    nrow_summary, # The number of rows in these resource objects
    ~ cli::cat_bullet(cli::format_inline("{.x}: {.y}"))
  )

  # Print out any resources that are not included in `x$data`
  custom_resources <- resources[!resources %in% names(nrow_summary)]
  purrr::walk(custom_resources, function(custom_resource)
    cli::cat_bullet(
      cli::format_inline(
        "{custom_resource}: Custom table/resource not part of the Camtrap DP model"
      )
    ))

  invisible(x)
}
