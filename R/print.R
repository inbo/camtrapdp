#' Print a Camera Trap Data Package
#'
#' Prints a human-readable summary of a Camera Trap Data Package, as an
#' extension of [frictionless::print.datapackage()].
#'
#' @param x Camera Trap Data Package object, as returned by [read_camtrapdp()].
#' @param ... Further arguments, they are ignored by this function.
#' @return [print()] with a summary of the Camera Trap Data Package object.
#' @family print functions
#' @export
#' @examples
#' x <- example_dataset()
#'
#' # Print a summary
#' print(x)
#'
#' # Print a summary after filtering
#' filter_deployments(x, deploymentID == "62c200a9")
print.camtrapdp <- function(x, ...) {
  # check_camtrapdp() not necessary: print only triggered for camtrapdp object

  # Calculate number of rows for the tables (resources in x$data)
  tables <-
    purrr::pluck(x, "data") %>%
    purrr::map(nrow)
  tables_length <- length(tables)

  # Show name and tables
  name <- if (!is.null(x$name)) cli::format_inline("{.val {x$name}} ") else ""
  cli::cat_line(
    cli::format_inline(
      "A Camera Trap Data Package {name}with {tables_length} table{?s}{?./:/:}"
    )
  )
  purrr::walk2(
    names(tables),
    tables,
    ~ cli::cat_bullet(cli::format_inline("{.x}: {.val {.y}} rows"))
  )

  # List additional resources, if any
  extra_resources <- additional_resources(x)
  if (length(extra_resources) > 0) {
    cli::cat_line("")
    cli::cat_line(
      cli::format_inline(
        "And {length(extra_resources)} additional resource{?s}:",
        keep_whitespace = FALSE
      )
    )
    purrr::walk(
      extra_resources,
      function(extra_resource)
      cli::cat_bullet(cli::format_inline("{extra_resource}"))
    )
  }

  # Provide help
  cli::cat_line(
    cli::format_inline(
      "Use {.fun unclass} to print the Data Package as a list."
    ),
    col = "silver"
  )

  invisible(x)
}
