#' Read the Camtrap DP example dataset
#'
#' Reads the [Camtrap DP example dataset](https://camtrap-dp.tdwg.org/example/).
#' This dataset is maintained and versioned with the Camtrap DP standard.
#'
#' @return Camera Trap Data Package object.
#' @family sample data
#' @export
#' @examples
#' example_dataset()
.example_dataset <- function() {
  url <- file.path(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp",
    "1.0/example/datapackage.json"
  )
  read_camtrapdp(url)
}

example_dataset <- memoise::memoise(.example_dataset)
