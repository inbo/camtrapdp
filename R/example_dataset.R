#' Read the Camtrap DP example dataset
#'
#' Reads the [Camtrap DP example dataset](https://camtrap-dp.tdwg.org/example/).
#' This dataset is maintained and versioned with the Camtrap DP standard.
#'
#' @return Camera Trap Data Package object.
#' @family sample data
#' @export
#' @importFrom memoise memoise
#' @examples
#' example_dataset()
example_dataset <- function() {
  url <- file.path(
    "https://raw.githubusercontent.com/tdwg/camtrap-dp",
    "1.0.2/example/datapackage.json"
  )
  read_camtrapdp_cache(url)
}

read_camtrapdp_cache <- memoise::memoise(function(file) {
  read_camtrapdp(file)
})
