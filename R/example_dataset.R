#' Read the Camtrap DP example dataset
#'
#' Reads the [Camtrap DP example dataset](https://camtrap-dp.tdwg.org/example/).
#' This dataset is maintained and versioned with the Camtrap DP standard.
#'
#' @return Camera Trap Data Package object.
example_dataset <- function() {
  read_camtrap_dp(file = file.path(
    "https://raw.githubusercontent.com",
    "tdwg",
    "camtrap-dp", "1.0", "example", "datapackage.json"
  ))
}
