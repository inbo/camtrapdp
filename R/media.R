#' Get media
#'
#' Gets the media from a Camera Trap Data Package object.
#'
#' @inheritParams check_camtrapdp
#' @return [tibble()] data frame with media.
#' @family accessor functions
#' @export
#' @examples
#' x <- example_dataset()
#' # Get the media of x
#' media(x)
#' # Change the media of x
#' media(x) <-
#'   data.frame(
#'     stringsAsFactors = FALSE,
#'     mediaID = c("5b0ccf8a", "1640ad29"),
#'     deploymentID = c("577b543a", "00a2c20d"),
#'     timestamp = c("2020-06-23 23:33:56", "2020-05-31 20:07:31"),
#'     filePath = c("https://multimedia.agouti.eu/assets/5b0ccf8a-4696-4814-8bb6-c96bb248bc3f/file",
#'                  "https://multimedia.agouti.eu/assets/1640ad29-e2cb-43cc-8cd6-c1ef2ffaadd7/file"),
#'     filePublic = c(TRUE, TRUE),
#'     fileName = c("20200811211208-RCNX0061.JPG",
#'                  "20200709093338-RCNX0043.JPG"),
#'     fileMediatype = c("image/jpeg", "image/jpeg"),
#'     exifData = c(NA, NA),
#'     favorite = c(NA, NA),
#'     mediaComments = c(NA, NA),
#'     eventID = c("b4b39b00", "45abeadc"),
#'     captureMethod = as.factor(c("activityDetection", "activityDetection"))
#'    )
#'
media <- function(x) {
  check_camtrapdp(x)
  purrr::pluck(x, "data", "media")
}

#' @rdname media
#' @param value data.frame to assign to the media of a Camera Trap Data Package Object.
#' @export
'media<-' <- function(x, value) {
  if (!is.data.frame(value)) {
    cli::cli_abort("{.arg value} is a {.obj_type_friendly {value}} but needs to
                    be a {.code data.frame}",
                   class = "camtrapdp_error_assignment_wrong_class")
  }

  purrr::assign_in(x, list("data", "media"), value)
}
