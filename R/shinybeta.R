#' shinybeta
#'
#' An interactive app which produces a plot of the posterior
#'
#' @return an interactive plot
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples \dontrun{shinycoins()}
shinybeta <- function() {
  runApp(system.file("shinybeta",
                     package = "RosaDSA5403lab3"),
         launch.browser = TRUE)
}

