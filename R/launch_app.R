#' @title Launch expenditR
#'
#' @param port The port, expenditR is running on (default: 3838)
#'
#' @return expenditR application
#'
#' @import shiny shinydashboard
#' @importFrom magrittr "%>%"
#' @importFrom data.table .N ":="
#'
#' @export
#'
launchApp <- function(port=3838){
  options(shiny.port = port)
  shiny::shinyAppDir(appDir = system.file("application", package = "expenditR"))
}
