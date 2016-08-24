#' Run the default TBploter app for analysis locally
#'
#' \code{TBploter} run TBploter locally
#' @author Qi Zhao

InteractiveVenny <- function() {
  shiny::runApp(system.file("InteractiveVenny", package = "InteractiveVenny"))
}
