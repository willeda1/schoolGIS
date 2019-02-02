#' runs the app in Shiny
#' 
#' This is the to level function to run the application.
#'
#' @import Shiny
#' @export
#' @examples 
#' runApp()

runApp=function(){
  
  library(shiny)
  
  ui = fluidPage(
    titlePanel("how is the healthest European?")
  )
  
  server=function(input,output){
    
  }
  shinyApp(ui,server)

}

runApp()
