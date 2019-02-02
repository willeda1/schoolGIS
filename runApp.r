#' runs a the app in Shiny
#'
#' @import Shiny
#' @export

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
