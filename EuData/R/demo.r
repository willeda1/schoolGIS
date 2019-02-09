#' runs the app in Shiny
#'
#' This is the to level function to run the application.
#'
#' @import shiny
#' @export
#' @examples
#' demo()

# Note: the name runApp() is already taken by Shiny

Demo=function(){
  data("egData")
  
  require(shiny)
  cx=reactiveValues(data=egData)
  
  ui = fluidPage(
    titlePanel("how healthy is Europe?"),
    tabsetPanel(
      tabPanel("simple view",
               simpleView("simple",cx=cx))
    )
  )
  
  server=function(input,output){
    
    simpleView("simple",input,output,cx=cx,ui=F)
    
  }
  
  shinyApp(ui=ui,server=server)
  
}


  


