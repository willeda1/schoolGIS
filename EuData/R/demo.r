#' runs the app in Shiny
#'
#' This is the to level function to run the application.
#'
#' @import shiny
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyverse
#' @import sp
#' @import spData
#' @import sf
#' 
#' @export
#' @examples
#' demo()

# Note: the name runApp() is already taken by Shiny

demo=function(){
  
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(tidyverse)
  require(sp)
  require(spData)
  require(sf)
  
  
  data("egData")
  
  require(shiny)
  cx=reactiveValues(data=egData)
  
  ui = fluidPage(
    titlePanel("how healthy is Europe?"),
    tabsetPanel(
      tabPanel("simple view",
               simpleView("simple",cx=cx)),
      tabPanel("slider view",
               sliderView("slider",cx=cx))
    )
  )
  
  server=function(input,output){
    
    simpleView("simple",input,output,cx=cx,ui=F)
    sliderView("slider",input,output,cx=cx,ui=F)
    
  }
  
  shinyApp(ui=ui,server=server)
  
}


  


