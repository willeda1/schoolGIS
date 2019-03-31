#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)

require(httr)
require(jsonlite)
require(dplyr)
require(tidyverse)
require(sp)
require(spData)
require(sf)

local=F
if (local){
#  library(EuData)
  data("egData")
} else {
  source("library.r")
  load("egData.rda")
}




require(shiny)
cx=reactiveValues(data=egData,new.data=NULL)

ui = fluidPage(
  titlePanel("how healthy is Europe?"),
  tabsetPanel(
    tabPanel("simple view",
             simpleView("simple",cx=cx)),
    tabPanel("slider view",
             sliderView("slider",cx=cx)),
    tabPanel("new data",
             fetchEndpoint("new")),
    tabPanel("info",
             infoPanel("info"))
  )
)

server=function(input,output){
  

  simpleView("simple",input,output,cx=cx,ui=F)
  sliderView("slider",input,output,cx=cx,ui=F)
  fetchEndpoint("new",input,output,cx=cx,ui=F)
  
}

#

shinyApp(ui=ui,server=server)
