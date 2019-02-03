#' runs the app in Shiny
#'
#' This is the to level function to run the application.
#'
#' @import shiny
#' @export
#' @examples
#' demo()

# Note: the name runApp() is already taken by Shiny

demo=function(){
  require(shiny)

  ui = fluidPage(
    titlePanel("how healthy is Europe?")
  )

  server=function(input,output){}

  shinyApp(ui=ui,server=server)
}

