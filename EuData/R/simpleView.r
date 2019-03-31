#' plots a simple panel for given codes and years
#' 
#' Provides two controls - one to select the endpoint and the other 
#' to select the year
#'
#' @param id the widget id
#' @param input shiny input 
#' @param output shiny output
#' @param ui TRUE for the user interface, FALSE for server
#' @param cx a reactive list, reads data from cx$egData
#'
#' @import ggplot2
#' @export

simpleView=function(id,input=NULL,output=NULL,ui=T,cx=NULL){
  
  require(ggplot2)
  
  ns=NS(id)
  
  if (ui){
    
    fluidPage(
      fluidRow(
        column(6,
               uiOutput(ns("endpoint.choice"))),
        column(3,
               uiOutput(ns("gender.choice"))),
        column(3,
               uiOutput(ns("year.choice")))),
      fluidRow(
        column(12,
               plotOutput(ns("map"))))
    )
    
    
  } else {
    
    print("boo")
    
    years=reactive({
      
      selectedCode = input[[ns("endpoint.selection")]]
      print(selectedCode)
      
      
      if (is.null(selectedCode)){
        sort(as.character(unique(cx$data$year)),
             decreasing=T)
      } else {
        sort(as.character(unique(subset(cx$data,
                                        code == selectedCode)$year)),
             decreasing=T)
      }
    })
    
    endpoints=reactive({
      
      # need first to remove the sticky $geom slot
      w=cx$data
      w$geom=NULL
      
      # and now return a named list
      x=unique(w[,c("code","name")])
      print(x)
      out=x$code
      names(out)=x$name
      out
    })
    
    dataView0=reactive({
      thisEndpoint=input[[ns("endpoint.selection")]]
      thisYear=input[[ns("year.selection")]]
      
      if (any(is.null(c(thisEndpoint,thisYear)))){
        NULL
      } else {
        subset(cx$data, year == thisYear & code == thisEndpoint)
      }
    })
    
    genders=reactive({
      a=dataView0()
      if (!is.null(a)){
        unique(a$sex)
      } else {
        NULL
      }
    })
    
    
    dataView=reactive({
      a=dataView0()
      if (!is.null(a)){
        subset(a,sex==input[[ns("gender.selection")]])
      } else {
        NULL
      }
      
    })
    
    output[[ns("endpoint.choice")]]=renderUI({
      print(endpoints())
      selectInput(ns("endpoint.selection"),
                  label="measure",
                  choices=endpoints())
    }) 
    
    output[[ns("gender.choice")]]=renderUI({
      print(genders())
      selectInput(ns("gender.selection"),
                  label="gender",
                  choices=genders())
    }) 
    
    
    output[[ns("year.choice")]]=renderUI({
      selectInput(ns("year.selection"),
                  label="year",
                  choices=years())
    }) 
    
    output[[ns("map")]]=renderPlot({
      
      a=dataView()
      
      if (is.null(a)){
        plot(0,0,axes=F,xlab="",ylab="",type="n")
        text(0,0,"wait...")
      } else {
        ggplot()+
          scale_fill_distiller(palette = "Spectral") +
          geom_sf(data=world,fill="white") + 
          geom_sf(data=a,aes(fill=value)) +
          xlim(c(-10,50))+ ylim(c(35,70)) +
          ggtitle(a[1,"full_name"],
                  subtitle=a[1,"year"])
      }
      
      
    })
    
  }
}