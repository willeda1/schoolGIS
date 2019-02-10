#' plots a simple slider view for given codes and years
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

sliderView=function(id,input=NULL,output=NULL,ui=T,cx=NULL){
  
  require(ggplot2)
  
  ns=NS(id)

  if (ui){
    
    list(titlePanel(""),
    
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("endpoint.choice")),
        uiOutput(ns("year.choice")),
        uiOutput(ns("slider.widget")),
        htmlOutput(ns("summary"))
      ),
      mainPanel(
        plotOutput(ns("map"))
      )
    )
    )
    
  } else {
    
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
      out=x$code
      names(out)=x$name
      out
    })
    
    dataView=reactive({
      thisEndpoint=input[[ns("endpoint.selection")]]
      thisYear=input[[ns("year.selection")]]
      
      if (any(is.null(c(thisEndpoint,thisYear)))){
        NULL
      } else {
        subset(cx$data, year == thisYear & 
                 code == thisEndpoint &
                 sex == "ALL"
               )
      }
    })
    
    valueRange=reactive({
      x=dataView()
      if (is.null(x)){
        NULL
      } else {
        range(x$value)
      }
    })
    
    output[[ns("slider.widget")]]=renderUI({
      
      range=valueRange()
      
      if (is.null(range)){
        NULL
      } else {
        
        if (!is.finite(range[1])){
          HTML("<b>no data - try different year</b>")
        } else {
          sliderInput(ns("slider.value"),
                      label = "value",
                      min=range[1],max=range[2],
                      value=0)
        }
      }
    })
    
    output[[ns("endpoint.choice")]]=renderUI({
      selectInput(ns("endpoint.selection"),
                  label="measure",
                  choices=endpoints())
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
        
        cutoff=input[[ns("slider.value")]]
        
        a$selected=a$value >= cutoff
        

        
        ggplot()+
          scale_fill_manual(values=c("lightblue","lightgray"),
                            guide=F) +
          geom_sf(data=world,fill="white") + 
          geom_sf(data=a,aes(fill=!selected)) +
          xlim(c(-10,50))+ ylim(c(35,70)) +
          ggtitle(a[1,"name"],
                  subtitle=a[1,"year"])
      }
      })
    
    output[[ns("summary")]]=renderText({
      x=dataView()
      cutoff=input[[ns("slider.value")]]
      
      sprintf(" %d out of %d selected<br>code: %s",
              sum(x$value >= cutoff),
              nrow(x),
              input[[ns("endpoint.selection")]])
    })
    
  }
}