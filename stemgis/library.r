
# copied from EuData/R/addGisData.R

#' adds the Gis data to downloaded dataset
#' 
#'
#' @param x a downloaded dataset created by \link{getDatasets}
#' @param host the WHO/EU host (use default)
#' 
#' @return a GIS annotated dataset
#' 
#' @import spData
#' @import sp
#' @import sf
#' @import plyr
#' @import tidyverse
#' 
#' @export
#'
#' @examples
#' a=addGisData(getDatasets(c("HFA_13","HFA_426")))
#' plot(subset(a,code=="HFA_13")["value"])
#' 
#' table(a$iso3)
#' table(a$year)
#' 
#' data(world)
#' b=subset(a,code=="HFA_426" & year == 2011)
#' 
#' ggplot()+
#'   scale_fill_distiller(palette = "Spectral") +
#'   geom_sf(data=world,fill="white") + 
#'   geom_sf(data=b,aes(fill=value)) +
#'   xlim(c(-10,50))+ ylim(c(35,70)) +
#'   ggtitle(b[1,"full_name"],
#'           subtitle=b[1,"year"])
#' 
#' # for some reason have problems with the region clipping
#' data(world)
#' b=subset(a,code=="HFA_13" & year %in% c(2011,2015))
#' ggplot()+
#'   facet_grid(year~sex) +
#'   scale_fill_distiller(palette = "Spectral") +
#'   geom_sf(data=world,fill="white") + 
#'   geom_sf(data=b,aes(fill=value)) +
#'   xlim(c(-10,50))+ ylim(c(30,70)) +
#'   ggtitle(b[1,"full_name"],
#'           subtitle=b[1,"year"])

addGisData=function(x,host="https://dw.euro.who.int"){
  
  require(spData)
  require(sp)
  require(sf)
  require(plyr)
  require(tidyverse)

  data(world)
  w = world
  
  # first, tidy some coding issues
  w[grep("France",w$name_long),"iso_a2"]="FR"
  w[grep("Norway",w$name_long),"iso_a2"]="NO"
  w=plyr::rename(w,c("iso_a2"="iso2"))
  
  # get country information to convert form iso2 to iso3 codes
  a=GET(paste0(host,"/api/v3/countries"))
  a1=httr::content(a)
  isoRecode=plyr::ldply(a1,unlist) %>%
    filter(iso2 != "") %>% 
    dplyr::select(iso2,iso3)
  head(isoRecode)
  
  # now merge everything together
  w %>% 
    inner_join(isoRecode,by="iso2") %>%
    inner_join(x,by="iso3")
}

# copied from EuData/R/demo.r

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


  



# copied from EuData/R/egData.r

#' example data
#' 
#' Example data for this library
#'
#' @name egData
#' @docType data
#' @keywords data
# copied from EuData/R/fetchEndpoint.r

#' Title
#'
#' @param id
#' @param input
#' @param output
#' @param ui
#' @param cx
#'
#' @return
#' @export
#'

fetchEndpoint=function(id,input=NULL,output=NULL,ui=T,cx=NULL){

  ns=NS(id)

  if (ui){
    list(textInput(ns("code"),"code for new data"),
         verbatimTextOutput(ns("info")),
         actionButton(ns("download"),"download"),
         actionButton(ns("add"),"add to list"))
  } else {

    newData=reactive({

      input[[ns("download")]]

      code=isolate(input[[ns("code")]])

      if (code %in% unique(cx$data$code)){
        showNotification("data already uploaded",
                         duration=3,
                         type="error")
        cx$new.data=NULL
        NULL
      } else {
        a=try(getSingleDataset(code),silent=T)
        if (class(a) == "try-error"){
          cx$new.data=NULL
          if (code != ""){
            showNotification("no data found for code",
                             duration=3,
                             type="error")
          }
          NULL
        } else {
          cx$new.data=addGisData(a)
          a
        }
      }

    })

    output[[ns("info")]]=renderPrint({

      input[[ns("download")]]
      a=newData()
      if (is.null(a)){
        "enter valid data code"
      } else {
        with(a,table(full_name,sex))
      }
    },
    quoted = FALSE)

    observeEvent(input[[ns("add")]],{
      cx$data=rbind(cx$data,
                    cx$new.data)
    })


  }

}

# copied from EuData/R/getDatasets.r

#' download multiple datasets by code
#'
#' You can get indicator codes by browsing from
#' \url{https://gateway.euro.who.int/en/datasets/european-health-for-all-database/}
#' and looking at the head of the .csv file.
#'
#' @param codes a vector of HFA codes
#' @param host the WHO/EU host (use default)
#'
#' @return a dataset of values and dimensions
#' 
#' @import plyr
#' @export
#'
#' @examples
#' a=getDatasets(c("HFA_13","HFA_426"))
#' str(a)
#' with(a,table(name,sex))

getDatasets=function(codes=c("HFA_13","HFA_426"),
                     host="https://dw.euro.who.int"){
  
  require(plyr)
  
  output = NULL
  
  for (code in codes){
    output=plyr::rbind.fill(output,
                            getSingleDataset(code,host))
  }
  
  output
}


# copied from EuData/R/getSingleDataset.r

#' downloads a single dataset from the API
#' 
#' Downloads the data and dimensions (country, year etc.) for a single code.
#' Reports only single country data (returning an iso3 country code) 
#' and converts values and years into numerical types.
#' You can get indicator codes by browsing from
#'  \url{https://gateway.euro.who.int/en/datasets/european-health-for-all-database/}
#' and looking at the head of the .csv file.
#'
#' @param code the HFA code for the data set 
#' @param host the WHO/EU host (use default)
#'
#' @return a dataset of values and dimensions
#' 
#' @import httr
#' @import jsonlite
#' @import dplyr
#' @import tidyverse
#' 
#' @export 
#'
#' @examples
#' a=getSingleDataset("HFA_13")
#' str(a)
#' a=getSingleDataset("HFA_426")
#' str(a)

getSingleDataset=function(code="HFA_13",
                 host="https://dw.euro.who.int"){
  
  require(httr)
  require(jsonlite)
  require(dplyr)
  require(tidyverse)
  
  a=GET(paste0(host,
               sprintf("/api/v3/measures/%s?output=data",code)))
  a1=httr::content(a)
  
  output=plyr::ldply(a1$data,unlist)
  
  toSelect=c("fact_id","value")
  
  for (dimension in grep("dimensions.",colnames(output),
                         value=T)){
    
    dimension0 = tolower(sub("dimensions\\.","",dimension))
    output[[dimension0]]=output[[dimension]]
    toSelect=c(toSelect,dimension0)
  }

  output$value=as.numeric(as.character(output$value.numeric))
  output$year=as.numeric(as.character(output$year))
  

  
  output = output[,toSelect] 
  
  output$iso3=output$country
  output$country=NULL
  output[output$iso3 !="",]
  
  output$country_grp=NULL
 
  # using base - tidyverse stopped working
  output$code=a1$code
  output$name=a1$short_name
  output$full_name=a1$full_name
  output
}



# copied from EuData/R/infoPanel.r

#' displays basic information about the app
#'
#' @param id 
#' @param input 
#' @param output 
#' @param ui 
#' @export
#'
infoPanel=function(id,input=NULL,output=NULL,ui=T){
  
  ns=NS(id)
  
  if (ui){
    
    HTML(paste0("<br>data taken from WHO/EURO site. See ",
                "<a href='https://gateway.euro.who.int/en/datasets/european-health-for-all-database/'>",
                "link</a> for further details. Data available for educational and private use only."
                ))
    
    
    
  } else {
    stop("server side not currently needed")
  }
  
  
  
}
# copied from EuData/R/simpleView.r

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
# copied from EuData/R/sliderView.r

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
        uiOutput(ns("gender.choice")),
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
    
    output[[ns("gender.choice")]]=renderUI({
      print(genders())
      selectInput(ns("gender.selection"),
                  label="gender",
                  choices=genders())
    }) 
    
    
    
    dataView=reactive({
      thisEndpoint=input[[ns("endpoint.selection")]]
      thisYear=input[[ns("year.selection")]]
      thisGender=input[[ns("gender.selection")]]
      
      if (any(is.null(c(thisEndpoint,thisYear)))){
        NULL
      } else {
        subset(cx$data, year == thisYear & 
                 code == thisEndpoint &
                 sex == thisGender
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