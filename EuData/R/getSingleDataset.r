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


