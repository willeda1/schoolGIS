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