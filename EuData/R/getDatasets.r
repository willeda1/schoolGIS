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

