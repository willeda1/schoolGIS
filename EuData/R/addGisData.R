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
