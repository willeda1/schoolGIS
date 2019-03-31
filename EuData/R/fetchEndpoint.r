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
