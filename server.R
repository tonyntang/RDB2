infoBox_ui <- function(id) {
  ns <- NS(id)
  
  infoBoxOutput(ns('infobox'))
}

infoBox_server <- function(id, dp, ...) {
  moduleServer(
    id,
    function(input, output, session) {
      output$infobox <- renderInfoBox({
        if(is.null(dp)) return(NULL)
        
        infoBox(value = dp, ...)
      })
    }
  )
}
