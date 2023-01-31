trans_plot_ui <- function(id) {
  ns <- NS(id)
  
  plotlyOutput(ns('trans_plot'))
}

trans_plot_server <- function(id, dp, direction = 'both') {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$trans_plot <- renderPlotly({
        if(is.null(dp)) return(NULL)
        
        # begin plotly
        dp %>%
          plot_ly(
            x = ~month,
            y = ~total_amt_usd,
            mode = 'markers',
            type = 'scatter',
            color = ~dia_trans_direction,
            linetype = ~dia_trans_direction
            )
      })
    }  
  )
}
