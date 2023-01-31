tab_overall_display <- tabItem(
  tabName = 'overall_display',
  fluidRow(
    # box to show tweak options ------------------------------------------
    box(
      title = 'Options',
      status = 'primary',
      width = 12,
      height = 600,
      closable = FALSE,
      solidHeader = TRUE,
      collapsible = TRUE,
      
      # gadgets put here -------------------------------------------------
      column(
        6,
        dateRangeInput(
          'shy_dt_range', 
          label = h3('Date Range'), 
          start = glb_min_dt,
          end = glb_max_dt,
          min = glb_min_dt,
          max = glb_max_dt
        )
      )
    ),
    
    # box to show Major Visualization ------------------------------------
    box(
      title = 'Network for SOIs only',
      status = 'success',
      width = 6,
      height = 600,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      
      visNetworkOutput('shy_visnet_main') %>% 
        withSpinner()
    ),
    
    # box to show detailed Visualization ---------------------------------
    box(
      title = 'Network for selected SOI ',
      status = 'success',
      width = 6,
      height = 600,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      
      visNetworkOutput('shy_visnet_side') %>% 
        withSpinner()
    ),
    
    # box to show data ---------------------------------------------------
    box(
      title = 'Data',
      status = 'orange',
      width = 12,
      height = 600,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = TRUE,
      
      # show data
      column(12, DT::DTOutput('shy_tbl_overall_selected'))
    )
  )
)
