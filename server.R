tab_target_display <- tabItem(
  tabName = 'target_display',
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
      fluidRow(
        column(
          width = 6,
          pickerInput(
            inputId = 'shy_target_select',
            label = 'Select target(s)',
            choices = dg_txn_raw %>% activate(nodes) %>% pull(bus_name) %>% sort,
            multiple = TRUE,
            options = pickerOptions(
              liveSearch = TRUE,
              actionsBox = TRUE,
              size = 10,
              title = 'Choose maximum two from the list...',
              selectedTextFormat = 'count > 5',
              selectOnTab = TRUE,
              maxOptions = 2
            )
          )
        )
      ),
      
      column(
        width = 2,
        actionBttn(
          inputId = "shy_target_update_btn",
          label = "Update", 
          style = "gradient",
          color = "danger",
          icon = icon("thumbs-up")
        )
      )
    ),
    
    # box to show Major Visualization ------------------------------------
    box(
      title = 'Network for Selected Target(s)',
      status = 'success',
      width = 6,
      height = 600,
      closable = FALSE,
      solidHeader = FALSE,
      collapsible = FALSE,
      
      visNetworkOutput('shy_visnet_target') %>% 
      withSpinner()
    ),
    
    # box to show detailed Visualization ---------------------------------
    tabBox(
      title = 'Statistics',
      id = 'shy_na_detail_stat',
      width = 6,
      height = 600,
      
      tabPanel(
        'Summary', 
        icon = fa_i('desktop'),
        fluidRow(
          infoBox_ui('shy_detail_summary_amt_stat'),
          infoBox_ui('shy_detail_summary_cnt_stat'),
          infoBox_ui('shy_detail_summary_date_stat')
        ),
        trans_plot_ui('shy_detail_summary_plot')
      ),
      
      tabPanel(
        'Originator', 
        icon = fa_i('arrow-circle-left'),
        fluidRow(
          infoBox_ui('shy_detail_orig_amt_stat'),
          infoBox_ui('shy_detail_orig_cnt_stat'),
          infoBox_ui('shy_detail_orig_date_stat')
        ),
        trans_plot_ui('shy_detail_orig_plot')
      ),
      
      tabPanel(
        'Beneficiary', 
        icon = fa_i('arrow-circle-right'),
        fluidRow(
          infoBox_ui('shy_detail_bene_amt_stat'),
          infoBox_ui('shy_detail_bene_cnt_stat'),
          infoBox_ui('shy_detail_bene_date_stat')
        ),
        trans_plot_ui('shy_detail_bene_plot')
      )
      
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
      column(12, DT::DTOutput('shy_tbl_detail_selected'))
    )
  )
)
