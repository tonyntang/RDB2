# server.R =====================================================================

function(input, output, session) {

  # output change_log.txt in the welcome page ----------------------------------
  output$shy_news <- renderText({
    str_c(change_log_txt , '<br>', collapse = '')
  })
  
  # A. create reactive data for analysis ---------------------------------------
  # raw transactions
  dg_txn_dt <- reactive({
    if(is.null(input$shy_dt_range)) {
      return(dg_txn_raw)
    } else {
      dg_txn_raw %>% 
        activate(edges) %>% 
        filter(
          transaction_date_time >= input$shy_dt_range[1],
          transaction_date_time <= input$shy_dt_range[2]
        ) %>% 
        activate(nodes) %>% 
        filter(!node_is_isolated())
    }
  }) %>% 
    bindEvent(input$shy_dt_range, ignoreNULL = FALSE)
  
  # aggregated transactions
  dg_agg_dt <- reactive({
    dg_txn_dt() %>% 
      agg_graph() 
  })
  
  # B. Below is for Tab Overall ================================================
  # the data for drawing the main Net Graph of SOIs ----------------------------
  dv_overall_main <- reactive({
    dg_agg_dt() %>%
      activate(nodes) %>% 
      filter(is_SOI == 'Y') %>% 
      toVisNetworkData(idToLabel = FALSE)
  })
  
  # Draw main VisNetwork Display -----------------------------------------------
  output$shy_visnet_main <- renderVisNetwork({
    
    dv <- dv_overall_main()
    
    visNetwork(
      nodes = dv$nodes,
      edges = dv$edges,
      height = '600px',
      width = '100%'
    ) %>% 
      visGroups(
        groupname = 'SOI_Shell',
        color = 'red',
        shape = 'icon',
        icon = list(code = 'f007', color = 'red'),
        shadow = list(enabled = TRUE)
      ) %>% 
      visGroups(
        groupname = 'SOI_non_Shell',
        shape = 'icon',
        icon = list(code = 'f007', color = 'darkblue'),
        shadow = list(enabled = TRUE)
      ) %>% 
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = .5)),
        color = list(color = 'lightblue', highlight = 'darkblue')
      ) %>% 
      visLayout(randomSeed = 2021) %>% 
      addFontAwesome(name = 'font-awesome-visNetwork') %>% 
      visInteraction(
        multiselect = FALSE
      ) %>% 
      visPhysics(stabilization = FALSE) %>% 
      visOptions(
        nodesIdSelection = TRUE
      )
    
      # obtain what node is currently clicked (selected)
      # visEvents(
      #   select = "
      #     function(nodes) {
      #     Shiny.onInputChange('seleted_node_id_overall', nodes.nodes)
      #     }
      #   ",
      #   click = "
      #     function(nodes) {
      #     Shiny.onInputChange('click_in_overall', nodes.nodes.length)
      #     }
      #   "
      # )
  })
  
  # create reactive data for drawing the side Net Graph for selected node ------
  dv_overall_selected <- reactive({
    
    if(input$shy_visnet_main_selected == '') return(NULL)
    
    dv <- dv_overall_main()
    dg_agg <- dg_agg_dt()
    
    selected_node_name <- dv$nodes %>%
      filter(id == input$shy_visnet_main_selected) %>%
      pull(bus_name)
    
    selected_node_id <- dg_agg %>% 
      activate(nodes) %>%
      mutate(N = row_number()) %>%
      filter(bus_name == selected_node_name) %>%
      pull(N)
    
    list(
      # data for drawing network
      dv = dg_agg %>% 
        activate(edges) %>% 
        mutate(
          dia_trans_direction = case_when(from == selected_node_id & to == selected_node_id ~ 'self',
                                          from == selected_node_id & to != selected_node_id ~ 'outward',
                                          from != selected_node_id & to == selected_node_id ~ 'inward'),
          dashes = case_when(dia_trans_direction == 'outward' ~ TRUE,
                             TRUE ~ FALSE)
        ) %>% 
        
        # "pairs" was created in data loading stage to identify same pairs (same from & to, regardless of order)
        group_by(pairs) %>%
        mutate(N = n()) %>%
        ungroup() %>%
        mutate(
          smooth = case_when(N > 1 ~ TRUE,
                             TRUE ~ FALSE)

        ) %>%
        convert(
          to_local_neighborhood,
          node = selected_node_id,
          order = 1,
          mode = 'all'
        ) %>% 
        toVisNetworkData(idToLabel = FALSE),
      
      # raw transaction data for data table ouput
      dv_trans = dg_txn_dt() %>% 
        activate(edges) %>%
        filter(from == selected_node_id | to == selected_node_id) %>%
        toVisNetworkData(idToLabel = FALSE)
    )
    
  }) %>% 
    bindEvent(input$shy_visnet_main_selected)
  
  
  # Side VisNetwork Display ----------------------------------------------------
  output$shy_visnet_side <- renderVisNetwork({
    if(input$shy_visnet_main_selected == '') return(NULL)
    
    dv <- dv_overall_selected()$dv
    
    visNetwork(
      nodes = dv$nodes,
      edges = dv$edges,
      height = '600px',
      width = '100%'
    ) %>% 
      # set icons for different groups
      visGroups(
        groupname = 'SOI_Shell',
        color = 'red',
        shape = 'icon',
        icon = list(code = 'f007', color = 'red'),
        shadow = list(enabled = TRUE)
      ) %>% 
      visGroups(
        groupname = 'SOI_non_Shell',
        shape = 'icon',
        icon = list(code = 'f007', color = 'darkblue'),
        shadow = list(enabled = TRUE)
      ) %>% 
      visGroups(
        groupname = 'non_SOI_Shell',
        color = 'red',
        shape = 'square'
      ) %>%
      visGroups(
        groupname = 'non_SOI_non_Shell',
        color = 'darkblue',
        shape = 'square'
      ) %>%
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = .5)),
        color = list(color = 'lightblue', highlight = 'darkblue')
      ) %>% 
      visLayout(randomSeed = 2021) %>% 
      # visPhysics(stabilization = FALSE) %>% 
      visIgraphLayout() %>% 
      addFontAwesome(name = 'font-awesome-visNetwork')
  }) %>% 
    bindEvent(input$shy_visnet_main_selected)
  
  # show data for selected customer in Tab Overall -----------------------------
  output$shy_tbl_overall_selected <- DT::renderDT(
    filter = 'top',
    options = list(
      width = '100%',
      scrollX = TRUE
    ),
    class = 'white-space: nowrap',
    # render content starts below
    {
      if(input$shy_visnet_main_selected == '') return(tibble(No_Data = 'No Data Selected'))
      
      dv_overall_selected()$dv_trans$edges %>% 
        as_tibble() %>% 
        select(-from, -to) %>% 
        relocate(
          originator_name,
          beneficiary_name
        )
    }
  ) %>% 
    bindEvent(input$shy_visnet_main_selected)
  
  # C. Below is for Tab Target =================================================
  # 0. update the dropdown list to reflect data change due to date range change
  observe({
    updatePickerInput(
      session,
      inputId = 'shy_target_select',
      choices = dg_txn_dt() %>% activate(nodes) %>% pull(bus_name) %>% sort
    )
  }) %>% 
    bindEvent(input$shy_dt_range)
  
  # 1. create reactive data for net graph
  dv_target <- reactive({
    
    if(is.null(input$shy_target_select)) {
      
      sendSweetAlert(
        session = session,
        title = "Forgot to select?",
        text = 'Please select at least one subject of interest!',
        type = "warning"
      )
      
      return(NULL)
    }
    
    selected_node_id <- dg_agg_dt() %>% 
      activate(nodes) %>% 
      mutate(N = row_number()) %>% 
      filter(bus_name %in% input$shy_target_select) %>% 
      pull(N)
    
    # note: currently only take maximum 2 nodes for targeted display 
    dg <- dg_agg_dt() %>% 
      convert(
        to_local_neighborhood,
        node = selected_node_id[1],
        order = 1,
        mode = 'all'
      )
    
    if(length(selected_node_id) == 2L) {
      dg <- dg %>% 
        graph_join(
          dg_agg_dt() %>% 
            convert(
              to_local_neighborhood,
              node = selected_node_id[2],
              order = 1,
              mode = 'all'
            )
        ) %>% 
        activate(edges) %>% 
        group_by(from, to) %>% 
        filter(row_number() == 1L) %>% 
        ungroup
    }
    
    dg %>% 
      toVisNetworkData(idToLabel = FALSE)
    
  }) %>% 
    # bindEvent(input$shy_target_select)
    bindEvent(input$shy_target_update_btn)
  
  # 2. Detail VisNetwork Display - Main ----------------------------------------
  output$shy_visnet_target <- renderVisNetwork({
    
    dv <- dv_target()
    
    visNetwork(
      nodes = dv$nodes,
      edges = dv$edges,
      height = '600px',
      width = '100%'
    ) %>% 
      # set icons for different groups
      visGroups(
        groupname = 'SOI_Shell',
        color = 'red',
        shape = 'icon',
        icon = list(code = 'f007', color = 'red'),
        shadow = list(enabled = TRUE)
      ) %>% 
      visGroups(
        groupname = 'SOI_non_Shell',
        shape = 'icon',
        icon = list(code = 'f007', color = 'darkblue'),
        shadow = list(enabled = TRUE)
      ) %>% 
      visGroups(
        groupname = 'non_SOI_Shell',
        color = 'red',
        shape = 'square'
      ) %>%
      visGroups(
        groupname = 'non_SOI_non_Shell',
        color = 'darkblue',
        shape = 'square'
      ) %>%
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = .5)),
        color = list(color = 'lightblue', highlight = 'darkblue')
      ) %>% 
      visLayout(randomSeed = 2021) %>% 
      # visPhysics(stabilization = FALSE) %>% 
      # visIgraphLayout() %>%
      addFontAwesome(name = 'font-awesome-visNetwork') %>% 
      
      # obtain what node is currently clicked (selected)
      visInteraction(
        multiselect = FALSE
      ) %>% 
      # this will create input$shy_visnet_target_selected ...
      visOptions(
        nodesIdSelection = TRUE
      ) %>%
      visEvents(
        select = "
          function(nodes) {
            Shiny.onInputChange('seleted_node_id_detail', nodes.nodes)
          }
        "
      )
  }) %>% 
    bindEvent(dv_target())
  
  # 3. monitor if any node is selected in the detail Net graph -----------------
  # store all direct transactions around selected node ... (not a neighborhood)
  # i.e. all transactions should involve the selected node
  dv_target_selected <- reactive({
    
    dv <- dv_target()
    
    selected_node_name <- dv$nodes %>% 
      filter(id == input$shy_visnet_target_selected) %>% 
      pull(bus_name)
    
    selected_node_id <- dg_txn_dt() %>% 
      activate(nodes) %>% 
      mutate(N = row_number()) %>% 
      filter(bus_name %in% selected_node_name) %>% 
      pull(N)
    
    # store data into reactive DB ...
    re <- dg_txn_dt() %>% 
      activate(edges) %>%
      filter(from == selected_node_id | to == selected_node_id) %>%
      toVisNetworkData(idToLabel = FALSE)
    
    re$selected_node_id <- selected_node_id
    
    re
  }) %>% 
    bindEvent(input$shy_visnet_target_selected)
  
  # 4. show data for selected customer in Tab Details --------------------------
  output$shy_tbl_detail_selected <- DT::renderDT(
    filter = 'top',
    options = list(
      width = '100%',
      scrollX = TRUE
    ),
    class = 'white-space: nowrap',
    # render content starts below
    {
      if(input$shy_visnet_target_selected == '') return(tibble(No_Data = 'No Data Selected'))
      
      dv_selected <- dv_target_selected()
      if(is.null(dv_selected)) return(tibble(No_Data = 'No Data Selected'))
      
      dv_selected$edges %>% 
        as_tibble() %>% 
        select(-from, -to) %>% 
        relocate(
          originator_name,
          beneficiary_name
        )
    }
  ) %>% 
    bindEvent(input$shy_visnet_target_selected)
  
  # update title in the data and statistics display area -----------------------
  # to reflect which SOI is being shown

  # Actions when a node is selected in the Network Plot of the Tab detail ------
  # Action #1 : update the statistics
  # Action #2 : generate a line plot
  observe({
    
    # clear all contents in the Statistics Tab Panel
    if(is.null(input$shy_visnet_target_selected) || input$shy_visnet_target_selected == '') {
      
      trans_plot_server('shy_detail_summary_plot', NULL)
      trans_plot_server('shy_detail_orig_plot', NULL)
      trans_plot_server('shy_detail_bene_plot', NULL)
      
      infoBox_server(
        'shy_detail_summary_amt_stat',
        0,
        title = 'Avg. Trans. Amount (USD)',
        icon = fa_i('dollar-sign'),
        color = 'green'
      )
      
      infoBox_server(
        'shy_detail_summary_cnt_stat',
        0,
        title = 'Trans. Count',
        icon = fa_i('calculator'),
        color = 'light-blue'
      )
      
      infoBox_server(
        'shy_detail_summary_date_stat',
        0,
        title = 'Days',
        icon = fa_i('calendar'),
        color = 'orange'
      )
      
      return(NULL)
    }
    
    dv_selected <- dv_target_selected()
    selected_node_id <- dv_selected$selected_node_id
    
    trans_data <- dv_selected$edges %>% 
      as_tibble() %>% 
      mutate(
        dia_trans_direction = case_when(
          from == selected_node_id & to == selected_node_id ~ 'self',
          from == selected_node_id & to != selected_node_id ~ 'outward',
          from != selected_node_id & to == selected_node_id ~ 'inward'
        ),
        month = floor_date(transaction_date_time, 'month')
      )
    
    # calculate data for making transaction plot for selected node
    dp <- trans_data %>% 
      group_by(
        dia_trans_direction,
        month
      ) %>% 
      summarize(
        total_amt_usd = sum(dia_trans_amt_usd),
        trans_count = n(),
        .groups = 'drop'
      )
    
    # Transaction plots for selected node ------------------------------------
    # output summary trans plot
    trans_plot_server(
      'shy_detail_summary_plot', 
      dp
    )
    
    # output originator trans plot
    trans_plot_server(
      'shy_detail_orig_plot', 
      dp %>% 
        filter(
          dia_trans_direction == 'outward'
        )
    )
    
    # output beneficiary trans plot
    trans_plot_server(
      'shy_detail_bene_plot', 
      dp %>% 
        filter(
          dia_trans_direction == 'inward'
        )
    )
    
    # calculate data for statistics infoboxes --------------------------------
    info_summary <- trans_data %>% 
      summarize(
        summary_amount = dollar(mean(dia_trans_amt_usd)),
        summary_trans_count = number(n(), big.mark = ','),
        summary_duration = number(as.integer(max(transaction_date_time) - min(transaction_date_time)), big.mark = ',')
      )
    
    # infoboxes for selected node --------------------------------------------
    # summary tab
    infoBox_server(
      'shy_detail_summary_amt_stat',
      info_summary %>% pull(summary_amount),
      title = 'Avg. Trans. Amount (USD)',
      icon = fa_i('dollar-sign'),
      color = 'green'
    )
    
    infoBox_server(
      'shy_detail_summary_cnt_stat',
      info_summary %>% pull(summary_trans_count),
      title = 'Trans. Count',
      icon = fa_i('calculator'),
      color = 'light-blue'
    )
    
    infoBox_server(
      'shy_detail_summary_date_stat',
      info_summary %>% pull(summary_duration),
      title = 'Days',
      icon = fa_i('calendar'),
      color = 'orange'
    )
   
  }) %>% 
    bindEvent(input$shy_visnet_target_selected, ignoreNULL = TRUE)
  
}
