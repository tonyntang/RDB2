# compute aggregations based on from/to pairs ----------------------------------
agg_graph <- function(x) {
  agg_edge <- x %>% 
    activate(edges) %>% 
    as_tibble %>% 
    group_by(from, to) %>% 
    summarize(
      total_amt_usd = sum(dia_trans_amt_usd),
      trans_count = n(),
      pairs = first(pairs),
      .groups = 'drop'
    )
  
  tbl_graph(
    nodes = x %>% activate(nodes) %>% as_tibble,
    edges = agg_edge,
    node_key = 'id'
  ) %>% 
    mutate(
      # create centrality statistics ---------------------------------------------
      centrality = centrality_hub(),
      community = group_components(type = 'strong'),
      
      # create other variables for visualization ---------------------------------
      # on nodes -----------------------------------------------------------------
      title = bus_name,
      label = bus_name,
      group = case_when(
        is_SOI == 'Y' & is_Shell == 'Y' ~ 'SOI_Shell',
        is_SOI == 'Y' & is_Shell == 'N' ~ 'SOI_non_Shell',
        is_SOI == 'N' & is_Shell == 'Y' ~ 'non_SOI_Shell',
        is_SOI == 'N' & is_Shell == 'N' ~ 'non_SOI_non_Shell'
      ),
      size = centrality * 10 + 5
    ) %>%
    # on edges -------------------------------------------------------------------
  activate(edges) %>% 
    mutate(
      label = glue('{dollar(total_amt_usd / 1000, accuracy = 1)}K ({number(trans_count, accuracy = 1)})')
    )
}

# 
clear_contents <- function(x) {
  
}
