node_fn <- file.path(data_folder, 'node_v2.csv')
edge_fn <- file.path(data_folder, 'edge_v2.csv')


# dx_node <- read_excel(node_fn, col_types = 'text') %>% 
#   mutate(`Clean Name` = replace_na(`Clean Name`, ''))
# 
# dx_edge <-  read_excel(edge_fn, col_types = 'text') %>% 
#   rename(
#     to = to_id,
#     from = from_id
#   )

dx_node <- read_csv(node_fn)

dx_edge <- read_csv(
  edge_fn, 
  col_types = list(.default = col_character())
  ) %>% 
  mutate(
    across(contains(c('_amount', '_amt')), ~ as.numeric(.x)),
    across(contains(c('_date', '_dt')), ~ as_date(.x))
  ) %>% 
  # when in/out bound directions both exist, make sure edges are separated
  # otherwise, make it as straight by default
  rowwise() %>% 
  mutate(
    pairs = c(from, to) %>% sort %>% str_c(collapse = '') 
  ) %>%
  ungroup


# global variables =============================================================
# Network analysis processing --------------------------------------------------
dg_txn_raw <- tbl_graph(
  nodes = dx_node,
  edges = dx_edge,
  node_key = 'id'
)

glb_min_dt <- dg_txn_raw %>% activate(edges) %>% pull(transaction_date_time) %>% min
glb_max_dt <- dg_txn_raw %>% activate(edges) %>% pull(transaction_date_time) %>% max

# dg %>% 
#   filter(is_SOI == 'Y') %>% 
#   ggraph(layout = 'kk') +
#   geom_edge_link() +
#   geom_node_point(size = 8, color = 'steelblue') +
#   geom_node_text(aes(label = bus_name), color = 'white', vjust = .4)

