library(progress)
library(crayon)

read_all_jp_data <- function(path, exclude = NULL, skip = 0) {
  
  files <- dir(path, pattern = '.*xlsx$', full.names = TRUE, all.files = FALSE)
  
  if(!is.null(exclude)) {
    files <- subset(files, !(basename(files) %in% exclude))
    
    cat('The following files are excluded ========== \n')
    cat(green(str_c(exclude, '\n')))
    cat('\n')
  }
  
  # create progress bar --------------------------------------------------------
  bar_total <- length(files)
  pb <- progress_bar$new(
    format = '(:percent) [:bar] Reading :what (:current/:total)  time elapsed: [:elapsed] eta: [:eta]',
    total = bar_total,
    clear = FALSE
  )
  pb$tick(0)
  
  map_dfr(
    files,
    function(x) {
      
      pb$tick(tokens = list(what = basename(x)))
      # read all in as text ------------------------------------------------------
      read_excel(x, col_types = 'text', skip = skip) %>% 
        filter(!is.na(transfer_key)) %>% 
        mutate(file_nm = basename(x)) %>% 
        mutate(
          across(contains('_date'), 
                 function(x) {
                   as.numeric(x) %>%                    
                     as_date(origin = '1899-12-30')
                })
        ) %>% 
        # convert to numeric -----------------------------------------------------
        mutate(
          across(contains('_amount'),
                 function(x) {
                   str_replace_all(x, ',', '') %>% 
                     as.numeric()
                })
        ) %>% 
        mutate(
          across(where(is.character), ~ replace_na(.x, ''))
        )
    }) %>% 
    group_by(transfer_key) %>% 
    filter(row_number() == 1L) %>% 
    ungroup
}
