# server.R
function(input, output, session) {

  # create a session-wide global variable
  v <- 
    reactiveValues(
      question_list_file = character(),
      responder_netid = character(),
      responder_name = character(),
      responder_key = character(),
      page = 0L,
      n_pages = 0L,
      question_pages = NULL,
      status = 'in_progress',
      data = tibble(
        input_type = character(),
        input_id = character(),
        response = character(),
        update_type = character(),
        update_arg = character()
      )
    )
  
  # Validation functions
  
  
  # This renderUI function holds the primary actions of the
  # survey area.
  output$id_uiMainAction <- renderUI({
    dynamicUi()
  })
  
  observeEvent(
    input$id_btn_goto_question,
    {
      updateTabItems(session, 'id_sidebarmenu', 'main_questionnaire')
    }
  )
  
  observeEvent(
    input$id_btn_start,
    {
      if(is.null(input$id_btn_start)) return(NULL)
      # disable all input for the processing moment
      toggle_inputs(reactiveValuesToList(input), FALSE)
    
      user_info <- tibble(
        ID = isolate(input$id_responder_id),
        NAME = isolate(input$id_responder_name),
        KEY = isolate(input$id_responder_key)
      )
      # clean the text field inputs, leave no extra space,etc.
      user_info <- user_info %>% 
        mutate_if(is.character, str_squish)
      
      assignment <- read_excel('data/Assignment.xlsx', sheet = 'CUSTOMER_2020')
      
      user_info <- user_info %>% 
        inner_join(assignment, by = c('ID', 'NAME', 'KEY'))
      
      if(nrow(user_info) != 1L) {
        sendSweetAlert(
          session = session,
          title = "Warning !!!",
          text = "The provided information was not valid! Please try again or contact Chadi for support. ",
          type = "warning"
        )

        # enable all input and buttons
        toggle_inputs(reactiveValuesToList(input), TRUE)
      } else {

        v$question_list_file <- user_info %>% pull(QUESTION_SET)
        v$responder_netid <- user_info[1, ] %>% pull(ID)
        v$responder_name <- user_info[1, ] %>% pull(NAME)
        v$responder_key <- user_info[1, ] %>% pull(KEY)
        # begin to create questionnaire pages
        source('generate_q_pages.R', local = TRUE)
        
        # update the page number and it will automatically forward to the next page
        v$page <- v$page + 1
      }
    }
  )
  
  observeEvent(
    input$id_btn_next,
    {
      if(is.null(input$id_btn_next)) return(NULL)
      toggle_inputs(reactiveValuesToList(input),
                    FALSE)
      
      # if there is saved data, then update the inputs first
      if(nrow(v$data) > 0) {
        load_saved_info(v, input, session)
      }
      
      # starting after the 1st page, we can save the responses
      if(v$page > 0) {
        save_response(v, input, session)
      }
      v$page <- v$page + 1
    })
  
  observeEvent(
    input$id_btn_back,
    {
      if(is.null(input$id_btn_back)) return(NULL)
      
      toggle_inputs(reactiveValuesToList(input),
                    FALSE)
      
      # starting after the 1st page, we can save the responses
      if(v$page > 0 & v$page <= v$n_pages) {
        save_response(v, input, session)
      }
      
      # update current page anchor
      v$page <- v$page - 1
      # if there is saved data, then load the saved inputs first
      if(nrow(v$data) > 0 & v$page > 0) {
        load_saved_info(v, input, session)
      }
    })
  
  observeEvent(
    input$id_btn_submit,
    {
      # perform any save tasks
      out_fn <- str_c('output/', v$responder_netid, '-', v$responder_key, '.xlsx')
      write_xlsx(list(AML = v$data %>% select(input_id, response)),
                 out_fn)
      v$status <- 'done'
      
      # show a success popup alert
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "Questions Submitted",
        type = "success"
      )
    }
  )
  
  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (v$page == 0)
      return(
        welcome_page
      )
  
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (v$page > 0 & v$page <= v$n_pages)  
      return(
        v$question_pages[[v$page]]$page_code
      )
    
    # Finally we see results of the survey as well as a
    # download button.
    
    if (v$page > v$n_pages) {
      if(v$status == 'in_progress') return(submit_page)
      else if(v$status == 'done') return(thankyou_page)
    }
      
  })
  
}

server===============================================

# ui.R

dashboardPage(
  header = dashboardHeader(
    title = tagList(
      span(class = "logo-lg", 'Risk Assessment'), 
      # img(src = "https://image.flaticon.com/icons/svg/204/204074.svg")
      icon('poll-h')
      )
    ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = 'id_sidebarmenu',
      menuItem("What's new?", tabName = 'info_page', icon = icon('info-circle')),
      menuItem('Questionnaire', tabName = 'main_questionnaire', icon = icon("question-circle"))
    )
  ),
  
  body = dashboardBody(
    useShinyjs(),  # Include shinyjs
    useSweetAlert(),
    
    tabItems(
      tabItem(
        tabName = 'info_page',
        fluidRow(
          box(
            title = "What's New?",
            status = 'info',
            width = 12,
            closable = FALSE,
            collapsible = FALSE,
            solidHeader = TRUE,
            h5('Introduction'),
            p('We can put some informational text here.'),
            actionButton("id_btn_goto_question", "Next")
          )
        )
      ),
      
      tabItem(
        tabName = 'main_questionnaire',
        uiOutput("id_uiMainAction")
      )
    )
  )
)
UI ================================
# utility packages
library(dplyr)
library(readr)
library(readxl)
library(writexl)
library(stringr)
library(tidyr)
library(glue)

# Shiny packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)

# preload all required codes
source('functions.R', local = TRUE)
source('welcome_page.R', local = TRUE)
source('thankyou_page.R', local = TRUE)
source('submit_page.R', local = TRUE)

global ===========================

# functions.R

# Function to toggle input elements ---------------------------------------------------------------
# input_list: List of inputs, reactiveValuesToList(input)
# enable_inputs: Enable or disable inputs?
# Only buttons: Toggle all inputs, or only buttons?
toggle_inputs <- function(input_list, enable_inputs = TRUE, only_buttons = FALSE)
{
  # Subset if only_buttons is TRUE.
  if(only_buttons){
    buttons <- which(sapply(input_list, function(x) {any(grepl('Button',attr(x,"class")))}))
    input_list = input_list[buttons]
  }
  
  # Toggle elements
  for(x in names(input_list))
    if(enable_inputs){
      shinyjs::enable(x)} else {
        shinyjs::disable(x) }
}

# save current page responses ---------------------------------------------------------------------
save_response <- function(v, input, session) {
  
  x <- v$question_pages[[v$page]]$page_questions %>% 
    filter(!is.na(input_type)) %>% 
    select(input_type, input_id, update_arg) %>% 
    mutate(response = str_c('input$', input_id)) %>% 
    rowwise() %>% 
    mutate(response = eval(parse(text = response)),
           response = as.character(response))
  
  v$data <- v$data %>% 
    rows_upsert(x, by = 'input_id')
}

# load saved responses for current page -----------------------------------------------------------
load_saved_info <- function(v, input, session) {
  x <- v$data %>% 
    mutate(update_type = str_c(str_to_upper(str_sub(input_type, 1, 1)), str_sub(input_type, 2)),
           update_type = str_c('update', update_type)) %>% 
    mutate(update_code = glue('{update_type}(session, inputId = "{input_id}", {update_arg} = "{response}")')) 
  
  apply(x %>% select(update_code), 1, function(x) eval(parse(text = x)))
}

functions =================================
        
library(tidyverse)
library(readxl)
library(writexl)

# settings ----------------------------------------------------------------------------------------
fn <- 'data/Assignment.xlsx'
sn <- 'CUSTOMER_2020'

# Do not change anything below --------------------------------------------------------------------
`%ni%` <- Negate('%in%')

assignment <- read_excel(fn, na = '', col_types = 'text', sheet = sn)

if('KEY' %ni% colnames(assignment)) {
  assignment$KEY <- NA_character_
}

assignment <- assignment %>% 
  filter(is.na(KEY)) %>% 
  mutate(KEY = stringi::stri_rand_strings(nrow(.), 16)) %>% 
  bind_rows(assignment %>% filter(!is.na(KEY))) %>% 
  arrange(RECORD_ID)

l <- list(assignment)
l <- setNames(l, sn)
write_xlsx(l, path = fn)

generate key ======================

 # Read in the survey questions
# the following codes are to be executed after user enters their questionnaire info correctly

d <- read_excel(
  v$question_list_file,
  # 'data/Questionnaire_1.xlsx',
  sheet = 'AML_Customer',
  col_types = 'text'
  ) %>% 
  mutate(
    to_ask = str_to_upper(str_trim(to_ask))
  ) %>% 
  filter(
    is.na(to_ask) | to_ask != 'N'
  ) %>% 
  fill(id) %>% 
  group_by(id, .drop = TRUE) %>% 
  mutate(
    n_to_ask = sum(to_ask == 'Y', na.rm = TRUE)
  ) %>% 
  filter(n_to_ask != 0) %>%
  select(-n_to_ask) %>% 
  ungroup() %>% 
  mutate(
    text = str_replace_all(text, '"([:alnum:]*)"', '<b>\\1'),
    text = str_squish(text),
    input_id = str_c(id, str_replace_na(sub_id, replacement = ''), sep = '')
  ) %>% 
  mutate(PAGE = as.integer(as.factor(id)))

# calculate total number of questions
v$n_pages <- d %>% 
  summarize(N = n_distinct(id)) %>% 
  pull(N)

v$question_pages <- d %>% 
  group_by(id) %>% 
  group_map(
    .keep = TRUE,
    function(x, ...) {
      contents <- x %>% 
        mutate(COMMAND = case_when(is.na(input_type) ~ glue('p("{text}")'),
                                   TRUE ~ glue('{input_type}(inputId = "{input_id}",  label = "{text}", {args})')
        )
        ) %>% 
        select(COMMAND) %>% 
        summarize(COMMANDS = str_c(COMMAND, collapse = ', '))
      
      id_txt <- x$id[1]
      
      show_backbutton <- if(x$PAGE[1] == 1) "" else "actionButton('id_btn_back', 'Back'),"
      cmd <- glue("box(title = str_c('Question {x$PAGE[1]} of {v$n_pages}: ', '{id_txt}'),
                  {contents},
                  status = 'success',
                  width = 12,
                  closable = FALSE,
                  collapsible = FALSE,
                  solidHeader = TRUE,
                  footer_padding = FALSE,
                  {show_backbutton}
                  actionButton('id_btn_next', 'Next'))
                  ")
      
      list(page_code = eval(parse(text = cmd)),
           page_id = id_txt,
           page_questions = x
      )
    })

# v$n_pages <- length(v$question_pages)
names(v$question_pages) <- str_c(1:v$n_pages, ' of ', v$n_pages)
        
        generate q page ===================================
        
        
        
        # construct the Submit page
submit_page <- fluidRow(
  box(
    title = 'MUFG FY2019 AML, Sanctions, and ABC GRA - Inherent Risk Questionnaire',
    status = 'success',
    width = 12,
    closable = FALSE,
    collapsible = FALSE,
    solidHeader = TRUE,
    footer_padding = FALSE,
    p('Congratulations! You finished all the questions! Do you want to submit now?'),
    actionButton('id_btn_back', 'Back'),
    actionButton('id_btn_submit', 'Submit')
  )
)

        submit page ==============================
        
        # construct a thank you page
thankyou_page <- fluidRow(
  box(
    title = 'MUFG FY2019 AML, Sanctions, and ABC GRA - Inherent Risk Questionnaire',
    status = 'success',
    width = 12,
    closable = FALSE,
    collapsible = FALSE,
    solidHeader = TRUE,
    footer_padding = FALSE,
    p('You finished the quetionnaire! Thank you!')
  )
)
thank you page ============================
        
        
        # construct welcome page
welcome_page <- fluidRow(
  box(
    title = 'MUFG FY2019 AML, Sanctions, and ABC GRA - Inherent Risk Questionnaire',
    status = 'danger',
    width = 12,
    closable = FALSE,
    collapsible = FALSE,
    solidHeader = TRUE,
    footer_padding = FALSE,
    p('Review Period: April , 2019- March 31, 2020')
  ),
  
  box(
    title = 'Contents',
    status = 'danger',
    width = 12,
    closable = FALSE,
    collapsible = TRUE,
    solidHeader = TRUE,
    h5('Introduction'),
    p('Business Overview'),
    p('Category 1: Customer'),
    p('Category 2: Products/Services'),
    p('Category 3: Transactions'),
    p('Category 4: Geography_1'),
    p('Category 4: Geography_2')
  ),
  
  box(
    title = 'Workbook Overview',
    status = 'danger',
    width = 12,
    closable = FALSE,
    collapsible = TRUE,
    solidHeader = TRUE,
    p("This workbook is the Inherent Risk Questionnaire (IRQ) for MUFG's FY 2019 AML, sanctions, and ABC Risk Assessment. Inherent risk refers to MUFG's specific level of exposure to money laundering and sanctions risks which arise from their business model, before taking into consideration any mitigating factors."),
    p("This IRQ is used to gather critical information regarding Line of Business (LoB)-specific risks, related to AML, sanctions, and ABC concerns. An LoB's inherent risk rating is determined through the analysis of qualitative and quantitative data related to MUFG's customer, products/services, transactions, and geography risk exposure."),
    p("All questions in this questionnaire should be answered by the LoB, after which the completeness and accuracy of the data should be reviewed and approved by the General Manager (or a local equivalent) to ensure timely submission. The information provided in this workbook will be utilized for the risk assessment, including scoring and reporting. Please express values of transactions in terms of USD, using the conversion rate as of the reporting date."),
    p('If the requested information is not available, please select a drop down option "Data Not Available" or type "Data Not Available" in the corresponding cell. If the cell doesn\'t allow to enter free text, please type "DATA NOT AVAILABLE" in a corresponding cell of the "Notes for LoB" column.'),
    p("REVIEW PERIOD"), 
    p("April 1, 2019 through March 31, 2020 (FY2019)")
  ),
  
  # This displays the action putton Next.
  box(
    fluidRow(
      column(
        width = 4,
        textInput("id_responder_id", label = '', width = '100%', placeholder = "Enter your NetID...")
      ),
      column(
        width = 4,
        textInput("id_responder_name", label = '', width = '100%', placeholder = "Enter your name...")
      ),
      column(
        width = 4,
        textInput("id_responder_key", label = '', width = '100%', placeholder = "Enter your questionnaire key...")
      )
    ),
    actionButton("id_btn_start", "Next"),
    status = 'primary',
    width = 12,
    closable = FALSE,
    collapsible = FALSE,
    solidHeader = FALSE
  )
)

        welcome page===============================
        
