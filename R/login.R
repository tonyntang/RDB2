#' @import shiny miniUI
getLoginDetails <- function(user, passwd)
{

  ui <- miniPage(
    gadgetTitleBar("IBM DB2 Credential Imput"),
    miniContentPanel(
      textInput("user_txt", "Login ID:", value = user),
      checkboxInput("user_ckbx", label = "Remember me?", value = TRUE),

      passwordInput("pass_txt", "Password:", value = passwd),
      checkboxInput("pass_ckbx", label = "Save the password?", value = TRUE)
    )
  )

  server <- function(input, output, session)
  {

    # When the Done button is clicked, return a value
    observeEvent(input$done,
                 {
                   returnValue <- list(loginID = input$user_txt,
                                       password = input$pass_txt,
                                       save_user = input$user_ckbx,
                                       save_pass = input$pass_ckbx,
                                       success = 1)
                   stopApp(returnValue)
                   })

    observeEvent(input$cancel,
                 {

                   returnValue <- list(loginID = input$user_txt,
                                       password = input$pass_txt,
                                       save_user = input$user_ckbx,
                                       save_pass = input$pass_ckbx,
                                       success = -1)
                   stopApp(returnValue)
                 })
  }

  runGadget(ui, server)
}
