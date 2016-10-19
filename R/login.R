#' @import shiny miniUI
getLoginDetails <- function(user, passwd)
{

  ui <- miniPage(
    gadgetTitleBar("IBM DB2 Credential Imput"),
    miniContentPanel(
      textInput("user", "Login ID:", value = user),
      passwordInput("pass", "Password:", value = passwd)
    )
  )

  server <- function(input, output, session)
  {

    # When the Done button is clicked, return a value
    observeEvent(input$done,
                 {
                   returnValue <- list(loginID = input$user,
                                       password = input$pass,
                                       success = 1)
                   stopApp(returnValue)
                   })

    observeEvent(input$cancel,
                 {

                   returnValue <- list(loginID = input$user,
                                       password = input$pass,
                                       success = -1)
                   stopApp(NULL)
                 })
  }

  runGadget(ui, server)
}
