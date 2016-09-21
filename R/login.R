#' @import tcltk2
#' @import tcltk
getLoginDetails <- function(user, passwd)
{
  tt <- tktoplevel()
  tkwm.title(tt, "Login Credentials")

  Name <- if(is.null(user)) tclVar("Login ID") else tclVar(user)
  Password <- if(is.null(passwd)) tclVar("Password") else tclVar(passwd)
  Success <- tclVar(0)

  entry.Name <- tkentry(tt, width = "20", textvariable = Name)
  entry.Password <- tkentry(tt, width= "20", show = "*",
                            textvariable = Password)

  tkgrid(tklabel(tt, text = "Please enter your login details."))
  tkgrid(entry.Name)
  tkgrid(entry.Password)

  OnOK <- function()
  {
    tclvalue(Success) <- 1
    tkdestroy(tt)
  }

  onCancel <- function()
  {
    tclvalue(Success) <- -1
    tkdestroy(tt)
  }

  butOK <- tkbutton(tt, text = " OK ", width = -6, command = OnOK)
  butCancel <- tkbutton(tt, text = "  Cancel ", width = -6, command = onCancel)


  tkbind(entry.Password, "<Return>", OnOK)

  tkbind(entry.Name, "<Return>",
         function()
         {
           tkfocus(entry.Password)
           tclvalue(Password) <- ""
         })

  tkgrid(butCancel, butOK, padx = 10, pady = c(0, 15))

  tkfocus(tt)
  tkwait.window(tt)

  invisible(list(loginID = tclvalue(Name), password = tclvalue(Password), success = tclvalue(Success)))
}

