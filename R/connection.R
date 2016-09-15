#' @include login.R
#' @include driver.R
NULL

#' @keywords internal
setOldClass("RODBC")

#' DB2 connection class.
#'
#' @export
#' @import methods DBI
#' @keywords internal
setClass("DB2Connection",
         contains = "DBIConnection",
         slots = list(dbname = "character",
                      host = "character",
                      port = "character",
                      user = "character",
                      odbc = "RODBC"
                      ))

#' DB2 Connection Functions
#' @name DB2Connection-class
NULL

#' @param drv An object created by \code{DB2()}
#' @rdname DB2Connection-class
#' @export
#' @examples
#' \dontrun{
#' db <- dbConnect(DB2::DB2())
#' dbWriteTable(db, "mtcars", mtcars)
#' dbGetQuery(db, "SELECT * FROM mtcars WHERE cyl == 4")
#' }
setMethod("dbConnect",
          "DB2Driver",
          function(drv, dbname = NULL, host = NULL, port = 50000, user = NULL, password = NULL, ...)
          {
            # Check if DB2 connection credential provided
            if(is.null(user) | is.null(password))
            {
              credential <- getLoginDetails(user, password)
              # return(credential)

              if(credential$success == -1) stop("You pressed Cancel")

              user <- credential$loginID
              password <- credential$password
            }

            # basic settings
            if(is.null(dbname)) stop("dbname is not provided!")

            if(is.null(host)) stop("host is not provided")

            # Use a full connection string to connect to database
            txt_con <- paste0("DRIVER=",       drv@driver,
                              ";Database=",    dbname,
                              ";Hostname=",    host,
                              ";Port=",        port,
                              ";PROTOCOL=TCPIP",
                              ";UID=",         user,
                              ";PWD=",         password)

            if(!requireNamespace("RODBC", quietly = TRUE)) stop("RODBC package needed, please install it.", call. = FALSE)
            obj <- tryCatch(RODBC::odbcDriverConnect(txt_con), error = function(e) e)

            new("DB2Connection",
                dbname = dbname,
                host = host,
                port = as.character(port),
                user = user,
                odbc = obj)
          })

#' @export
#' @rdname DB2Connection-class
setMethod("dbGetInfo",
          "DB2Connection",
          function(dbObj, ...)
          {
            info <- RODBC::odbcGetInfo(dbObj@odbc)
            list(dbname = unname(info["DBMS_Name"]),
                 db.version = unname(info["DBMS_Ver"]),
                 username = dbObj@user,
                 host = dbObj@host,
                 port = dbObj@port,
                 sourcename = unname(info["Data_Source_Name"]),
                 servername = unname(info["Server_Name"]),
                 drivername = unname(info["Driver_Name"]),
                 odbc.version = unname(info["ODBC_Ver"]),
                 driver.version = unname(info["Driver_Ver"]),
                 odbcderiver.version = unname(info["Driver_ODBC_Ver"]))
          })


#' @export
#' @rdname DB2Connection-class
setMethod("dbIsValid",
          "DB2Connection",
          function(dbObj, ...)
          {
            tryCatch(
              {
                RODBC::odbcGetInfo(dbObj@odbc)
                TRUE
              },
              error = function(e) FALSE)
          })


#' @export
#' @rdname DB2Connection-class
setMethod("show",
          "DB2Connection",
          function(object)
          {
            if (!dbIsValid(object)) cat("<Database connection invalid>")
            else
            {
              info <- dbGetInfo(object)
              cat("<Database>= ", info$dbname, "\n",
                  "<DB2Connection> ", object@dbname, "@", object@host, ":", object@port, "\n", sep = "")
            }
          })

#' @export
#' @rdname DB2Connection-class
setMethod("dbDisconnect",
          "DB2Connection",
          function(conn)
          {
            tryCatch(RODBC::odbcClose(conn@odbc), error = function(e) e)
            invisible(TRUE)
          })
