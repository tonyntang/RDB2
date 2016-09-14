#' DB2 database driver functions.
#' @name DB2Driver-class
NULL

#' @export
#' @import methods DBI
#' @keywords internal
setClass("DB2Driver",
         contains = "DBIDriver",
         slots = list(driver = "character"))

#' DB2 driver
#'
#' This driver is for IBM DB2 database
#'
#' @export
#' @examples
#' RDB2::DB2()
DB2 <- function(driver = "{IBM DB2 ODBC DRIVER}")
{
  new("DB2Driver", driver = driver)
}

#' @export
#' @rdname DB2Driver-class
setMethod("dbUnloadDriver",
          "DB2Driver",
          function(drv, ...)
          {
            TRUE
          })

#' @export
#' @rdname DB2Driver-class
setMethod("show",
          "DB2Driver",
          function(object)
          {
            cat("<DB2Driver = ", object@driver, ">", sep = "")
          })

#' @export
#' @rdname DB2Driver-class
setMethod("dbGetInfo",
          "DB2Driver",
          function(dbObj, ...)
          {
            # cat("<DB2 Driver = ", dbObj@driver, ">", sep = "")
            c(DB2_Drivername = dbObj@driver)
          })

#' @export
#' @rdname DB2Driver-class
setMethod("dbIsValid",
          "DB2Driver",
          function(dbObj, ...)
          {
            ifelse(dbObj@driver != "", TRUE, FALSE)
          })
