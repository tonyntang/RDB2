#' @import methods DBI
NULL

#' DB2 results class.
#'
#' @keywords internal
#' @export
DB2Result <- setClass("DB2Result",
                      contains = "DBIResult",
                      slots = list(conn = "DB2Connection",
                                   statement = "character",
                                   status = "environment"
                                   ))

#' @title DB2 Result Functions
#'
#' @name  DB2Result-class
NULL

#' @export
setGeneric("getStatus",
           function(x, what, ...) {standardGeneric("getStatus")})

#' @export
setGeneric("setStatus<-",
           function(x, what, value, ...) {standardGeneric("setStatus<-")},
           valueClass = "DB2Result")

#' @export
setMethod("getStatus",
          c("DB2Result", "character"),
          function(x, what, ...)
          {
            x@status[[what]]
          })

#' @export
setMethod("setStatus<-",
          c("DB2Result", "character"),
          function(x, what, value, ...)
          {
            x@status[[what]] <- value
            x
          })

#' Send a query to DB2
#'
#' @export
#' @rdname DB2Result-class
#' @examples
#' # This is another good place to put examples
setMethod("dbSendQuery",
          c("DB2Connection", "character"),
          function(conn, statement, ...)
          {
            statement <- enc2utf8(statement)

            ans <- RODBC::odbcQuery(conn@odbc, statement)
            if(ans == -1) stop("Query failed.")

            env <- new.env()
            # rowCount = "integer",
            # valid = "logical",
            # completed = "logical",
            # fields = "data.frame"

            assign("completed", FALSE, envir = env)
            assign("valid", TRUE, envir = env)
            assign("rowCount", 0, envir = env)
            assign("fields", data.frame(Name = character(), Type = character()), envir = env)

            new("DB2Result",
                conn = conn,
                statement = statement,
                status = env
              )
          })

#' @export
#' @rdname DB2Result-class
setMethod("dbClearResult", "DB2Result",
          function(res, ...)
          {
            setStatus(res, "valid") <- FALSE
            TRUE
          })

#' Retrieve records from DB2 query
#' @rdname DB2Result-class
#' @export
setMethod("dbFetch",
          "DB2Result",
          function(res, n = -1, stringsAsFactors = FALSE, ...)
          {
            n <- as.integer(n)

            if(getStatus(res, "completed")) stop("Query alreday completed!")

            if(n == -1L)
            {
              ans <- RODBC::sqlGetResults(res@conn@odbc, max = 0, ...)

              if(!is.data.frame(ans)) ans <- data.frame()
              else
              {
                m <- dim(ans)[1]

                if(getStatus(res, "rowCount") == 0)
                {
                  setStatus(res, "fields") <- data.frame(Name = colnames(ans),
                                                       Type = sapply(ans, typeof),
                                                       row.names = 1:dim(ans)[2]
                                                       )
                  setStatus(res, "rowCount") <- m
                }else setStatus(res, "rowCount") <- m + getStatus(res, "rowCount")
              }
              setStatus(res, "completed") <- TRUE
            }else if(n == 0L)
            {
              ans <- data.frame()
            }else if(n >= 1L)
            {
              ans <- RODBC::sqlGetResults(res@conn@odbc, max = n, ...)

              if(!is.data.frame(ans))
              {
                ans <- data.frame()
                setStatus(res, "completed") <- TRUE
              }else
              {
                m <- dim(ans)[1]

                if(getStatus(res, "rowCount") == 0)
                {
                  setStatus(res, "fields") <- data.frame(Name = colnames(ans),
                                                         Type = sapply(ans, typeof),
                                                         row.names = 1:dim(ans)[2])
                  setStatus(res, "rowCount") <- m
                }else setStatus(res, "rowCount") <- m + getStatus(res, "rowCount")

                if(m < n)
                  setStatus(res, "completed") <- TRUE
              }
            }
            trimws(ans, stringsAsFactors = stringsAsFactors)
            })

#' @export
#' @rdname DB2Result-class
setMethod("dbHasCompleted",
          "DB2Result",
          function(res, ...)
          {
            getStatus(res, "completed")
          })

#' @export
#' @rdname DB2Result-class
setMethod("dbGetInfo",
          "DB2Result",
          function(dbObj, ...)
          {
            list(statement = dbObj@statement,
                 connection = dbObj@conn,
                 status = dbObj@status
                 )
          })

#' @export
#' @rdname DB2Result-class
setMethod("dbIsValid",
          "DB2Result",
          function(dbObj, ...)
          {
            getStatus(dbObj, "valid")
          })

#' @rdname DB2Result-class
#' @export
setMethod("dbGetStatement", "DB2Result",
          function(res, ...)
          {
            res@statement
          })

#' @rdname DB2Result-class
#' @export
setMethod("dbColumnInfo", "DB2Result",
          function(res, ...)
          {
            getStatus(res, "fields")
          })

#' @export
#' @rdname DB2Result-class
setMethod("dbGetRowCount",
          "DB2Result",
          function(res, ...)
          {
            getStatus(res, "rowCount")
          })
