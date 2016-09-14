#' @include connection.R
NULL

#' Quote DB2 strings and identifiers.
#'
#' @param conn A \linkS4class{DB2Connection} created by \code{dbConnect()}
#' @param x A character to escaped
#' @param ... Other arguments needed for compatibility with generic
#' @examples
#' library(DBI)
#' con <- dbConnect(RDB2::DB2())
#'
#' x <- c("a", "b c", "d'e", "\\f")
#' dbQuoteString(con, x)
#' dbQuoteIdentifier(con, x)
#' @name quote
NULL

#' @rdname quote
#' @export
setMethod("dbQuoteString",
          c("DB2Connection", "character"),
          function(conn, x, ...)
          {
            x <- gsub("'", "''", x, fixed = TRUE)

            # Not calling encodeString() here, see also http://stackoverflow.com/a/549244/946850
            # and especially the comment by Álvaro González
            str <- paste("'", x, "'", sep = "")
            str[is.na(x)] <- "NULL"
            SQL(str)
          })

#' @rdname quote
#' @export
setMethod("dbQuoteString",
          c("DB2Connection", "SQL"),
          function(conn, x, ...)
          {
            x
          })

#' @rdname quote
#' @export
setMethod("dbQuoteIdentifier",
          c("DB2Connection", "character"),
          function(conn, x, ...)
          {
            # x <- gsub('"', '""', x, fixed = TRUE)
            # SQL(paste('"', encodeString(x), '"', sep = ""))
            SQL(paste(encodeString(x), sep = ""))
          })

#' @rdname quote
#' @export
setMethod("dbQuoteIdentifier",
          c("DB2Connection", "SQL"),
          function(conn, x, ...)
          {
            x
          })
