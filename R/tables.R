#' Convenience functions for reading/writing DB2 tables
#' @name  DB2-tables
NULL

#' Read the table
#' @return A data.frame in the case of \code{dbReadTable};
#' @note Note that the data.frame returned by \code{dbReadTable} only has
#' primitive data, e.g., it does not coerce character data to factors.
#'
#' @param conn a \code{\linkS4class{DB2Connection}} object, produced by
#'   \code{\link[DBI]{dbConnect}}
#' @param name a character string specifying a table name with schema, e.g syscat.tables. DB2 table names
#' are \emph{not} case sensitive, e.g., table names \code{ABC} and \code{abc} are considered equal.
#' @param check.names If \code{TRUE}, the default, column names will be
#'   converted to valid R identifiers.
#' @param select.cols  A SQL statement, either in the form of
#' 1) a character vector of length 1, e.g. "x, y, z"
#' 2) a character vector of length > 1, e.g. c("x", "y", "z")
#' 3) an expression using ..(), e.g. ..(x, y, z)
#' "*" selects all columns,
#' @param schema a character. If name does not include schema (ie. not like syscat.tables),
#' user must provide schema through this parameter, or by default use "syscat"
#'
#' @export
#' @rdname DB2-tables
setMethod("dbReadTable",
          c("DB2Connection", "character"),
          function(conn, name, check.names = TRUE, select.cols = "*", schema = "syscat")
          {
            if(!grepl("\\.", name))
            {
              # name does not include schema, so add it
              name <- paste(schema, name, sep = ".")
            }

            # select.cols could be character, or something like ..(x, y, z)
            select.cols <- paste(as.character(select.cols), collapse = ",")

            query <- paste("SELECT", select.cols, "FROM",
                           dbQuoteIdentifier(conn, toupper(name)))

            out <- dbGetQuery(conn, query)

            if (check.names)
            {
              names(out) <- make.names(names(out), unique = TRUE)
            }
            out
          })


#' @export
#' @rdname DB2-tables
setMethod("dbListTables",
          c("DB2Connection"),
          function(conn, schemas = NULL, ...)
          {
            if(!is.null(schemas))
            {
              schemas <- dbQuoteString(conn, toupper(schemas))
              schemas <- paste(schemas, collapse = ",")


              out <- dbGetQuery(conn,
                                sprintf("select TABNAME from syscat.tables WHERE TABSCHEMA in (%s)", schemas))[[1]]
            }else
            {

              out <- dbGetQuery(conn, "select TABNAME from syscat.tables")[[1]]
            }
            trimws(out, stringsAsFactors = FALSE)
          })

#' @export
#' @param conn a \code{\linkS4class{DB2Connection}} object, produced by \code{\link[DBI]{dbConnect}}
#' @param name, schema a character vector of length 1 (extra entries will be ignored)
#' @rdname DB2-tables
setMethod("dbExistsTable",
          c("DB2Connection", "character"),
          function(conn, name, schema = NULL)
          {
            toupper(name[1]) %in% dbListTables(conn, schemas = schema[1])
          })

#' @export
#' @keywords internal
setGeneric("dbFindTables",
           def = function(conn, name, ...) {standardGeneric("dbFindTables")})

#' @export
#' @rdname DB2-tables
setMethod("dbFindTables",
          c("DB2Connection", "character"),
          function(conn, name, schema = NULL, , sys.include = FALSE)
          {
            if(is.null(schema)) schema <- dbListSchemas(conn = conn, sys.include = sys.include)
            name <- toupper(name[1])
            schema <- toupper(schema)

            FUNC <- function(schema, name, conn)
            {
              tbs <- dbListTables(conn, schemas = schema)
              tbs <- tbs[grep(name, tbs)]

              if(length(tbs) == 0) NULL
              else paste(schema, tbs, sep = ".")
            }
            unlist(lapply(schema, FUNC, name = name, conn = conn))

          })



#' List fields in specified table.
#'
#' @param conn An existing \code{\linkS4class{DB2Connection}}
#' @param name a length 1 character vector giving the name of a table.
#' @rdname DB2-tables
#' @export
#' @examples
#' \dontrun{
#' con <- dbConnect(DB2(), user = user, password = password)
#' dbListFields(con, "TABNAME")
#' dbDisconnect(con)
#' }
setMethod("dbListFields",
          c("DB2Connection", "character"),
          function(conn, name, schema = "syscat")
          {
            if(!grepl("\\.", name))
            {
              if(is.null(schema))
              {
                stop("Include schema in the name, or a vailid schema must be provided! Try dbListSchemas().")
              }
              schema <- toupper(schema)
              name <- toupper(name)
            }else
            {
              schema <- toupper(gsub("(.*)\\.(.*)", "\\1", name))
              name <- toupper(gsub("(.*)\\.(.*)", "\\2", name))
            }

            out <- dbGetQuery(conn, "select TABSCHEMA, TABNAME, COLNAME from SYSCAT.COLUMNS")

            # out <- dplyr::mutate(out,
            #                      TABSCHEMA = trimws(TABSCHEMA),
            #                      TABNAME = trimws(TABNAME),
            #                      COLNAME = trimws(COLNAME))
            out <- trimws(out)
            out <- dplyr::filter(out, TABSCHEMA == schema & TABNAME == name)
            out$COLNAME
          })
