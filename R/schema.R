#' DB2 Schema-related functions
#' @name DB2-schema
#' @import methods DBI
NULL

#' @export
#' @keywords internal
setGeneric("dbExistsSchema",
           def = function(conn, schema, ...) {standardGeneric("dbExistsSchema")},
           valueClass = "logical")

#' @export
#' @rdname DB2-schema
setMethod("dbExistsSchema",
          c("DB2Connection", "character"),
          function(conn, schema, ...)
          {
            sys_schemas <- dbGetQuery(conn, "SELECT SCHEMANAME from SYSCAT.SCHEMATA")[[1]]
            sys_schemas <- trimws(sys_schemas)
            toupper(schema) %in% sys_schemas
          })

#' @export
#' @keywords internal
setGeneric("dbListSchemas",
           def = function(conn, ...) {standardGeneric("dbListSchemas")},
           valueClass = "character")

#' @export
#' @rdname DB2-schema
setMethod("dbListSchemas",
          c("DB2Connection"),
          function(conn, sys.include = FALSE)
          {
            cond <- ifelse(sys.include, "", "where SCHEMANAME not like 'SYS%'")
            query <- paste("SELECT SCHEMANAME from SYSCAT.SCHEMATA", cond)

            out <- dbGetQuery(conn, query)[[1]]
            trimws(out)
          })
