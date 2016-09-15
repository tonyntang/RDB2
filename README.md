# RDB2
An IBM DB2 database interface for R  
**Under development**: *This package currently can only read from the database.*

## Basic usage
```R
library(RDB2)
# connect to database
drv <- RDB2::DB2("{IBM DB2 ODBC DRIVER}")
drv

# get information about the database driver
dbGetInfo(drv)
dbIsValid(drv)

con <- dbConnect(drv, dbname = "yourdbname", host = "yourwebsite.com", 
                 user = "yourusername", password = "yourpassword")
con

# check if a schema exists
dbExistsSchema(con, "syscat")
dbExistsSchema(con, "SYSCAT")

# To obtain some meta information
dbListSchemas(con)  # list all schemas in the data source
dbListSchemas(con, sys.include = TRUE) # by default, it does not include DB2 system schemas, but in case you want

dbListTables(con) # list all data tables in the source
dbListTables(con, schemas = "syscat") # list all data tables in the source under SYSCAT schema
dbListTables(con, schemas = "SYSCAT") # same as above, NOT case-sensitive
dbListTables(con, schemas = c("syscat", "sysibm")) # list tables under 2 schemas

dbExistsTable(con, "tables") # check if a data table named "tables" exists in the data source
dbExistsTable(con, "NiNg_tANg")

dbExistsTable(con, "tables", "syscat") # you can also restrict the search within a schema

# if you want to find tables according to a name pattern, say any table with name ending in "BLES"
# note: the schema name will be prefixed, so it will look like a full table name (like "libraryname.tablename")
dbFindTables(con, ".*BLES$") # .*BLES is a regular expression
dbFindTables(con, ".*bles$", sys.include = TRUE) 
dbFindTables(con, ".*bles$", schema = 'syscat') 

# You can also list the fields (columns, variables) in a data table
# by default, it will search in the "syscat" schema
dbListFields(con, "tables")

# examples for obtaining data from the database
tb <- dbReadTable(con, "syscat.tables", select.cols = ..(TABSCHEMA, TABNAME))
tb <- dbReadTable(con, "SYSCAT.TABLES", select.cols = ..(TABSCHEMA, TABNAME))
tb <- dbReadTable(con, "SYSCAT.TABLES", select.cols = ..(tabschema, tabname))
tb <- dbReadTable(con, "SYSCAT.TABLES", select.cols = c("tabschema", "tabname"))
tb <- dbReadTable(con, "SYSCAT.TABLES", select.cols = c("TABSCHEMA", "TABNAME"))
tb <- dbReadTable(con, "tables", select.cols = ..(TABSCHEMA, TABNAME), schema = "syscat")
tb <- dbReadTable(con, "TABLES", select.cols = ..(TABSCHEMA, TABNAME), schema = "SYSCAT")

# if you want to submit SQL query directly
qq <- "SELECT * FROM (SELECT TABSCHEMA AS TABSCHEMA, tabname as TABNAME FROM (select * from syscat.tables))"

# 1st, do dbSendQuery.
r <- dbSendQuery(con, qq)
r

# some utility functions for the DB2Result object
dbIsValid(r)
dbColumnInfo(r) # b/c have not started fetching, no information obtained yet
dbGetInfo(r)
dbGetStatement(r)

# 2nd, fetch data using dbFetch
dd <- dbFetch(r, n = 0) # 0 means fetch 0 rows, so a data.frame with 0 rows 0 cols will be returned
dd
r # see the status
dbIsValid(r)
dbColumnInfo(r)

dd <- dbFetch(r, n = 5)
dd
r
dbIsValid(r)
dbColumnInfo(r)

# to check if the query has been completed
dbHasCompleted(r) # since only 5 rows fetched, it's not completed yet

# so try another 5 rows?
dd <- dbFetch(r, n = 5)
dd
dbHasCompleted(r)

# how about fetch all rows on
dd <- dbFetch(r, n = -1)
dd
r # see the difference
dbHasCompleted(r) # should be completed now, meaning all rows have been fetched

# even all rows have been fetched, the query is still valid
dbIsValid(r)
# until you clear it
dbClearResult(r)
dbIsValid(r)

# when finished
dbDisconnect(con)
dbIsValid(con)
```
