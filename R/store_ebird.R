#' Send filtered eBird data frame to SQLite database.
#'
#' This function appends a \code{\link{data.frame}} created by \code{\link{parse_ebird}}
#' to an existing SQLite database.
#'
#' This requires an existing SQLite database that is ready to receive the output of
#'  \code{\link{parse_ebird}}.  See example below for how the local database was set up
#'  for southeast refuges.
#'
#' @param df a \code{\link{data.frame}} created by \code{parse_ebird}
#' @param file character string of the file path to the tab-delimited eBird .txt file; if
#'  specified, \code{df} is ignored. Useful for combining the parsing and storing of eBird
#'  records in a single step
#' @param db character string containing the file path to the appropriate SQLite database
#' @param table_name character string specifying a DBMS table name
#' @import RSQLite
#' @export
#'
#' @examples
#' \dontrun{
#' # Set up eBird SQLite database
#' library(sqldf)
#' db <- dbConnect(SQLite(), dbname="./Data/SE_eBird.sqlite")
#'
#' dbSendQuery(conn = db,
#'             "CREATE TABLE SE_eBird_Aug2015
#'             (id TEXT,
#'             tax_order REAL,
#'             category TEXT,
#'             common_name TEXT,
#'             sci_name TEXT,
#'             count INTEGER,
#'             county TEXT,
#'             lat REAL,
#'             lon REAL,
#'             date INTEGER,
#'             checklist TEXT,
#'             all_spp INTEGER)")
#'
#' ga_birds <- parse_ebird("./Data/ebd_US-GA_relAug-2015.txt")
#' store_ebird(ga_birds)
#'
#' # Close connection
#' dbDisconnect(db)
#' }

store_ebird <- function(df, file = NULL, db = "../Data/SE_eBird.sqlite", table_name = "SE_eBird_Aug2015") {

    # If file is specified, parse records prior to storage
    # ignoring df if present

    if (!is.null(file)) {
        if (!is.character(file)) stop("File must be specified as a string.")
        df <- parse_ebird(file)
    }

    # Connect and upload to SQLite DB
    db <- dbConnect(SQLite(), dbname = db)
    dbWriteTable(conn = db, name = table_name, value = df, append = TRUE)

    # Close connection
    dbDisconnect(db)

}
