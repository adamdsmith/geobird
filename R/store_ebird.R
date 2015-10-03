#' Send filtered eBird data frame to SQLite database.
#'
#' This function appends a \code{\link{data.frame}} created by \code{parse_ebird}
#' to an existing SQLite database.
#'
#' @param df a \code{\link{data.frame}} created by \code{parse_ebird}
#' @param db character string containing the file path to the appropriate SQLite database
#' @param table_name character string specifying a DBMS table name
#' @import RSQLite
#' @export

store_ebird <- function(df, db = "../Data/SE_eBird.sqlite", table_name = "SE_eBird_Aug2015") {

    # Connect and upload to SQLite DB
    db <- dbConnect(SQLite(), dbname = db)
    dbWriteTable(conn = db, name = table_name, value = df, append = TRUE)

    # Close connection
    dbDisconnect(db)

}
