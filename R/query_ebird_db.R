#' Extract records from local eBird SQLite database that occur in US counties overlapped by
#' user-specified \code{\link[sp]{SpatialPolygonsDataFrame-class}} plus an optional buffer.
#'
#' This function is called from \code{\link{geo_ebird}} and is thus not intended to
#' be called outside of that functionality.
#'
#' @param query_polys \code{\link[sp]{SpatialPolygonDataFrame-class}} within and around which
#'  the user wishes to extract eBird records.  It should also work, but is untested, with
#'  \code{\link[sp]{SpatialPointsDataFrame-class}} or
#'  \code{\link[sp]{SpatialLinesDataFrame-class}}) if a buffer is provided.  The buffer settings
#'  currently assume that input \code{query_polys} are in a geographic (i.e., latitude and
#'  longitude) projection.
#' @param ebird_sqlite character string file path to local eBird SQLite (.sqlite) database file
#' @param buffer Approximate distance (km) from \code{query_polys} to include
#'  in the search for eBird records.  A numeric vector of length 1. Negative values
#'  are allowed.
#' @param table_name character string naming the table within \code{ebird_sqlite} that
#'  contains the relevant eBird data.  If not specified, defaults to the first
#'  table in \code{ebird_sqlite}
#' @param SPDF logical (default = TRUE) indicating whether the output should be
#'  converted into a \code{\link[sp]{SpatialPointsDataFrame-class}}.  If TRUE,
#'  the returned \code{\link[sp]{SpatialPointsDataFrame-class}} is in the same
#'  projection as \code{query_polys}
#' @return a \code{\link{data.frame}} of eBird records in the counties represented in the input
#'  polygons (and an associated buffer)
#' @import dplyr

query_ebird_db <- function(query_polys, ebird_sqlite = "../Data/SE_eBird.sqlite",
                          buffer = 2, table_name = NULL, SPDF = TRUE) {

    ## Create a minimally large buffer around polygons
    # Conversion is not exact, but represents a minimum buffer of `buffer` km of longitude @ 38o latitude
    buffer <- buffer / 87.723
    byid <- ifelse(length(query_polys) == 1, FALSE, TRUE)
    query_polys_buffer <- suppressWarnings(rgeos::gBuffer(query_polys, width = buffer, byid = byid))

    ## Load US county shapefile and transform to match polygons
	counties <- system.file("extdata", "us_counties.shp", package = "geobird")
    path_counties <- counties %>% strsplit(., "/") %>% unlist() %>% head(., -1) %>% paste(., collapse = "/")
    shape_counties <- counties %>% strsplit(., "/") %>% unlist() %>% tail(., 1) %>% tools::file_path_sans_ext()
	counties <- rgdal::readOGR(path_counties, shape_counties, verbose = FALSE, stringsAsFactors = FALSE)
    counties <- sp::spTransform(counties, sp::CRS(sp::proj4string(query_polys_buffer)))

    ## Clip to relevant counties
    counties_clip <- counties[query_polys_buffer,]
    county_list <- unique(counties_clip@data$county)

    ## Load eBird data in relevant counties
    # Establish connection with SQLite database
    db <- DBI::dbConnect(RSQLite::SQLite(), ebird_sqlite)

    # If no table specified, use the first
    if (is.null(table_name)) table_name <- DBI::dbListTables(db)[1]

    # Set up and execute query
    where <- paste0("county IN (", paste0("'", county_list, "'", collapse = ","), ")")
    ebird <- DBI::dbGetQuery(db, sprintf("SELECT * FROM %s WHERE %s", table_name, where))
    DBI::dbDisconnect(db)

    # Correct date for SQLite's inability to store in a date format
    ebird$date <- as.POSIXct(ebird$date, origin = "1970-01-01", tz = "GMT")

    if (SPDF) {
        sp::coordinates(ebird) <- ~ lon + lat
        sp::proj4string(ebird) <- sp::CRS(sp::proj4string(query_polys_buffer))
        ebird <- ebird[query_polys_buffer, ]
    }

    ebird
}
