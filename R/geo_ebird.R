#' Perform the clip of eBird records based on the exact geometry (plus optional buffers) of
#' a \code{\link[sp]{SpatialPolygonsDataFrame-class}}.
#'
#' The general process is to conservatively buffer \code{query_polys}, identify the US counties
#'  intersecting the buffered \code{query_polys}, then query the local eBird database for records
#'  occurring in these counties.  This may seem inefficient compared to querying the database
#'  directly with the precise desired buffer, and it may be, but it is often impracticable to
#'  have the entire eBird database loaded into RAM.  Thus, this function pares the eBird database
#'  to a manageable size (provided you're not querying an unreasonably large area of interest)
#'  prior to doing the precise extraction of records based on the and any associated buffers.
#'  The precise extraction of records occurs on a polygon-by-polygon basis to avoid complications
#'  arising from eBird record assignment when buffered polygons overlap.  Thus, the same eBird
#'  record may occur in multiple polygons depending on the buffer specification.
#'
#' This function should also work, but is untested, with \code{\link[sp]{SpatialPointsDataFrame-class}} or
#'  \code{\link[sp]{SpatialLinesDataFrame-class}}) if a buffer is provided.
#'
#' As the function currently relies on US counties to query the local eBird database, it works
#'  only for US locations.  This could be expanded quite easily, however, to apply in other locations
#'  (e.g., Canadian provinces, Mexican states, etc.).
#'
#' The WGS 84 / UTM (northern hemisphere) projection within which to perform the precise eBird
#'  record extraction is estimated programmatically based on the centroid (latitude and longitude)
#'  of each polygon.  This should work for all US locations and most of the rest of the planet with
#'  the exception of the area around the southwestern coast of Norway and Svalbard (but it's the least
#'  of our concerns at the moment).
#'
#' @param query_polys \code{\link[sp]{SpatialPolygonDataFrame-class}} within and around which
#'  the user wishes to extract eBird records.  Currently, it is assumed that input \code{query_polys}
#'  are in a geographic (i.e., latitude and longitude) projection.  See Details.
#' @param ebird_sqlite character string file path to local eBird SQLite (.sqlite) database file
#' @param table_name character string naming the table within \code{ebird_sqlite} that
#'  contains the relevant eBird data.  If not specified, defaults to the first table in
#'  \code{ebird_sqlite}
#' @param buffers numeric vector (multiple buffers allowed) of the distance (km) from
#' \code{query_polys} to include in the search for eBird records.  Negative values are permitted.
#' @param which_polys character vector of polygon names in \code{query_polys} for which the
#'  geographic query is desired.  At least as far as default settings go (i.e., searching for
#'  specific refuges in our southeast region eBird database), only the capitalization insensitive
#'  name of the refuge or fish hatchery is required, not the full name (e.g., "piEdMonT" will
#'  perform the geographic query on Piedmont National Wildlife Refuge).
#' @param poly_id character string of the column in \code{query_polys} that contains the name
#'   of the polygon to be used in the output.
##' @param projection character string of PROJ.4 projection arguments; see also
#'   \code{\link[sp]{CRS-class}}.  Default (NULL) buffers based on a WGS 84 / UTM (northern
#'   hemisphere) projection in the UTM zone derived from the centroid (longitude, latitude) of
#'   each \code{query_polys}.  See Details.
#' @return a \code{\link{data.frame}} of eBird records in the \code{query_polys} plus any
#'  associated buffers
#' @export

geo_ebird <- function(query_polys, ebird_sqlite = "../Data/SE_eBird.sqlite",
                           table_name = NULL, buffers = 0, which_polys = NULL,
						   poly_id = "ORGNAME", projection = NULL) {

    if (!is.null(which_polys)) {
        which_polys <- tolower(gsub(" national.*", "", which_polys, ignore.case = TRUE))
        query_polys <- query_polys[tolower(gsub(" NATIONAL.*", "", query_polys@data[, poly_id])) %in% which_polys, ]
    }

    # Extract eBird records in counties intersected by polygons plus buffer
    # Buffer distance at this stage is conservative to avoid excluding potential records
    approx_buffer <- max( max(buffers + 0.25), 2)
    bird_recs <- query_ebird_db(query_polys, ebird_sqlite, table_name = table_name, buffer = approx_buffer)

    # Add UTM zone information to shapefile and bird records
    query_polys@data$zone <- get_UTM_zone(rgeos::gCentroid(query_polys, byid=TRUE)@coords[, 1])
    bird_recs@data$zone <- get_UTM_zone(bird_recs@coords[, 1])

    # Restrict the process to only those UTM zones present in both polygons and bird records
    utm_zones <- sort(intersect(unique(bird_recs@data$zone), unique(query_polys@data$zone)))

    # Split polygons into a list by zone to simplify reprojection for buffering
    zone_list <- lapply(utm_zones, function(zone) query_polys[query_polys@data$zone == zone, ])

    out <- lapply(zone_list, function(zone_polys) {

        # Which zone is to be processed?
        zone <- unique(zone_polys$zone)

        # Extract bird records in this zone
        zone_birds <- bird_recs[bird_recs$zone == zone, ]

        # Project polygons in preparation for precise buffer
        zone_polys_utm <- sp::spTransform(zone_polys, sp::CRS(paste0("+init=epsg:326", zone)))

        # Buffer them at the desired distances
        buff_assess <- lapply(buffers, function(buffer) {
            if (buffer != 0) {
                buffs <- rgeos::gBuffer(zone_polys_utm, width = buffer * 1000, byid = TRUE)
                buffs <- sp::spTransform(buffs, sp::CRS(proj4string(zone_polys)))
            } else {
                buffs <- zone_polys
            }

            bird_list <- lapply(1:length(buffs), function(i) {
                poly <- buffs[i, ]
                tmp <- zone_birds[poly, ]@data
            })

            names(bird_list) <- zone_polys@data[, poly_id]
            bird_list <- plyr::ldply(bird_list)
            names(bird_list)[1] <- "name"
            bird_list

        })

        names(buff_assess) <- buffers
        buff_assess <- plyr::ldply(buff_assess)
        names(buff_assess)[1] <- "buff_dist_km"
        buff_assess$buff_dist_km <- round(as.numeric(buff_assess$buff_dist_km), 3)
        buff_assess

    })

    plyr::ldply(out)

}
