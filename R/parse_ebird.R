#' Parse eBird database query and prepare for SQLite database storage
#'
#' This function parses the tab-delimited text files (.txt) provided by Cornell.  Only fields
#' relevant to the calculation of species-level geographic occurrence are preserved, and
#' filters are applied to remove eBird records that were not identified to at least the species
#' level, duplicate records present in group checklists and non-approved records.  Records for
#' species with an unspecified count of individuals are assigne a minimum value (i.e., 1) to
#' indicate occurrence.
#'
#' @param file character string of the file path to the tab-delimited eBird .txt file
#' @return a \code{\link{data.frame}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Parse and store eBird data from Florida into our SE_ebird SQLite database
#' # Assumes local database exists and prepared to accept records; see ?store_ebird
#' fl_birds <- parse_ebird("./Data/ebd_US-FL_relAug-2015.txt")
#' store_ebird(fl_birds)
#' }

parse_ebird <- function(file) {

    df <- data.table::fread(file, sep = "\t", header = TRUE, na.strings = c("NA", ""), quote = "",
                            colClasses = c("character", "numeric", rep("character", 3),
                                           rep("NULL", 2), "character", rep("NULL", 7), "character",
                                           rep("NULL", 6), rep("numeric", 2), "character",
                                           rep("NULL", 6), "character", rep("NULL", 6),
                                           "integer", "character", "integer", rep("NULL", 3)),
                            col.names = c("id", "tax_order", "category", "common_name", "sci_name",
                                          "count", "county", "lat", "lon", "date", "checklist",
                                          "all_spp", "checklist_grp", "approved"))

    # Filter out categories "spuh", "domestic", and "slash" & only to approved records
    df <- df[!(category %in% c("spuh", "domestic", "slash", "hybrid")) & approved == 1, , ]

    # Convert "X" counts (i.e., not recorded) to 1 and convert to integer
    # Convert date to POSIXct object
    df <- dplyr::mutate(df,
                        count = as.integer(ifelse(count == "X", 1, count)),
                        date = lubridate::ymd(date))

    # Find and remove duplicates (i.e., checklist groups)
    # Sort so preserved record has highest count (if discrepancy)
    df <- dplyr::arrange(df, checklist_grp, tax_order, checklist, -count)
    dups <- duplicated(dplyr::select(df, -checklist, -id, -count))
    df <- df[!dups, ]

    # Drop "approved" and "checklist_grp" columns
    df <- dplyr::select(df, -approved, -checklist_grp)

}
