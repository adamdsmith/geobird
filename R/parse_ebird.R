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

parse_ebird <- function(file) {

    # This method has file size limitations, somewhere in the 2 - 3 GB range
    # We're waiting on a bug fix in data.table::fread
    # https://github.com/Rdatatable/data.table/issues/1367

    size <- file.info(file)$size
    vec <- readChar(file, size, useBytes=T)
    vec <- strsplit(vec, c("\t"), fixed=T, useBytes=T)[[1]]
    length(vec) <- length(vec) - 1 # Drop last entry (line break)
    mat <- matrix(vec, ncol = 43, byrow = TRUE)
    col_classes = c("character", "numeric", rep("character", 3),
                    rep("NULL", 2), "character", rep("NULL", 7), "character",
                    rep("NULL", 6), rep("numeric", 2), "character",
                    rep("NULL", 6), "character", rep("NULL", 6),
                    "integer", "character", "integer", rep("NULL", 3))
    keep_cols <- which(col_classes != "NULL")
    col_classes <- col_classes[keep_cols]

    # Drop unwanted columns and first row (eBird column headers)
    mat <- mat[-1, keep_cols]

    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    names(df) <- c("id", "tax_order", "category", "common_name", "sci_name",
                   "count", "county", "lat", "lon", "date", "checklist",
                   "all_spp", "checklist_grp", "approved")

    # Convert column types
    for (i in 1:length(df)){
        FUN = switch(col_classes[i],
                     character = as.character,
                     numeric = as.numeric,
                     integer = as.integer)
        df[,i] = FUN(df[,i])
    }

    # Filter out categories "spuh", "domestic", and "slash" & only to approved records
    df <- dplyr::filter(df,
                        !(category %in% c("spuh", "domestic", "slash")) & approved == 1)

    # Convert "X" counts (i.e., not recorded) to 1 and convert to integer
    # Convert date to POSIXct object
    df <- dplyr::mutate(df,
                        id = gsub("\n", "", id), # get rid of line break characters
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
