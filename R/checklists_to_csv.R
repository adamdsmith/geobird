#' Output *.csv file of the species checklist
#'
#' This function generates, for each queried polygon, a *.csv file of the final species
#'  checklist and abundance classification created by \code{\link{make_checklists}}.  It
#'  requires that the output of \code{\link{make_checklists}} be output to an R object and
#'  the argument \code{xls = FALSE}; see example.  This produces a clean file for easy import
#'  into, e.g., an external database.
#'
#' @param checklist a \code{\link{list}} created by \code{\link{make_checklists}} with
#'  argument \code{xls = FALSE}
#' @param out_dir character string of the file path to the directory in which to save output
#'  comma-separated file(s)
#' @return a comma-separated file for each distinct polygon (i.e., each \code{name} in the
#'   \code{geo_ebird_df} passed to \code{\link{make_checklists}})
#' @export
#'
#' @examples
#' \dontrun{
#' # Get shapefile
#' SErefuges <- rgdal::readOGR("../GIS", "refuges", verbose = FALSE, stringsAsFactors = FALSE)
#'
#' # Query eBird records within Piedmont NWR and 10 km buffer
#' piedmont <- geo_ebird(SErefuges, which_polys = "Piedmont", buffers = c(0, 10))
#'
#' # Generate R object containing abundance, occurrence, final checklist, and effort information
#' # Note to store the output in an R object and specify xls = FALSE
#' piedmont_cl <- make_checklists(piedmont, xls = FALSE)
#' checklists_to_csv(piedmont_cl)
#' }

checklists_to_csv <- function(checklist, out_dir = "../Output/") {

    if (names(checklist)[3] != "checklist")
        stop("Checklist not found in object.  Did your buffers include a distance of zero km?")

    if (substr(out_dir, nchar(out_dir), nchar(out_dir)) != "/")
        out_dir <- paste0(out_dir, "/")

    list_names <- names(checklist[["checklist"]])

    lapply(1:length(checklist[["checklist"]]), function(i) {
        site_name <- list_names[i]
        out_file <- paste0(out_dir, Cap(site_name), ".csv")
        write.csv(checklist[["checklist"]][[site_name]], file = out_file, row.names = FALSE)
    })
    invisible()

}
