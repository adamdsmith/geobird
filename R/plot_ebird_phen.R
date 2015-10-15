#' Create species phenology plots (monthly) of eBird species abundance and occurrence (# checklists).
#'
#' This function generates, for each queried polygon, a phenology plot of eBird sightings by month.
#'  The user can choose between displaying the proportion of only complete checklists or all
#'  available checklists. The number of checklists is indicated graphically.
#'
#' @param geo_ebird_df a \code{\link{data.frame}} created by \code{\link{geo_ebird}}
#' @param species a character string of length = 1 indicating the species to visualize.  If NULL
#'  (the default), the user is presented with a list of available species from which to choose.
#'  Use the accepted American Ornithologists' Union name (see http://checklist.aou.org/taxa),
#'  insensitive to capitalization (e.g., "PuRpLe MaRtIn" works).
#' @param complete_only logical indicating whether only eBird checklists designated as reporting
#'  all species observed (i.e., complete checklists; default = TRUE) should be used to calculate
#'  proportion of checklists or whether all available checklists should be used (FALSE)
#' @return a \code{\link[ggplot2]{ggplot}} object
#' @import dplyr ggplot2
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
#' # Plot phenology of Pileated Woodpecker
#' # Species not case sensitive
#' plot_ebird_phen(piedmont, species = "PiLEAted WOOdPECKer")
#' }

plot_ebird_phen <- function(geo_ebird_df, species = NULL, complete_only = TRUE) {

    ## Prompt user to specify species, if not specified
    if (!is.null(species)) {
        spp_name <- tolower(species)
        if (!(spp_name %in% tolower(geo_ebird_df$common_name)))
            stop("No matches for that species.  Check your spelling or consider a different species.")
    } else {
        spp <- sort(unique(geo_ebird_df$common_name))
        spp_name <- tcltk::tk_select.list(spp, title="Choose the species to visualize.", multiple = FALSE)
        spp_name <- tolower(spp_name)
    }

    geo_ebird_df <- mutate(geo_ebird_df,
                           name = factor(shorten_nwr(Cap(name))),
                           month = as.integer(lubridate::month(date)),
                           common_name = tolower(common_name))

    if (complete_only) geo_ebird_df <- geo_ebird_df %>% filter(all_spp == 1)

    checklists <- geo_ebird_df %>%
        group_by(name, month, buff_dist_km) %>%
        summarise(monthly_checklists = length(unique(checklist)))

    plot_dat <- geo_ebird_df %>% filter(common_name == spp_name) %>%
        group_by(name, common_name, sci_name, buff_dist_km, month) %>%
        summarise(checklist = n()) %>% #,
        #          avg_count = round(sum(count) / checklist, 1)) %>%
        data.table::melt(id = c("name", "common_name", "sci_name", "month", "buff_dist_km"))
        # mutate(variable = factor(variable, labels = c("# checklists", "Average count / checklist")),

    # Add zeros for months without data
    if (nrow(plot_dat) < 12) {
        df_add <- expand.grid(name = levels(plot_dat$name),
                              common_name = spp_name, sci_name = unique(plot_dat$sci_name),
                              month = 1:12, buff_dist_km = unique(plot_dat$buff_dist_km),
                              variable = levels(plot_dat$variable))
        df_add <- suppressWarnings(anti_join(df_add, select(plot_dat, -value)))
        df_add$value <- 0
        plot_dat <- rbind(plot_dat, df_add)
    }

    plot_dat <- plot_dat %>% left_join(checklists, by = c("name", "month", "buff_dist_km")) %>%
        mutate(buff_dist_km = as.factor(buff_dist_km))

    p <- ggplot(plot_dat, aes(x = month, y = value / monthly_checklists,
                              group = buff_dist_km, colour = buff_dist_km)) +
        geom_line() + geom_point(aes(size = monthly_checklists)) +
        facet_grid(. ~ name, scales = "free_y") +
        scale_colour_discrete("Buffer\ndistance (km)") +
        scale_size("# eBird\nchecklists", breaks = scales::pretty_breaks()) +
        theme_bw() + ggtitle(Cap(spp_name)) +
        theme(plot.title = element_text(face="bold"))

	# Adjust y axis labels based on checklist requirements
	y_lab <- "Proportion of complete eBird checklists"
	if (!complete_only) y_lab <- "Proportion of all eBird checklists"

	p <- p + ylab(y_lab)

    # Adjust x axis labels if lots of polygons
    if (length(levels(plot_dat$name)) < 5) {
        p <- p + scale_x_continuous("Month", breaks = 1:12, labels = month.abb)
    } else {
        p <- p + scale_x_continuous("Month", breaks = 1:12, labels = substr(month.abb, 1, 1))
    }

    p

}
