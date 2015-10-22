#' Create spreadsheets of eBird species abundance and occurrence (i.e., # checklists).
#'
#' This function generates, for each queried polygon, a spreadsheet of all species detected on at
#'  least one checklist.  For each season (see Details), an estimated abundance classification
#'  is provided (see Details) if a minimum number of complete checklists \code{min_lists} is
#'  available.  Regardless, the total number of checklists (complete or not) on which each species
#'  was observed in each season is reported.  Likewise, the amount of effort (# of complete and
#'  total checklists) is reported by season.
#'
#' Seasons are classified as follows: spring (Mar - May), summer (Jun - Aug), fall (Sep - Nov),
#'  and winter (Dec - Feb).  Note, this corresponds roughly to the seasonal bird checklists
#'  present in many National Wildlife Refuge brochures (which may have had a small part in the
#'  decision to use this classification).  For monthly patterns of eBird detections by species,
#'  relative to eBird effort (i.e., # checklists), see \code{\link{plot_ebird_phen}}.
#'
#' Abundance designations are classified based on the proportion of complete checklists on
#'  which a given species occurs.  The \code{min_lists} argument allows the user to determine the
#'  number of checklists necessary to even attempt such a designation.  The default is 10 complete
#'  checklists, although a larger minimum (e.g., 50 or 100) is probably a more reasonable choice for
#'  generating an estimate of species relative abundance.  Abundance classifications (proportion of
#'  complete checklists) are:  Abundant (A; > 0.50), Common (C; (0.3 - 0.5]), Uncommon (U; (0.2 -
#'  0.3]), Occasional (O; (0.1 - 0.2]), Rare (R; (0.01 - 0.1]), and Vagrant (V; <= 0.01).  Notice
#'  that the cutoff for a "Vagrant" designation requires at least 100 checklists; with fewer checklists
#'  this designation is lumped with "Rare".
#'
#' @param geo_ebird_df a \code{\link{data.frame}} created by \code{\link{geo_ebird}}
#' @param min_lists integer or numeric vector of length = 1 indicating the minimum number of complete
#'  eBird checklists to require in a given season before assigning a seasonal abundance
#'  classification
#' @param exclude_form logical indicating whether observations from the slightly nebulous "form"
#'  category should be excluded (default = TRUE).  Setting to FALSE may add certain species (e.g.,
#'  Red Crossbill) but also more controversial entries (e.g., Northern Red-tailed Hawk)
#' @param xls logical indicating whether to output nicely formatted *.xls file (TRUE; default) or to
#'  create a \code{\link{list}} of relevant information without any formatting (FALSE; i.e., for
#'  further manipulation in R)
#' @param out_dir character string of the file path to the directory in which to save output
#'  spreadsheets
#' @return a Microsoft Excel spreadsheet for each distinct polygon (i.e., each (\code{name}) in
#'   \code{geo_ebird_df}) when \code{xls = TRUE} or a \code{\link{list}} of \code{\link{list}}s
#' @importFrom plyr "."
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
#' # Generate .xls spreadsheet contain abundance, occurrence, and effort information
#' # Require 100 complete checklists to generate abundance classification
#' make_checklists(piedmont, min_lists = 100)
#' }

make_checklists <- function(geo_ebird_df, min_lists = 10L, exclude_form = TRUE,
                            xls = TRUE, out_dir = "../Output/")  {

    if (length(min_lists) != 1 | !(class(min_lists) %in% c("integer", "numeric")))
        stop("The min_lists argument must be an integer or numeric vector of length = 1")

    # Get number of buffers assessed
    n_buffs <- length(unique(geo_ebird_df$buff_dist_km))
    buff_dists <- sort(unique(geo_ebird_df$buff_dist_km))

    # Add season
    geo_ebird_df <- mutate(geo_ebird_df,
                           season = factor(get_season(lubridate::month(date)),
                                           levels = c("spring", "summer", "fall", "winter")))
    if (exclude_form) geo_ebird_df <- dplyr::filter(geo_ebird_df, category != "form")

    # Store taxonomic order info for later
    tax_order <- geo_ebird_df %>% select(common_name, tax_order) %>%
        group_by(common_name) %>% summarise(tax_order = min(tax_order))

    # Tabulate the number of complete checklists by season (i.e., all_spp == 1)
    complete_checklists <-geo_ebird_df %>% filter(all_spp == 1) %>%
        group_by(name, season, buff_dist_km) %>%
        summarise(season_checklists = length(unique(checklist)))

    # Tabulate all checklists by season (complete or incomplete)
    all_checklists <- geo_ebird_df %>%
        group_by(name, season, buff_dist_km) %>%
        summarise(season_checklists = length(unique(checklist)))

    # If sufficient complete checklists, assign abundance classification by season
    abundance <- geo_ebird_df %>% filter(all_spp == 1) %>%
        left_join(complete_checklists, by = c("buff_dist_km", "name", "season")) %>%
        group_by(name, season, common_name, sci_name, buff_dist_km, season_checklists) %>%
        summarise(spp_checklists = n()) %>%
        mutate(p_lists = round(spp_checklists / season_checklists, 3),
               abundance = code_abundance(p_lists)) %>%
        mutate(abundance = ifelse(season_checklists >= min_lists, abundance, NA_character_)) %>%
        select(-season_checklists, -spp_checklists, -p_lists) %>%
        left_join(tax_order, by = "common_name") %>%
        reshape2::dcast(name + common_name + sci_name + tax_order ~ buff_dist_km + season, value.var = "abundance")
    names(abundance)[-4:-1] <- flip_string(names(abundance)[-4:-1], add_on = "km")

    # Extract abundance at actual polygon boundaries only for use in final checklist generation
    if (0 %in% buff_dists) {
        drop <- apply(abundance[, 5:8], 1, function(row) all(is.na(row)))
        actual_abund <- abundance[!drop, 1:8]
    }

    # Tabulate total number of checklists recording a given species by season
    occurrence <- geo_ebird_df %>%
        left_join(all_checklists, by = c("buff_dist_km", "name", "season")) %>%
        group_by(name, season, common_name, sci_name, buff_dist_km, season_checklists) %>%
        summarise(n_checklists = n()) %>%
        select(-season_checklists) %>%
        left_join(tax_order, by = "common_name") %>%
        reshape2::dcast(name + common_name + sci_name + tax_order ~ buff_dist_km + season, value.var = "n_checklists")
    names(occurrence)[-4:-1] <- flip_string(names(occurrence)[-4:-1], add_on = "km")

    # Extract occurrence at actual polygon boundaries only for use in final checklist generation
    if (0 %in% buff_dists) {
        drop <- apply(occurrence[, 5:8], 1, function(row) all(is.na(row)))
        actual_occ <- occurrence[!drop, 1:8]
    }

    # If comparing actual boundary and larger landscape (i.e., buffer >= 5 km),
    # document first detected distance and compare abundance classifications within boundary
    # and in larger landscape
    if (min(buff_dists) == 0 && max(buff_dists) >= 5) {

        abund_det_stats <- geo_ebird_df %>% filter(all_spp == 1) %>%
            select(name, common_name, buff_dist_km) %>% unique() %>%
            arrange(name, common_name, buff_dist_km)
        min_buff <- !duplicated(select(abund_det_stats, name, common_name))
        abund_det_stats <- abund_det_stats[min_buff, ]
        names(abund_det_stats)[3] <- "first_detected_km"
        abund_det_stats <- mutate(abund_det_stats,
                                  Status = ifelse(first_detected_km == 0, "abundance-refuge",
                                                  "abundance-buffer"))

        abund_traj <- geo_ebird_df %>% filter(all_spp == 1) %>%
            left_join(complete_checklists, by = c("buff_dist_km", "name", "season")) %>%
            group_by(name, season, common_name, sci_name, buff_dist_km, season_checklists) %>%
            summarise(spp_checklists = n()) %>%
            mutate(p_lists = round(spp_checklists / season_checklists, 3)) %>%
            group_by(name, season, common_name) %>%
            arrange(buff_dist_km) %>%
            summarise(traj = ifelse(tail(p_lists, 1) - head(p_lists, 1) > 0.25, "off-refuge",
                                    ifelse (tail(p_lists, 1) - head(p_lists, 1) < -0.25, "on-refuge", "-"))) %>%
            reshape2::dcast(name + common_name ~ season, value.var = "traj")

        abundance <- left_join(abundance, abund_det_stats, by = c("name", "common_name")) %>%
            left_join(abund_traj, by = c("name", "common_name"))
    }

    # If more than one distance explored, document first detected distance and, if comparing actual
    # boundary, note whether abundance estimates were possible or only occurrence
    if (n_buffs >= 2) {

        occ_det_stats <- geo_ebird_df %>%
            select(name, common_name, buff_dist_km) %>% unique() %>%
            arrange(name, common_name, buff_dist_km)
        min_buff <- !duplicated(select(occ_det_stats, name, common_name))
        occ_det_stats <- occ_det_stats[min_buff, ]
        names(occ_det_stats)[3] <- "first_detected_km"
        if (exists("abund_det_stats")) {
            occ_det_stats <- mutate(occ_det_stats,
                                    Status = ifelse(!(occ_det_stats$common_name %in% abund_det_stats$common_name),
                                                    ifelse(first_detected_km == 0, "occurrence-refuge",
                                                           "occurrence-buffer"), "abundance"))
        }

        occurrence <- left_join(occurrence, occ_det_stats, by = c("name", "common_name"))

    }

    # Summary table to complete checklists
    complete_checklists <- complete_checklists %>%
        reshape2::dcast(name + buff_dist_km ~ season, value.var = "season_checklists") %>%
        mutate(Effort = "Complete checklists only")

    # Summary table to all checklists
    all_checklists <- all_checklists %>%
        reshape2::dcast(name + buff_dist_km ~ season, value.var = "season_checklists") %>%
        mutate(Effort = "All checklists")

    # Consolidate effort into a single data frame
    checklists <- rbind(complete_checklists, all_checklists) %>% select(Effort, everything())
    # Change NAs to indicate to checklists
    checklists[, c("spring", "summer", "fall", "winter")][is.na(checklists[, c("spring", "summer", "fall", "winter")])] <- 0

    # Generate output spreadsheets, if requested
    if (xls) {
        poly_abund <- plyr::dlply(abundance, .(name), df_to_checklist,
                                  type = "abund", n_buffs, buff_dists)
        poly_occurrence <- plyr::dlply(occurrence, .(name), df_to_checklist,
                                       type = "occ", n_buffs, buff_dists)
        # Extract species with occurrence data only, combine with abundance data at actual boundary
        if (0 %in% buff_dists) {
            poly_final <- plyr::dlply(actual_abund, .(name), df_to_checklist,
                                      type = "final", n_buffs = 1, buff_dists = 0,
                                      actual_occ = actual_occ)
        }

        poly_effort <- plyr::dlply(checklists, .(name), select, -name)

        for (name in names(poly_occurrence)) {

            if (substr(out_dir, nchar(out_dir), nchar(out_dir)) != "/") out_dir <- paste0(out_dir, "/")
            out_file <- paste0(out_dir, Cap(name), ".xls")

            # Do not output abundance sheet if no seasons meet the minimum checklist requirement
            # at the actual boundary (i.e., no abundance is generated beyond the actual boundary)
            if (!(0 %in% buff_dists) || all(is.na(poly_abund[[name]][, 2:5]))) {
                save.xlsx(poly_occurrence[[name]], out_file, sheetName = "eBird_all_records",
                          start.row = 3)
            } else {
                save.xlsx(poly_abund[[name]], out_file, sheetName = "eBird_abundance",
                          start.row = 3)
                save.xlsx(poly_occurrence[[name]], out_file, sheetName = "eBird_all_records",
                          start.row = 3, append = TRUE)
                save.xlsx(poly_final[[name]], out_file, sheetName = "Checklist",
                          start.row = 3, append = TRUE)
            }

            save.xlsx(poly_effort[[name]], out_file, sheetName = "eBird_effort", append = TRUE)

        }

    } else {

        poly_abund <- plyr::dlply(abundance, .(name), select, -name)
        poly_occurrence <- plyr::dlply(occurrence, .(name), select, -name)
        poly_effort <- plyr::dlply(checklists, .(name), select, -name)
        # Extract species with occurrence data only, combine with abundance data at actual boundary
        if (0 %in% buff_dists) {
            poly_final <- plyr::dlply(actual_abund, .(name), function(df) {
                poly_id <- unique(df$name)
                poly_act_occ <- subset(actual_occ, name == poly_id)
                spp_occ <- poly_act_occ$common_name[!(poly_act_occ$common_name %in% df$common_name)]
                occ_only <- poly_act_occ[poly_act_occ$common_name %in% spp_occ, ]
                # Convert to vagrancy status
                occ_only[, 5:8] <- apply(occ_only[, 5:8], 2, as.character)
                occ_only[, 5:8][!is.na(occ_only[, 5:8])] <- "V"
                df <- rbind(df, occ_only)
                df <- select(df, -name)
                df
            })
        }

        out <- list(eBird_abundnace = poly_abund,
                    eBird_all_records = poly_occurrence,
                    checklist = poly_final,
                    eBird_effort = poly_effort)
        out

    }

}
