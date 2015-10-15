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
#' @param xls logical indicating whether to output nicely formatted *.xls file (TRUE; default) or to
#'  create a \code{\link{list}} of relevant information (FALSE)
#' @param out_dir character string of the file path to the directory in which to save output
#'  spreadsheets
#' @return a Microsoft Excel spreadsheet for each distinct polygon (i.e., each (\code{name}) in
#'   \code{geo_ebird_df}
#' @import dplyr
#' @importFrom plyr "."
#' @export

make_checklists <- function(geo_ebird_df, min_lists = 10L, xls = TRUE, out_dir = "../Output/")  {

    if (length(min_lists) != 1 | !(class(min_lists) %in% c("integer", "numeric")))
        stop("The min_lists argument must be an integer or numeric vector of length = 1")

    # Get number of buffers assessed
    n_buffs <- length(unique(geo_ebird_df$buff_dist_km))
    buff_dists <- sort(unique(geo_ebird_df$buff_dist_km))

    # Add season
    geo_ebird_df <- mutate(geo_ebird_df,
                           season = factor(get_season(lubridate::month(date)),
                                           levels = c("spring", "summer", "fall", "winter")))

    tax_order <- geo_ebird_df %>% select(common_name, tax_order) %>%
        group_by(common_name) %>% summarise(tax_order = min(tax_order))

    # Tabulate the number of complete checklists by season (i.e., all_spp == 1)
    complete_checklists <-geo_ebird_df %>% filter(all_spp == 1) %>%
        group_by(name, season, buff_dist_km) %>%
        summarise(season_checklists = length(unique(checklist)))

    all_checklists <- geo_ebird_df %>%
        group_by(name, season, buff_dist_km) %>%
        summarise(season_checklists = length(unique(checklist)))

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

    occurrence <- geo_ebird_df %>%
        left_join(all_checklists, by = c("buff_dist_km", "name", "season")) %>%
        group_by(name, season, common_name, sci_name, buff_dist_km, season_checklists) %>%
        summarise(n_checklists = n()) %>%
        select(-season_checklists) %>%
        left_join(tax_order, by = "common_name") %>%
        reshape2::dcast(name + common_name + sci_name + tax_order ~ buff_dist_km + season, value.var = "n_checklists")
    names(occurrence)[-4:-1] <- flip_string(names(occurrence)[-4:-1], add_on = "km")

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

    if (n_buffs >= 2) {

        occ_det_stats <- geo_ebird_df %>%
            select(name, common_name, buff_dist_km) %>% unique() %>%
            arrange(name, common_name, buff_dist_km)
        min_buff <- !duplicated(select(occ_det_stats, name, common_name))
        occ_det_stats <- occ_det_stats[min_buff, ]
        names(occ_det_stats)[3] <- "first_detected_km"
        occ_det_stats <- mutate(occ_det_stats,
                                Status = ifelse(!(occ_det_stats$common_name %in% abund_det_stats$common_name),
                                                ifelse(first_detected_km == 0, "occurrence-refuge",
                                                       "occurrence-buffer"), "abundance"))

        occurrence <- left_join(occurrence, occ_det_stats, by = c("name", "common_name"))

    }

    complete_checklists <- complete_checklists %>%
        reshape2::dcast(name + buff_dist_km ~ season, value.var = "season_checklists") %>%
        mutate(Effort = "Complete checklists only")

    all_checklists <- all_checklists %>%
        reshape2::dcast(name + buff_dist_km ~ season, value.var = "season_checklists") %>%
        mutate(Effort = "All checklists")

    checklists <- rbind(complete_checklists, all_checklists) %>% select(Effort, everything())
    # Change NAs to indicate to checklists
    checklists[, c("spring", "summer", "fall", "winter")][is.na(checklists[, c("spring", "summer", "fall", "winter")])] <- 0

    # Generate a list by name
    poly_abund <- plyr::dlply(abundance, plyr::.(name), df_to_checklist,
                              type = "abund", n_buffs, buff_dists)
    poly_occurrence <- plyr::dlply(occurrence, plyr::.(name), df_to_checklist,
                                   type = "occ", n_buffs, buff_dists)
    poly_effort <- plyr::dlply(checklists, plyr::.(name), select, -name)

    # Generate output spreadsheets, if requested
    if (xls) {

        for (name in names(poly_abund)) {

            if (substr(out_dir, nchar(out_dir), nchar(out_dir)) != "/") out_dir <- paste0(out_dir, "/")
            out_file <- paste0(out_dir, Cap(name), ".xls")

            # Do not output abundance sheet if no seasons meet the minimum checklist requirement
            if (all(is.na(poly_abund[[name]][, 2:ncol(poly_abund[[name]])]))) {
                save.xlsx(poly_occurrence[[name]], out_file, sheetName = "eBird_all_records",
                          start.row = 3)
            } else {
                save.xlsx(poly_abund[[name]], out_file, sheetName = "eBird_abundance",
                          start.row = 3)
                save.xlsx(poly_occurrence[[name]], out_file, sheetName = "eBird_all_records",
                          start.row = 3, append = TRUE)
            }

            save.xlsx(poly_effort[[name]], out_file, sheetName = "eBird_effort", append = TRUE)

        }

    } else {

        out <- list(eBird_abundnace = poly_abund,
                    eBird_all_records = poly_occurrence,
                    eBird_effort = poly_effort)
        out

    }

}
