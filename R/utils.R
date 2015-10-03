get_UTM_zone <- function(lon) {
    (floor((lon + 180)/6) %% 60) + 1
}

Cap <- function(string) {
    s <- tolower(string)
    s <- strsplit(s, " ")
    unlist(lapply(s, function(i) {
        paste(toupper(substring(i, 1,1)), substring(i, 2), sep="", collapse=" ")
    }))
}

get_season <- function(months) {
    cuts <- c(-Inf, 3, 6, 9, 12, Inf)
    seasons <- c("winter", "spring", "summer", "fall", "winter")
    seasons[findInterval(months, cuts)]
}

code_abundance <- function(p_lists) {
    cuts <- c(0, 0.01, 0.1, 0.2, 0.3, 0.5, 1)
    abundance <- c("V", "R", "O", "U", "C", "A")
    abundance[findInterval(p_lists, cuts, rightmost.closed = TRUE)]
}

df_to_checklist <- function(df) {
    df %>% arrange(tax_order) %>% select(-name, -sci_name, -buff_dist_km, -tax_order)
}

shorten_nwr <- function(refuge_names) {
    # Simplify names for refuges and hatcheries
    refuge_names <- gsub("National W.*", "NWR", refuge_names)
    refuge_names <- gsub("National F.*", "NFH", refuge_names)
}
