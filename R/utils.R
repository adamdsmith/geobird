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
    cuts <- c(0, 0.01, 0.025, 0.05, 0.2, 1)
    abundance <- c("Rare", "Occasional", "Uncommon", "Common", "Abundant")
    abundance[findInterval(p_lists, cuts, rightmost.closed = TRUE)]
}

utils::globalVariables(c("name", "tax_order", "sci_name"))
df_to_checklist <- function(df, type, n_buffs, buff_dists, ...) {

    dots <- list(...)

    poly_id <- unique(df$name)

    if (type == "final") {
        poly_act_occ <- subset(dots[["actual_occ"]], name == poly_id)
        spp_occ <- poly_act_occ$common_name[!(poly_act_occ$common_name %in% df$common_name)]
        if (length(spp_occ) > 0) {
            occ_only <- poly_act_occ[poly_act_occ$common_name %in% spp_occ, ]
            # Convert to vagrancy status
            occ_only[, 5:8] <- apply(occ_only[, 5:8], 2, as.character)
            occ_only[, 5:8][!is.na(occ_only[, 5:8])] <- "V"
            df <- rbind(df, occ_only)
        }
    }

    df <- df %>% arrange(tax_order) %>% select(-name, -sci_name, -tax_order)
    df[!sapply(df, is.character)] <- lapply(df[!sapply(df, is.character)], as.character)
    which_cols <- names(df)[2:(4 * n_buffs + 1)]
    spl_names <- unlist(regmatches(which_cols, regexpr("_", which_cols), invert = TRUE))
    new_names <- every_nth(spl_names, 2, blank = FALSE)
    names(df)[2:(4 * n_buffs + 1)] <- new_names
    names(df) <- Cap(gsub("_", " ", names(df)))

    new_row <- paste0("Buffer: ", every_nth(spl_names, 2, blank = FALSE, inverse = TRUE))
    new_row <- gsub("Buffer: 0km", "Act. Boundary", new_row)
    first_row <- names(df)
    first_row[2:(4 * n_buffs + 1)] <- new_row
    names(df)[1] <- ""

    # Insert new top row
    df <- rbind(first_row, df)

    if (type == "abund") {
        if (min(buff_dists) == 0 && max(buff_dists) >= 5) {
            names(df)[length(df) - (5:0)] <- c("First detected", " ", rep("On vs. All", 4))
            df[1, length(df) - 5] <- "distance (km)"
        }
    } else if (type == "occ") {
        if (n_buffs >= 2) {
            if (min(buff_dists) == 0 && max(buff_dists) >= 5) {
                names(df)[length(df) - (1:0)] <- c("First detected", " ")
                df[1, length(df) - 1] <- "distance (km)"
            } else {
                names(df)[length(df)] <- "First detected"
                df[1, length(df)] <- "distance (km)"
            }
        }
    }
    df

}

shorten_nwr <- function(refuge_names) {
    # Simplify names for refuges and hatcheries
    refuge_names <- gsub("National W.*", "NWR", refuge_names)
    refuge_names <- gsub("National F.*", "NFH", refuge_names)
}

flip_string <- function(string, flip_char = "_", add_on = NULL) {
    n_splits <- length(gregexpr("_", string)[[1]])
    if (n_splits != 1) stop("flip_string only works if there is one flip_char in string")

    spl <- strsplit(string, flip_char, fixed = TRUE)
    out <- sapply(spl, function(x) {
        tmp <- paste(x[2:1], collapse = flip_char)
        if (!is.null(add_on)) tmp <- paste0(tmp, add_on)
        tmp
    })
    out
}

# Modify xlsx::write.xlsx to autosize columns and freeze top row
save.xlsx <- function (x, file, sheetName = "Sheet1", col.names = TRUE,
                       start.row = 2, append = FALSE)
{
    iOffset <- 1
    jOffset <- 0
    if (append && file.exists(file)) {
        wb <- xlsx::loadWorkbook(file)
    } else {
        ext <- gsub(".*\\.(.*)$", "\\1", basename(file))
        wb <- xlsx::createWorkbook(type = ext)
    }
    sheet <- xlsx::createSheet(wb, sheetName)
    noRows <- nrow(x) + iOffset
    noCols <- ncol(x) + jOffset
    if (col.names) {
        rows <- xlsx::createRow(sheet, 1)
        cells <- xlsx::createCell(rows, colIndex = 1:noCols)
        mapply(xlsx::setCellValue, cells[1, (1 + jOffset):noCols],
               colnames(x))
    }
    colIndex <- seq_len(ncol(x))
    rowIndex <- seq_len(nrow(x)) + iOffset
    xlsx::createFreezePane(sheet, start.row, 2, start.row, 2)
    xlsx:::.write_block(wb, sheet, x, rowIndex, colIndex, showNA = FALSE)
    xlsx::autoSizeColumn(xlsx::getSheets(wb)[[sheetName]], 1:ncol(x))
    xlsx::saveWorkbook(wb, file)
    invisible()
}

every_nth <- function(x, nth, blank = TRUE, inverse = FALSE) {
    if (!inverse) {
        if(blank) {
            x[1:nth == 1] <- ""
            x
        } else {
            x[1:nth == 1]
        }
    } else {
        if(blank) {
            x[1:nth != 1] <- ""
            x
        } else {
            x[1:nth != 1]
        }
    }
}

compare <- function(v) {
    if (any(is.na(v))) return(FALSE)
    identical(min(v), max(v, na.rm = TRUE))
}

insensitive <- function(fun = left_join) {
    # From Jim Hester
    # https://gist.github.com/jimhester/a060323a05b40c6ada34
    new_fun <- fun
    body(new_fun) <- substitute({
        by <- dplyr:::common_by(by, x, y)

        tmp_by_x <- paste0("_", by$x, "_")
        tmp_by_y <- paste0("_", by$y, "_")
        for (i in seq_along(by$x)) {
            x[[tmp_by_x[[i]]]] <- tolower(x[[by$x[[i]]]])
            y[[tmp_by_y[[i]]]] <- tolower(y[[by$y[[i]]]])
            y[[by$y[[i]]]] <- NULL
        }

        res <- fun(x, y, list(x = tmp_by_x, y = tmp_by_y))
        res[tmp_by_x] <- list(NULL)

        res
    })

    new_fun

}
