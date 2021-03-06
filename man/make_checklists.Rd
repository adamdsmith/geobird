% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_checklists.R
\name{make_checklists}
\alias{make_checklists}
\title{Create spreadsheets of eBird species abundance and occurrence (i.e., # checklists).}
\usage{
make_checklists(geo_ebird_df, min_lists = 10L, exclude_form = TRUE,
  xls = TRUE, nwrspp = FALSE, out_dir = "../Output/")
}
\arguments{
\item{geo_ebird_df}{a \code{\link{data.frame}} created by \code{\link{geo_ebird}}}

\item{min_lists}{integer or numeric vector of length = 1 indicating the minimum number of complete
eBird checklists to require in a given season before assigning a seasonal abundance
classification}

\item{exclude_form}{logical indicating whether observations from the slightly nebulous "form"
category should be excluded (default = TRUE).  Setting to FALSE may add certain species (e.g.,
Red Crossbill) but also more controversial entries (e.g., Northern Red-tailed Hawk)}

\item{xls}{logical indicating whether to output nicely formatted *.xls file (TRUE; default) or to
create a \code{\link{list}} of relevant information without any formatting (FALSE; i.e., for
further manipulation in R)}

\item{nwrspp}{logical indicating whether a sheet formatted for easy importing into the NWRSpecies
database is requested (default = FALSE); only applies if `xls = TRUE`.}

\item{out_dir}{character string of the file path to the directory in which to save output
spreadsheets}
}
\value{
a Microsoft Excel spreadsheet for each distinct polygon (i.e., each (\code{name}) in
  \code{geo_ebird_df}) when \code{xls = TRUE} or a \code{\link{list}} of \code{\link{list}}s
}
\description{
This function generates, for each queried polygon, a spreadsheet of all species detected on at
 least one checklist.  For each season (see Details), an estimated abundance classification
 is provided (see Details) if a minimum number of complete checklists \code{min_lists} is
 available.  Regardless, the total number of checklists (complete or not) on which each species
 was observed in each season is reported.  Likewise, the amount of effort (# of complete and
 total checklists) is reported by season.
}
\details{
Seasons are classified as follows: spring (Mar - May), summer (Jun - Aug), fall (Sep - Nov),
 and winter (Dec - Feb).  Note, this corresponds roughly to the seasonal bird checklists
 present in many National Wildlife Refuge brochures (which may have had a small part in the
 decision to use this classification).  For monthly patterns of eBird detections by species,
 relative to eBird effort (i.e., # checklists), see \code{\link{plot_ebird_phen}}.

Abundance designations are classified based on the proportion of complete checklists on
 which a given species occurs.  The \code{min_lists} argument allows the user to determine the
 number of checklists necessary to even attempt such a designation.  The default is 10 complete
 checklists, although a larger minimum (e.g., 50 or 100) is probably a more reasonable choice for
 generating an estimate of species relative abundance.  Abundance classifications (proportion of
 complete checklists) are:  Abundant (A; > 0.50), Common (C; (0.3 - 0.5]), Uncommon (U; (0.2 -
 0.3]), Occasional (O; (0.1 - 0.2]), Rare (R; (0.01 - 0.1]), and Vagrant (V; <= 0.01).  Notice
 that the cutoff for a "Vagrant" designation requires at least 100 checklists; with fewer checklists
 this designation is lumped with "Rare".
}
\examples{
\dontrun{
# Get shapefile
SErefuges <- rgdal::readOGR("../GIS", "refuges", verbose = FALSE, stringsAsFactors = FALSE)

# Query eBird records within Piedmont NWR and 10 km buffer
piedmont <- geo_ebird(SErefuges, which_polys = "Piedmont", buffers = c(0, 10))

# Generate .xls spreadsheet contain abundance, occurrence, and effort information
# Require 100 complete checklists to generate abundance classification
make_checklists(piedmont, min_lists = 100)
}
}

