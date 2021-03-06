% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query_ebird_db.R
\name{query_ebird_db}
\alias{query_ebird_db}
\title{Extract records from local eBird SQLite database that occur in US counties overlapped by
user-specified \code{\link[sp]{SpatialPolygonsDataFrame-class}} plus an optional buffer.}
\usage{
query_ebird_db(query_polys, ebird_sqlite = "../Data/SE_eBird.sqlite",
  buffer = 2, table_name = NULL, SPDF = TRUE)
}
\arguments{
\item{query_polys}{\code{\link[sp]{SpatialPolygonsDataFrame-class}} within and around which
the user wishes to extract eBird records.  It should also work, but is untested, with
\code{\link[sp]{SpatialPointsDataFrame-class}} or
\code{\link[sp]{SpatialLinesDataFrame-class}}) if a buffer is provided.  The buffer settings
currently assume that input \code{query_polys} are in a geographic (i.e., latitude and
longitude) projection.}

\item{ebird_sqlite}{character string file path to local eBird SQLite (.sqlite) database file}

\item{buffer}{Approximate distance (km) from \code{query_polys} to include
in the search for eBird records.  A numeric vector of length 1. Negative values
are allowed.}

\item{table_name}{character string naming the table within \code{ebird_sqlite} that
contains the relevant eBird data.  If not specified, defaults to the first
table in \code{ebird_sqlite}}

\item{SPDF}{logical (default = TRUE) indicating whether the output should be
converted into a \code{\link[sp]{SpatialPointsDataFrame-class}}.  If TRUE,
the returned \code{\link[sp]{SpatialPointsDataFrame-class}} is in the same
projection as \code{query_polys}}
}
\value{
a \code{\link{data.frame}} of eBird records in the counties represented in the input
 polygons (and an associated buffer)
}
\description{
This function is called from \code{\link{geo_ebird}} and is thus not intended to
be called outside of that functionality.
}

