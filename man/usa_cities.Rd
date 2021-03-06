% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{usa_cities}
\alias{usa_cities}
\title{USA Cities, 2014}
\format{
A \code{sf}.
}
\source{
\url{https://earthworks.stanford.edu/catalog/stanford-bx729wr3020}
}
\usage{
usa_cities
}
\description{
From the original dataset, some fields have been selected and renamed, and
only includes the Mortality Reporting System cities.
}
\examples{

# Read by:
#
# filepath <- "data/citiesx010g/citiesx010g.shp"
# usa_cities <- st_read(filepath)  \%>\%
#   dplyr::select(
#     gnis_id = GNIS_ID,
#     ansi_code = ANSICODE,
#     city = NAME,
#     state = STATE,
#     county = COUNTY,
#     latitude = LATITUDE,
#     longitude = LONGITUDE,
#     elev_m = ELEV_IN_M
#   )

}
\keyword{datasets}
