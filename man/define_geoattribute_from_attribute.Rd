% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geomultistar_define_geoattribute.R
\name{define_geoattribute_from_attribute}
\alias{define_geoattribute_from_attribute}
\title{Define a geoattribute from another}
\usage{
define_geoattribute_from_attribute(
  gms,
  dimension = NULL,
  attribute = NULL,
  from_attribute = NULL,
  additional_attributes = NULL
)
}
\arguments{
\item{gms}{A \code{geomultistar} object.}

\item{dimension}{A string, dimension name.}

\item{attribute}{A string, attribute name.}

\item{from_attribute}{A string, attribute name.}

\item{additional_attributes}{A vector, attribute names.}
}
\value{
A \code{geomultistar} object.
}
\description{
Define a geoattribute from another.
}
\keyword{internal}
