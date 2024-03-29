
#' USA Cities, 2014
#'
#' From the original dataset, some fields have been selected and renamed, and
#' only includes the Mortality Reporting System cities.
#'
#' @format A `sf`.
#' @source
#'   \url{https://earthworks.stanford.edu/catalog/stanford-bx729wr3020}
"usa_cities"


#' USA Counties, 2018
#'
#' From the original dataset, some fields have been selected and renamed, and
#' only includes the Mortality Reporting System counties.
#'
#' Some counties appear with the same repeated name within the same state, they
#' are the following: Baltimore, MD; Richmond, VA; St. Louis, MO. Since they are
#' accessed by name (county and state), those of the same name within the state
#' have been grouped together.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_20m.zip}
"usa_counties"


#' USA States, 2018
#'
#' From the original dataset, some fields have been selected and renamed, and
#' only includes the Mortality Reporting System states.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_20m.zip}
"usa_states"

#' USA Divisions, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_division_20m.zip}
"usa_divisions"

#' USA Regions, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_region_20m.zip}
"usa_regions"

#' USA Nation, 2018
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' @format A `sf`.
#' @source
#'   \url{https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_nation_20m.zip}
"usa_nation"


#' UK London Boroughs
#'
#' From the original dataset, some fields have been selected and renamed.
#'
#' Since not so much detail is needed, the geometry has been simplified 20 m.
#'
#' @format A `sf`.
#' @source
#'   \url{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}
"uk_london_boroughs"

#' Dimension *when*
#'
#' *When* dimension table of the Mortality Reporting System. Defined from
#' `ms_mrs`. The primary key has been renamed and its type has been changed. The
#' other attributes have also been renamed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_when"

#' Dimension *where*
#'
#' *Where* dimension table of the Mortality Reporting System. Defined from
#' `ms_mrs`. The primary key has been renamed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_where"

#' Dimension *who*
#'
#' *Who* dimension table of the Mortality Reporting System. Defined from
#' `ms_mrs`. The primary key has been renamed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_who"

#' Fact *age*
#'
#' Fact *age* table of the Mortality Reporting System. Defined from `ms_mrs`.
#' Foreign keys have been renamed, only a *when* dimension has been considered,
#' the type for the *when* dimension has been changed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_fact_age"

#' Fact *cause*
#'
#' Fact *cause* table of the Mortality Reporting System. Defined from `ms_mrs`.
#' Foreign keys have been renamed, only a *when* dimension has been considered,
#' the type for the *when* dimension has been changed.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"mrs_fact_cause"

#' Multistar for Mortality Reporting System
#'
#' Multistar for the Mortality Reporting System considering age and cause
#' classification.
#'
#' @format A `multistar` object.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"ms_mrs"

#' Multistar for Mortality Reporting System Test
#'
#' Multistar for the Mortality Reporting System considering age and cause
#' classification data test.
#'
#' @format A `multistar` object.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"ms_mrs_test"

#' Mortality Reporting System by Age Test
#'
#' Selection of data from the 2 Cities Mortality Reporting System by age
#' group, for the first 3 weeks of 1962.
#'
#' The original dataset begins in 1962. For each week, in 122 US cities,
#' mortality figures by age group and cause, considered separately, are included
#' (i.e., the combination of age group and cause is not included). In the cause,
#' only a distinction is made between pneumonia or influenza and others.
#'
#' Two additional dates have been generated, which were not present in the
#' original dataset.
#'
#' @format A `tibble`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"mrs_age_test"

#' Star Schema for Mortality Reporting System by Age Test
#'
#' Star Schema for the Mortality Reporting System considering the age
#' classification data test.
#'
#' @format A `star_schema` object.
#' @source
#'   \url{https://CRAN.R-project.org/package=starschemar}
"st_mrs_age_test"
