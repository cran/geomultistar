## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(geomultistar)

## -----------------------------------------------------------------------------
ms <- multistar() |>
  add_facts(
    fact_name = "mrs_age",
    fact_table = mrs_fact_age,
    measures = "n_deaths",
    nrow_agg = "count"
  ) 

## -----------------------------------------------------------------------------
ms <- ms |>
  add_facts(
    fact_name = "mrs_cause",
    fact_table = mrs_fact_cause,
    measures = c("pneumonia_and_influenza_deaths", "other_deaths"),
    nrow_agg = "nrow_agg"
  )

## -----------------------------------------------------------------------------
ms <- ms |>
  add_dimension(
    dimension_name = "where",
    dimension_table = mrs_where,
    dimension_key = "where_pk",
    fact_name = "mrs_age",
    fact_key = "where_fk"
  )

## -----------------------------------------------------------------------------
ms <- ms |>
  add_dimension(
    dimension_name = "when",
    dimension_table = mrs_when,
    dimension_key = "when_pk",
    fact_name = "mrs_age",
    fact_key = "when_fk",
    key_as_data = TRUE
  ) |>
  add_dimension(
    dimension_name = "who",
    dimension_table = mrs_who,
    dimension_key = "who_pk",
    fact_name = "mrs_age",
    fact_key = "who_fk"
  )

## -----------------------------------------------------------------------------
ms <- ms |>
  relate_dimension(dimension_name = "where",
                   fact_name = "mrs_cause",
                   fact_key = "where_fk") |>
  relate_dimension(dimension_name = "when",
                   fact_name = "mrs_cause",
                   fact_key = "when_fk")

## -----------------------------------------------------------------------------
dq <- dimensional_query(ms)

## -----------------------------------------------------------------------------
dq_1 <- dq |>
  select_fact(
    name = "mrs_age",
    measures = "n_deaths",
    agg_functions = "MAX"
  )

## -----------------------------------------------------------------------------
dq_2 <- dq |>
  select_fact(name = "mrs_age",
              measures = "n_deaths")

## -----------------------------------------------------------------------------
dq_3 <- dq |>
  select_fact(name = "mrs_age")

## -----------------------------------------------------------------------------
dq_4 <- dq |>
  select_fact(name = "mrs_age",
              measures = "n_deaths") |>
  select_fact(name = "mrs_cause")

## -----------------------------------------------------------------------------
dq_1 <- dq |>
  select_dimension(name = "where",
                   attributes = c("city", "state"))

## -----------------------------------------------------------------------------
dq_2 <- dq |>
  select_dimension(name = "where")

## -----------------------------------------------------------------------------
dq <- dq |>
  filter_dimension(name = "when", week <= "03") |>
  filter_dimension(name = "where", city == "Bridgeport")

## -----------------------------------------------------------------------------
dq <- dimensional_query(ms) |>
  select_dimension(name = "where",
                   attributes = c("division_name", "region_name")) |>
  select_dimension(name = "when",
                   attributes = c("year", "week")) |>
  select_fact(name = "mrs_age",
              measures = "n_deaths") |>
  filter_dimension(name = "when", week <= "03")

ms_2 <- dq |>
  run_query()

class(ms_2)

## -----------------------------------------------------------------------------
ft <- ms_2 |>
  multistar_as_flat_table()

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(head(ft), split.table = Inf)

## -----------------------------------------------------------------------------
gms <-
  geomultistar(ms, geodimension = "where")

## -----------------------------------------------------------------------------
gms <- gms |>
  define_geoattribute(
    attribute = "city",
    from_layer = usa_cities,
    by = c("city" = "city", "state" = "state")
  ) 

## -----------------------------------------------------------------------------
empty_city <- gms |>
  get_empty_geoinstances(attribute = "city")

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(empty_city, split.table = Inf)

## -----------------------------------------------------------------------------
gms <- gms |>
  define_geoattribute(
    attribute = "county",
    from_layer = usa_counties,
    by = c("county" = "county", "state" = "state")
  )  

## -----------------------------------------------------------------------------
empty_county <- gms |>
  get_empty_geoinstances(attribute = "county")

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(empty_county, split.table = Inf)

## -----------------------------------------------------------------------------
gms <- gms |>
  define_geoattribute(
    attribute = c("state"),
    from_layer = usa_states,
    by = c("state" = "state")
  ) 

## -----------------------------------------------------------------------------
gms <- gms |>
  define_geoattribute(
    attribute = "division",
    from_attribute = "state"
  ) 

## -----------------------------------------------------------------------------
gms <- gms |>
  define_geoattribute(from_attribute = "state")

## -----------------------------------------------------------------------------
gdq <- dimensional_query(gms) |>
  select_dimension(name = "where",
                   attributes = c("division_name", "region_name")) |>
  select_dimension(name = "when",
                   attributes = c("year", "week")) |>
  select_fact(name = "mrs_age",
              measures = "n_deaths") |>
  filter_dimension(name = "when", week <= "03")

gms_2 <- gdq |>
  run_query()

class(gms_2)

## -----------------------------------------------------------------------------
vl_sf <- gdq |>
  run_geoquery()

class(vl_sf)

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(head(vl_sf), split.table = Inf)

## -----------------------------------------------------------------------------
plot(vl_sf[,"n_deaths"])

## -----------------------------------------------------------------------------
vl_sf_w <- gdq |>
  run_geoquery(wider = TRUE)

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(head(vl_sf_w$sf), split.table = Inf)

## ----results = "asis", echo = FALSE-------------------------------------------
pander::pandoc.table(head(vl_sf_w$variables), split.table = Inf)

## -----------------------------------------------------------------------------
filepath <- tempdir()
l <- save_as_geopackage(vl_sf_w, "division", filepath = filepath)

file <- paste0(filepath, "/division.gpkg")
sf::st_layers(file)

