#' `multistar` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' @return A `multistar` object.
#' @keywords internal
new_multistar_empty <-
  function() {
    star <-
      list(
        fact = vector("list", length = 0),
        dimension =  vector("list", length = 0)
      )

    structure(star,
              class = "multistar")
  }

#' `multistar` S3 class
#'
#' Creates an empty `multistar` object that allows you to import fact and
#' dimension tables.
#'
#' @return A `multistar` object.
#'
#' @family multistar functions
#'
#' @examples
#'
#' ms <- multistar()
#'
#' @export
multistar <- function() {
  new_multistar_empty()
}


#' `multistar` S3 class
#'
#' Internal low-level constructor that creates new objects with the correct
#' structure.
#'
#' It only distinguishes between general and conformed dimensions, each
#' dimension has its own data. It can contain multiple fact tables.
#'
#' @param fl A `fact_table` list.
#' @param dl A `dimension_table` list.
#'
#' @return A `multistar` object.
#' @keywords internal
new_multistar <-
  function(fl = list(), dl = list()) {
    star <-
      list(
        fact = vector("list", length = length(fl)),
        dimension =  vector("list", length = length(dl))
      )
    names(star$fact) <- names(fl)
    names(star$dimension) <- names(dl)
    for (f in seq_along(fl)) {
      star$fact[[f]] <- fl[[f]]
      attr(star$fact[[f]], "spec") <- NULL
    }

    for (d in seq_along(dl)) {
      star$dimension[[d]] <- dl[[d]]
      attr(star$dimension[[d]], "role_playing") <- NULL
      attr(star$dimension[[d]], "spec") <- NULL
      if ("conformed" %in% attr(star$dimension[[d]], "type")) {
        attr(star$dimension[[d]], "type") <- "conformed"
      } else {
        attr(star$dimension[[d]], "type") <- "general"
      }
    }

    structure(star,
              class = "multistar")
  }



#' Export a `multistar` as a flat table
#'
#' We can obtain a flat table, implemented using a `tibble`, from a `multistar`
#' (which can be the result of a query). If it only has one fact table, it is
#' not necessary to provide its name.
#'
#' @param ms A `multistar` object.
#' @param fact A string, name of the fact.
#'
#' @return A `tibble`.
#'
#' @family results export functions
#'
#' @examples
#'
#' ft <- ms_mrs |>
#'   multistar_as_flat_table(fact = "mrs_age")
#'
#' ms <- dimensional_query(ms_mrs) |>
#'   select_dimension(name = "where",
#'                    attributes = c("city", "state")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year")) |>
#'   select_fact(name = "mrs_age",
#'               measures = c("n_deaths")) |>
#'   select_fact(
#'     name = "mrs_cause",
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths")
#'   ) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston") |>
#'   run_query()
#'
#' ft <- ms |>
#'   multistar_as_flat_table()
#'
#' @export
multistar_as_flat_table <- function(ms, fact = NULL) {
  UseMethod("multistar_as_flat_table")
}


#' @rdname multistar_as_flat_table
#' @export
multistar_as_flat_table.multistar <- function(ms, fact = NULL) {
  if (length(ms$fact) == 1) {
    ft <- ms$fact[[1]]
  } else {
    stopifnot("The name of facts must be indicated." = !is.null(fact))
    validate_names(names(ms$fact), fact, concept = 'fact name')
    ft <- ms$fact[[fact]]
  }
  ft_fk <- attr(ft, "foreign_keys")
  for (d in names(ms$dimension)) {
    if (sprintf("%s_key", d) %in% ft_fk) {
      ft <- dereference_dimension(ft, ms$dimension[[d]], conversion = FALSE)
    }
  }
  tibble::as_tibble(ft)
}

