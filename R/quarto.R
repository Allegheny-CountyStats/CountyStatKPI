#' Function to build the KPI callout bar for Quarto Sites. This function should be inside an 'asis' code chunk.
#'
#' @param type What type of callout found at: https://quarto.org/docs/authoring/callouts.html
#' @param kpi Callout/KPI Title
#' @param text Content of the callout box
#'
#' @return Quarto Callout Markdown
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ```{r results='asis'}
#' total_pop <- nrow(filter(penguins, year == max(penguins$year)))
#' type <- if_else(total_pop < 200, "important", "tip")
#' kpi_callout(type, "Penguin Population", paste("The Penguin Population for 2009 was", total_pop))
#' ```
#' }
kpi_callout <- function(type, kpi, text) {
  sprintf(
    paste(
      "::: {.callout-%s icon=false}",
      "## %s",
      "%s",
      ":::",
      sep = "\n\n"
    ),
    type, kpi, text
  ) %>%
    cat()
}

#' Function to generate the proper date generated text.
#'
#' @return HTML Date Rendered Text
#' @param page Page type of rendered text, default = "Dept" which includes entire text, any other value will just provide the Month, Day and Year as text.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' ```{r results='asis'}
#' gen_date_readable()
#' ```
#' }
gen_date_readable <- function(page = "Dept") {
  mon <- format(Sys.Date(), "%B")
  year <- format(Sys.Date(), "%Y")
  day <- as.numeric(format(Sys.Date(), "%d"))
  day_form <- scales::label_ordinal()(as.integer(day))
  if (page == "Dept") {
    txt <- glue::glue("<small>Page generated on {mon} {day_form}, {year}</small>")
  } else {
    txt <- glue::glue("{mon} {day_form}, {year}")
  }
  return(txt)
}
