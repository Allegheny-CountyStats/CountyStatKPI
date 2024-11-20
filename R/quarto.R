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
