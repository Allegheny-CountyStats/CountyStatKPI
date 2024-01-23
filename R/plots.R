#' A function that creates a trend plot, showing monthly data for the current year and four previous years, with the current year highlighted. It must be followed with labs and theme arguments.
#'
#' @param data A data frame.
#' @param cnt_col The column with the counts or values you want to plot.
#' @param month_lag How many months you want it to lag from the report date (default 0).
#' @param rpt_month The first date of the month of the KPI report (default rpt_date).
#' @param date_col The date column in the data frame (default rpting_date).
#' @param palette The palette you want it to use (default pal).
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' metric_data %>% kpi_trend_plot(totals) + labs(title = 'Plot title', ylab = 'Count'))
#' metric_data %>% kpi_trend_plot(totals, month_lag = 3, date_col = date) + labs(title = 'Plot title', ylab = 'Count') + theme_minimal())
kpi_trend_plot <- function(data, cnt_col, month_lag = 0, rpt_month = rpt_date, date_col = rpting_date, palette = pal) {
  plot <- data %>%
    filter(year({{date_col}}) >= year(rpt_month - months(month_lag)) - 4,
           {{date_col}} <= rpt_month - months(month_lag)) %>%
    mutate(month_abb = month({{date_col}}, label = TRUE),
           year_fct = factor(year({{date_col}})),
           this_year = ifelse(year({{date_col}}) == year(rpt_month), TRUE, FALSE)) %>%
    ggplot(aes(month_abb, {{cnt_col}}, group = year_fct, color = year_fct, alpha = this_year)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_color_manual(values = palette) +
    scale_alpha_discrete(range = c(0.3, 1)) +
    guides(alpha = FALSE)

  return(plot)
}

#' A theme for KPI trend plots
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' plot + labs(title = "Plot title", y = "Count") + kpi_trend_theme())
kpi_trend_theme <- ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 axis.title.x = ggplot2::element_blank())

#' A function that creates a uniform color scheme for KPI reports.
#'
#' @return A palette.
#' @export
#'
#' @examples
#' palette = kpiPal(rpt_date)
kpiPal <- function(end_date) {
  this_year <- as.numeric(format(end_date, "%Y"))
  pal_cols <- c('#1b9e77', '#d95f02', '#e6ab02', '#66a61e', '#7570b3', '#e7298a')
  years <- 2017:this_year
  color_pal <- data.frame(years)
  color_pal$colors <- rep_len(pal_cols, length.out=length(years))
  color_pal %>% deframe()
}

