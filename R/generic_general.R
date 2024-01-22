#' Function to calculate month-over-month and year-over-year or year-to-date percentage change.
#'
#' @param rm_cnt Reporting month count in "my" mode and current year-to-date count in "ytd" mode.
#' @param pm_cnt Previous month count in "my" mode or previous year-to-date count in "ytd" mode.
#' @param py_cnt Same month of previous year count (this is optional and only needed in my mode)
#' @param mode Can be either "my" or "ytd". The former calculates month-over-month and year-over-year change, the latter calculates change between current year-to-date and the previous year over the same period.
#'
#' @return A list with the entered values, the percent change, and up or down arrow, whether it increased or decreased, and the absolute change
#' @export
#'
#' @examples
#' \dontrun{metric_pcts <- kpi_pcts(metric_cnt, metric_pm_cnt, metric_py_cnt)}

kpi_pcts <- function(rm_cnt, pm_cnt, py_cnt, mode = "my") {

  if (mode == "my") {

    mom_pct_chg <- round((rm_cnt - pm_cnt)/pm_cnt * 100, 1)

    # creates an up or down arrow depending on whether the metric increased or
    # decreased
    mom_up_down <- dplyr::case_when(mom_pct_chg > 0 ~ "\u2191",
                                    mom_pct_chg < 0 ~ "\u2193",
                                    TRUE ~ "—")

    mom_inc_dec <- dplyr::case_when(mom_pct_chg > 0 ~ "increase",
                                    mom_pct_chg < 0 ~ "decrease",
                                    TRUE ~ "no change")

    mom_abs_chg <- abs(mom_pct_chg)

    yoy_pct_chg <- round((rm_cnt - py_cnt)/py_cnt * 100, 1)

    # creates an up or down arrow depending on whether the metric increased or
    # decreased
    yoy_up_down <- dplyr::case_when(yoy_pct_chg > 0 ~ "\u2191",
                                    yoy_pct_chg < 0 ~ "\u2193",
                                    TRUE ~ "—")

    yoy_inc_dec <- dplyr::case_when(yoy_pct_chg > 0 ~ "increase",
                                    yoy_pct_chg < 0 ~ "decrease",
                                    TRUE ~ "no change")

    yoy_abs_chg <- abs(yoy_pct_chg)

    return(list(rm_cnt = rm_cnt, pm_cnt = pm_cnt, py_cnt = py_cnt,
                mom_pct_chg = mom_pct_chg, mom_up_down = mom_up_down,
                mom_inc_dec = mom_inc_dec, mom_abs_chg = mom_abs_chg,
                yoy_pct_chg = yoy_pct_chg, yoy_up_down = yoy_up_down,
                yoy_inc_dec = yoy_inc_dec, yoy_abs_chg = yoy_abs_chg))

  } else if (mode == "ytd") {

    ytd_pct_chg <- round((rm_cnt - pm_cnt)/pm_cnt * 100, 1)

    # creates an up or down arrow depending on whether the metric increased or
    # decreased
    ytd_up_down <- dplyr::case_when(ytd_pct_chg > 0 ~ "\u2191",
                                    ytd_pct_chg < 0 ~ "\u2193",
                                    TRUE ~ "—")

    ytd_inc_dec <- dplyr::case_when(ytd_pct_chg > 0 ~ "increase",
                                    ytd_pct_chg < 0 ~ "decrease",
                                    TRUE ~ "no change")

    ytd_abs_chg <- abs(ytd_pct_chg)

    return(list(rytd_cnt = rm_cnt, pytd_cnt = pm_cnt,
                ytd_pct_chg = ytd_pct_chg, ytd_up_down = ytd_up_down,
                ytd_inc_dec = ytd_inc_dec, ytd_abs_chg = ytd_abs_chg))

  } else {
    return("Mode must be either 'my' or 'ytd'")
  }
}

#' A function to obtain current month, previous month, and same month of previous year values, or current year-to-date and the same period of the previous year values and calculate percent changes.
#'
#' @param data A data frame.
#' @param date_col The date column in the data dataframe.
#' @param rpt_date The first day of the month of the report (this will serve as the base month in the comparisons).
#' @param method The method by which you want to process the data. It has four values "tally" counts rows, "sum" sums values in a specified column, "average" averages values in a specified column, and "distinct" counts the distinct values in a specified column.
#' @param val_col Only necessary for methods "sum", "average", and "distinct". The column which has the values you want to perform that function on.
#' @param mode Can be either "my" or "ytd". The former calculates month-over-month and year-over-year change, the latter calculates current change between current year-to-date and the previous year over the same period.
#'
#' @return A list with the values being compared and the comparisons.
#' @export
#'
#' @examples
#' \dontrun{metric_cmp <- kpi_compare(metric_data, date, rpt_date, mode = "ytd")}
#' \dontrun{metric_cmp <- kpi_compare(metric_data, date, rpt_date, method = "avg", val_col = totals)}

kpi_compare <- function(data, date_col, rpt_date, method = "tally", val_col, mode = "my") {

  if (mode == "my") {

    if (method == "tally") {

      # filters using floor date and tallies values for the reporting month,
      # previous month, and same month of the previous year

      rm_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      pm_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - months(1)) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      py_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - lubridate::years(1)) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

    } else if (method == "sum") {

      # filters using floor date and sums a specified column for the reporting month,
      # previous month, and same month of the previous year

      rm_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date) %>%
        dplyr::summarize(n = sum({{val_col}}, na.rm = T)) %>%
        dplyr::pull(n)

      pm_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - months(1)) %>%
        dplyr::summarize(n = sum({{val_col}}, na.rm = T)) %>%
        dplyr::pull(n)

      py_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - lubridate::years(1)) %>%
        dplyr::summarize(n = sum({{val_col}}, na.rm = T)) %>%
        dplyr::pull(n)

    } else if (method == "avg") {

      # filters using floor date and averages a specified column for the reporting month,
      # previous month, and same month of the previous year

      rm_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date) %>%
        dplyr::summarize(n = round(mean({{val_col}}, na.rm = T), 1)) %>%
        dplyr::pull(n)

      pm_cnt <- data %>%
        filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - months(1)) %>%
        summarize(n = round(mean({{val_col}}, na.rm = T), 1)) %>%
        pull(n)

      py_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - lubridate::years(1)) %>%
        dplyr::summarize(n = round(mean({{val_col}}, na.rm = T), 1)) %>%
        dplyr::pull(n)

    } else if (method == "distinct") {

      # filters using floor date and counts distinct values in a specified column
      # for the reporting month, previous month, and same month of the previous year

      rm_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date) %>%
        dplyr::summarize(n = dplyr::n_distinct({{val_col}})) %>%
        dplyr::pull(n)

      pm_cnt <- data %>%
        filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - months(1)) %>%
        dplyr::summarize(n = dplyr::n_distinct({{val_col}})) %>%
        dplyr::pull(n)

      py_cnt <- data %>%
        dplyr::filter(lubridate::floor_date({{date_col}}, unit = "months") == rpt_date - lubridate::years(1)) %>%
        dplyr::summarize(n = dplyr::n_distinct({{val_col}})) %>%
        dplyr::pull(n)

    } else {

      return("method must be 'tally', 'sum', 'avg', or 'distinct'.")

    }

    # calculate percentage change based on the above values

    kpi_pcts(rm_cnt, pm_cnt, py_cnt)

  } else if (mode == "ytd") {

    if (method == "tally") {

      # filters and tallies values for the reporting year-to-date and the
      # same period in the previous year

      rytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      pytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date - lubridate::years(1)),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date - lubridate::years(1)) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

    } else if (method == "sum") {

      # filters and sums values in a specified column for the reporting year-to-date
      # and the same period in the previous year

      rytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date) %>%
        dplyr::summarize(n = sum({{val_col}}, na.rm = T)) %>%
        dplyr::pull(n)

      pytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date - lubridate::years(1)),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date - lubridate::years(1)) %>%
        dplyr::summarize(n = sum({{val_col}}, na.rm = T)) %>%
        dplyr::pull(n)

    } else if (method == "avg") {

      # filters and averages values in a specified column for the reporting year-to-date
      # and the same period in the previous year

      rytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date) %>%
        dplyr::summarize(n = round(mean({{val_col}}, na.rm = T), 1)) %>%
        dplyr::pull(n)

      pytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date - lubridate::years(1)),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date - lubridate::years(1)) %>%
        dplyr::summarize(n = round(mean({{val_col}}, na.rm = T), 1)) %>%
        dplyr::pull(n)

    } else if (method == "distinct") {

      # filters and counts distinct values in a specified column for the reporting
      # year-to-date and the same period in the previous year

      rytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date) %>%
        dplyr::summarize(n = dplyr::n_distinct({{val_col}})) %>%
        dplyr::pull(n)

      pytd_cnt <- data %>%
        dplyr::filter(lubridate::year({{date_col}}) == lubridate::year(rpt_date - lubridate::years(1)),
                      lubridate::floor_date({{date_col}}, unit = "month") <= rpt_date - lubridate::years(1)) %>%
        dplyr::summarize(n = dplyr::n_distinct({{val_col}})) %>%
        dplyr::pull(n)

    } else {

      return("method must be 'tally', 'sum', 'avg', or 'distinct'.")

    }

    # calculate percentage change

    kpi_pcts(rytd_cnt, pytd_cnt, mode = 'ytd')

  } else {

    return("mode must be 'my' or 'ytd'.")

  }

}

#' A function that creates the text at the top of each KPI showing the value and change from the previous specified period
#'
#' @param kpi_cmp A kpi_compare list produced by kpi_compare or kpi_pcts.
#' @param rpt_date The first day of the month of the report.
#' @param prior_val The value you want to compare to "month" for previous month, "year" for the same month of the previous year, or "ytd" to compare the current year-to-date with the same period of the previous year.
#' @param metric_txt The text you want to appear after the reporting month count.
#'
#' @return A string with R markdown formatting.
#' @export
#'
#' #' @examples
#' \dontrun{metric_header_txt <- kpi_header_text(metric_cmp, rpt_date, prior_val = "year", metric_txt = "calls to IT help desk")}

kpi_header_text <- function(kpi_cmp, rpt_date, prior_val = "month", metric_txt) {

  # put the reporting date into readable month year format
  my <- format(rpt_date, "%B %Y")

  if (prior_val == "month") {

    # get the prior month and year in readable format
    pmy <- format(rpt_date - months(1), "%B %Y")

    if (kpi_cmp$mom_inc_dec == "no change") {

      header_text <- stringr::str_c("## ", my, ": ", prettyNum(kpi_cmp$rm_cnt, big.mark = ','), " ", metric_txt, "\n",
                                    "**", kpi_cmp$mom_up_down, " No change compared to ", pmy, "**")

    } else {

      header_text <- stringr::str_c("## ", my, ": ", prettyNum(kpi_cmp$rm_cnt, big.mark = ','), " ", metric_txt, "\n",
                                    "**", kpi_cmp$mom_up_down, " ", kpi_cmp$mom_abs_chg, "% ", kpi_cmp$mom_inc_dec, " compared to ", pmy, "**")

    }

  } else if (prior_val == "year") {

    # get the same month of the previous year in readable month year format
    pmy <- format(rpt_date - lubridate::years(1), "%B %Y")

    if (kpi_cmp$yoy_inc_dec == "no change") {

      header_text <- stringr::str_c("## ", my, ": ", prettyNum(kpi_cmp$rm_cnt, big.mark = ','), " ", metric_txt, "\n",
                                    "**", kpi_cmp$yoy_up_down, " No change compared to ", pmy, "**")

    } else {

      header_text <- stringr::str_c("## ", my, ": ", prettyNum(kpi_cmp$rm_cnt, big.mark = ','), " ", metric_txt, "\n",
                                    "**", kpi_cmp$yoy_up_down, " ", kpi_cmp$yoy_abs_chg, "% ", kpi_cmp$yoy_inc_dec, " compared to ", pmy, "**")

    }

  } else if (prior_val == "ytd") {

    if (kpi_cmp$ytd_inc_dec == "no change") {

      header_text <- stringr::str_c("## YTD: ", prettyNum(kpi_cmp$rytd_cnt, big.mark = ','), " ", metric_txt, "\n",
                                    "**", kpi_cmp$ytd_up_down, " ",  "No change compared to the same period in the previous year.**")

    } else {

      header_text <- stringr::str_c("## YTD: ", prettyNum(kpi_cmp$rytd_cnt, big.mark = ','), " ", metric_txt, "\n",
                                    "**", kpi_cmp$ytd_up_down, " ", kpi_cmp$ytd_abs_chg, "% ", kpi_cmp$ytd_inc_dec, " compared the same period in the previous year.**")

    }

  } else {

    return("prior_val must be 'month', 'year', or 'ytd'.")

  }

  return(header_text)

}

#' A function creating text for year-over-year change.
#'
#' @param kpi_cmp A kpi_compare object.
#' @param rpt_date The first day of the reporting month.
#'
#' @return A string with R markdown formatting.
#' @export
#'
#' #' @examples
#' \dontrun{metric_yoy_text <- kpi_yoy_text(metric_cmp, rpt_date)}
kpi_yoy_text <- function(kpi_cmp, rpt_date) {
  month <- month(rpt_date, label = TRUE, abbr = FALSE)
  if (kpi_cmp$yoy_inc_dec == "no change") {
    yoy_text <- str_c("There has been **no change compared to last ", month, "**")
  } else {
    yoy_text <- str_c("This month represents **a ", kpi_cmp$yoy_abs_chg, "% ",
                      kpi_cmp$yoy_inc_dec, " compared to last ", month, "**")
  }
  return(yoy_text)
}
