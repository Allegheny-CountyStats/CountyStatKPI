#' Function to pull RTK data for a particular department. If a department includes multiple divisions or if the report requires multiple departments to be shown, this distinction can be made.
#'
#' @param con Data warehouse connection
#' @param dept_list A string containing the department name as it appears in GovQA. If there are multiple departments/divisions, each value must be surrounded by singular quotation marks and separated by a comma.
#' @param col_list A string containing the department-specific columns that show information type in GovQA. If there are multiple, they must be separated by a comma.
#' @param mode Can either be "single" or "multi" and defaults to "single". Use "multi" if there are multiple departments/divisions.
#'
#' @return A data frame of RTK requests for the specified department
#' @export
#'
#' @examples
#' \dontrun{
#' dept_list <- "'Administrative Services', 'Elections', 'Property Assessment', 'Purchasing', 'Real Estate'"
#' col_list <- "department_information_type_administrative_servi, department_information_type_elections, department_information_type_property_assessment, department_information_type_purchasing, department_information_type_real_estate"
#' data_as <- rtk_pull(wh_con, dept_list, col_list, mode = "multi")
#' data_ems <- rtk_pull(wh_con, "'Emergency Services'", "department_information_type_emergency_services", mode = "single")
#' }

rtk_pull <- function(con, dept_list, col_list, mode = "single") {
  if (mode == "single") {
    rtkdata <- DBI:: dbGetQuery(con, DBI::SQL(paste0("SELECT close_date,
    create_date,
    reference_no,
    request_status,
    source,
    department_requesting_information_from,",
    col_list, " AS information_type
    FROM Master.AS_OpenRecords_GovQA_OpenRecordsRequests
        WHERE department_requesting_information_from = ", dept_list)))

    duedates <- DBI::dbGetQuery(con, DBI::SQL("SELECT reference_no,
      required_completion_date,
      first_response_date
      FROM Master.AS_OpenRecords_GovQA_OpenRecordsActivities"))

    rtkjoin <- rtkdata %>%
      dplyr::left_join(duedates, by = "reference_no") %>%
      dplyr::mutate(required_completion_date = as.Date(lubridate::parse_date_time(required_completion_date, "%m-%d-%Y %I:%M:%S %p"), format = "%Y-%m-%d"),
                    first_response_date = as.Date(lubridate::parse_date_time(first_response_date, "%m-%d-%Y %I:%M:%S %p"), format = "%Y-%m-%d"))

    return(data.frame(rtkjoin))
  } else if (mode == "multi") {
    rtkdata <- DBI::dbGetQuery(con, SQL(paste0("SELECT close_date,
      create_date,
      reference_no,
      request_status,
      source,
      department_requesting_information_from,",
      col_list,
        " FROM Master.AS_OpenRecords_GovQA_OpenRecordsRequests
        WHERE department_requesting_information_from IN (", dept_list, ")")))

    duedates <- DBI::dbGetQuery(con, SQL("SELECT reference_no,
      required_completion_date,
      first_response_date
      FROM Master.AS_OpenRecords_GovQA_OpenRecordsActivities"))

    rtkjoin<- rtkdata %>%
      dplyr::left_join(duedates, by = "reference_no") %>%
      dplyr::mutate(required_completion_date = as.Date(lubridate::parse_date_time(required_completion_date, "%m-%d-%Y %I:%M:%S %p"), format = "%Y-%m-%d"),
                    first_response_date = as.Date(lubridate::parse_date_time(first_response_date, "%m-%d-%Y %I:%M:%S %p"), format = "%Y-%m-%d"))

    return(data.frame(rtkjoin))
  } else {
    return("Mode must be either 'single' or 'multi'.")
  }
}

#' Function that manipulates RTK data, retrieved using function rtk_pull, into a list of data frames that can be used to create RTK KPI-related text, tables, charts, and graphs. Can be used with either 'single' or 'multi' data.
#'
#' @param data data frame of RTK data
#' @param rpt_sdate a date value that represents the first day of the reporting month
#' @param rpt_edate a date value that represents the last day of the reporting month
#'
#' @return A list of data frames of aggregated RTK data for the department(s) selected
#' @export
#'
#' @examples
#' \dontrun{
#' as_transform <- rtk_data(data_as, start_last_month, end_last_month)
#' }

rtk_data <- function(data, rpt_sdate, rpt_edate) {
  business_calendar <- bizdays::create.calendar("my_calendar", weekdays = c('saturday', 'sunday'))

  clean_data <- data %>%
    dplyr::mutate(information_type =  dplyr::coalesce(!!!dplyr::select(., 7:(ncol(data)-2)))) %>%
    dplyr::mutate(date_open = as.Date(create_date),
                  date_close = dplyr::case_when(as.Date(close_date) <= rpt_edate ~ as.Date(close_date)),
                  open_month = lubridate::floor_date(date_open, 'month'),
                  close_month = lubridate::floor_date(date_close, 'month'),
                  status = factor(ifelse(is.na(date_close), "Unresolved", "Resolved"), levels = c("Unresolved", "Resolved")),
                  date_required_completion = dplyr::case_when(is.na(required_completion_date) ~ as.Date(date_open + lubridate::days(30)),
                                                              TRUE ~ as.Date(required_completion_date)),
                  bizdays_toclose = ifelse(is.na(date_close), NA, round(as.numeric(bizdays::bizdays(date_open, date_close, cal = business_calendar)), digits = 1)),
                  late = ifelse(date_close > date_required_completion, 1, 0),
                  bizdays_late = ifelse(late == 1, round(as.numeric(bizdays::bizdays(date_required_completion, date_close, cal = business_calendar)), digits = 1), 0),
                  bizdays_response = round(as.numeric(bizdays::bizdays(date_open, first_response_date, cal = business_calendar)), digits = 1),
                  response_late = ifelse(bizdays_response > 5, 1, 0),
                  response_bizdayslate = ifelse(bizdays_response > 5, bizdays_response-5, 0),
                  no_response = ifelse(is.na(first_response_date), 1, 0)) %>%
    dplyr::filter(open_month <= rpt_sdate) %>%
    dplyr::select(reference_no, date_open, open_month, date_close, close_month, department_requesting_information_from, status, request_status, source, information_type, date_required_completion, bizdays_toclose, late, bizdays_late, bizdays_response, response_late, response_bizdayslate, no_response)

  type <- clean_data %>%
    dplyr::mutate(information_type = tidyr::replace_na(information_type, "No Type Selected")) %>%
    dplyr::group_by(open_month, information_type) %>%
    dplyr::summarise(count = n_distinct(reference_no, na.rm = TRUE)) %>%
    dplyr::rename("month" = "open_month")

  source <- clean_data %>%
    dplyr::group_by(open_month, source) %>%
    dplyr::summarise(count = n_distinct(reference_no, na.rm = TRUE)) %>%
    dplyr::rename("month" = "open_month")

  close <- clean_data %>%
    dplyr::filter(!is.na(date_close)) %>%
    dplyr::group_by(close_month) %>%
    dplyr::summarise(avg_daystoclose = round(mean(bizdays_toclose, na.rm = TRUE), 1),
                     med_daystoclose = round(median(bizdays_toclose, na.rm = TRUE), 1)) %>%
    dplyr::rename("month" = "close_month")

  late <- clean_data %>%
    dplyr::filter(!is.na(date_close)) %>%
    dplyr::group_by(close_month) %>%
    dplyr::summarise(count = sum(late, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(close_month = seq.Date(lubridate::floor_date(rpt_sdate, "year")- lubridate::years(4), rpt_sdate, by = "month"), fill = list(count = 0)) %>%
    dplyr::mutate(year = lubridate::year(close_month),
                  month_abb = factor(month.abb[lubridate::month(close_month)], levels = month.abb),
                  thisyear = ifelse(lubridate::year(close_month) == lubridate::year(rpt_sdate), TRUE, FALSE)) %>%
    dplyr::rename("month" = "close_month")

  table <- clean_data %>%
    dplyr::rename("month" = "open_month")

  response <- clean_data %>%
    dplyr::filter(response_late == 1) %>%
    dplyr::group_by(open_month) %>%
    dplyr::summarise(count = sum(response_late, na.rm = TRUE),
                     avg_bizdayslate = round(mean(bizdays_response, na.rm = TRUE), 1)) %>%
    tidyr::complete(open_month = seq.Date(lubridate::floor_date(rpt_sdate, "year")- lubridate::years(4), rpt_sdate, by = "month"), fill = list(count = 0, avg_bizdayslate = 0)) %>%
    dplyr::mutate(year = lubridate::year(open_month),
                  month_abb = factor(month.abb[lubridate::month(open_month)], levels = month.abb),
                  thisyear = ifelse(lubridate::year(open_month) == lubridate::year(rpt_sdate), TRUE, FALSE)) %>%
    dplyr::rename("month" = "open_month")


  if (length(unique(clean_data$department_requesting_information_from)) == 1) {

    status <- clean_data %>%
      dplyr::group_by(open_month, status) %>%
      dplyr::summarise(count = n_distinct(reference_no, na.rm = TRUE)) %>%
      dplyr::rename("month" = "open_month")

    received <- clean_data %>%
      dplyr::group_by(open_month) %>%
      dplyr::summarise(count = n_distinct(reference_no, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(open_month = seq.Date(lubridate::floor_date(rpt_sdate, "year")- lubridate::years(4), rpt_sdate, by = "month"), fill = list(count = 0)) %>%
      dplyr::mutate(year = lubridate::year(open_month),
                    month_abb = factor(month.abb[lubridate::month(open_month)], levels = month.abb),
                    thisyear = ifelse(lubridate::year(open_month) == lubridate::year(rpt_sdate), TRUE, FALSE)) %>%
      dplyr::rename("month" = "open_month")

    } else if (length(unique(clean_data$department_requesting_information_from)) > 1) {

      status <- clean_data %>%
        dplyr::group_by(open_month, department_requesting_information_from, status) %>%
        dplyr::summarise(count = n_distinct(reference_no, na.rm = TRUE)) %>%
        dplyr::rename("month" = "open_month")

      received <- clean_data %>%
        dplyr::group_by(open_month, department_requesting_information_from) %>%
        dplyr::summarise(count = n_distinct(reference_no, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        tidyr::complete(open_month = seq.Date(lubridate::floor_date(rpt_sdate, "year")-lubridate::years(4), rpt_sdate, by = "month"), nesting(department_requesting_information_from), fill = list(count = 0)) %>%
        dplyr::mutate(year = lubridate::year(open_month),
                      month_abb = factor(month.abb[lubridate::month(open_month)], levels = month.abb),
                      thisyear = ifelse(lubridate::year(open_month) == lubridate::year(rpt_sdate), TRUE, FALSE)) %>%
        dplyr::rename("month" = "open_month")

    }  else {
      return("Must supply data frame, report start date, and report end date. Check dataframe for NA values.")
    }
  return(list(clean = clean_data, table = table, type = type, source = source, close = close, late = late, response = response, status = status, received = received))
}

#' Function that filters RTK data, cleaned and formatted by rtk_data function, down to a specified date range, then creates the selected kable or ggplot object.
#'
#' @param data Data frame of clean and formatted RTK data
#' @param rpt_sdate A date value that represents the first day of the reporting month
#' @param palette Color palette for time series
#' @param type Type of kable or gglot object returned. Must be 'type', 'source', 'close', 'late', 'table', 'status', or 'received'. 'Status' and 'received' types facet by department if more than one is present in data. Data format must match type selected.
#' @param range Date range displayed in plot or table. Must be 'month', '12months', 'timeseries', or 'ytd'.
#'
#' @return A kable or ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' as_plots <- rtk_chart(as_transform$late, start_last_month, pal, type = "late", range = "timeseries")
#' print(as_plots)
#' ems_plots <- rtk_chart(ems_transform$status, start_last_month, kpipalette, type = "status", range = "12months")
#' print(ems_plots)
#' }

rtk_chart <- function(data, rpt_sdate, palette, type = "type", range = "month") {
  if (range == "month") {
    range_data <- data %>%
      dplyr::filter(month == rpt_sdate)
    subtitle <- format(rpt_sdate, format = "%B %Y")
  } else if (range == "12months") {
    range_data <- data %>%
      dplyr::filter(month >= rpt_sdate - months(12))
    subtitle <- paste0("From ", format(min(range_data$month), format = "%B %Y"), " to ", format(max(range_data$month), format = "%B %Y"))
  } else if (range == "timeseries") {
    range_data <- data %>%
      dplyr::filter(lubridate::year(month) >= lubridate::year(rpt_sdate) - 4)
    subtitle <- paste0(lubridate::year(min(range_data$month)), " to ", lubridate::year(max(range_data$month)))
  } else if (range == "ytd") {
    range_data <- data %>%
      dplyr::filter(lubridate::year(month) == lubridate::year(rpt_sdate))
    subtitle <- paste0(lubridate::year(min(range_data$month)), " to date")
  }else {
    return("Range must be 'month', '12months', 'timeseries', or 'ytd'.")
  }
  if (type == "type") {
    plot <- range_data %>%
      dplyr::group_by(information_type) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE)) %>%
      ggplot2::ggplot(aes(area = count, fill = information_type, label = paste0(stringr::str_wrap(information_type, 10), "\n", count))) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(color = "white",
                                    place = "center",
                                    size = 10) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = "Dark2") +
      ggplot2::labs(title = "RTK Requests by Information Type Requested",
                    subtitle = subtitle)
    return(plot)
  } else if (type == "source") {
    plot <- range_data %>%
      dplyr::group_by(source) %>%
      dplyr::summarise(count = sum(count, na.rm = TRUE)) %>%
      ggplot2::ggplot(aes(area = count, fill = source, label = paste0(stringr::str_wrap(source, 10), "\n", count))) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(color = "white",
                                    place = "center",
                                    size = 10) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = "Dark2") +
      ggplot2::labs(title = "RTK Requests by Source",
                    subtitle = subtitle)
    return(plot)
  } else if (type == "close") {
    x_year_lab <- as.Date(rpt_sdate) - lubridate::days(182)
    plot <- range_data %>%
      ggplot2::ggplot(aes(x=month, y = med_daystoclose)) +
      ggplot2::geom_bar(stat = "identity", fill = "#E6AB02") +
      ggplot2::labs(title = "Median and Average Days to Close RTK Requests",
                    subtitle = subtitle,
                    caption = "Bars represent the median days to close and lines display the average days to close, which could be inflated by outliers.\n Average lines are absent where the median exceeds the average, and therefore no outliers are present.") +
      ggplot2::geom_text(aes(label = round(med_daystoclose, 0)), vjust = 1.5, size = 3) +
      ggplot2::geom_errorbar(data = subset(range_data, avg_daystoclose > med_daystoclose), aes(ymin = med_daystoclose, ymax = avg_daystoclose), width = 1) +
      ggplot2::geom_text(data = subset(range_data, avg_daystoclose > med_daystoclose), aes(label = round(avg_daystoclose, 0), y = avg_daystoclose), vjust = -0.5, size = 3) +
      ggplot2::geom_hline(yintercept = 30,
                          linetype = "longdash",
                          color = "#D95F02") +
      ggplot2::geom_label(aes(x = x_year_lab, y=32.5, label = "Target: 30 days"),
                          color = "white", fill= "#D95F02", alpha = 0.1, label.size = NA) +
      ggplot2::ylim(0, max(range_data$avg_daystoclose)+10) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = element_blank(),
                     axis.title.y = element_blank())
    return(plot)
  } else if (type == "late") {
    plot <- range_data %>%
      ggplot2::ggplot(aes(x = factor(month_abb), y = count, group = factor(year), color = factor(year), alpha = thisyear)) +
      ggplot2::geom_line(size = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(title = "RTK Requests Closed After Required \nCompletion Date",
           subtitle = subtitle,
           alpha = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, vjust = 0.5),
            legend.position = "bottom",
            legend.title = element_blank()) +
      ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
      ggplot2::scale_alpha_discrete(range = c(0.3, 1)) +
      ggplot2::guides(alpha = 'none') +
      ggplot2::scale_color_manual(values = palette)
    return(plot)
  } else if (type == "table") {
    table <- range_data %>%
      dplyr::group_by(department_requesting_information_from) %>%
      dplyr::summarise(count = n_distinct(reference_no, na.rm = TRUE)) %>%
      kableExtra::kable(caption = "RTK Requests by Division", col.names = c("Division", "Requests"), align = "lr", format.args = list(big.mark = ",")) %>%
      kableExtra::kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
      kableExtra::row_spec(0, bold = T)
    return(table)
  } else if (type == "status") {
    if ("department_requesting_information_from" %in% colnames(range_data)) {
      plot <- range_data %>%
        ggplot2::ggplot(aes(x = month, y = count, fill = factor(status), group = factor(status))) +
        ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack()) +
        ggplot2::geom_label(aes(x= month, y= count, label = count), size = 3, position = ggplot2::position_stack(vjust=0.5), color = "black", fill=alpha(c("white"), 0.3), label.size= 0) +
        ggplot2::scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
        ggplot2::labs(title = "RTK Requests by Month",
                      subtitle = subtitle,
                      y = "",
                      fill = "Status") +
        ggplot2::facet_wrap(department_requesting_information_from ~ ., ncol = 1) +
        ggplot2::theme_light() +
        ggplot2::theme(axis.title.x = element_blank(),
                       legend.position = "bottom")
      return(plot)
    } else if (!"department_requesting_information_from" %in% colnames(range_data)) {
      plot <- range_data %>%
        ggplot2::ggplot(aes(x = month, y = count, fill = factor(status), group = factor(status))) +
        ggplot2::geom_bar(stat = "identity", position = ggplot2::position_stack()) +
        ggplot2::geom_label(aes(x= month, y= count, label = count), size = 3, position= ggplot2::position_stack(vjust=0.5), color = "black", fill=alpha(c("white"), 0.3), label.size= 0) +
        ggplot2::scale_fill_manual(values = c("#D95F02", "#1B9E77")) +
        ggplot2::labs(title = "RTK Requests by Month",
                      subtitle = subtitle,
                      y = "",
                      fill = "Status") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.x = element_blank(),
                       legend.position = "bottom")
      return(plot)
    }
  } else if (type == "received") {
    if ("department_requesting_information_from" %in% colnames(range_data)) {
      plot <- range_data %>%
        ggplot2::ggplot(aes(x = factor(month_abb), y = count, group = factor(year), color = factor(year), alpha = thisyear)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(title = "RTK Requests Received",
                      subtitle = subtitle,
                      alpha = "") +
        ggplot2::facet_wrap(department_requesting_information_from ~ ., ncol = 1) +
        ggplot2::theme_light() +
        ggplot2::theme(axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.title = element_blank(),
                       legend.position = "bottom") +
        ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
        ggplot2::scale_alpha_discrete(range = c(0.3, 1)) +
        ggplot2::guides(alpha = 'none') +
        ggplot2::scale_color_manual(values = palette)
      return(plot)
    } else if (!"department_requesting_information_from" %in% colnames(range_data)) {
      plot <- range_data %>%
        ggplot2::ggplot(aes(x = factor(month_abb), y = count, group = factor(year), color = factor(year), alpha = thisyear)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(size = 2) +
        ggplot2::labs(title = "RTK Requests Received",
                      subtitle = subtitle,
                      alpha = "") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.title = element_blank(),
                       legend.position = "bottom") +
        ggplot2::scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
        ggplot2::scale_alpha_discrete(range = c(0.3, 1)) +
        ggplot2::guides(alpha = 'none') +
        ggplot2::scale_color_manual(values = palette)
      return(plot)
    }
  } else {
    return("Type must be 'type', 'source', 'close', 'late', 'status', 'received', or 'table'. Type must be compatible with data format.")
  }
}

#' Function that manipulates RTK data, retrieved using function rtk_pull, into a list of values that can be used to create RTK KPI-related text. Can be used with either 'single' or 'multi' data, but 'multi' data will be aggregated.
#'
#' @param data data frame of RTK data
#' @param rpt_sdate a date value that represents the first day of the reporting month
#' @param rpt_edate a date value that represents the last day of the reporting month
#'
#' @return A named list of values of RTK KPIs
#' @export
#'
#' @examples
#' \dontrun{
#' as_summary <- rtk_data(data_as, start_last_month, end_last_month)
#' }

rtk_summary <- function(data, rpt_sdate, rpt_edate) {
  business_calendar <- bizdays::create.calendar("my_calendar", weekdays = c('saturday', 'sunday'))

  clean_data <- data %>%
    dplyr::mutate(information_type =  dplyr::coalesce(!!!dplyr::select(., 7:(ncol(data)-2)))) %>%
    dplyr::mutate(date_open = as.Date(create_date),
                  date_close = dplyr::case_when(as.Date(close_date) <= rpt_edate ~  as.Date(close_date)),
                  open_month = lubridate::floor_date(date_open, 'month'),
                  close_month = lubridate::floor_date(date_close, 'month'),
                  status = factor(ifelse(is.na(date_close), "Unresolved", "Resolved"), levels = c("Unresolved", "Resolved")),
                  date_required_completion = dplyr::case_when(is.na(required_completion_date) ~ date_open + lubridate::days(30),
                                                              TRUE~ required_completion_date),
                  bizdays_toclose = ifelse(is.na(date_close), NA, round(as.numeric(bizdays::bizdays(date_open, date_close, cal = business_calendar)), digits = 1)),
                  late = ifelse(date_close > date_required_completion, 1, 0),
                  bizdays_late = ifelse(late == 1, round(as.numeric(bizdays::bizdays(date_required_completion, date_close, cal = business_calendar, origin = "1970-01-01")), digits = 1), 0),
                  bizdays_response = round(as.numeric(bizdays::bizdays(date_open, first_response_date, cal = business_calendar)), digits = 1),
                  response_late = ifelse(bizdays_response > 5, 1, 0),
                  response_bizdayslate = ifelse(bizdays_response > 5, bizdays_response-5, 0),
                  no_response = ifelse(is.na(first_response_date), 1, 0)) %>%
    dplyr::filter(open_month <= rpt_sdate) %>%
    dplyr::select(reference_no, date_open, open_month, date_close, close_month, department_requesting_information_from, status, request_status, source, information_type, date_required_completion, bizdays_toclose, late, bizdays_late, bizdays_response, response_late, response_bizdayslate, no_response)

  current_month_open <- clean_data %>%
    dplyr::filter(open_month == rpt_sdate)

  month_received <- dplyr::n_distinct(current_month_open$reference_no, na.rm = TRUE)
  month_resolved <- dplyr::n_distinct(subset(current_month_open, !is.na(current_month_open$date_close))$reference_no, na.rm = TRUE)
  month_responselate <- sum(current_month_open$response_late, na.rm = TRUE)
  month_responsedayslate <- round(mean(current_month_open$response_bizdayslate, na.rm = TRUE), 1)
  month_noresponse <- sum(current_month_open$no_response, na.rm = TRUE)

  current_month_close <- clean_data %>%
    dplyr::filter(close_month == rpt_sdate)

  month_closed <- dplyr::n_distinct(current_month_close$reference_no, na.rm = TRUE)
  month_avgdaystoclose <- round(mean(current_month_close$bizdays_toclose, na.rm = TRUE), 1)
  month_maxdaystoclose <- max(current_month_close$bizdays_toclose)
  month_mindaystoclose <- min(current_month_close$bizdays_toclose)
  month_late <- sum(current_month_close$late, na.rm = TRUE)
  month_avgbizdayslate <- round(mean(current_month_close$bizdays_late, na.rm = TRUE), 1)

  ytd_open <- clean_data %>%
    dplyr::filter(lubridate::year(open_month) == lubridate::year(rpt_sdate))

  ytd_received <- dplyr::n_distinct(ytd_open$reference_no, na.rm = TRUE)
  ytd_monavg_received <- round(mean(ytd_open %>% dplyr::group_by(open_month) %>% summarise(count = dplyr::n_distinct(reference_no)) %>% dplyr::pull(count), na.rm = TRUE), 0)

  ytd_close <- clean_data %>%
    dplyr::filter(lubridate::year(close_month) == lubridate::year(rpt_sdate))

  ytd_closed <- dplyr::n_distinct(ytd_close$reference_no, na.rm = TRUE)
  ytd_avgdaystoclose <- round(mean(ytd_close$bizdays_toclose, na.rm = TRUE), 1)

  alltime_received <- dplyr::n_distinct(clean_data$reference_no, na.rm = TRUE)
  alltime_monavg_received <- round(mean(clean_data %>% dplyr::group_by(open_month) %>%  summarise(count = dplyr::n_distinct(reference_no)) %>% dplyr::pull(count), na.rm = TRUE), 0)
  alltime_avgdaystoclose <- round(mean(clean_data$bizdays_toclose, na.rm = TRUE), 1)


  return(list(month_received = month_received, month_resolved = month_resolved, month_closed = month_closed,
              month_responselate = month_responselate, month_responsedayslate = month_responsedayslate, month_noresponse = month_noresponse,
              month_avgdaystoclose = month_avgdaystoclose, month_maxdaystoclose = month_maxdaystoclose, month_mindaystoclose = month_mindaystoclose,
              month_late = month_late, month_avgbizdayslate = month_avgbizdayslate,
              ytd_received = ytd_received, ytd_monavg_received = ytd_monavg_received,
              ytd_closed = ytd_closed, ytd_avgdaystoclose = ytd_avgdaystoclose,
              alltime_received = alltime_received, alltime_monavg_received = alltime_monavg_received, alltime_avgdaystoclose = alltime_avgdaystoclose))
}

#' Function that uses RTK data, retrieved using function rtk_pull, to run rtk_summary which retrieves a list of values that are then used to create the text in the departmental RTK report. Can be used with either 'single' or 'multi' data, but 'multi' data will be aggregated.
#'
#' @param data data frame of RTK data
#' @param rpt_sdate a date value that represents the first day of the reporting month
#' @param rpt_edate a date value that represents the last day of the reporting month
#' @param dept_text a text value that represents how the department or departments will be referred to within the text blocks of the report
#'
#' @return A named list of text strings for the departmental KPI report
#' @export
#'
#' @examples
#' \dontrun{
#' as_text <- rtk_text(data_as, start_last_month, end_last_month, "Administrative Services and its related divisions")
#' ems_text <- rtk_text(data_ems, start_last_month, end_last_month, "Emergency Services")
#' }

rtk_text <- function(data, rpt_sdate, rpt_edate, dept_text) {
  my <- format(rpt_sdate, "%B %Y")
  summary <- rtk_summary(data, rpt_sdate, rpt_edate)

  received_pct_chg <- round((summary$month_received - summary$alltime_monavg_received)/summary$alltime_monavg_received * 100, 1)

  received_up_down <- dplyr::case_when(received_pct_chg > 0 ~ "\u2191",
                                  received_pct_chg < 0 ~ "\u2193",
                                  TRUE ~ "—")

  received_inc_dec <- dplyr::case_when(received_pct_chg > 0 ~ "more than",
                                  received_pct_chg < 0 ~ "less than",
                                  TRUE ~ "")

  received_abs_chg <- dplyr::case_when(received_inc_dec == "" ~ "Equal to",
                                  TRUE ~ paste0(abs(received_pct_chg), "%"))

  received_header <- stringr::str_c("## ", my, ": ", summary$month_received, " RTK requests received\n", "** ", received_up_down, received_abs_chg, " ", received_inc_dec, " the all-time monthly average **")

  received_text <- stringr::str_c(dept_text, " received ", summary$month_received, " right-to-know requests in ", my, ". This compares to an all-time monthly average of ", summary$alltime_monavg_received, " and a year-to-date monthly average of ", summary$ytd_monavg_received, " requests received. Of the RTK requests submitted this month, ", summary$month_resolved, " have since been resolved.")

  close_pct_chg <- round((summary$month_avgdaystoclose - summary$alltime_avgdaystoclose)/summary$alltime_avgdaystoclose * 100, 1)

  close_up_down <- dplyr::case_when(close_pct_chg > 0 ~ "\u2191",
                                       close_pct_chg < 0 ~ "\u2193",
                                       TRUE ~ "—")

  close_inc_dec <- dplyr::case_when(close_pct_chg > 0 ~ "more than",
                                       close_pct_chg < 0 ~ "less than",
                                       TRUE ~ "")

  close_abs_chg <- dplyr::case_when(close_inc_dec == "" ~ "Equal to",
                                    TRUE ~ paste0(abs(close_pct_chg), "%"))

  close_day <- dplyr::case_when(summary$month_mindaystoclose < 1 ~ "less than a day.",
                                TRUE ~ paste(summary$month_mindaystoclose, "days."))

  close_header <- stringr::str_c("## ", my, ": ", summary$month_avgdaystoclose, " business days to close\n", "** ", close_up_down, close_abs_chg, " ", close_inc_dec, " the business days to close all-time average **")

  close_text <- stringr::str_c("The ", summary$month_closed, " requests closed in ", my, " by ", dept_text, " were closed in ", summary$month_avgdaystoclose, " days on average. This compares to an all-time average of ", summary$alltime_avgdaystoclose, " business days to close and a year-to-date average of ", summary$ytd_avgdaystoclose, " business days to close. The requests closed in ", my, " were resolved in a maximum of ", summary$month_maxdaystoclose, " days and a minimum of ", close_day)

  late_header <- stringr::str_c("## ", my, ": ", summary$month_late, " requests closed after required completion date")

  late_text <- stringr::str_c("The required completion date is determined by the Open Records Officer to be the reasonable date that a response to the requester is expected to be provided, typically 30 days or less from the request submission date. Right-to-know requests closed in ", my, " that closed after their required completion date were late by an average of ", summary$month_avgbizdayslate, " business days.")

  return(list(received_header = received_header, received_text = received_text,
              close_header = close_header, close_text = close_text,
              late_header = late_header, late_text = late_text))

}

#test
test <- rtk_data(rtk_admin, start_last_month, end_last_month)

clean_data <- rtk_admin %>%
  dplyr::mutate(information_type =  dplyr::coalesce(!!!dplyr::select(., 7:(ncol(rtk_admin)-2)))) %>%
  dplyr::mutate(date_open = as.Date(create_date),
                date_close = dplyr::case_when(as.Date(close_date) <= end_last_month ~ as.Date(close_date)),
                open_month = lubridate::floor_date(date_open, 'month'),
                close_month = lubridate::floor_date(date_close, 'month'),
                status = factor(ifelse(is.na(date_close), "Unresolved", "Resolved"), levels = c("Unresolved", "Resolved")),
                date_required_completion = dplyr::case_when(is.na(required_completion_date) ~ as.Date(date_open + lubridate::days(30)),
                                                            TRUE ~ as.Date(required_completion_date)),
                bizdays_toclose = ifelse(is.na(date_close), NA, round(as.numeric(bizdays::bizdays(date_open, date_close, cal = business_calendar)), digits = 1)),
                late = ifelse(date_close > date_required_completion, 1, 0),
                bizdays_late = ifelse(late == 1, round(as.numeric(bizdays::bizdays(date_required_completion, date_close, cal = business_calendar)), digits = 1), 0),
                bizdays_response = round(as.numeric(bizdays::bizdays(date_open, first_response_date, cal = business_calendar)), digits = 1),
                response_late = ifelse(bizdays_response > 5, 1, 0),
                response_bizdayslate = ifelse(bizdays_response > 5, bizdays_response-5, 0),
                no_response = ifelse(is.na(first_response_date), 1, 0)) %>%
  dplyr::filter(open_month <= start_last_month) %>%
  dplyr::select(reference_no, date_open, open_month, date_close, close_month, department_requesting_information_from, status, request_status, source, information_type, date_required_completion, bizdays_toclose, late, bizdays_late, bizdays_response, response_late, response_bizdayslate, no_response)
