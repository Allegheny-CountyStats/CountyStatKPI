#' Function to create all dates necessary for all HR functions. Use this as the dates object in all functions that call for it
#' @return A list of four dates -- reporting month floor date, reporting month ceiling date, reporting period floor date (start of previous year) and reporting period end date
#' @export
#'
#' @examples
#' dates <- HR_dates()


HR_dates <- function(){
  reporting_emonth <- lubridate::rollback(floor_date(today, unit = "month"))
  reporting_smonth <- lubridate::floor_date(reporting_emonth, unit = "month")
  HRtimeseries_floordate <- lubridate::floor_date(reporting_smonth, unit = "year") - lubridate::years(1)
  HRtimeseries_ceilingdate <- reporting_emonth

  return(list(reporting_smonth = reporting_smonth, reporting_emonth = reporting_emonth,
              HRtimeseries_floordate = HRtimeseries_floordate, HRtimeseries_ceilingdate = HRtimeseries_ceilingdate))
}

#' Function to pull all HR-related data. Before running this function, you must have a warehouse connection and the name of the department as it is listed in JDE, OnBase, and Novatime (if pulling for a specific department)
#'
#' @param wh_con Data warehouse connection
#' @param jde_dept Name of department as it is listed in JDE. Defaults to 'All'.
#' @param onbase_dept Name of department as it is listed in OnBase. Defaults to 'All'.
#' @param novatime_dept Name of department as it is listed in Novatime. Defaults to 'All'.
#' @param rpt_enddate reporting_emonth in HR_dates() function
#' @param include_seasonals TRUE or FALSE binary to include seasonal and temporary employees. Defaults to FALSE.
#'
#' @return A list of dataframes containing all HR-related data
#' @export
#'
#' @examples
#' dept_HRdata <- HR_pulldata(wh_con, jde_dept, onbase_dept, novatime_dept, rpt_enddate = dates$reporting_emonth, include_seasonals = FALSE)
#' all_HRdata <- HR_pulldata(wh_con, rpt_enddate = dates$reporting_emonth, include_seasonals = FALSE)

HR_pulldata <- function(wh_con, jde_dept = "All", onbase_dept = "All", novatime_dept = "All", rpt_enddate, include_seasonals = FALSE) {
  # Pull applications with an entry date  less than the reporting end date
  applications <- DBI::dbGetQuery(wh_con, SQL(paste0("SELECT DISTINCT
    AppID,
    IsPaper,
    EntryDate,
    [Position],
    Dept,
    Div,
    ScreenDate,
    CurrentEligStatus,
    COVID,
    Residency,
    [Source],
    SourceOther,
    HireStatus
  FROM Reporting.HumanResources_Job_Applications_V
  WHERE EntryDate <= '", rpt_enddate, "'")))
  # Pull current employees (who meet data requirements) who have a null termination dates or who terminated after the report end date and who started prior to the report end date
  current_employees <- DBI::dbGetQuery(wh_con, SQL(paste0("SELECT DISTINCT *
                                    FROM Reporting.HumanResources_JDE_CurrentEmployeeDetails_C
                                    WHERE (Date_Terminated IS NULL OR Date_Terminated > '", rpt_enddate, "')
                                    AND union_name IS NOT NULL
                                    AND Job_Title <> 'One Time Only'
                                    AND TRIM(Benefit_Group) <> ''
                                    AND Date_Started <= '", rpt_enddate, "'")))
  # Pull employee history where position start date is prior to the report end date
  employee_history <- DBI::dbGetQuery(wh_con, SQL(paste0("SELECT *
                                    FROM Reporting.HumanResources_JDE_EmployeePositionHistoryShort_V
                                    WHERE Position_Start_Date <= '", rpt_enddate, "'
                                    AND Employee_Status != '6'
                                    AND Job_Title NOT LIKE '%ONE TIME ONLY%'")))
  # Pull time reported where date worked is prior to the report end date
  timesheet <- DBI::dbGetQuery(wh_con, SQL(paste0("SELECT *
                                    FROM Reporting.HumanResources_JDE_EmployeeWorkDays_V
                                    WHERE Date_Worked <= '", rpt_enddate, "'")))
  # Pull employee scheduled, approved vacation time
  schedule <- DBI::dbGetQuery(wh_con, SQL(paste0("SELECT
    EmployeeID,
    Hours,
    WorkDate,
    PayPeriod,
    RequestStatus,
    EmployeeName,
    Dept,
    Title,
    PayCodeTxt
  FROM Reporting.HumanResources_Novatime_Schedule_V
  WHERE RequestStatus = 'Approved' AND PayCodeTxt = 'VACA'")))
  # Pull pay periods
  payperiods <- DBI::dbGetQuery(wh_con, SQL("SELECT *
                                       FROM Master.HumanResources_Direct_CountyPayPeriods"))
  # Pull vacancies with job title
  positionvacancies <- DBI::dbGetQuery(wh_con, SQL(paste0("SELECT *
                                            FROM Reporting.Budget_PowerPlanJDE_EmployeesVacancies_V
                                            WHERE Status = 'Vacant'
                                            AND Job_Title IS NOT NULL")))
  # Pull vacancy history
  vacancyhistory <- DBI::dbGetQuery(wh_con, SQL(paste0("SELECT Date_Stamp,
    Department,
    Status,
    Positions
      FROM Reporting.Budget_PowerPlanJDE_VacancyHistory_V
      WHERE Status = 'Vacant'
      AND Date_Stamp <= '", rpt_enddate, "'")))
  # If function designates a department, filter to department, otherwise keep all executive departments (be sure to trim white space as precaution)
  #Create list of all iterations of department names
  nonexec_depts <- c('ALL COUNTY DEPARTMENTS', 'CONTROLLERS OFFICE', 'Controller', 'COUNTY CONCIL', 'County Council', 'Miscellaneous Agencies', 'Court of Common Pleas', 'DCS Test - Department', 'DISTRICT ATTORNEYS OFFICE', 'District Attorney', 'Juvenile Court Placement', 'LAW', 'M/W/DBE', 'No Department', 'Non-Dept Expenditures', 'PENN STATE COOPERATIVE EXTENSION', 'RETIREMENT', 'Retirement System', "SHERIFF'S OFFICE", 'SHERIFFS OFFICE', 'Sheriff', 'SHUMAN', 'Shuman Center', 'TRAINING - Department', 'TREASURERS OFFICE', 'Treasurer')
  if (jde_dept != "All" & onbase_dept != "All" & novatime_dept != "All") {
    applications <- applications %>%
      dplyr::filter(trimws(Dept) %like% onbase_dept)
    current_employees <- current_employees %>%
      dplyr::filter(trimws(Department) == jde_dept)
    employee_history <- employee_history %>%
      dplyr::filter(trimws(Department) == jde_dept | trimws(Old_Department) == jde_dept)
    timesheet <- timesheet %>%
      dplyr::filter(trimws(Department) == jde_dept)
    schedule <- schedule %>%
      dplyr::filter(trimws(Dept) == novatime_dept)
    positionvacancies <- positionvacancies %>%
      dplyr::filter(trimws(Department) %like% jde_dept)
    vacancyhistory <- vacancyhistory %>%
      dplyr::filter(trimws(Department) %like% jde_dept)
  } else {
    applications <- applications %>%
      dplyr::filter(!trimws(Dept) %in% nonexec_depts)
    current_employees <- current_employees %>%
      dplyr::filter(!trimws(Department) %in% nonexec_depts)
    # Do not filter employee history
    timesheet <- timesheet %>%
      dplyr::filter(!trimws(Department) %in% nonexec_depts)
    schedule <- schedule %>%
      dplyr::filter(!trimws(Dept) %in% nonexec_depts)
    # Vacancies are already filtered to executive departments
  }
  # By default, most reports will not include seasonals, with a few exceptions
  if (include_seasonals == FALSE) {
    # Applications do not include seasonal positions
    current_employees <- current_employees %>%
      dplyr::filter(!union_name %like% "%Temporary/Seasonal%",
                    !Job_Title %like any% c("%SEASONAL%", "%TEMP%"))
    employee_history <- employee_history %>%
      dplyr::filter(Employee_Status != "2",
                    !Job_Title %like any% c("%SEASONAL%", "%TEMP%"))
    timesheet <- timesheet %>%
      dplyr::filter(!Union_Code %like% "%Temporary/Seasonal%",
                    !Job_Title %like any% c("%SEASONAL%", "%TEMP%"))
    # Create list of IDs to pull schedules because there is no union information
    ids <- timesheet$JDE_ID
    schedule <- schedule %>%
      dplyr::filter(EmployeeID %in% ids)
    # Vacancies do not include seasonal positions
    # Return tables
    return(list(applications = applications,
                current_employees = current_employees,
                employee_history = employee_history,
                timesheet = timesheet,
                schedule = schedule,
                payperiods = payperiods,
                positionvacancies = positionvacancies,
                vacancyhistory = vacancyhistory,
                nonexec_depts = nonexec_depts))
  } else {
    # If seasons are included, pull table of seasonal rehires
    seasonal_rehires <- employee_history %>%
      dplyr::filter(Employee_Status == '2',
                    EPeriod_Start != Date_Started_Original)
    # Return tables (additional seasonal rehires table included)
    return(list(applications = applications,
                current_employees = current_employees,
                employee_history = employee_history,
                timesheet = timesheet,
                schedule = schedule,
                payperiods = payperiods,
                positionvacancies = positionvacancies,
                vacancyhistory = vacancyhistory,
                seasonal_rehires = seasonal_rehires,
                nonexec_depts = nonexec_depts))
  }
}

#' Function to pull categorize employees by status
#'
#' @param data The current_employees data frame from the HR_pulldata() function
#' @param include_seasonals TRUE or FALSE binary to include seasonal and temporary employees. Defaults to FALSE.
#'
#' @return A dataframe with employee counts by category that can be used in the current_employees_text() and current_employees_table() functions
#' @export
#'
#' @examples
#' dept_employees_cat <- current_employees(dept_HRdata$current_employees, include_seasonals = FALSE)

current_employees <- function(data, include_seasonals = FALSE) {
  if (include_seasonals == FALSE) {
    # Categorize employees by pt/ft and union/non-union
    employee_categories <- data %>%
      dplyr::mutate(pt_ft = dplyr::case_when(Benefit_Group %like% '%PT%' ~ 'Part-Time',
                                             TRUE ~ 'Full-Time')) %>%
      dplyr::group_by(pt_ft) %>%
      dplyr::summarise(Union = n_distinct(JDE_ID[union_status == "Union"]),
                       `Non-Union` = n_distinct(JDE_ID[union_status == "Non-Union" & !union_name %like% "%Temporary/Seasonal%"])) %>%
      dplyr::ungroup() %>%
      janitor::adorn_totals(c("col", "row"))
  } else {
    # Categorize employees by pt/ft and union/non-union/seasonal
    employee_categories <- data %>%
      dplyr::mutate(pt_ft = dplyr::case_when(Benefit_Group %like% '%PT%' ~ 'Part-Time',
                                             TRUE ~ 'Full-Time')) %>%
      dplyr::group_by(pt_ft) %>%
      dplyr::summarise(Union = n_distinct(JDE_ID[union_status == "Union"]),
                       `Non-Union` = n_distinct(JDE_ID[union_status == "Non-Union" & !union_name %like% "%Temporary/Seasonal%"]),
                       Seasonal = n_distinct(JDE_ID[union_name %like% "%Temporary/Seasonal%" | Job_Title %like any% c("%SEASONAL%", "%TEMP%")])) %>%
      dplyr::ungroup() %>%
      janitor::adorn_totals(c("col", "row"))
  }
  return(employee_categories)
}

#' Function to make table of current employee categories
#'
#' @param data The data frame resulting from current_employees() function
#' @param include_seasonals TRUE or FALSE binary to include seasonal and temporary employees. Defaults to FALSE.
#'
#' @return A kable table with employee counts by category
#' @export
#'
#' @examples
#' print(current_employees_table(dept_employees_cat, include_seasonals = FALSE))

current_employees_table <- function(data, include_seasonals = FALSE) {
  if (include_seasonals == FALSE) {
    #Make table without seasonals
    current_employees_table <- data %>%
      kableExtra::kable(caption = "Current Employees by Status", col.names = c("Status", "Union", "Non-Union", "Total"), align = "lrrr", format.args = list(big.mark = ",")) %>%
      kableExtra::kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
      kableExtra::row_spec(0, bold = T) %>%
      kableExtra::column_spec(1, bold = T)
  } else {
    # Make table with seasonals
    current_employees_table <- data %>%
      kableExtra::kable(caption = "Current Employees by Status", col.names = c("Status", "Union", "Non-Union", "Seasonal", "Total"), align = "lrrrr", format.args = list(big.mark = ",")) %>%
      kableExtra::kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
      kableExtra::row_spec(0, bold = T) %>%
      kableExtra::column_spec(1, bold = T)
  }
}

#' Function to create header and explanatory text for current employees section of HR report
#'
#' @param data The data frame resulting from current_employees() function
#' @param dept_name Name of department(s) as it should appear in text
#' @param rpt_enddate Last day in reporting month (produced by HR_dates() function)
#' @param include_seasonals TRUE or FALSE binary to include seasonal and temporary employees. Defaults to FALSE.
#'
#' @return A character value
#' @export
#'
#' @examples
#' dept_employees_txt <- current_employees_text(dept_employees_cat, "Health Department", dates$reporting_emonth, include_seasonals = FALSE)

current_employees_text <- function(data, dept_name, rpt_enddate, include_seasonals = FALSE) {
  pct_ft <- round(ifelse(is_empty(data$Total[data$pt_ft == "Full-Time"]),
                         0, data$Total[data$pt_ft == "Full-Time"])/data$Total[data$pt_ft == "Total"]*100, 1)
  pct_union <- round(ifelse(is_empty(data$Union[data$pt_ft == "Total"]),
                            0, data$Union[data$pt_ft == "Total"])/data$Total[data$pt_ft == "Total"]*100, 1)
  seasonal_count <- ifelse(is_empty(data$Seasonal[data$pt_ft == "Total"]), 0, data$Seasonal[data$pt_ft == "Total"])

  if (include_seasonals == FALSE) {
    current_employees_text <- paste0("## ", format(data$Total[data$pt_ft == "Total"], big.mark = ','), " current employees\nAs of ", format(rpt_enddate, "%B %d, %Y"), ", ", data$Total[data$pt_ft == "Total"], " people work for Allegheny County's ", dept_name, ", ", pct_ft, "% of which are full-time employees and ", pct_union, "% of which are union employees. This report excludes seasonal and temporary employees, for which there are currently ", seasonal_count, " employed by the ", dept_name, ".")
  } else {
    current_employees_text <- paste0("## ", format(data$Total[data$pt_ft == "Total"], big.mark = ','), " current employees\nAs of ", format(rpt_enddate, "%B %d, %Y"), ", ", data$Total[data$pt_ft == "Total"], " people work for Allegheny County's ", dept_name, ", ", pct_ft, "% of which are full-time employees and ", pct_union, "% of which are union employees. This report *includes* seasonal and temporary employees (except where noted) because of their major contribution to ", dept_name, " operations.")
  }
}

#' Function to create data frames for hiring
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#' @param jde_dept Name of the department in JDE (recommended to set as variable). Supports 'All' departments.
#' @param dates_obj The list of dates resulting from HR_dates() function
#' @param seasonal_rehires TRUE or FALSE binary to produce additional data frame with seasonal rehires time series. Defaults to FALSE.
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dept_hires <- hires(dept_HRdata, jde_dept, dates, seasonal_rehires = FALSE)

hires <- function(data, jde_dept, dates_obj, seasonal_rehires = FALSE) {
  if (jde_dept == "All"){
    # Keep all executive departments
    hires <- data$employee_history %>%
      filter(!Department %in% data$nonexec_depts)
  } else {
    # Fitler to current department equals jde_dept
    hires <- data$employee_history %>%
      dplyr::filter(Department == jde_dept)
  }
  if (seasonal_rehires == FALSE) {
    # Keep records only where employment period start date is the same as position start date (starting position); calculate new and retained employees
    hires <- hires %>%
      dplyr::filter(EPeriod_Start == Position_Start_Date) %>%
      dplyr::mutate(new = ifelse(Date_Started_Original == EPeriod_Start, 1, 0),
                    retained = ifelse(is.na(Date_Terminated), 1, 0))
  } else {
    # Keep records only where employment period start date is the same as position start date (starting position), do not keep seasonal hires but if they are seasonal employees hired for the first time (date start original) they are included; calculate new and retained
    hires <- hires %>%
      dplyr::mutate(seasonal_firsthire = ifelse(Date_Started_Original == EPeriod_Start & Employee_Status == "2", 1, 0)) %>%
      dplyr::filter(EPeriod_Start == Position_Start_Date,
                    !((Employee_Status == "2" | Job_Title %like any% c("%SEASONAL%", "%TEMP%")) & seasonal_firsthire == 0)) %>%
      dplyr::mutate(new = ifelse(Date_Started_Original == EPeriod_Start, 1, 0),
                    retained = ifelse(is.na(Date_Terminated), 1, 0))
    # Seasonal rehires time series
    seasonal_rehires_time <- data$seasonal_rehires %>%
      dplyr::mutate(hire_month = lubridate::floor_date(EPeriod_Start, unit = "month"),
                    id_start = paste(JDE_ID, EPeriod_Start)) %>%
      dplyr::filter(hire_month >= dates_obj$HRtimeseries_floordate,
                    hire_month <= dates_obj$HRtimeseries_ceilingdate) %>%
      dplyr::group_by(hire_month) %>%
      dplyr::summarise(rehires = dplyr::n_distinct(id_start)) %>%
      tidyr::complete(hire_month = seq.Date(dates_obj$HRtimeseries_floordate, dates_obj$HRtimeseries_ceilingdate, by = "month"), fill = list(rehires = 0)) %>%
      dplyr::mutate(hire_year = lubridate::year(hire_month),
                    hire_monthname = factor(month.abb[month(hire_month)], levels = month.abb))
  }
  # Hires time series
  hires_time <- hires %>%
    dplyr::mutate(hire_month = lubridate::floor_date(EPeriod_Start, unit = "month"),
                  id_start = paste(JDE_ID, EPeriod_Start)) %>%
    dplyr::filter(hire_month >= dates_obj$HRtimeseries_floordate,
                  hire_month <= dates_obj$HRtimeseries_ceilingdate) %>%
    dplyr::group_by(hire_month) %>%
    dplyr::summarise(hires = dplyr::n_distinct(id_start),
                     new =  sum(new, na.rm = TRUE),
                     retained = sum(retained, na.rm = TRUE)) %>%
    tidyr::complete(hire_month = seq.Date(dates_obj$HRtimeseries_floordate, dates_obj$HRtimeseries_ceilingdate, by = "month"), fill = list(hires = 0, new = 0, retained = 0)) %>%
    dplyr::mutate(hire_year = lubridate::year(hire_month),
                  hire_monthname = factor(month.abb[month(hire_month)], levels = month.abb))
  # Compare month to previous month
  hires_month <- CountyStatKPI::kpi_compare(hires_time, hire_month, rpt_date = dates_obj$reporting_smonth, method = "sum", val_col = hires, mode = "my")
  # Compare year to date to previous
  hires_year <- CountyStatKPI::kpi_compare(hires_time, hire_month, rpt_date = dates_obj$reporting_smonth, method = "sum", val_col = hires, mode = "ytd")
  # Calculate monthly average hires for current year
  avg_monthly_hires <- round(mean(hires_time$hires[lubridate::year(hires_time$hire_month) == lubridate::year(dates_obj$reporting_smonth)], na.rm = TRUE), 0)
  # Add to ytd compare object
  hires_year$avg_monthly_hires <- avg_monthly_hires
  if (seasonal_rehires == FALSE) {
    return(list(hires = hires,
                hires_time = hires_time,
                hires_month = hires_month,
                hires_year = hires_year))
  } else {
    return(list(hires = hires,
                hires_time = hires_time,
                hires_month = hires_month,
                hires_year = hires_year,
                seasonal_rehires_time = seasonal_rehires_time))
  }
}

#' Function to generate reporting month and year to date text for hiring section of report
#'
#' @param data The list of data frames resulting from the hires() function
#' @param rpt_date Date in reporting month
#' @param seasonal TRUE or FALSE binary to compare hiring numbers to previous month (FALSE) or same month of the previous year (TRUE). Defaults to FALSE.
#' @param seasonal_rehires TRUE or FALSE binary to add rehires numbers to header. Defaults to FALSE.
#'
#' @return A list of character values
#' @export
#'
#' @examples
#' dept_hire_text <- hires_text(dept_hires, dates$reporting_smonth, seasonal = FALSE, seasonal_rehires = FALSE)

hires_text <- function(data, rpt_date, seasonal = FALSE, seasonal_rehires = FALSE){
  # Calculate the number of new hires in reporting month
  new_month <- data$hires_time$new[data$hires_time$hire_month == rpt_date]
  # Calculate the percent retained in reporting year
  ytd_pct_retained <-  round(sum(data$hires_time$retained[data$hires_time$hire_year == lubridate::year(rpt_date)], na.rm = TRUE)/
                               sum(data$hires_time$hires[data$hires_time$hire_year == lubridate::year(rpt_date)], na.rm = TRUE)*100, 1)
  if (seasonal == FALSE) {
    # Compare to prior month
    hires_month_header <- CountyStatKPI::kpi_header_text(data$hires_month, rpt_date, prior_val = "month", metric_txt = "hire(s)", prefix = NA)
    hires_month_text <- paste0("In ", format(rpt_date, "%B %Y"), ", ", data$hires_month$rm_cnt, " employee(s) were hired, compared to ", data$hires_month$pm_cnt, " employee(s) in the previous month. Of the employees hired in ", format(rpt_date, "%B %Y"), ", ", new_month, " were new hires.")
    hires_year_header <- CountyStatKPI::kpi_header_text(data$hires_year, rpt_date, prior_val = "ytd", metric_txt = "hire(s)", prefix = NA)
    hires_year_text <- paste0("Year to date, ", data$hires_year$rytd_cnt, " employee(s) have been hired, compared to ", data$hires_year$pytd_cnt, " employee(s) in the prior year to date. On average, ", data$hires_year$avg_monthly_hires, " employee(s) have been hired monthly year to date. Of the employees hired during the reporting year, ", ytd_pct_retained, "% retained their employment with Allegheny County.")
  } else {
    # Compare to same month in prior year
    hires_month_header <- CountyStatKPI::kpi_header_text(data$hires_month, rpt_date, prior_val = "year", metric_txt = "hire(s)", prefix = NA)
    hires_month_text <- paste0("In ", format(rpt_date, "%B %Y"), ", ", data$hires_month$rm_cnt, " employee(s) were hired, compared to ", data$hires_month$pm_cnt, " employee(s) in the same month of the previous year. Of the employees hired in ", format(rpt_date, "%B %Y"), ", ", new_month, " were new hires.")
    hires_year_header <- CountyStatKPI::kpi_header_text(data$hires_year, rpt_date, prior_val = "ytd", metric_txt = "hire(s)", prefix = NA)
    hires_year_text <- paste0("Year to date, ", data$hires_year$rytd_cnt, " employee(s) have been hired, compared to ", data$hires_year$pytd_cnt, " employee(s) in the prior year to date. On average, ", data$hires_year$avg_monthly_hires, " employee(s) have been hired monthly year to date. Of the employees hired during the reporting year, ", ytd_pct_retained, "% retained their employment with Allegheny County.")
  }
  if (seasonal_rehires == TRUE) {
    # Add rehires text
    rehires_month <- paste(" and", data$seasonal_rehires_time$rehires[data$seasonal_rehires_time$hire_month == rpt_date], "seasonal rehire(s)\n")
    rehires_year <- paste(" and", sum(data$seasonal_rehires_time$rehires[data$seasonal_rehires_time$hire_year == lubridate::year(rpt_date)], na.rm = TRUE), "seasonal rehire(s)\n")
    # Compare to same month in previous year
    month_header_pre <- CountyStatKPI::kpi_header_text(data$hires_month, rpt_date, prior_val = "year", metric_txt = "hire(s)", prefix = NA)
    hires_month_header <- gsub("[\r\n]", rehires_month, month_header_pre)
    hires_month_text <- paste0("In ", format(rpt_date, "%B %Y"), ", ", data$hires_month$rm_cnt, " employee(s) were hired, compared to ", data$hires_month$py_cnt, " employee(s) in the same month of the previous year. Of the employees hired in ", format(rpt_date, "%B %Y"), ", ", new_month, " were new hires.")
    year_header_pre <- CountyStatKPI::kpi_header_text(data$hires_year, rpt_date, prior_val = "ytd", metric_txt = "hire(s)", prefix = NA)
    hires_year_header <- gsub("[\r\n]", rehires_year, year_header_pre)
    hires_year_text <- paste0("Year to date, ", data$hires_year$rytd_cnt, " employee(s) have been hired, compared to ", data$hires_year$pytd_cnt, " employee(s) in the prior year to date. On average, ", data$hires_year$avg_monthly_hires, " employee(s) have been hired monthly year to date. Of the employees hired during the reporting year, ", ytd_pct_retained, "% retained their employment with Allegheny County.")
  }
  return(list(hires_month_header = hires_month_header,
              hires_month_text = hires_month_text,
              hires_year_header = hires_year_header,
              hires_year_text = hires_year_text))
}

#' Function to generate plot(s) of hiring time series
#'
#' @param data The list of data frames resulting from the hires() function
#' @param pal Palette of colors, usually resulting from kpiPal() function
#' @param seasonal_rehires TRUE or FALSE binary to produce additional ggplot of seasonal rehires time series. Defaults to FALSE.
#'
#' @return A single ggplot or list of ggplot objects
#' @export
#'
#' @examples
#' print(hires_plot(dept_hires, pal, seasonal_rehires = FALSE))

hires_plot <- function(data, pal, seasonal_rehires = FALSE) {

  if (seasonal_rehires == TRUE) {

    hire_plot <- data$hires_time %>%
      ggplot2::ggplot(aes(x= hire_monthname, y = hires)) +
      ggplot2::geom_line(aes(group= factor(hire_year), color = factor(hire_year)), size =1) +
      ggplot2::geom_point(aes(group = factor(hire_year), color = factor(hire_year)), size=3) +
      ggplot2::labs(title = "Monthly Hires",
                    y = "Employees",
                    caption = "Includes non-seasonal rehires and first-time seasonal hires.") +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA)) +
      ggplot2::scale_color_manual(name = "Year", values =  pal) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.title.x = element_blank())
    rehires_plot <- data$seasonal_rehires_time %>%
      ggplot2::ggplot(aes(x= hire_monthname, y = rehires)) +
      ggplot2::geom_line(aes(group= factor(hire_year), color = factor(hire_year)), size =1) +
      ggplot2::geom_point(aes(group = factor(hire_year), color = factor(hire_year)), size=3) +
      ggplot2::labs(title = "Monthly Seasonal Rehires",
                    y = "Employees",
                    caption = "Seasonal rehires only.") +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA)) +
      ggplot2::scale_color_manual(name = "Year", values =  pal) +
      ggplot2::theme_minimal()+
      ggplot2::theme(axis.title.x = element_blank())
    return(list(hire_plot = hire_plot,
                rehire_plot = rehires_plot))
  } else {
    hire_plot <- data$hires_time %>%
      ggplot2::ggplot(aes(x= hire_monthname, y = hires)) +
      ggplot2::geom_line(aes(group= factor(hire_year), color = factor(hire_year)), size =1) +
      ggplot2::geom_point(aes(group = factor(hire_year), color = factor(hire_year)), size=3) +
      ggplot2::labs(title = "Monthly Hires",
                    y = "Employees",
                    caption = "Includes rehires.") +
      ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA), labels = scales::comma) +
      ggplot2::scale_color_manual(name = "Year", values =  pal) +
      ggplot2::theme_minimal()+
      ggplot2::theme(axis.title.x = element_blank())
    return(hire_plot)
  }
}

#' Function to create data frames for terminations
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#' @param jde_dept Name of the department in JDE (recommended to set as variable). Supports 'All' departments.
#' @param dates_obj The list of dates resulting from HR_dates() function
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dept_terminations <- terminations(dept_HRdata, jde_dept, dates)

terminations <- function(data, jde_dept, dates_obj) {
  if (jde_dept == "All"){
    # Keep all executive departments
    terminations <- data$employee_history %>%
      filter(!Department %in% data$nonexec_depts)
  } else {
    # Filter to current department equals jde_dept
    terminations <- data$employee_history %>%
      dplyr::filter(Department == jde_dept)
  }
  # Keep records only if employment period end date is the same as position end date (final position); calculate tenure in years and if tenure is less than one year (binary)
  terminations <- terminations %>%
    dplyr::filter(EPeriod_End == Position_End_Date) %>%
    dplyr::mutate(Tenure = round(as.numeric(difftime(EPeriod_End, EPeriod_Start, units = "days"))/365.25, 1),
                  Year_Tenure = ifelse(Tenure < 1, 1, 0))
  # Termination time series
  terminations_time <- terminations %>%
    dplyr::mutate(term_month = lubridate::floor_date(EPeriod_End, unit = "month"),
                  id_start = paste(JDE_ID, EPeriod_Start)) %>%
    dplyr::filter(term_month >= dates_obj$HRtimeseries_floordate,
                  term_month <= dates_obj$HRtimeseries_ceilingdate) %>%
    dplyr::group_by(term_month) %>%
    dplyr::summarise(terminations = dplyr::n_distinct(id_start),
                     year_tenure = sum(Year_Tenure, na.rm = TRUE)) %>%
    tidyr::complete(term_month = seq.Date(dates_obj$HRtimeseries_floordate, dates_obj$HRtimeseries_ceilingdate, by = "month"), fill = list(terminations = 0, year_tenure = 0)) %>%
    dplyr::mutate(term_year = lubridate::year(term_month),
                  term_monthname = factor(month.abb[month(term_month)], levels = month.abb))
  # Term reason count in reporting month
  termreason_month <- terminations %>%
    dplyr::mutate(term_month = lubridate::floor_date(EPeriod_End, unit = "month"),
                  id_start = paste(JDE_ID, EPeriod_Start)) %>%
    dplyr::filter(term_month == dates_obj$reporting_smonth) %>%
    dplyr::group_by(Term_Reason) %>%
    dplyr::summarise(Count = dplyr::n_distinct(id_start)) %>%
    arrange(desc(Count))
  # Term reason count year to date
  termreason_year <- terminations %>%
    dplyr::mutate(term_month = lubridate::floor_date(EPeriod_End, unit = "month"),
                  id_start = paste(JDE_ID, EPeriod_Start)) %>%
    dplyr::filter(lubridate::year(term_month) == lubridate::year(dates_obj$reporting_smonth)) %>%
    dplyr::group_by(Term_Reason) %>%
    dplyr::summarise(Count = dplyr::n_distinct(id_start)) %>%
    arrange(desc(Count))
  # Compare terminations to prior month
  terminations_month <- CountyStatKPI::kpi_compare(terminations_time, term_month, rpt_date = dates_obj$reporting_smonth, method = "sum", val_col = terminations, mode = "my")
  # Compare terminations to prior year to date
  terminations_year <- CountyStatKPI::kpi_compare(terminations_time, term_month, rpt_date = dates_obj$reporting_smonth, method = "sum", val_col = terminations, mode = "ytd")
  # Calculate monthly average terminations for current year
  avg_monthly_terminations <- round(mean(terminations_time$terminations[lubridate::year(terminations_time$term_month) == lubridate::year(dates_obj$reporting_smonth)], na.rm = TRUE), 0)
  # Add to ytd compare object
  terminations_year$avg_monthly_terminations <- avg_monthly_terminations
  return(list(terminations = terminations,
              terminations_time = terminations_time,
              termreason_month = termreason_month,
              termreason_year = termreason_year,
              terminations_month = terminations_month,
              terminations_year = terminations_year))
}

#' Function to generate reporting month and year to date text for termination section of report
#'
#' @param data The list of data frames resulting from the hires() function
#' @param rpt_date Date in reporting month
#' @param seasonal TRUE or FALSE binary to compare hiring numbers to previous month (FALSE) or same month of the previous year (TRUE). Defaults to FALSE.
#'
#' @return A list of character values
#' @export
#'
#' @examples
#' dept_termination_text <- terminations_text(dept_terminations, dates$reporting_smonth, seasonal = FALSE)

terminations_text <- function(data, rpt_date, seasonal = FALSE) {
  # Calculate top reason in current month
  top_termreason_month <- trimws(data$termreason_month$Term_Reason[1])
  # Calculate average tenure in reporting year
  avg_tenure_year <- round(mean(data$terminations$Tenure[year(data$terminations$EPeriod_End) == year(rpt_date)], na.rm = TRUE), 1)
  # Calculate percent left in less than a year in current year
  ytd_pct_tenureyear <- round(sum(data$terminations_time$year_tenure[year(data$terminations_time$term_month) == year(rpt_date)])/data$terminations_year$rytd_cnt*100,1)
  if (seasonal == FALSE) {
    # Compare to prior month
    terminations_month_header <- CountyStatKPI::kpi_header_text(data$terminations_month, rpt_date, prior_val = "month", metric_txt = "terminations(s)", prefix = NA)
    terminations_month_text <- paste0("In ", format(rpt_date, "%B %Y"), ", ", data$terminations_month$rm_cnt, " employee(s) were terminated, compared to ", data$terminations_month$pm_cnt, " employee(s) in the previous month. The most common termination reason in ", format(rpt_date, "%B %Y"), " was *", top_termreason_month, "*.")
  } else {
    #Compare to same month in previous year
    terminations_month_header <- CountyStatKPI::kpi_header_text(data$terminations_month, rpt_date, prior_val = "year", metric_txt = "terminations(s)", prefix = NA)
    terminations_month_text <- paste0("In ", format(rpt_date, "%B %Y"), ", ", data$terminations_month$rm_cnt, " employee(s) were terminated, compared to ", data$terminations_month$py_cnt, " employee(s) in the same month of the previous year. The most common termination reason in ", format(rpt_date, "%B %Y"), " was *", top_termreason_month, "*.")
  }
  terminations_year_header <- CountyStatKPI::kpi_header_text(data$terminations_year, rpt_date, prior_val = "ytd", metric_txt = "terminations(s)", prefix = NA)
  terminations_year_text <- paste0("Year to date, ", data$terminations_year$rytd_cnt, " employee(s) have been terminated, compared to ", data$terminations_year$pytd_cnt, " employee(s) in the prior year to date. On average, ", data$terminations_year$avg_monthly_terminations, " employee(s) have been terminated monthly year to date. Of the employees terminated during the reporting year, ", ytd_pct_tenureyear, "% worked for Allegheny County for less than a year. The average tenure of those terminated this year is ", avg_tenure_year, " year(s).")
  return(list(terminations_month_header = terminations_month_header,
              terminations_month_text = terminations_month_text,
              terminations_year_header = terminations_year_header,
              terminations_year_text = terminations_year_text))
}

#' Function to generate plot for termination time series
#'
#' @param data The list of data frames resulting from the hires() function
#' @param pal Palette of colors, usually resulting from kpiPal() function
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' print(terminations_plot(dept_terminations, pal))

terminations_plot <- function(data, pal) {
  terminations_plot <- data$terminations_time %>%
    ggplot2::ggplot(aes(x= term_monthname, y = terminations)) +
    ggplot2::geom_line(aes(group= factor(term_year), color = factor(term_year)), size =1) +
    ggplot2::geom_point(aes(group = factor(term_year), color = factor(term_year)), size=3) +
    ggplot2::labs(title = "Monthly Terminations",
                  y = "Employees") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA), labels = scales::comma) +
    ggplot2::scale_color_manual(name = "Year", values =  pal) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = element_blank())
  return(terminations_plot)
}

#' Function to generate tables of termination reasons for reporting month and year to date
#'
#' @param data The list of data frames resulting from the hires() function
#' @param rpt_date Date in reporting month
#'
#' @return A list of kable objects
#' @export
#'
#' @examples
#' print(terminations_table(dept_terminations, dates$reporting_smonth)$month_termreason_table)
#' print(terminations_table(dept_terminations, dates$reporting_smonth)$ytd_termreason_table)

terminations_table <- function(data, rpt_date) {
  month_termreason_table <- data$termreason_month %>%
    kable(caption = paste0(format(rpt_date, "%B %Y"), " Termination Reasons"), col.names = c("Reason", "Count"), align = "lr", format.args = list(big.mark = ",")) %>%
    kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
    row_spec(0, bold = T)
  ytd_termreason_table <- data$termreason_year %>%
    kable(caption = "YTD Termination Reasons", col.names = c("Reason", "Count"), align = "lr", format.args = list(big.mark = ",")) %>%
    kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
    row_spec(0, bold = T)
  return(list(month_termreason_table = month_termreason_table,
              ytd_termreason_table = ytd_termreason_table))
}

#' Function to create data frames for transfers
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#' @param jde_dept Name of the department in JDE (recommended to set as variable). Does not support 'All' departments.
#' @param dates_obj The list of dates resulting from HR_dates() function
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dept_transfers <- transfers(dept_HRdata, jde_dept, dates)

transfers <- function(data, jde_dept, dates_obj){
  # Find transfers into the department; where old department is null is a new hire, current department equals jde_dept, and current department doesn't equal old department (title change)
  transfers_in <- data$employee_history %>%
    dplyr::filter(!is.na(Old_Department),
                  Department == jde_dept,
                  Department != Old_Department,
                  Position_Start_Date >= dates_obj$HRtimeseries_floordate,
                  Position_Start_Date <= dates_obj$HRtimeseries_ceilingdate)
  # Transfer in time series
  transfers_in_time <- transfers_in %>%
    dplyr::mutate(start_month = lubridate::floor_date(Position_Start_Date, unit = "month"),
                  id_start = paste0(JDE_ID, EPeriod_Start)) %>%
    dplyr::group_by(start_month) %>%
    dplyr::summarise(In = n_distinct(id_start)) %>%
    tidyr::complete(start_month = seq.Date(dates_obj$HRtimeseries_floordate, dates_obj$HRtimeseries_ceilingdate, by = "month"), fill = list(In = 0))
  # Find transfers out of the department; old department equals jde_dept, and current department doesn't equal old department (title change)
  transfers_out <- data$employee_history %>%
    dplyr::filter(Old_Department == jde_dept,
                  Department != Old_Department,
                  Position_Start_Date >= dates_obj$HRtimeseries_floordate,
                  Position_Start_Date <= dates_obj$HRtimeseries_ceilingdate)
  # Transfer out time series
  transfers_out_time <- transfers_out %>%
    dplyr::mutate(start_month = lubridate::floor_date(Position_Start_Date, unit = "month"),
                  id_start = paste0(JDE_ID, EPeriod_Start)) %>%
    dplyr::group_by(start_month) %>%
    dplyr::summarise(Out = n_distinct(id_start)) %>%
    tidyr::complete(start_month = seq.Date(dates_obj$HRtimeseries_floordate, dates_obj$HRtimeseries_ceilingdate, by = "month"), fill = list(Out = 0))
  # Combine transfers in and transfers out
  transfers_time <- transfers_in_time %>%
    dplyr::left_join(transfers_out_time, by = "start_month") %>%
    dplyr::mutate(start_year = lubridate::year(start_month),
                  start_monthname = factor(month.abb[month(start_month)], levels = month.abb))
  # Count in and out transfers by department
  transfers_in_dept <- transfers_in %>%
    dplyr::filter(lubridate::year(Position_Start_Date) ==  lubridate::year(dates_obj$reporting_smonth)) %>%
    dplyr::mutate(id_start = paste0(JDE_ID, EPeriod_Start)) %>%
    dplyr::group_by(Old_Department) %>%
    dplyr::summarise(In = n_distinct(id_start)) %>%
    dplyr::rename("Department" = "Old_Department")
  transfers_out_dept <- transfers_out %>%
    dplyr::filter(lubridate::year(Position_Start_Date) == lubridate::year(dates_obj$reporting_smonth)) %>%
    dplyr::mutate(id_start = paste0(JDE_ID, EPeriod_Start)) %>%
    dplyr::group_by(Department) %>%
    dplyr::summarise(Out = n_distinct(id_start))
  # Join
  transfers_dept <- transfers_in_dept %>%
    dplyr::full_join(transfers_out_dept, by = "Department") %>%
    tidyr::replace_na(list(In = 0, Out = 0)) %>%
    dplyr::mutate(Total = In + Out) %>%
    dplyr::arrange(desc(Total)) %>%
    dplyr::select(-Total) %>%
    janitor::adorn_totals("row")
  return(list(transfers_time = transfers_time,
              transfers_dept = transfers_dept))
}

#' Function to generate reporting year to date text for transfer section of report
#'
#' @param data The list of data frames resulting from the transfers() function
#' @param rpt_date Date in reporting month
#'
#' @return A character value
#' @export
#'
#' @examples
#' dept_transfers_text <- transfers_text(dept_transfers, dates$reporting_smonth)

transfers_text <- function(data, rpt_date){
  # YTD in and out counts
  ytdin_count <- sum(data$transfers_time$In[lubridate::year(data$transfers_time$start_month) == lubridate::year(rpt_date)], na.rm = TRUE)
  ytdout_count <- sum(data$transfers_time$Out[lubridate::year(data$transfers_time$start_month) == lubridate::year(rpt_date)], na.rm = TRUE)
  # Net
  net <- ytdin_count - ytdout_count
  # Gain/loss language
  gainloss <- dplyr::case_when(net == 0 ~ "net zero transfers.",
                               net > 0 ~ paste0("a net gain of ", abs(net), " transfers."),
                               TRUE ~ paste0("a net loss of ", abs(net), " transfers."))
  text <- paste0("## YTD: ", ytdin_count, " transfers in", "\nAdditionally, ", ytdout_count, " employees transferred out of the department, resulting in ", gainloss)
}

#' Function to generate a table of transfers in and out of the selected department
#'
#' @param data The list of data frames resulting from the transfers() function
#' @param dept_name Name of department as it should appear in table title
#'
#' @return A kable object
#' @export
#'
#' @examples
#' print(transfers_table(dept_transfers, dept_name = "Health"))

transfers_table <- function(data, dept_name){
  ytd_transfers_table <- data$transfers_dept %>%
    kable(caption = paste0("YTD Transfers In and Out of the ", dept_name, " Department"), col.names = c("Department", "In", "Out"), align = "lrr", format.args = list(big.mark = ",")) %>%
    kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
    row_spec(0, bold = T)
}

#' Function to create data frames for title changes
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#' @param jde_dept Name of the department in JDE (recommended to set as variable). Supports 'All' departments.
#' @param dates_obj The list of dates resulting from HR_dates() function
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dept_titlechanges <- title_changes(dept_HRdata, jde_dept, dates)

title_changes <- function(data, jde_dept, dates_obj){
  if (jde_dept == "All"){
    # Keep all executive departments
    title_changes <- data$employee_history %>%
      filter(!Department %in% data$nonexec_depts)
  } else {
    # Filter to selected department
    title_changes <- data$employee_history %>%
      dplyr::filter(Department == jde_dept)
  }
  # Find title changes; where current department is the same as old department but job titles are different; calculate time in previous position
  title_changes <- data$employee_history %>%
    dplyr::group_by(JDE_ID) %>%
    dplyr::mutate(old_start = dplyr::lag(Position_Start_Date, n=1, order_by = Position_Start_Date),
                  old_end = dplyr::lag(Position_End_Date, n=1, order_by = Position_Start_Date),
                  time_in_previous = round(as.numeric(difftime(old_end, old_start, units = "days"))/365.25, 1)) %>%
    dplyr::filter(Department == Old_Department,
                  Job_Title != Old_Job_Title,
                  Position_Start_Date >= dates_obj$HRtimeseries_floordate,
                  Position_Start_Date <= dates_obj$HRtimeseries_ceilingdate)
  # Title change time series
  title_changes_time <- title_changes %>%
    dplyr::mutate(start_month = lubridate::floor_date(Position_Start_Date, unit = "month"),
                  id_start = paste0(JDE_ID, EPeriod_Start)) %>%
    dplyr::group_by(start_month) %>%
    dplyr::summarise(promotions = n_distinct(id_start)) %>%
    tidyr::complete(start_month = seq.Date(dates_obj$HRtimeseries_floordate, dates_obj$HRtimeseries_ceilingdate, by = "month"), fill = list(promotions = 0)) %>%
    dplyr::mutate(start_year = lubridate::year(start_month),
                  start_monthname = factor(month.abb[month(start_month)], levels = month.abb))
  # YTD title change comparison
  title_changes_year <- CountyStatKPI::kpi_compare(title_changes_time, start_month, dates_obj$reporting_smonth, method = "sum", val_col = promotions, mode = "ytd")
  return(list(title_changes = title_changes,
              title_changes_time = title_changes_time,
              title_changes_year = title_changes_year))
}

#' Function to generate reporting year to date text for title change section of report
#'
#' @param data The list of data frames resulting from the title_changes() function
#' @param rpt_date Date in reporting month
#'
#' @return A list of character values
#' @export
#'
#' @examples
#' dept_titlechange_text <- title_change_text(dept_titlechanges, dates$reporting_smonth)

title_change_text <- function(data, rpt_date){
  # Calculate median time in previous position
  med_time <- stats::median(data$title_changes$time_in_previous[year(data$title_changes$Position_Start_Date) == year(rpt_date)], na.rm = TRUE)
  title_changes_header <- CountyStatKPI::kpi_header_text(data$title_changes_year, rpt_date, prior_val = "ytd", metric_txt = "title change(s)")
  title_changes_text <- paste0("Year to date, the median amount of time spent in a previous position before receiving a within-department title change is ", med_time, " years.")
  return(list(title_changes_header = title_changes_header,
              title_changes_text = title_changes_text))
}

#' Function to generate plot for title change time series
#'
#' @param data The list of data frames resulting from the title_changes() function
#' @param pal Palette of colors, usually resulting from kpiPal() function
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' print(title_changes_plot(dept_titlechanges, pal))

title_changes_plot <- function(data, pal){
  titlechanges_plot <- data$title_changes_time %>%
    ggplot2::ggplot(aes(x= start_monthname, y = promotions)) +
    ggplot2::geom_line(aes(group= factor(start_year), color = factor(start_year)), size =1) +
    ggplot2::geom_point(aes(group = factor(start_year), color = factor(start_year)), size=3) +
    ggplot2::labs(title = "Monthly Title Changes",
                  y = "Changes") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA), labels = scales::comma) +
    ggplot2::scale_color_manual(name = "Year", values =  pal) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = element_blank(),
                   legend.position = "bottom")
  return(titlechanges_plot)
}

#' Function to create data frames for net employees. Does not support 'All' departments.
#'
#' @param hires_obj The list of data frames resulting from the hires() function
#' @param terminations_df The terminations_time data frame produced by the terminations() function
#' @param transfers_df The transfers_time data frame produced by the transfers() function
#' @param dates_obj The list of dates resulting from HR_dates() function
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dept_net <- net(dept_hires, dept_terminations$terminations_time, dept_transfers$transfers_time, dates)

net <- function(hires_obj, terminations_df, transfers_df, dates_obj, seasonal_rehires = FALSE) {
  if (seasonal_rehires == FALSE) {
    # Join all data frames together, select necessary columns, and make net calculation
    net_time <- hires_obj$hires_time %>%
      dplyr::left_join(terminations_df, by = c("hire_month" = "term_month")) %>%
      dplyr::left_join(transfers_df, by = c("hire_month" = "start_month")) %>%
      dplyr::select(hire_month, hire_monthname, hire_year, hires, terminations, In, Out) %>%
      dplyr::mutate(Net = hires - terminations + In - Out) %>%
      rename("month" = "hire_month",
             "year" = "hire_year",
             "monthname" = "hire_monthname")
  } else {
    # Join all data frames together (including seasonal rehires), select necessary columns, and make net calculation
    net_time <- hires_obj$hires_time %>%
      dplyr::left_join(terminations_df, by = c("hire_month" = "term_month")) %>%
      dplyr::left_join(transfers_df, by = c("hire_month" = "start_month")) %>%
      dplyr::left_join(subset(hires_obj$seasonal_rehires_time, select = c("hire_month", "rehires")), by = "hire_month") %>%
      dplyr::select(hire_month, hire_monthname, hire_year, hires, terminations, rehires, In, Out) %>%
      dplyr::mutate(Net = hires - terminations + rehires + In - Out) %>%
      rename("month" = "hire_month",
             "year" = "hire_year",
             "monthname" = "hire_monthname")
  }
  # Compare net to previous year to date
  net_year <- CountyStatKPI::kpi_compare(net_time, date_col = month, dates_obj$reporting_smonth, method = "sum", val_col = Net, mode = "ytd")
  return(list(net_time = net_time,
              net_year = net_year))
}

#' Function to generate reporting year to date text for the net employees section of report
#'
#' @param data The list of data frames resulting from the net() function
#' @param rpt_date Date in reporting month
#'
#' @return A character value
#' @export
#'
#' @examples
#' dept_net_text <- net_text(dept_net, dates$reporting_smonth)

net_text <- function(data, rpt_date){
  plusminus <- case_when(data$net_year$rytd_cnt > 0 ~ ": +",
                         TRUE ~ ": ")
  header_pre <- CountyStatKPI::kpi_header_text(data$net_year, rpt_date, prior_val = "ytd", "net employees")
  header <- gsub(": ", plusminus, header_pre)
}

#' Function to generate plot for title change time series
#'
#' @param data The list of data frames resulting from the net() function
#' @param seasonal_rehires A TRUE or FALSE binary that changes the caption of the ggplot. Defaults to FALSE.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' print(net_plot(dept_net, seasonal_rehires = FALSE))

net_plot <- function(data, seasonal_rehires = FALSE){
  nudge <- max(data$net_time$Net)/10
  monthly_net_plot <- data$net_time %>%
    ggplot2::ggplot(aes(x = monthname, y = Net)) +
    ggplot2::geom_bar(aes(fill = Net < 0), stat = "identity") +
    ggplot2::geom_text(data = subset(data$net_time, Net != 0), aes(x = monthname, y = ifelse(Net > 0, Net + nudge, Net - nudge), label = ifelse(Net > 0, paste0("+", Net), Net)), size = 3) +
    ggplot2::scale_fill_manual(guide = "none", breaks = c(TRUE, FALSE), values = c("#D73027", "#5AAE61")) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(min(data$net_time$Net)-nudge-1, max(data$net_time$Net+nudge+1))) +
    ggplot2::facet_grid(. ~ year) +
    ggplot2::theme_light() +
    ggplot2::theme(axis.title.x = element_blank(),
                   panel.border = element_blank())
  if(seasonal_rehires == FALSE){
    monthly_net_plot +
      ggplot2::labs(title = "Monthly Net Employees",
                    caption = "Net amount accounts for hires, rehires, terminations, and transfers in and out of the department.",
                    y = "Net Employees")
  } else
    monthly_net_plot +
    ggplot2::labs(title = "Monthly Net Employees",
                  caption = "Net amount accounts for hires, seasonal and non-seasonal rehires,\nterminations, and transfers in and out of the department.",
                  y = "Net Employees")
}

#' Function to generate plot for historical change of vacancies
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' print(vacancies_plot(dept_HRdata))

vacancies_plot <- function(data) {
  # Filter vacancy data to reporting period (last two years), group sum and arrange
  vacancy_history <- data$vacancyhistory %>%
    dplyr::filter(Date_Stamp >= dates$HRtimeseries_floordate) %>%
    dplyr::group_by(Date_Stamp) %>%
    dplyr::summarise(Positions = sum(Positions)) %>%
    dplyr::arrange(desc(Date_Stamp))
  # Get nudge amount for plot
  nudge <- max(vacancy_history$Positions)/2
  # Vacancy plot marking most recent high and low points
  # Eventually this chart will show two years of data and caption can be removed
  plot <- vacancy_history %>%
    ggplot2::ggplot(aes(x = Date_Stamp, y = Positions)) +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 3) +
    ggrepel::geom_label_repel(data = slice_head(subset(vacancy_history,  Positions == max(Positions))), aes(x = Date_Stamp, y = Positions, label = paste0("High: ", format(Date_Stamp, "%b %Y"), "\n", Positions, " vacancies")), nudge_y = nudge, size = 3, color = "#D73027") +
    ggrepel::geom_label_repel(data = slice_head(subset(vacancy_history, Positions == min(Positions))), aes(x = Date_Stamp, y = Positions, label = paste0("Low: ", format(Date_Stamp, "%b %Y"), "\n", Positions, " vacancies")), nudge_y = nudge, size = 3, color = "#5AAE61") +
    ggplot2::labs(title = "Historical Vacancies",
                  y = "Vacancies",
                  caption = "Historical data collection began in May 2024. Data is typically cached every two weeks.") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, max(vacancy_history$Positions)+nudge+10), labels = scales::comma) +
    ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b %Y", expand = c(0.1, 0.1)) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = element_blank())
}

#' Function to generate text stating the number of vacant jobs and positions.
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#' @param rpt_date Date in reporting month
#' @param dept_name Name of department as it should appear in text
#'
#' @return A character value
#' @export
#'
#' @examples
#' dept_vacancy_text <- vacancies_text(dept_HRdata, dates$reporting_emonth, dept_name = "Health Department")

vacancies_text <- function(data, rpt_enddate, dept_name){
  vacancies <- data$positionvacancies %>%
    dplyr::group_by(Department) %>%
    dplyr::summarise(jobs = n_distinct(Job_Title),
                     positions = n_distinct(sEmpNum)) %>%
    dplyr::arrange(jobs)
  openpositions <- sum(vacancies$positions, na.rm = TRUE)
  openjobs <- sum(vacancies$jobs, na.rm = TRUE)
  text <- paste0("## Open Positions: ", openpositions, " budgeted, vacant positions(s)\nAs of ", format(rpt_enddate, "%B %d, %Y"), " there are ", openpositions, " open position(s) that fall under ", openjobs, " unique job title(s) in the ", dept_name, ". This number does not necessarily reflect the number of posted positions that applicants can currently apply to.")
}

#' Function to create data frames for applications received. Supports 'All' departments.
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#' @param dates_obj The list of dates resulting from HR_dates() function
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dept_applications <- applications(dept_HRdata, dates)

applications <- function(data, dates_obj) {
  # Statuses that fall each each group
  eligible <- c("ELIGIBLE", "INTERVIEW QUEUE")
  testing <- c("ELIGIBLE FOR TEST", "AWAITING TEST", "TEST SCHEDULED")
  # Applications time series; calculate total, eligible, and testing
  applications_time <- data$applications %>%
    dplyr:: filter(EntryDate >= dates_obj$HRtimeseries_floordate,
                   EntryDate <= dates_obj$HRtimeseries_ceilingdate) %>%
    dplyr::mutate(submit_month = lubridate::floor_date(EntryDate, unit = "month"),
                  eligible = ifelse(trimws(CurrentEligStatus) %in% eligible, 1, 0),
                  testing = ifelse(trimws(CurrentEligStatus) %in% testing, 1, 0)) %>%
    dplyr::group_by(submit_month) %>%
    dplyr::summarise(apps = dplyr::n_distinct(AppID),
                     eligible = sum(eligible, na.rm = TRUE),
                     testing = sum(testing, na.rm = TRUE)) %>%
    tidyr::complete(submit_month = seq.Date(dates_obj$HRtimeseries_floordate, dates_obj$HRtimeseries_ceilingdate, by = "month"), fill = list(apps = 0, eligible = 0, testing = 0)) %>%
    dplyr::mutate(submit_year = lubridate::year(submit_month),
                  submit_monthname = factor(month.abb[month(submit_month)], levels = month.abb))
  # Applications by position for reporting month
  apps_pos_month <- data$applications %>%
    dplyr::filter(lubridate::floor_date(EntryDate, unit = "month") == dates_obj$reporting_smonth) %>%
    dplyr::group_by(Position) %>%
    dplyr::summarise(Applications = dplyr::n_distinct(AppID))
  # Applications by position for reporting year
  apps_pos_year <- data$applications %>%
    dplyr::filter(lubridate::year(EntryDate) == lubridate::year(dates_obj$reporting_smonth)) %>%
    dplyr::group_by(Position) %>%
    dplyr::summarise(Applications = dplyr::n_distinct(AppID))
  # Compare object for reporting month
  applications_month <- CountyStatKPI::kpi_compare(applications_time, submit_month, rpt_date = dates_obj$reporting_smonth, method = "sum", val_col = apps, mode = "my")
  # Compare object for year to date
  applications_year <- CountyStatKPI::kpi_compare(applications_time, submit_month, rpt_date = dates_obj$reporting_smonth, method = "sum", val_col = apps, mode = "ytd")
  return(list(applications_time = applications_time,
              apps_pos_month = apps_pos_month,
              apps_pos_year = apps_pos_year,
              applications_month = applications_month,
              applications_year = applications_year))
}

#' Function to generate reporting month and year to date text for application section of report
#'
#' @param data The list of data frames resulting from the applications() function
#' @param rpt_date Date in reporting month
#' @param seasonal TRUE or FALSE binary to compare hiring numbers to previous month (FALSE) or same month of the previous year (TRUE). Defaults to FALSE.
#'
#' @return A list of character values
#' @export
#'
#' @examples
#' dept_apps_text <- applications_text(dept_applications, dates$reporting_smonth)

applications_text <- function(data, rpt_date, seasonal = FALSE) {
  # Percent eligible/testing for reporting month
  pct_eligible_month <- round(sum(data$applications_time$eligible[data$applications_time$submit_month == rpt_date])/data$applications_month$rm_cnt*100, 1)
  pct_testing_month <- round(sum(data$applications_time$testing[data$applications_time$submit_month == rpt_date])/data$applications_month$rm_cnt*100, 1)
  # Compare to previous month or same month from previous year
  if (seasonal == FALSE) {
    applications_month_header <- CountyStatKPI::kpi_header_text(data$applications_month, rpt_date, prior_val = "month", metric_txt = "application(s) recevied")
  } else {
    applications_month_header <- CountyStatKPI::kpi_header_text(data$applications_month, rpt_date, prior_val = "year", metric_txt = "application(s) recevied")
  }
  applications_month_text <- paste0("Of the applications recevied in ", format(rpt_date, "%B %Y"), ", ", pct_eligible_month, "% have been deemed eligible or moved to the interview queue, and an additional ", pct_testing_month, "% are eligible for or awaiting testing.")
  # Percent eligible/testing for reporting year
  pct_eligible_year <- round(sum(data$applications_time$eligible[data$applications_time$submit_year == year(rpt_date)])/data$applications_year$rytd_cnt*100, 1)
  pct_testing_year <- round(sum(data$applications_time$testing[data$applications_time$submit_year == year(rpt_date)])/data$applications_year$rytd_cnt*100, 1)
  avg_monthly_apps <- round(mean(data$applications_time$apps[data$applications_time$submit_year == year(rpt_date)], na.rm = TRUE), 0)
  applications_year_header <- CountyStatKPI::kpi_header_text(data$applications_year, rpt_date, prior_val = "ytd", metric_txt = "application(s) recevied")
  applications_year_text <- paste0("On average, the department received ", avg_monthly_apps, " applications each month this year. Of the applications recevied so far in ", format(rpt_date, "%Y"), ", ", pct_eligible_year, "% have been deemed eligible or moved to the interview queue, and an additional ", pct_testing_year, "% are eligible for or awaiting testing.")
  return(list(applications_month_header = applications_month_header,
              applications_month_text = applications_month_text,
              applications_year_header = applications_year_header,
              applications_year_text = applications_year_text))
}

#' Function to generate a table of applications received in the reporting month or reporting year
#'
#' @param data The list of data frames resulting from the applications() function
#' @param rpt_date A date during the reporting month
#' @param period To show either reporting month or reporting year. Must be either 'month' or 'year'
#'
#' @return A kable object
#' @export
#'
#' @examples
#' print(applications_table(dept_applications, dates$reporting_smonth, period = 'month'))
#' print(applications_table(dept_applications, dates$reporting_smonth, period = 'year'))

applications_table <- function(data, rpt_date, period = 'month'){
  if (period == 'month'){
    application_table <- data$apps_pos_month %>%
      kable(caption = paste0(format(rpt_date, "%B %Y"), " Applications Received by Position"), col.names = c("Job Title", "Applications"), align = "lr", format.args = list(big.mark = ",")) %>%
      kable_styling(font_size = 11, latex_options = "HOLD_position")
    # Had to add if else because of column_spec
    if (nrow(data$apps_pos_month) > 0) {
      application_table <- application_table %>%
        row_spec(0, bold = T) %>%
        column_spec(1, width = "40em")
    } else {
      application_table
    }
  } else {
    application_table <- data$apps_pos_year %>%
      kable(caption = paste0(format(rpt_date, "%Y"), " Applications Received by Position"), col.names = c("Job Title", "Applications"), align = "lr", format.args = list(big.mark = ",")) %>%
      kable_styling(font_size = 11, latex_options = "HOLD_position")
    # Had to add if else because of column_spec
    if (nrow(data$apps_pos_year) > 0) {
      application_table <- application_table %>%
        row_spec(0, bold = T) %>%
        column_spec(1, width = "40em")
    } else {
      application_table
    }
  }
}

#' Function to generate plot for applications time series
#'
#' @param data The list of data frames resulting from the applications() function
#' @param pal Palette of colors, usually resulting from kpiPal() function
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' print(applications_plot(dept_applications, pal))

applications_plot <- function(data, pal){
  applications_plot <- data$applications_time %>%
    ggplot2::ggplot(aes(x= submit_monthname, y = apps)) +
    ggplot2::geom_line(aes(group= factor(submit_year), color = factor(submit_year)), size =1) +
    ggplot2::geom_point(aes(group = factor(submit_year), color = factor(submit_year)), size=3) +
    ggplot2::labs(title = "Monthly Applications Received",
                  y = "Applications") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA), labels = scales::comma) +
    ggplot2::scale_color_manual(name = "Year", values =  pal) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = element_blank())
  return(applications_plot)
}

#' Function to create data frames for sick and vacation time taken. Supports 'All' departments.
#'
#' @param data The list of data frames resulting from the HR_pulldata() function
#' @param dates_obj The list of dates resulting from HR_dates() function
#'
#' @return A list of data frames
#' @export
#'
#' @examples
#' dept_timeoff <- time_off(dept_HRdata, dates)

time_off <- function(data, dates_obj){
  # Find max pay period
  max_payperiod <- data$payperiods %>%
    dplyr::filter(date == max(data$timesheet$Date_Worked)) %>%
    dplyr::pull(payPeriodEnd[1])
  # Find number of employees who clocked in in reporting month
  employee_count <- data$timesheet %>%
    dplyr::filter(Date_Worked >= dates_obj$reporting_smonth,
                  Date_Worked <= dates_obj$reporting_emonth) %>%
    dplyr::distinct(JDE_ID) %>%
    nrow()
  # Sick time series
  sick_time <- data$timesheet %>%
    dplyr::filter(Pay_Subcategory == 'Sick') %>%
    dplyr::mutate(sick_month = lubridate::floor_date(Date_Worked, unit = "month")) %>%
    dplyr::group_by(sick_month) %>%
    dplyr::summarise(days = round(sum(Hrs_Worked)/8, 0)) %>%
    tidyr::complete(sick_month = seq.Date(min(data$timesheet$Date_Worked), max(data$timesheet$Date_Worked), by = "month"), fill = list(days = 0)) %>%
    dplyr::mutate(sick_year = lubridate::year(sick_month),
                  sick_monthname = factor(month.abb[month(sick_month)], levels = month.abb),
                  sick_monthnum = lubridate::month(sick_month))
  # Sick days in current month, prior month
  sick_cmonth <- sick_time$days[sick_time$sick_month == dates_obj$reporting_smonth]
  # Average sick days in month (data goes back 3 years)
  avg_sick <- round(mean(sick_time$days[sick_time$sick_monthnum == lubridate::month(dates_obj$reporting_smonth)], na.rm = TRUE), 0)
  # Sick days per employee
  sick_peremp <- round(sick_cmonth/employee_count, 1)
  # Compare current month to monthly average
  sick_compare <- CountyStatKPI::kpi_pcts(sick_cmonth, avg_sick, 0, mode = "my")
  sick_compare$mom_inc_dec <- dplyr::case_when(sick_compare$mom_inc_dec == "increase" ~ "higher than",
                                               sick_compare$mom_inc_dec == "decrease" ~ "lower than",
                                               sick_compare$mom_inc_dec == "no change" ~ "No change")
  # Finalize sick time series
  sick_time <- sick_time %>%
    dplyr::filter(sick_month >= dates_obj$HRtimeseries_floordate,
                  sick_month <= dates_obj$HRtimeseries_ceilingdate)
  # Vacation time series
  vac_time <- data$timesheet %>%
    dplyr::filter(Pay_Subcategory == 'Vacation') %>%
    dplyr::mutate(vac_month = lubridate::floor_date(Date_Worked, unit = "month")) %>%
    dplyr::group_by(vac_month) %>%
    dplyr::summarise(days = round(sum(Hrs_Worked)/8, 0)) %>%
    tidyr::complete(vac_month = seq.Date(min(data$timesheet$Date_Worked), max(data$timesheet$Date_Worked), by = "month"), fill = list(days = 0)) %>%
    dplyr::mutate(vac_year = lubridate::year(vac_month),
                  vac_monthname = factor(month.abb[month(vac_month)], levels = month.abb),
                  vac_monthnum = lubridate::month(vac_month))
  # Vacation days in current month, prior month
  vac_cmonth <- vac_time$days[vac_time$vac_month == dates_obj$reporting_smonth]
  # Average vacation days in month (data goes back 3 years)
  avg_vac <- round(mean(vac_time$days[vac_time$vac_monthnum == lubridate::month(dates_obj$reporting_smonth)], na.rm = TRUE), 0)
  # Vacation days per employee
  vac_peremp <- round(vac_cmonth/employee_count, 1)
  # Compare current month to monthly average
  vac_compare <- CountyStatKPI::kpi_pcts(vac_cmonth, avg_vac, 0, mode = "my")
  vac_compare$mom_inc_dec <- dplyr::case_when(vac_compare$mom_inc_dec == "increase" ~ "higher than",
                                              vac_compare$mom_inc_dec == "decrease" ~ "lower than",
                                              vac_compare$mom_inc_dec == "no change" ~ "No change")
  # Finalize sick time series
  vac_time <- vac_time %>%
    dplyr::filter(vac_month >= dates_obj$HRtimeseries_floordate,
                  vac_month <= dates_obj$HRtimeseries_ceilingdate)
  # Scheduled vacation time
  scheduled_vac <- data$schedule %>%
    dplyr::filter(WorkDate > dates_obj$reporting_emonth,
                  lubridate::year(WorkDate) == year(dates_obj$reporting_emonth)) %>%
    dplyr::mutate(vac_month = lubridate::floor_date(WorkDate, unit = "month")) %>%
    dplyr::group_by(vac_month) %>%
    dplyr::summarise(days = round(sum(Hours)/8, 0)) %>%
    dplyr::mutate(vac_year = lubridate::year(vac_month),
                  vac_monthname = factor(month.abb[month(vac_month)], levels = month.abb),
                  vac_monthnum = lubridate::month(vac_month))
  #Fill in time off info data frame with calculations
  time_info <- data.frame(max_payperiod = max_payperiod,
                          employee_count = employee_count,
                          sick_days = sick_cmonth,
                          avg_sick = avg_sick,
                          sick_peremp = sick_peremp,
                          sickcmp_updown = sick_compare$mom_up_down,
                          sickcmp_abspct = sick_compare$mom_abs_chg,
                          sickcmp_incdec = sick_compare$mom_inc_dec,
                          vac_days = vac_cmonth,
                          avg_vac = avg_vac,
                          vac_peremp = vac_peremp,
                          vaccmp_updown = vac_compare$mom_up_down,
                          vaccmp_abspct = vac_compare$mom_abs_chg,
                          vaccmp_incdec = vac_compare$mom_inc_dec)
  return(list(sick_time = sick_time,
              vac_time = vac_time,
              scheduled_vac = scheduled_vac,
              time_info = time_info))
}

#' Function to generate reporting month text for time off section of report
#'
#' @param data The list of data frames resulting from the time_off() function
#' @param rpt_date Date in reporting month
#'
#' @return A list of character values
#' @export
#'
#' @examples
#' dept_timeoff_text <- time_off_text(dept_timeoff, dates$reporting_smonth)

time_off_text <- function(data, rpt_date){
  sick_change = ifelse(data$time_info$sickcmp_incdec == "no change",
                       paste0("No change compared to ", format(rpt_date, "%B"), " average"),
                       paste0(data$time_info$sickcmp_updown, " ", data$time_info$sickcmp_abspct, "% ", data$time_info$sickcmp_incdec, " ", format(rpt_date, "%B"), " average"))
  vac_change = ifelse(data$time_info$vaccmp_incdec == "no change",
                      paste0("No change compared to ", format(rpt_date, "%B"), " average"),
                      paste0(data$time_info$vaccmp_updown, " ", data$time_info$vaccmp_abspct, "% ", data$time_info$vaccmp_incdec, " ", format(rpt_date, "%B"), " average"))
  intro_text <- paste0("Monthly values for sick and vacation time represent totals through the last approved pay period ending on ", format(data$time_info$max_payperiod, "%B %d, %Y"), ". Averages are based on data from ", lubridate::year(rpt_date)-3, " onward.")
  sick_header <- paste0("## Sick Time in ", format(rpt_date, "%B %Y"), ": ", data$time_info$sick_days, " day(s)\n **", sick_change, "**")
  sick_text <- paste0("Employees used a total of ", data$time_info$sick_days, " sick day(s) in the reporting month, while the average for that month is ", data$time_info$avg_sick, " days. Each employee used an average of ", data$time_info$sick_peremp, " sick day(s) this month.")
  vac_header <- paste0("## Vacation Time in ", format(rpt_date, "%B %Y"), ": ", data$time_info$vac_days, " day(s)\n **", vac_change, "**")
  vac_text <- paste0("Employees used a total of ", data$time_info$vac_days, " vacation day(s) in the reporting month, while the average for that month is ", data$time_info$avg_vac, " days. Each employee used an average of ", data$time_info$vac_peremp, " vacation day(s) this month.")
  return(list(intro_text = intro_text,
              sick_header = sick_header,
              sick_text = sick_text,
              vac_header = vac_header,
              vac_text = vac_text))
}

#' Function to generate plots for sick and vacation time time series
#'
#' @param data The list of data frames resulting from the time_off() function
#' @param pal Palette of colors, usually resulting from kpiPal() function
#'
#' @return A list of ggplot objects
#' @export
#'
#' @examples
#' print(time_off_plots(dept_timeoff, pal)$sick_plot)
#' print(time_off_plots(dept_timeoff, pal)$vac_plot)

time_off_plots <- function(data, pal){
  sick_plot <- data$sick_time %>%
    ggplot2::ggplot(aes(x= sick_monthname, y = days)) +
    ggplot2::geom_line(aes(group= factor(sick_year), color = factor(sick_year)), size =1) +
    ggplot2::geom_point(aes(group = factor(sick_year), color = factor(sick_year)), size=3) +
    ggplot2::labs(title = "Monthly Use of Sick Time",
                  y = "Days") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA), labels = scales::comma) +
    ggplot2::scale_color_manual(name = "Year", values =  pal) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = element_blank(),
                   legend.position = "bottom")
  vac_plot <- data$vac_time %>%
    ggplot2::ggplot(aes(x= vac_monthname, y = days)) +
    ggplot2::geom_line(aes(group= factor(vac_year), color = factor(vac_year)), size =1) +
    ggplot2::geom_point(aes(group= factor(vac_year), color = factor(vac_year)), size =3) +
    ggplot2::geom_line(data = data$scheduled_vac, aes(group= factor(vac_year), color = factor(vac_year)), alpha = 0.3, size =1) +
    ggplot2::geom_point(data = data$scheduled_vac, aes(group= factor(vac_year), color = factor(vac_year)), alpha = 0.3, size =3) +
    ggplot2::labs(title = "Monthly Use of Vacation Time",
                  subtitle = "With Scheduled Vacation Projection",
                  y = "Days") +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(), limits = c(0, NA), labels = scales::comma) +
    ggplot2::scale_color_manual(name = "Year", values =  pal) +
    ggplot2::theme_minimal()+
    ggplot2::theme(axis.title.x = element_blank(),
                   legend.position = "bottom")
  return(list(sick_plot = sick_plot,
              vac_plot = vac_plot))
}
