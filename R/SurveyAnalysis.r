
#' Run Single Month Analysis
#'
#' Run analaysis for single survey month and write results to an Excel file
#'
#' @param lic_year Licence year (e.g. 2016 for licence year 2016/17)
#' @param month_name Month to analyse take from month.name (e.g "February")
#' @param irec_dir_root Path to the iRec data root directory
#'
#' @return The Excel document file name that contains all the month's survey data
#'
#' @export
#'
runSingleMonthAnalysis <- function(lic_year, month_name, irec_dir_root= getiRecAnalysisDir()) {
  clearLogMessages()
  lic_year_txt <- getLicenceYearText(lic_year)

  if (month_name %notin% month.name) {
    stop(glue("The provided month {month_name} is invalid."))
  }

  config <- loadAnalysisConfig(lic_year_txt, month_name, irec_dir_root = irec_dir_root)
  addLogMessages("Reading survey response file: ",
                 basename(config$licence_filename))

  elic_data <-
    loadLicenceFile(config$licence_filename,
                    config$annual_date_range[1],
                    config$annual_date_range[2]) %>%
    subsetLicences(config$survey_dates[1], config$survey_dates[2])

  if ("stamp_purchase_date" %in% colnames(elic_data)) {
    elic_data <-
      elic_data %>%
      mutate(stamp = if_else(!is.na(stamp_purchase_date) &
                               (licence_type == LicTypeAnnual & stamp_purchase_date < config$survey_dates[2]) |
                               (licence_type != LicTypeAnnual & stamp_purchase_date <= config$survey_dates[2]),
                             TRUE,
                             FALSE))
  }
  if (lic_year < InstLicYearStart) {
    result_list <- runPrePostSurveyAnalysis(config, elic_data)
  } else {
    result_list <- runInstSurveyAnalysis(config, elic_data)
  }

  result_doc <- createWorkbook()

  addXlWorksheet(result_list$licence_summary, result_doc, "Licence Summary")
  addXlWorksheet(result_list$survey_summary, result_doc, "Survey Summary")
  addXlWorksheet(result_list$survey_sample_sizes, result_doc, "Survey Sample Sizes")
  addXlWorksheet(result_list$survey_specific_variance, result_doc, "Survey Individual Variance")
  addXlWorksheet(result_list$detail_estimated_catch, result_doc, "Detailed Estimated Catch")
  addXlWorksheet(result_list$summary_estimated_catch, result_doc, "Summary Estimated Catch")
  addXlWorksheet(result_list$total_variance, result_doc, "Summary Total Variance")
  addXlWorksheet(result_list$log, result_doc, "Log")

  saveWorkbook(result_doc, file = config$analysis_result_filename, overwrite = FALSE)

  return(config$analysis_result_filename)
}

#' Add Annual Excel Worksheet
#'
#' Helper funciton to combine list of monthly data frames into a single annual data frames
#' and writes it to a provided Excel document as a new worksheet
#'
#' @param data_list with monthly survey resultss
#' @param data_name name of data frames to combine
#' @param result_doc An excel document
#' @param worksheet_name The name of the work sheet to add
#'
#' @return The list provided as data_list is returned (allows writing mulitple worksheets in a pipe)
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr arrange
#'
addAnnualXlWorksheet <- function(data_list, data_name, result_doc, worksheet_name) {
  data_list %>%
    map_dfr(data_name) %>%
    arrange(Year, Month_Id) %>%
    select(-Month_Id) %>%
    addXlWorksheet(result_doc, worksheet_name)

  return(data_list)
}

#' Run Annual Analysis
#'
#' Run analysis for all months within a licence year to a single annual Excel file
#'
#' @param lic_year Licence year (e.g. 2016 for licence year 2016/17)
#' @param irec_dir_root Path to the iRec data root directory
#'
#' @return The Excel document file name that contains all the annual data
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom fs dir_exists
#' @importFrom openxlsx createWorkbook saveWorkbook
#'
runAnnualAnalysis <- function(lic_year, irec_dir_root = getiRecAnalysisDir()) {
  lic_year_txt <- getLicenceYearText(lic_year)

  year_config <- loadAnalysisYearConfig(lic_year_txt, irec_dir_root = irec_dir_root)

  elic_data <-
    loadLicenceFile(year_config$licence_filename,
                    year_config$annual_date_range[1],
                    year_config$annual_date_range[2])

  funRunMonthAnalysis <- function(month_name, lic_year_txt, elic_data, irec_dir_root, year_config) {
    print(glue("Processing {month_name}"))

    survey_path <- getSurveyPath(month_name,
                                 year_config$year_data_path,
                                 year_config$annual_expire_date)

    clearLogMessages()

    if (dir_exists(survey_path) == FALSE) {
      addLogMessages("WARNING: Survey data is missing from '{survey_path}'")
      return(list(log = getLogMessages()))
    }

    config <- loadAnalysisConfig(lic_year_txt,
                                 month_name,
                                 irec_dir_root = irec_dir_root,
                                 year_config = year_config)

    elic_data <-
      elic_data %>%
      subsetLicences(config$survey_dates[1], config$survey_dates[2])


    if ("stamp_purchase_date" %in% colnames(elic_data)) {
      elic_data <-
        elic_data %>%
        mutate(stamp = if_else(!is.na(stamp_purchase_date) &
                                 (licence_type == LicTypeAnnual & stamp_purchase_date < config$survey_dates[2]) |
                                 (licence_type != LicTypeAnnual & stamp_purchase_date <= config$survey_dates[2]),
                               TRUE,
                               FALSE))
    }

    if (lic_year < InstLicYearStart) {
      survey_data <- runPrePostSurveyAnalysis(config, elic_data)
    } else {
      survey_data <- runInstSurveyAnalysis(config, elic_data)
    }

    survey_data <- map(survey_data,
                       ~ .x %>%
                         mutate(Year = year(config$survey_dates[1]),
                                Month_Id= which(month_name %in% month.name),
                                Month=month_name) %>%
                         select(Year, Month_Id, Month, everything()),
                       month_name)

    return(survey_data)
  }

  x <- map(month.name,
           funRunMonthAnalysis,
           lic_year_txt,
           elic_data,
           irec_dir_root,
           year_config)

  result_doc <- createWorkbook()

  x %>%
    addAnnualXlWorksheet("licence_summary", result_doc, "Licence Summary") %>%
    addAnnualXlWorksheet("survey_summary", result_doc, "Survey Summary") %>%
    addAnnualXlWorksheet("survey_sample_sizes", result_doc, "Survey Sample Sizes") %>%
    addAnnualXlWorksheet("survey_specific_variance", result_doc, "Survey Individual Variance") %>%
    addAnnualXlWorksheet("detail_estimated_catch", result_doc, "Detailed Estimated Catch") %>%
    addAnnualXlWorksheet("summary_estimated_catch", result_doc, "Summary Estimated Catch") %>%
    addAnnualXlWorksheet("total_variance", result_doc, "Summary Total Variance") %>%
    addAnnualXlWorksheet("log", result_doc, "Log")

  saveWorkbook(result_doc, file = year_config$annual_irec_result_filename, overwrite = FALSE)

  return(year_config$annual_irec_result_filename)
}


#' Run iRecAnalysis User Interface
#'
#' Start a shiny server with the user interface for the iRecAnalysis package
#'
#' @param analysis_dir Directory where iRec analysis is maintained
#' @param port Port number to run shiny server on
#'
#' @export
#'
#' @import shiny
#' @importFrom utils packageName
#'
runUI <- function(analysis_dir, port = 9001) {
  setiRecAnalysisDir(analysis_dir)
  appDir <- system.file("shinyApp", package = packageName())
  if (appDir == "") {
    stop(glue("Could not find example directory. Try re-installing `{packageName()}`."), call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", port = port)
}
