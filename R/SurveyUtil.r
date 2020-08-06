IArcSurveyName <- "iArc"

UnspecifiedText <- "Unspecified"

"%notin%" <- Negate("%in%")

#' Clear Log Messages
#'
#' Clears the globally stored log message queue
#'
clearLogMessages <- function() {
  log_env <- .GlobalEnv
  log_env$LogMessageQueue <- NULL
}

#' Add Log Message
#'
#' Add a vector of log messages to the global log message queue
#'
#' @param ... character string of the message, use curly brackets (braces) to input objects (glue notation)
#'
#'
addLogMessages <- function(...) {
  logMsg <- glue(..., .sep = " ", .envir = parent.frame())

  log_env <- .GlobalEnv
  log_env$LogMessageQueue <- c(log_env$LogMessageQueue, logMsg)
}

#' Get Log Messages
#'
#' Retrieve the global vector of vector log messages
#'
#' @return A tibble with the log messages as a column
#'
#' @importFrom dplyr tibble
#'
getLogMessages <- function() {
  log_env <- .GlobalEnv
  return(tibble(log_messages = log_env$LogMessageQueue))
}

#' Get Time Stamp Text
#'
#' Provides a standardized text based time stamp for inclusion in file names
#'
#' @return A string with the current date and time
#'
getTimeStampText <- function() {
  current_time <- Sys.time()
  return(format(current_time, "%Y%m%d_%H%M%S"))
}

#' Get Time Stamp Text
#'
#' (Deprecated) Provides a standardized text based time stamp for inclusion in file names
#'
#' @return A string with the current date and time
#'
GetTimeStampText <- function() {
  cat("WARNING - Call to \"GetTimeStampText()\" should be updated to \"getTimeStampText()\"")
  return(getTimeStampText())
}

#' Get Month End Date
#'
#' A helper function for getting the end date for the month of the date provided
#'
#' @param date_param Date with month that you are requesting the end date for.
#'
#' @return A date for the last day of the month
getMonthEndDate <- function(date_param) {

  next_month <- as.integer(format(date_param, "%m")) + 1
  next_year <- as.integer(format(date_param, "%Y"))
  if (next_month > 12) {
    next_month <- 1
    next_year <- next_year + 1
  }

  result <- as.Date(paste(next_year, next_month, 1, sep = "-")) - 1
  return(result)
}

#' Parse dates.  This function tries to parse using the ymd then myd and finally dmy format.
#'
#' @param date_text Dates to parse
#'
#' @importFrom lubridate parse_date_time as_date
#'
parseDate <- function(date_text) {
  date_values <-
    parse_date_time(date_text,
                    orders = c("Ymd", "mdY", "dmY", "dbY", "dby")) %>%
    as_date()
  return(date_values)
}

#' Write Protected CSV File
#'
#' A helper function for writing CSV files and setting them to readonly for protection.
#' When writing the file, the file is first set to writable then written over then set to readonly
#'
#' @param data Data frame to be written to CSV file
#' @param file_name File name to write CSV file to
#'
#' @importFrom utils write.csv
#'
writeProtectedCsv <- function(data, file_name) {
  Sys.chmod(file_name, mode = "0777", use_umask = TRUE)
  write.csv(data, file = file_name, row.names = FALSE)
  #Make the file read-only to prevent accidental modificaiton/deletion
  Sys.chmod(file_name, mode = "0444", use_umask = TRUE)
}

#' Validate Value domains
#'
#' A helper function for validating data within a perdefined set of possible values.
#'
#' @param values The data that is to be validated
#' @param domain The set of valid values that "values" can take on.
#' @param error_message A template of a printf message for invalid values.
#'
#' @return The values provided
#'
#' @note If the values vector contains values that are not in the domain vector, then the method calls stop
#'   with an error message using the template provide in the "error_message" parameter.
validateValueDomain <- function(values,
                                domain,
                                error_message = "The following values are invalid:\n\n%s\n\n") {
  invalid_values <- values %notin% domain
  if (any(invalid_values)) {
    invalid_values <- values[invalid_values]
    invalid_values <- unique(invalid_values)
    stop(sprintf(error_message, paste(invalid_values, collapse = ", ")))
  }
  return(values)
}

#' Convert Column Types
#'
#' Convert column types of a data frame to match another tibble with the same column names.
#' This function helps to setup tibbles to use dplyr::bind_rows with
#'
#' NOTE: the column names must be exactly the same between the two tibbles
#'
#' @param df The tibble whos columns are to be converted
#' @param df_to_match The tibble with the column type definitions
#'
#' @return The df tibble with the column types converted to match the df_to_match tibble
#'
#' @importFrom tidyr gather nest unnest
#' @importFrom dplyr summarize_all vars
#'
convertColTypes <- function(df, df_to_match) {
  col_types <-
    df_to_match %>%
    summarize_all(class) %>%
    gather(col_name, col_type) %>%
    nest(data = -col_type)

  for (colIdx in 1:nrow(col_types)) {
    type_name <- pull(col_types[colIdx,1])
    col_names <-
      col_types[colIdx,] %>%
      unnest(cols = data) %>%
      pull(col_name)
    if ("character" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.character)
    } else if ("numeric" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.double)
    } else if ("integer" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.integer)
    } else if ("logical" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.logical)
    } else if ("Date" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), parseDate)
    } else {
      stop(glue("Unknown column type '{type_name}': {paste0(col_names, collapse=',')}"))
    }
  }

  return(df)
}


#' Load Analysis Year Config
#'
#' Load the Year configuration file, adjust file names to full paths
#'
#' @param lic_year Licence year text to load configuraion for (e.g "2016-17")
#' @param year_config_filename Path to the year configuration file relative to survey config
#' @param irec_dir_root The root directory of the iRec Survey Software
#'
#' @return A list of configuration parameters
#'
#' @importFrom yaml yaml.load_file
#' @importFrom fs path path_norm file_exists path_abs
#' @importFrom stringr str_replace_all
#' @importFrom lubridate ymd year day
#' @importFrom dplyr %>%
#'
loadAnalysisYearConfig <- function(lic_year,
                                   year_config_filename = "year_config.yml",
                                   irec_dir_root = getiRecAnalysisDir()) {

  if (is.numeric(lic_year)) {
    lic_year <- getLicenceYearText(lic_year)
  }

  ### Load Year Configuration
  year_data_path <- getSurveyYearPath(lic_year, irec_dir_root)

  year_config_filename <-
    path(year_data_path, year_config_filename) %>%
    path_norm()

  if (file_exists(year_config_filename) == FALSE) {
    stop(glue("No year configuration file: {year_config_filename}"))
  }

  year_config <- yaml.load_file(year_config_filename)

  names(year_config) <- str_replace_all(names(year_config),
                                        "[.]",
                                        "_")

  year_config$year_data_path <- year_data_path

  year_config$annual_expire_date <- ymd(year_config$annual_expire_date)

  year_config$annual_date_range <- c(year_config$annual_expire_date,
                                     year_config$annual_expire_date)

  #for some reason to set year you need to reference the lubridate package
  lubridate::year(year_config$annual_date_range[1]) <- year(year_config$annual_date_range[1]) - 1
  lubridate::day(year_config$annual_date_range[1]) <- day(year_config$annual_date_range[1]) + 1

  year_config$licence_filename <-
    path(year_data_path, year_config$licence_filename) %>%
    path_norm()

  year_config$annual_irec_result_filename <-
    path(year_data_path, glue(year_config$annual_irec_result_filename)) %>%
    path_norm()

  year_config$annual_iarc_result_filename <-
    path(year_data_path, glue(year_config$annual_iarc_result_filename)) %>%
    path_norm()

  year_config$vendor_sales_filename <-
    path(year_data_path, year_config$vendor_sales_filename) %>%
    path_norm()

  year_config$drawn_licence_filename <-
    path(year_data_path, year_config$drawn_licence_filename) %>%
    path_norm()

  year_config$exclude_filename <-
    path(year_data_path, year_config$exclude_filename) %>%
    path_norm()

  year_config$survey_db_path <-
    path(year_data_path, "../data/") %>%
    path_abs()

  return(year_config)
}


#' Load Analysis Configuration
#'
#' Load the Year and Survey specific configuration file, adjust file names to full paths
#'
#' @param lic_year Licence year text to load configuraion for (e.g "2016-17")
#' @param month_name Survey month name to load survey configuration for (e.g. "August")
#' @param year_config_filename Path to the year configuration file relative to survey config
#' @param survey_config_filename Survey configuration file name
#' @param irec_dir_root Root directory will iRec data
#' @param year_config Year configuration list if it has already been loaded
#'
#' NOTE: For iArc surveys, the month_name is "iArc"
#'
#' @return A list of configuration parameters
#'
#' @importFrom glue glue
#' @importFrom lubridate days
#' @importFrom fs path path_norm
#' @importFrom yaml yaml.load_file
#' @importFrom stringr str_replace_all
#'
#' @export
#'
loadAnalysisConfig <- function(lic_year,
                               month_name,
                               year_config_filename = "year_config.yml",
                               survey_config_filename = "survey_config.yml",
                               irec_dir_root = getiRecAnalysisDir(),
                               year_config = NULL) {

  if (is.null(year_config)) {
    year_config <- loadAnalysisYearConfig(lic_year, year_config_filename, irec_dir_root)
  }

  ### Load Survey Month Configuration
  survey_data_path <- getSurveyPath(month_name,
                                    year_config$year_data_path,
                                    year_config$annual_expire_date)

  survey_config_filename <- path(survey_data_path, survey_config_filename)
  survey_config <- yaml.load_file(survey_config_filename)
  names(survey_config) <-
    str_replace_all(names(survey_config),
                    "[.]",
                    "_")

  config <- c(year_config, survey_config)

  config$survey_data_path <- survey_data_path

  if(is.null(config$period_stratify)) {
    config$period_stratify <- FALSE
  } else if (!is.logical(config$period_stratify)) {
    stop(glue("Invalid period_stratify value ({config$period_stratify}), must be yes/no"))
  }


  if(!is.null(config$period_stratify_date)) {
    config$period_stratify_date <- ymd(config$period_stratify_date)
  } else {
    config$period_stratify_date <- as.Date(NA)
  }

  if (!is.null(config$analysis_pre_filename)) {
    config$analysis_pre_filename <-
      path(survey_data_path, config$analysis_pre_filename)
  } else {
    analysis_pre_filename <- NA_character_
  }

  if (!is.null(config$analysis_post_filename)) {
    config$analysis_post_filename <-
      path(survey_data_path, config$analysis_post_filename)
  } else {
    analysis_post_filename <- NA_character_
  }

  survey_start_date <- config$survey_start_date

  config$analysis_result_filename <-
    path(survey_data_path,
         glue(survey_config$analysis_result_filename)) %>%
    path_norm()

  config$survey_start_date <- ymd(config$survey_start_date)
  config$survey_dates <- c(config$survey_start_date,
                           config$survey_start_date + months(1) - days(1))

  if (!is.null(config$ekos_filename) && is.null(config$survey_result_filename)) {
    config$survey_result_filename <- config$ekos_filename
  }

  config$survey_result_filename <-
    path(survey_data_path, config$survey_result_filename) %>%
    path_norm()

  config$survey_month <- month.name[month(config$survey_start_date)]

  if (!is.null(config$survey_adj_filename)) {
    config$survey_adj_filename <-
      path(survey_data_path, config$survey_adj_filename)
  }

  return(config)
}

#' Converts a Licence year (e.g 2016) into a licence year text.
#' Commonly used for displaying to users or the folder name of the data
#'
#' @param lic_year Licence year vector to convert to licence year text
#'
#' @return A vector of characters
#'
#' @importFrom stringr str_c
#'
#' @export
#'
getLicenceYearText <- function(lic_year) {
  return(str_c(lic_year, "-", (lic_year + 1) %% 100))
}

#' Get Survey Year Path
#'
#' Get the survey path based on survey parameters.
#'
#' @param lic_year Licence year text (e.g. "2013-14")
#' @param irec_dir_root Root directory for iRec Analysis
#'
#' @return An absolute file path where the survey year data should be available
#'
#' @importFrom glue glue
#' @importFrom fs path_abs path
#'
#'
getSurveyYearPath <- function(lic_year, irec_dir_root = getiRecAnalysisDir()) {
  year_data_path <-
    path(irec_dir_root, glue("{lic_year} surveys")) %>%
    path_abs()
  return(year_data_path)
}

#' Get Survey Path
#'
#' Get the survey path based on survey parameters.
#'
#' @param month_name Name of month for the survey
#' @param year_data_path File path to the year data
#' @param annual_expire_date Date that the annual licence expires (e.g. March 31st)
#'
#' @return An absolute file path where the survey data should be availabe
#'
#' @importFrom lubridate month
#'
getSurveyPath <- function(month_name, year_data_path, annual_expire_date) {

  if (grepl(month_name, IArcSurveyName, ignore.case = TRUE)) {
    lic_month_idx <- 13
  } else {
    lic_year_month_cut <-
      month(annual_expire_date) %>%
      as.integer()

    lic_month_idx <- which(grepl(month_name, month.name, ignore.case = TRUE)) - lic_year_month_cut
    if (lic_month_idx < 1L) {
      lic_month_idx <- 12L + lic_month_idx
    }
  }

  survey_data_path <-
    fs::path(year_data_path, glue("{lic_month_idx} {month_name}")) %>%
    fs::path_abs()

  return(survey_data_path)
}


#' Get Truncated Text
#'
#' Provides a nicer chunk of text to display to the user by limiting to the first 'show.n' elements
#' for the vector of 'values' and providing text identifying how many more are in the vector
#'
#' @param values A vector of values to turn into text
#' @param show_n How many elements to show before truncating
#' @param collapse String used to combine the multiple vaues
#'
#' @return Text with the a vector of values to print to a log file
#'
#' @importFrom stringr str_c
#' @importFrom glue glue
#'
getTruncText <- function(values, show_n = 10, collapse = "\n") {
  text <- ""
  text <- str_c(values[1:min(length(values), show_n)], collapse = collapse)
  if (length(values) > show_n) {
    text <- glue("{text}{collapse}...+{length(values) - show_n} more...")
  }
  return(text)
}


#' Convert SPSS labels to text vector
#'
#' Columns read from an SPSS file with the haven R package provides labelled columns.
#' This function converts the labels (that are kind of like factors) into a text column.
#'
#' @param values A vector of labelled values
#'
#' @return Text vector
#'
#' @importFrom haven as_factor
#'
labelText <- function(values) {
  text <-
    values %>%
    haven::as_factor() %>%
    as.character()

  return(text)
}

#' Set iRecAnalysis Directory
#'
#' @param dir Root directory of iRecAnalysis data
#'

setiRecAnalysisDir <- function(dir) {
  log_env <- .GlobalEnv
  log_env$IRecAnlysisDir <- normalizePath(dir)
}

#' Set iRecAnalysis Directory
#'
#' @return Root directory of iRecAnalysis data
#'

getiRecAnalysisDir <- function() {
  log_env <- .GlobalEnv
  return(log_env$IRecAnlysisDir)
}
