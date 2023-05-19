

LicenceMonthStart <- 4 #licences start April 1st

AgeCatJuvenile <- "Juvenile (under 16 years)"
AgeCatAdult <- "Adult (16-64)"
AgeCatSenior <- "Senior (65+)"
AgeCatAdultSenior <- "All Ages above 15 (Pacific)"
AgeCatValid <- c(AgeCatJuvenile, AgeCatAdult, AgeCatSenior, AgeCatAdultSenior)

ResStatusRes <- "Resident"
ResStatusNon <- "Non-Resident"
ResStatusValid <- c(ResStatusRes, ResStatusNon)

LicTypeAnnual <- "Annual"
LicType5Day <- "5 Days"
LicType3Day <- "3 Days"
LicType1Day <- "1 Day"
LicTypeTerm <- c(LicType5Day, LicType3Day, LicType1Day)
LicTypeValid <- c(LicTypeAnnual, LicTypeTerm)

PeriodTypeMonthlyId <- 1
PeriodTypeAnnualId <- 2
PeriodTypeValidId <- c(PeriodTypeMonthlyId, PeriodTypeAnnualId)

DrawTypePost <- "Post"
DrawTypePre <- "Pre"
DrawTypeUnspec <- "Unspecified"

PraStartDate <- as.Date("2015-07-01")

PraDidNotFish <- 1 # "Did not fish in the month"
PraDidFish <- 2 # "Fished in the month"
PraDidFishValid <- c(PraDidNotFish, PraDidFish)

PraCompleteSurvey <- "Complete"
PraIncompleteSurvey <- "Incomplete"
PraCompleteSurveyValid <- c(PraCompleteSurvey, PraIncompleteSurvey)

#' Load Exclude File
#'
#' Read in the exclude csv file of licence IDs or Survey Keys that are to be
#' excluded from iRec Survey
#'
#' @param exclude_file_name File name of excluded licences or survey access code
#'
#' @return A data frame of licence_id or survey_access_key to exclude
#'
loadExcludeFile <- function(exclude_file_name = NA) {
  key_col_names <- c("licence_id", "survey_access_key")

  if (length(exclude_file_name) == 0 || is.null(exclude_file_name)) {
    return(NULL)
  }
  if (is.na(exclude_file_name)) {
    return(NULL)
  }

  if (fs::file_exists(exclude_file_name) == FALSE) {
    addLogMessages("WARNING - exclude file '{exclude_file_name}' does not exist")
    return(NULL)
  }

  exclude_df <- read_csv(exclude_file_name)
  if ("licence.id" %in%  names(exclude_df)) {
    exclude_df %<>%
      rename(licence_id = licence.id)
  }

  found_key_col <-
    names(exclude_df) %>%
    dplyr::intersect(key_col_names)

  if (length(found_key_col) == 0) {
    key_col_txt <-
      stop(glue("No ",
                str_c("\"", key_col_names, "\"", collapse=" or "),
                " columns identified in the exclude file"))
  }

  return(exclude_df)
}

#' Get Licence Year Start
#'
#' Helper method to get the starting date for the a licence year, inferred from provided date.
#'
#' @param survey_start_date Date a survey is carried out for.
#'
#' @return A start date for the licence year that the survey_start_date falls within
#'
getLicenceYearStart <- function(survey_start_date) {
  year_range <- getSurveyYears(survey_start_date)
  return(as_date(paste0(year_range[1], "-", LicenceMonthStart, "-01")))
}

#' Get Licence Year Start Date
#'
#' Helper method to get the starting date for the a licence year,
#' inferred from provided licence year end date.
#'
#' @param lic_end_date Date a licence year ends
#'
#' @return Start date for the licence year
#'
#' @importFrom lubridate years days
#'
getLicYearStartDate <- function(lic_end_date) {
  lic_start_date <- lic_end_date - lubridate::years(1) + lubridate::days(1)
  return(lic_start_date)
}

#' Get Survey Years
#'
#' Helper method to get the start and end year of the current licence year based on a survey.start.date
#'
#' @param survey_start_date Date a survey is carried out for
#'
#' @return A vector with two elements - e.g  c(Start Year, End Year)
#'
getSurveyYears <- function(survey_start_date) {
  month <- as.integer(format(survey_start_date, "%m"))
  year <- as.integer(format(survey_start_date, "%Y"))

  year_range <- c()
  #The licence year starts on April 1 of each year, so if after March then the licence year runs into the next year
  if (month >= LicenceMonthStart) {
    year_range <- c(year, year + 1)
  } else {
    year_range <- c(year - 1, year)
  }
  return(year_range)
}

#' LOad Survey Results
#'
#' Load Survey Results from either EKOS or PRA into a common format.
#'
#' @param survey_result_filename Survey result file name
#' @param survey_start_date Survey start data
#' @param exclude_lic_filename Exclusion licence file name
#' @param survey_adj_filename Survey adjustment file name (no adjustment applied if file name is excluded)
#'
#' @return A data frame with survey results with common stratification columns in common format
#'
#' @importFrom dplyr as_tibble if_else sym pull bind_rows filter
#' @importFrom stringr str_length str_to_lower str_sub
#' @importFrom glue glue
#'
loadSurveyResults <- function(survey_result_filename,
                              survey_start_date,
                              exclude_lic_filename,
                              survey_adj_filename) {
  exclude_id <- c()

  exclude_lic <- loadExcludeFile(exclude_lic_filename)
  if (!is.null(exclude_lic) && nrow(exclude_lic) > 0) {
    if ("licence_id" %in% colnames(exclude_lic)) {
      exclude_id <- pull(exclude_lic, licence_id)
    } else {
      exclude_id <- pull(exclude_lic, survey_access_key)
    }
  }

  if (survey_start_date < PraStartDate) {
    survey_results <-
      loadEkosSurveyResults(survey_result_filename, exclude_id)
  } else {
    survey_results <-
      loadPraSurveyResults(survey_result_filename, exclude_id)
  }

  id_col_name <-
    if_else("surveykey" %in% colnames(survey_results),
            "surveykey",
            "licence_id") %>%
    sym()

  if (!is.null(survey_adj_filename)) {
    if (!is.na(survey_adj_filename) &
       str_length(survey_adj_filename) > 0) {

      if (!file_exists(survey_adj_filename)) {
        addLogMessages("WARNING-The specified adjustment file does not exist: {survey_adj_filename}")
      } else {
        #Adjustments provided, so apply...
        adjust <-
          loadSurveyAdjustments(survey_adj_filename) %>%
          convertColTypes(survey_results)

        adjust_lic_ids <-
          adjust %>%
          pull(!!id_col_name) %>%
          unique()

        survey_results <-
          survey_results %>%
          filter(!!id_col_name %notin% adjust_lic_ids) %>%
          bind_rows(adjust)
      }
    }
  }

  return(survey_results)
}

#' Load EKOS Survey Results
#'
#' Load survey results from a EKOS survey file format
#'
#' @param ekos_filename File name of the Ekos survey results
#' @param exclude_lic_id A vector licences IDs to exclude from the results
#'
#' @return The survey results loaded from an Ekos file.
#'
#' @importFrom haven read_spss
#' @importFrom stringr str_trim
#' @importFrom dplyr mutate_all inner_join vars
#'
loadEkosSurveyResults <- function(ekos_filename, exclude_lic_id) {
  #The number below identifies the maximum total proportion that does not have a Licence ID in the results
  # For example, 0.01 means that if more then 1% of ekos records is missing licence IDs then throw an error.
  max_prop_miss_id <- 0.01

  #Ekos file name is in SPSS format.
  ekos_data <-
    read_spss(ekos_filename) %>%
    as_tibble()

  total_ekos_rows <- nrow(ekos_data)
  if (total_ekos_rows == 0) {
    stop("The Ekos file has no responses, please check the file and contact Ekos for an upated file.")
  }

  #remove results that do not have a licence ID, these seem like dumby responses
  ekos_data <-
    ekos_data %>%
    filter(!is.na(Licence_ID))

  if ( total_ekos_rows > nrow(ekos_data)) {
    addLogMessages("{total_ekos_rows - nrow(ekos_data)} Ekos result records are missing ",
                   "licence IDs")

    if ((total_ekos_rows - nrow(ekos_data)) > (total_ekos_rows * max_prop_miss_id)) {
      stop(glue("More then {format(max_prop_miss_id * 100.0, 3)}% ",
                "of {total_ekos_rows} Ekos result records are missing licence IDs"))
    }
  }

  ekos_data <-
    ekos_data %>%
    mutate(Licence_ID = trimws(Licence_ID),
           DNF_1 = !is.na(DNF_1),
           First_name = trimws(First_name),
           Last_name = trimws(Last_name),
           AMAIL = trimws(AMAIL),
           REPDAY = as.integer(REPDAY - 1000),
           REPYEAR = as.integer(REPYEAR)) %>%
    rename(did_not_fish = DNF_1,
           licence_id = Licence_ID,
           first_name = First_name,
           last_name = Last_name,
           email = AMAIL,
           area = REPZONE,
           method = REPMETHOD,
           year = REPYEAR,
           month = REPMONTH,
           day = REPDAY,
           effort_days = COMPLETE) %>%
    mutate_at(vars(area, method, month), labelText)

  if (!is.null(exclude_lic_id) && length(exclude_lic_id) > 0) {
    ekos_data <-
      ekos_data %>%
      filter(licence_id %notin% exclude_lic_id)
    if (total_ekos_rows > nrow(ekos_data)) {
      addLogMessages("Based on excluded licence IDs, {total_ekos_rows - nrow(ekos_data)}",
                     " Ekos result records were exclude from analysis.")
    }
  } else {
    addLogMessages("WARNING - no licences were excluded from the EKOS results")
  }

  complete_surveys <-
    ekos_data %>%
    filter(effort_days == 1 | did_not_fish == TRUE)

  addLogMessages("Total number of incomplete survey responses: ",
                 "{nrow(ekos_data) - nrow(complete_surveys)}")

  ekos_data <-
    complete_surveys %>%
    mutate(effort_days = if_else(did_not_fish == 1, 0, effort_days))

  if ("QLODGE" %in% names(ekos_data)) {
    ekos_data <-
      ekos_data %>%
      rename(lodge = QLODGE,
             guided = QGUIDE)

    ekos_last_catch_col <- which(names(ekos_data) == "REVIEW") - 1
    ekos_data_catch <- ekos_data[,c(13,16:ekos_last_catch_col)]

    later_col <- which(names(ekos_data) %in% c("effort_days", "did_not_fish"))
    ekos_data_strata <- ekos_data[,c(3:11,14:15,later_col)]

    ekos_data_strata <-
      ekos_data_strata %>%
      mutate(lodge = coalesce(as.character(lodge), UnspecifiedText),
             guided = coalesce(as.character(guided), UnspecifiedText))

  } else {
    #Past ekos result files do not have the QLODGE or QGUIDE columns
    ekos_data_catch <- ekos_data[,13:338]
    ekos_data_strata <-
      ekos_data[,c(3:11,341:342)] %>%
      mutate(lodge = UnspecifiedText,
             guided = UnspecifiedText)
  }

  #ensure the order of the strata columns, important for column indexing later.
  ekos_data_strata <-
    ekos_data_strata %>%
    select(licence_id,
           first_name,
           last_name,
           email,
           area,
           method,
           year,
           month,
           day,
           guided,
           lodge,
           did_not_fish,
           effort_days)

  #extracting all of the columns in the df starting with A
  ekos_data_catch <- ekos_data_catch[,str_sub(colnames(ekos_data_catch), 1, 1) == "A"]
  #Remove the leading "A" from from the catch data fields
  colnames(ekos_data_catch) <-
    str_sub(colnames(ekos_data_catch), 2) %>%
    str_to_lower()

  ekos_data_catch <-
    ekos_data_catch %>%
    mutate_all(coalesce, 0)

  ekos_data <-
    bind_cols(ekos_data_strata, ekos_data_catch) %>%
    rename(juv_effort_days = juvepres)

  return(ekos_data)
}

#' Load PRA Survey Results
#'
#' Loads the survey results from PRA and merging the data into common format
#' for analysis
#'
#' @param survey_result_filename The file name that the PRA survey results saved too
#' @param exclude_id A vector licence_id(s) or survey_access_key(s) that should have their survey results
#'                excluded from analysis
#' @param strata_col_names Strata and analysis columns to preserve in loaded file
#'
#' @return A tibble with the survey results
#'
#' @importFrom haven zap_labels as_factor is.labelled
#' @importFrom dplyr everything mutate_if
#' @importFrom haven read_spss
#' @importFrom stringr str_replace_all
#'
loadPraSurveyResults <- function(survey_result_filename,
                                 exclude_id,
                                 strata_col_names) {


  #  This commented code is another approach to loading SPSS
  #   survey.importer <- spss.system.file(survey.result.filename)
  #   survey.data <- as.data.set(survey.importer)
  #
  #   strata.names <- names(survey.data)[c(1:6, 11:12, 14:15)]
  #   catch.names <- names(survey.data)[c(13, 16:161)]
  #
  #   strata.data <- subset(survey.data, select=c(1:6, 11:12, 14:15))
  #
  #   catch.data <- data.frame(as.integer(survey.data[[catch.names[1]]]))
  #   for (col.idx in 2:length(catch.names)) {
  #     catch.data <- cbind(catch.data, as.integer(survey.data[[catch.names[col.idx]]]))
  #   }
  #   names(catch.data) <- catch.names
  #
  #   survey.result <- data.frame(strata.data, catch.data)


  #After some trial and error, the below "read_spss" function from
  survey_result <-
    read_spss(survey_result_filename) %>%
    as_tibble() %>%
    select_all(tolower)

  survey_result_spp <-
    survey_result %>%
    select()

  strata_col_names <-
    c("licence_id",
      "surveykey",
      "first_name",
      "last_name",
      "email",
      "didnotfish",
      "completesurvey",
      "datefished",
      "area",
      "method",
      "fishedfromlodge",
      "fishedwithguide",
      "totaljuveniles",
      "checkprawns",
      "checkcrabs",
      "checkcrabsprawns") %>%
    intersect(colnames(survey_result))

  survey_result <-
    survey_result %>%
    select(strata_col_names,
           getCatchColNames(survey_result))

  #The number below identifies the maximum total proportion that does not have a Licence ID in the results
  # For example, 0.01 means that if more then 1% of PRA records is missing licence IDs then throw an error.
  max.prop.miss.id <- 0.01

  total_rows <- nrow(survey_result)
  if (total_rows == 0) {
    stop("The PRA file has no responses, please check the file and contact PRA for an upated file.")
  }

  key_id <- ""
  if ("licence_id" %in% names(survey_result)) {
    key_id <- "licence_id"
    survey_result <-
      survey_result %>%
      mutate(licence_id = str_trim(str_replace_all(licence_id, "[.]", "")))
  } else if ("surveykey" %in% names(survey_result)) {
    key_id <- "surveykey"
  } else {
    stop("No key column found in survey results!")
  }
  key_id <- sym(key_id)

  #remove results that do not have a licence ID or survey keys, these seem like dumby responses
  survey_result <-
    survey_result %>%
    filter(!is.na(!!key_id)) %>%
    mutate_at(vars(!!key_id), str_trim)

  survey_result <-
    survey_result %>%
    mutate(area = str_trim(area),
           method = str_trim(method)) %>%
    mutate(area = if_else(str_length(area) == 0,
                          NA_character_,
                          area),
           method = if_else(str_length(method) == 0,
                            NA_character_,
                            method),
           didnotfish = validateValueDomain(didnotfish, PraDidFishValid))

  if (total_rows > nrow(survey_result)) {
    addLogMessages("{total_rows - nrow(survey_result)} PRA result records are missing licence IDs or survey keys")
    if ((total_rows - nrow(survey_result)) > (total_rows * max.prop.miss.id)) {
      stop(sprintf("More then %.3f%% of %d PRA result records are missing licence IDs or survey keys\n", max.prop.miss.id * 100.0, total_rows))
    }
  }

  #The effort days for each row is 1 if they actual fished (!DidNotFish)
  survey_result <-
    survey_result %>%
    mutate(didnotfish = if_else(didnotfish == PraDidNotFish, TRUE, FALSE)) %>%
    mutate(effort_days = if_else(didnotfish, 0L, 1L)) %>%
    rename(did_not_fish = didnotfish)

  if (!is.null(exclude_id) && any(!is.na(exclude_id))) {
    pre.exclude.total.rows <- nrow(survey_result)

    survey_result <-
      survey_result %>%
      filter(!!key_id %notin% exclude_id)

    if (pre.exclude.total.rows > nrow(survey_result)) {
      addLogMessages("Based on excluded licence IDs/access keys, ",
                     "{pre.exclude.total.rows - nrow(survey_result)} PRA result records were exclude from analysis.\n")
    }
  } else {
    addLogMessages("WARNING - no licences were identified for exclusion from the PRA results")
  }

  #make sure the licence ID is character as the licence file is loaded with character licence ID
  if ("licence_id" %in% colnames(survey_result)) {
    if (is.character(survey_result$licence_id) == FALSE) {
      survey_result <-
        survey_result %>%
        mutate(licence_id = as.character(licence_id))
    }
  }

  survey_result <-
    survey_result %>%
    mutate(completesurvey = validateValueDomain(str_trim(completesurvey),
                                                PraCompleteSurveyValid)) %>%
    mutate(completesurvey = completesurvey == PraCompleteSurvey)

  incomplete_total <-
    survey_result %>%
    filter(completesurvey == FALSE) %>%
    count() %>%
    pull()


  addLogMessages("Total number of incomplete survey responses: {incomplete_total}\n")

  #Reuse the Complete Survey column to use as effort days value
  survey_result <-
    survey_result %>%
    filter(completesurvey == TRUE) %>%
    rename(lodge = fishedfromlodge,
           guided = fishedwithguide,
           juv_effort_days = totaljuveniles) %>%
    select(-completesurvey)

  strata_col_names <-
    c("licence_id",
      "surveykey",
      "first_name",
      "last_name",
      "email",
      "did_not_fish",
      "datefished",
      "area",
      "method",
      "lodge",
      "guided",
      "checkcrabs",
      "checkprawns",
      "checkcrabsprawns") %>%
    intersect(colnames(survey_result))

  #Catch columns are all columns that are not stratafication fields
  #The "select(effort_days, everything())" moves the effort_days column to the first column
  survey_result_catch <-
    survey_result %>%
    select(getCatchColNames(survey_result)) %>%
    select(effort_days, everything()) %>%
    mutate_if(is.integer,coalesce, 0L) %>% #autofill all the empty catch cells with zero
    mutate_if(is.double,coalesce, 0)

  factor_to_char_col <-
    c("lodge", "guided", "checkcrabs", "checkprawns", "checkcrabsprawns") %>%
    base::intersect(colnames(survey_result))

  survey_result_strata <-
    survey_result %>%
    select(strata_col_names) %>%
    mutate_at(factor_to_char_col, ~ coalesce(labelText(.), UnspecifiedText))

  if("checkcrabsprawns" %in% factor_to_char_col) {
    survey_result_strata <-
      survey_result_strata %>%
      mutate(checkcrabsprawns = str_replace_all(checkcrabsprawns, "rrawn", "prawn"))

  }

  #ensure the order of the strata columns, important for column indexing later.
  survey_result <-
    survey_result_strata %>%
    bind_cols(survey_result_catch) %>%
    mutate_if(is.labelled, zap_labels)

  return(survey_result)
}


#' Load Survey Adjustments
#'
#' Loads and validates the survey adjustment file
#'
#' @param survey_adj_filename CSV file name of survey adjustments
#'
#' @return A tibble of updated survey data to replace original survey results
#'
#' @importFrom dplyr distinct count
#' @importFrom readr read_csv cols
#'
loadSurveyAdjustments <- function(survey_adj_filename) {
  adj_data <-
    read_csv(survey_adj_filename, col_types = cols(.default = "c")) %>%
    mutate(did_not_fish = str_to_lower(did_not_fish)) %>%
    mutate(did_not_fish = case_when("yes" == did_not_fish ~ TRUE,
                                    "no" == did_not_fish ~ FALSE,
                                    TRUE ~ NA)) %>%
    select_all(str_to_lower)


  id_col_name <-
    if_else("surveykey" %in% colnames(adj_data),
            "surveykey",
            "licence_id") %>%
    sym()
  #Identify if and licence IDs have mixed fishing/did not fish records
  mix_dnf <-
    adj_data %>%
    distinct(!!id_col_name, did_not_fish) %>%
    count(!!id_col_name) %>%
    filter(n > 1)

  if (nrow(mix_dnf) > 0) {
    mix_dnf_lic_text <-
      pull(mix_dnf, !!id_col_name) %>%
      paste0(collapse = ",")

    error_msg <- glue("Can not load survey adjustment file because the following licence IDs have a mix of fishing and did not fish:\n",
                      mix_dnf_lic_text,
                      "\n")
    stop(error_msg)
  }

  catch_col_names <- getCatchColNames(adj_data)

  mix_na <-
    adj_data %>%
    group_by(!!id_col_name) %>%
    summarize_at(catch_col_names, function(.) {all(is.na(.)) | all(!is.na(.))}) %>%
    ungroup() %>%
    gather(col_name, status, -!!id_col_name) %>%
    filter(status == FALSE) %>%
    select(-status)

  if (nrow(mix_na) > 0) {
    mix_na_lic_text <- str_c(pull(mix_na, !!id_col_name),
                             pull(mix_na, col_name),
                             sep=",",
                             collapse="\n")
    error_msg <- glue("Can not load survey adjustment file because the following licence IDs have a mix of NA and values for particular catch columns:\n{mix_na_lic_text}\n")
    stop(error_msg)
  }

  return(adj_data)
}

#' Subset Licences
#'
#' Limit licences to those eligible for a survey
#'
#' @param licence_df A data frame of all licences
#' @param start_survey_date start date of the survey (used to limit terms)
#' @param end_survey_date The date licence records are cut off at, this date
#'   will be used to exclude licence records that start after this date.
#'
#' @return A data frame with the licence records limited to those aligning with a survey
#'
subsetLicences <- function(licence_df, start_survey_date, end_survey_date) {
  #Remove licences that start on a date after the provided cutoff date.
  licence_df <-
    licence_df %>%
    filter(start_date <= end_survey_date)

  #Remove annual licences issued on the survey end date. Annual licences good for only 1 day in the survey period are excluded.
  annual_licence_df <-
    licence_df %>%
    filter(licence_type == LicTypeAnnual & start_date < end_survey_date)

  # With Instantaneous Licence Selection, term licences are limited to the month
  # that the majority of days occur.  So a licence that starts July 29th will have
  # 3 days in July and 2 days in August.  Because most of the days are in July, the
  # licence is valid for July only for the purposes of analyzing instantaneous licence
  # selection results.
  if (year(getLicenceYearStart(start_survey_date)) >= InstLicYearStart) {
    #the term licence mid point halfway between start and end date must be between survey dates
    term_licence_df <-
      licence_df %>%
      mutate(mid_date = start_date + trunc((end_date - start_date)/2L)) %>%
      filter(licence_type %in% LicTypeTerm,
             mid_date >= start_survey_date,
             mid_date <= end_survey_date) %>%
      mutate(mid_date = NULL)
  } else {
    term_licence_df <-
      licence_df %>%
      filter(licence_type %in% LicTypeTerm)
  }

  licence_df <-
    annual_licence_df %>%
    bind_rows(term_licence_df)

  return(licence_df)
}

#' Load Vendor Sales
#'
#' Loads the full set of vendor sales totals by month and licence categories (e.g. annual/term).
#'	Based on the survey month and the annual licence expire date, the months total sales are summarized into
#'   those that occurred before the month and in the month.
#'
#' @param vendor_sales_filename The file name that has vendors sales by month for all 12 months of the licence year.
#' @param survey_month The month name of the current month that is being analyzed
#' @param annual_expire_date The end date of the current annual licence year.
#'
#' @return A data frame with the vendor sales summarized to the licence types for before- and in-month sales.
#'
#' @importFrom dplyr contains n_distinct bind_cols rename
#'
loadVendorSalesData <- function(vendor_sales_filename,
                                survey_month,
                                annual_expire_date) {
  if (survey_month %notin% month.name) {
    stop(glue("Invalid survey month {survey.month} provided for loading the vendor sales file."))
  }

  survey_month_id <- which(month.name == survey_month)

  if (length(vendor_sales_filename) == 0) {
    stop(glue("No vendor sales filename defined in the year configuration file."))
  }

  if (fs::file_exists(vendor_sales_filename) == FALSE) {
    stop(glue("A vendor sale file '{vendor_sales_filename}' must exist"))
  }

  vendor_data <-
    read_csv(vendor_sales_filename) %>%
    rename(resident_status = resident.status,
           licence_type = licence.type,
           age_category = age.category)

  validateValueDomain(vendor_data$resident_status,
                      ResStatusValid,
                      "The following resident status values in the vendor sales file are invalid:\n\n%s\n\n")

  validateValueDomain(vendor_data$licence_type,
                      LicTypeValid,
                      "The following licence types in the vendor sales file are invalid:\n\n%s\n\n")

  validateValueDomain(vendor_data$age_category,
                      AgeCatValid,
                      "The following age categories in the vendor sales file are invalid:\n\n%s\n\n")

  vendor_months <- names(vendor_data)[4:ncol(vendor_data)]
  validateValueDomain(vendor_months,
                      month.name,
                      "The following month columns in the vendor sales file are invalid:\n\n%s\n\n")

  if (n_distinct(vendor_months) != 12) {
    stop("All 12 months must be provided in the vendor sales file.")
  }

  start_month_id <- as.integer(format(annual_expire_date, "%m")) + 1
  if (survey_month_id == start_month_id) {
    #if this is the first month of the licence year, there are no before months, so pad with zeros
    before_month_total <- rep(0, nrow(vendor_data))
  } else {
    #organize months into the sequence for licence year
    annual_months <- c(month.name[start_month_id:12], month.name[1:start_month_id - 1])
    #subset months to those that are before the current month
    before_months <- annual_months[1:which(survey_month == annual_months) - 1]

    before_month_values <- vendor_data[,names(vendor_data) %in% before_months]
    before_month_total <- 0
    if (is.vector(before_month_values)) {
      before_month_total <- sum(before_month_values, na.rm = TRUE)
    } else {
      before_month_total <- rowSums(before_month_values, na.rm = TRUE, dims = 1)
    }
  }

  month_total <-
    vendor_data %>%
    select(contains(survey_month))

  colnames(month_total) <- "month_total"

  if (sum(month_total, na.rm = TRUE) == 0 && as.integer(format(annual_expire_date, "%Y")) < 2015) {
    stop("No vendor sales provided for the survey month.  You must provide vendor sales for the month to analyze survey results.")
  }

  vendor_summary <-
    vendor_data %>%
    select(resident_status,
           licence_type,
           age_category) %>%
    mutate(before_month_total = before_month_total) %>%
    bind_cols(month_total)

  return(vendor_summary)
}

#' Load Licence File
#'
#' Based on the licence start year, this function reads the appropriate licence file
#' format
#'
#' @param licence_filename File name that licence records are saved in CSV format
#' @param lic_start_date Licence year start date
#' @param lic_end_date Licence year end date
#'
#' @return A data frame with the licence records
#'
#' @importFrom dplyr select_all select mutate mutate_at tally pull case_when count
#' @importFrom magrittr %<>%
#' @importFrom readr read_csv
#' @importFrom lubridate dmy
#' @importFrom stringr str_sub
#'
loadLicenceFile <- function(licence_filename,
                            lic_start_date,
                            lic_end_date) {
  elic_data <-
    loadNrlsLicenceFile(licence_filename,
                        lic_start_date,
                        lic_end_date)

  return(elic_data)
}
