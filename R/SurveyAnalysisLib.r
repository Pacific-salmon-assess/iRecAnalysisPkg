PurPeriodInSurvey <- "In Survey"
PurPeriodPreSurvey <- "Pre-Survey"
PurPeriodEarlySeason <- "Early Season"
PurPeriodLateSeason <- "Late Season"
PurPeriodNotSpec <- "Not Specified"

LicStrataColNames <- c("resident_status", "licence_type", "age_category", "purchase_period")
LicStampStrataColName <- "stamp"


CatchColPrefix <- c("effort_days", "juv_effort_days", "salmon_", "halibut_", "finfish_", "prawncrab_", "bivalves_", "othshellfish_")
#The next regular expression identifies the catch column names
CatchColRegEx <- stringr::str_c("^(", stringr::str_c(CatchColPrefix, collapse="|"), ")")

CatchSampleSizePrefix <- "n_"
#The next regular expression identifies the sample size column names
SampleColRegEx <- stringr::str_c("^(", stringr::str_c(CatchSampleSizePrefix, CatchColPrefix, collapse="|"), ")")


#' Add Draw Type to survey results (e.g. Pre or Post)
#'
#' Adds the draw type based on selection criteria and survey dates for
#' each licence record
#'
#' @param licence_data Data frame of licences
#' @param survey_dates Electronic licences data, including specifics of each licence
#'
#' @return The licence data frame with an additional draw_type column
#'
addDrawType <- function(licence_data, survey_dates) {
  licence_data <-
    licence_data %>%
    mutate(draw_type = if_else(annual_flag, DrawTypePre, NA_character_)) %>%
    mutate(draw_type = if_else(annual_flag &
                                 start_date >= survey_dates[1] &
                                 start_date < survey_dates[2],
                               DrawTypePost,
                               draw_type)) %>%
    mutate(draw_type = if_else(annual_flag == FALSE &
                                 ((start_date >= survey_dates[1] & start_date <= survey_dates[2]) |
                                    (end_date >= survey_dates[1] & end_date <= survey_dates[2])),
                               DrawTypePost,
                               draw_type))

  return (licence_data)
}

#' Add Purchase Periods to licence records (e.g. In-Survey, Pre-Survey, Early-Season, and Late-Season)
#'
#' Adds the purchase period identifiers to each licence record based on the if period stratification is
#' enabled (TRUE) and identify early/late season licences based on the period stratification date
#'
#' @param licence_data Data frame of licences
#' @param survey_dates Electronic licences data, including specifics of each licence
#' @param period_stratify Boolean identifying if period stratification is enabled
#' @param period_stratify_date If splitting into early/late season, this identifies date of split
#'
#' @return The licence data frame with an additional draw_type column
#'
addPurchasePeriod <- function(licence_data, survey_dates, period_stratify, period_stratify_date = NA) {
  if (period_stratify) {

    licence_data <-
      licence_data %>%
      mutate(purchase_period = if_else(annual_flag &
                                         start_date >= survey_dates[1] &
                                         start_date < survey_dates[2],
                                       PurPeriodInSurvey,
                                       NA_character_),
             purchase_period = if_else(annual_flag &
                                         is.na(purchase_period) &
                                         is.na(period_stratify_date),
                                       PurPeriodPreSurvey,
                                       purchase_period),
             purchase_period = if_else(annual_flag &
                                         is.na(purchase_period) &
                                         !is.na(period_stratify_date) &
                                         start_date < period_stratify_date,
                                       PurPeriodEarlySeason,
                                       purchase_period),
             purchase_period = if_else(annual_flag &
                                         is.na(purchase_period) &
                                         !is.na(period_stratify_date) &
                                         start_date >= period_stratify_date,
                                       PurPeriodLateSeason,
                                       purchase_period),
             purchase_period = coalesce(purchase_period, PurPeriodNotSpec))
  } else {
    licence_data <-
      licence_data %>%
      mutate(purchase_period = PurPeriodNotSpec)
  }

  return (licence_data)
}


#' Merge Electronic Licence Survey Data
#'
#' Merges the survey data with licence data so that the survey results
#'	will have the licence category information.  The two data sets are merged using
#' "licence_id"
#'
#' @param survey_data Survey results data frame
#' @param elic_data Electronic licences data, including specifics of each licence
#'
#' @return The survey results with the licence details attached to each survey result record
#'
#' @importFrom dplyr setdiff
mergeELicSurveyData <- function(survey_data, elic_data) {
  unknown_lic_id <- setdiff(survey_data$licence_id, elic_data$licence_id)
  if (any(!is.na(unknown_lic_id))) {
    addLogMessages("WARNING - Unknown licence IDs: {str_c(unknown_lic_id, collapse=\",\")}")
  }

  survey_data <-
    elic_data %>%
    select(-lic_year, -survey_definition_id, -fisher_id) %>%
    inner_join(survey_data, by = "licence_id")

  return(survey_data)
}

#' Survey Specific Variance
#' Calculates the licence/draw type category specific variance.
#'
#' @param survey_data All the survey result data for the month
#' @param survey_response A data frame of response totals by licence category/draw type
#' @param lic_uniq_col_name Column name identifying unique licences
#' @param strata_col_names Column names to stratify variances
#' @param non_var_col_names Columns that have no variance
#'
#' @return Variance values for the specific licence category/draw type in each
#'   area and method
#'
#' @importFrom purrr map2_dfc
#'
surveySpecificVariance <- function(survey_data,
                                   survey_response,
                                   lic_uniq_col_name = "licence_id",
                                   strata_col_names = c("draw_type",
                                                        "resident_status",
                                                        "licence_type",
                                                        "age_category"),
                                   non_var_col_names = c("area",
                                                         "method")) {
  #Sum to produce monthly total for each licence holder
  survey_pre_sum <-
    survey_data %>%
    group_by_at(c(lic_uniq_col_name, strata_col_names, non_var_col_names)) %>%
    summarize_at(getCatchColNames(.), sum, na.rm = TRUE) %>%
    ungroup()

  survey_pre <-
    survey_pre_sum %>%
    group_by_at(c(strata_col_names, non_var_col_names)) %>%
    summarize_at(getCatchColNames(.),
                 list) %>%
    ungroup()

  specific_variance <-
    survey_response %>%
    full_join(survey_pre, by = strata_col_names) %>%
    ungroup()


  catch_col_names <- getCatchColNames(specific_variance)
  samp_size_col_names <- getSampSizeColNames(specific_variance)


  getCatchVar <- function(catch_col_name, sample_col_name, specific_variance) {
    catch_col <- specific_variance[[catch_col_name]]
    sample_col <- specific_variance[[sample_col_name]]
    catch_col <-
      variance(catch_col,
               sample_col) %>%
      tibble()

    colnames(catch_col) <- catch_col_name
    return(catch_col)
  }

  variance_values <-
    map2_dfc(catch_col_names,
             samp_size_col_names,
             getCatchVar,
             specific_variance)

  specific_variance <-
    specific_variance %>%
    select(c(strata_col_names, non_var_col_names)) %>%
    bind_cols(variance_values)

  return(specific_variance)
}

#' Calculate Response Totals
#'
#' Add up responce total by strata and catch category.
#' Because adjusments may remove catch from a particular user, then each catch
#' cateogry may have a seperate response total.
#'
#' @param survey_data The survey response data
#' @param uniq_user_col_name Column name for unique users
#' @param strata_col_names Stratification column names
#'
#' @return the survey_data with additional columns that match the catch columns with the "n_" prefix
#'
calcResponseTotals <- function(survey_data,
                               uniq_user_col_name = "licence_id",
                               strata_col_names = c("draw_type",
                                                    "resident_status",
                                                    "licence_type",
                                                    "age_category")) {
  catch_col_names <- getCatchColNames(survey_data)

  survey_response <-
    survey_data %>%
    filter(effort_days > 0 | did_not_fish) %>%
    group_by_at(c(uniq_user_col_name, strata_col_names)) %>%
    summarize_at(catch_col_names, function(.) {all(!is.na(.))}) %>%
    ungroup() %>%
    group_by_at(strata_col_names) %>%
    summarize_at(catch_col_names, sum) %>%
    ungroup()

  catch_col_names <- grep(CatchColRegEx,
                          colnames(survey_response))

  colnames(survey_response)[catch_col_names] <-
    str_c(CatchSampleSizePrefix,
          colnames(survey_response)[catch_col_names])

  return(survey_response)
}


#' Survey Total Variance
#'
#' Combines the strata variances for the various licence categories/draw types to
#'	to estimate the total catch variance for each area and method there is data for.
#'
#' @param specific_variance All the survey result data for the month
#' @param sample_sizes Sample sizes for each specific variance values
#' @param lic_total Total valid licences data frames
#' @param lic_strata_col_names Licence column names used to stratify
#' @param catch_strata_col_names Catch column names used to stratify
#'
#' @return Variance values for the specific licence category/draw type in each area and method
#'
#' @importFrom dplyr vars select_at
#' @importFrom purrr map_dfc map2_dfc
#'
surveyTotalVariance <-  function(specific_variance,
                                 sample_sizes,
                                 lic_total,
                                 lic_strata_col_names = c("draw_type",
                                                          "resident_status",
                                                          "licence_type",
                                                          "age_category"),
                                 catch_strata_col_names = c("area", "method")) {
  pop_size <- sum(lic_total$lic_total, na.rm = TRUE)

  lic_weight <-
    lic_total %>%
    mutate(weight = lic_total / pop_size) %>%
    select(-lic_total)

  fpc <-
    specific_variance %>%
    full_join(lic_weight, by=lic_strata_col_names) %>%
    mutate_at(getCatchColNames(.), ~ weight * . / pop_size) %>%
    group_by_at(catch_strata_col_names) %>%
    summarize_at(getCatchColNames(.), sum, na.rm=TRUE) %>%
    ungroup()

  fpc_col_prefix <- "fpc_"

  temp_col_names <- getCatchColNames(fpc)

  colnames(fpc)[colnames(fpc) %in% temp_col_names] <-
    str_c(fpc_col_prefix, temp_col_names)

  sample_size_weight <-
    sample_sizes %>%
    filter(!is.na(n_effort_days)) %>%
    group_by_at(lic_strata_col_names) %>%
    summarize_at(vars(getSampSizeColNames(sample_sizes)),
                 max,
                 na.rm = TRUE) %>%
    ungroup() %>%
    full_join(lic_weight, by = lic_strata_col_names)

  strata_weight <-
    sample_size_weight %>%
    full_join(specific_variance, by=lic_strata_col_names)

  strataVarFunc <- function(variance_col_name, sample_col_name, strata_weight) {
    var_col <- strata_weight[[variance_col_name]]
    sample_col <- strata_weight[[sample_col_name]]
    weight_col <- strata_weight$weight

    calc_col <-
      (weight_col^2 * var_col / sample_col) %>%
      tibble()

    colnames(calc_col) <- variance_col_name
    return (calc_col)
  }

  var_col_names <- getCatchColNames(strata_weight)
  samp_size_col_names <- getSampSizeColNames(strata_weight)

  strata_var <-
    map2_dfc(var_col_names,
             samp_size_col_names,
             strataVarFunc,
             strata_weight)


  strata_var <-
    strata_weight %>%
    select_at(catch_strata_col_names) %>%
    bind_cols(strata_var) %>%
    group_by_at(catch_strata_col_names) %>%
    summarize_at(var_col_names, sum, na.rm=TRUE) %>%
    ungroup() %>%
    full_join(fpc, by = catch_strata_col_names)

  strataFpcCorrFunc <- function(variance_col_name, strata_var) {
    var_col <- strata_var[[variance_col_name]]
    fpc_col <- strata_var[[str_c(fpc_col_prefix, variance_col_name)]]

    calc_col <-
      ((var_col - fpc_col) * pop_size ^ 2) %>%
      tibble()

    colnames(calc_col) <- variance_col_name
    return(calc_col)
  }

  strata_fpc_var <-
    map_dfc(var_col_names,
            strataFpcCorrFunc,
            strata_var)

  strata_fpc_var <-
    strata_var %>%
    select_at(catch_strata_col_names) %>%
    bind_cols(strata_fpc_var)

  return (strata_fpc_var)
}


#' Summarize Licence Population
#'
#' Summarize the licence population totals using the E-Licence and Vendor Sales Data
#'
#' @param elic_data Electronic licences data
#' @param vendor_sales_data Vendor sales of licences
#' @param survey_dates Survey date range
#'
#' @return A data frame summarized by licence categories and by electronic/vendor sales
#'
#' @importFrom dplyr ungroup summarize full_join coalesce
#'
summarizeLicPop <- function(elic_data, vendor_sales_data, survey_dates) {
  survey_months <- format(survey_dates, format = "%Y-%m")
  if (survey_months[1] != survey_months[2]) {
    addLogMessages("WARNING: The Survey Start and End dates should be in the same year and month.")
  }

  elic_total <-
    elic_data %>%
    filter(!is.na(draw_type)) %>%
    count(draw_type,
          resident_status,
          licence_type,
          age_category) %>%
    rename(elic_total = n) %>%
    mutate(elic_total = as.double(elic_total))


  vendor_pre_data <-
    vendor_sales_data %>%
    select(resident_status,
           licence_type,
           age_category,
           vlic_total = before_month_total) %>%
    mutate(draw_type = DrawTypePre)

  #Remove Pre Term Draw types, these don't occur.
  vendor_pre_data <-
    vendor_pre_data %>%
    filter(licence_type == LicTypeAnnual & licence_type != DrawTypePre)

  vlic_total <-
    vendor_sales_data %>%
    select(resident_status,
           licence_type,
           age_category,
           vlic_total = month_total) %>%
    mutate(draw_type = DrawTypePost) %>%
    filter(vlic_total > 0) %>%
    bind_rows(vendor_pre_data) %>%
    group_by(resident_status,
             licence_type,
             age_category,
             draw_type) %>%
    summarize(vlic_total = sum(vlic_total, na.rm = TRUE)) %>%
    ungroup()

  lic_total <-
    vlic_total %>%
    full_join(elic_total, by = c("resident_status",
                                 "licence_type",
                                 "age_category",
                                 "draw_type")) %>%
    mutate(vlic_total = coalesce(vlic_total, 0),
           elic_total = coalesce(elic_total, 0)) %>%
    mutate(lic_total = vlic_total + elic_total)

  return(lic_total)
}


#'
#'  This Variance function calculates variance base on a sample larger then provided for
#'	 in the data.  Values for all other observations are assumed to be zero and the variance calulation
#'  handles all the additional zero values as a single term calculations.
#'
#' @param values The values to calculate variance on, the length of this data vector is typically less then "n"
#' @param sample_size The date licence records are cut off at, this date will be used to exclude
#'		licence records that start after this date.
#'
#' @return A data frame with the licence records loaded in a consistent R friendly format.
#'
#' @importFrom purrr map2_dbl
#'
variance <- function(values, sample_size) {
  var_result <-
    map2_dbl(values,
             sample_size,
             function(values, sample_size) {
               stopifnot(sample_size > 0)
               value_mean <- sum(values) / sample_size
               result <- (sum((values - value_mean) ^ 2) + ((sample_size - length(values)) * (value_mean ^ 2))) / (sample_size - 1)
               return(result)
             })
  return(var_result)
}

#' Sample Size Column Names
#'
#' Retrieve a vector of sample size column names that matches the catch column name vector length
#'
#' @param survey_df A survey related data frame
#'
#' @return A vector of column names related survey sample sizes, including effort_days and juv_effort_days
#'
#' @importFrom stringr str_extract
#'
getSampSizeColNames <- function(survey_df) {
  survey_col_names <- colnames(survey_df)
  samp_col_names <- str_extract(survey_col_names,
                                SampleColRegEx)

  samp_col_names <- survey_col_names[!is.na(samp_col_names)]
  return(samp_col_names)
}


#' Add Excel Worksheet
#'
#' This AddWorksheet function adds a worksheet to a previously create excel document
#'
#' The data table is the first parameter and it returns the first parameter unchanged
#' so that it is compatible with magrittr "%>%"
#'
#' @param data_table Data that is to be written to the new worksheet
#' @param excel_doc The excel document variable created with openxlsx::createworkbook()
#' @param sheet_name Name that the new worksheet is to be labelled as
#'
#' @return The data table provide, unmodified
#'
#' @importFrom openxlsx addWorksheet writeData
#'
addXlWorksheet <- function(data_table, excel_doc, sheet_name) {
  addWorksheet(excel_doc, sheet_name)
  writeData(excel_doc, sheet_name, data_table)
  return(data_table)
}

#' Get Catch Column Names
#'
#' Retrieve a vector of catch column names from the survey data frame provided
#'
#' @param survey_df A survey related data frame
#'
#' @return A vector of column names related to catch, including effort_days and juv_effort_days
#'
getCatchColNames <- function(survey_df) {
  survey_col_names <- colnames(survey_df)
  catch_col_name_idx <- grep(CatchColRegEx,
                             survey_col_names)
  return(survey_col_names[catch_col_name_idx])
}

