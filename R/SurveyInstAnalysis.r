
#Licence Year that identifies the start of Instantaneous licence selection
InstLicYearStart <- 2018

#' Get NRLS Population Sizes
#'
#' Summarize the E-Licence data to population sizes by licence stratification.
#'
#' @param elic_data Electronic licence data frame
#' @param lic_id_col_name Name of column that identifies the licence ID
#' @param lic_strata_col_names Column names that are used to stratify for population totals
#'
#' @return A data frame population sizes based on strata columns
#'
#' @importFrom dplyr group_by_at
#'
getNrlsPopSize <- function(elic_data, lic_id_col_name, lic_strata_col_names) {
  all_row_total <- nrow(elic_data)
  pop_size <-
    elic_data %>%
    select_at(c(lic_id_col_name, lic_strata_col_names)) %>%
    distinct()

  if (nrow(pop_size) < all_row_total) {
    addLogMessages("{all_row_total - nrow(pop_size)} licences were dropped as duplicates")
  }

  pop_size <-
    pop_size %>%
    group_by_at(lic_strata_col_names) %>%
    count() %>%
    ungroup() %>%
    rename(lic_total = n)
}

#' Run Instantaneous Selection Survey Analysis
#'
#' Analysis the responses to the iRec survey based on instantaneous selection method
#' integrated into the National Recreational Licence System (NRLS)
#'
#' @param config Configuration list
#' @param elic_data Electronic licence data frame
#'
#' @return list with survey analysis results
#'
#' @importFrom dplyr summarize_at one_of right_join
#' @importFrom purrr map2_dfc
#'
runInstSurveyAnalysis <- function(config, elic_data) {
  catch_strata_col_names <- c("area", "method", "lodge", "guided",
                              "checkcrabs", "checkprawns", "checkcrabsprawns")
  participate_id <- "survey_access_key"

  if (config$stamp_stratify) {
    LicStrataColNames <- c(LicStrataColNames, LicStampStrataColName)
  }

  elic_data %<>%
    addPurchasePeriod(config$survey_dates,
                      config$period_stratify,
                      config$period_stratify_date)

  pop_size <- getNrlsPopSize(elic_data, "licence_id", LicStrataColNames)

  elic_data %<>%
    select_at(c(participate_id, LicStrataColNames))

  result_list <- list(licence_summary = pop_size)

  survey_data <-
    loadSurveyResults(config$survey_result_filename,
                      config$survey_start_date,
                      config$exclude_filename,
                      config$survey_adj_filename) %>%
    select(-first_name, -last_name, -email)


  survey_data <-
    elic_data %>%
    filter(!is.na(survey_access_key)) %>%
    right_join(survey_data, by = c(survey_access_key = "surveykey"))

  result_list$raw_data <- survey_data

  invalidKey <-
    survey_data %>%
    filter(is.na(resident_status)) %>%
    distinct(survey_access_key) %>%
    pull()

  if (length(invalidKey) > 0) {
    addLogMessages("WARNING-The following survey keys were invalid: {str_c(invalidKey, collapse=\",\")}")
  }

  survey_data <-
    survey_data %>%
    filter(survey_access_key %notin% invalidKey)

  mixed_dnf <-
    survey_data %>%
    distinct(survey_access_key, did_not_fish) %>%
    count(survey_access_key)  %>%
    filter(n > 1) %>%
    pull(survey_access_key)

  if (length(mixed_dnf) > 0) {
    error_msg <- glue("ERROR-The following survey keys have multiple did not fish statuses: {str_c(mixed_dnf, collapse=\",\")}")
    addLogMessages(error_msg)
    stop(error_msg)
  }

  missing_catch_stra_names <-
    dplyr::setdiff(catch_strata_col_names, colnames(survey_data))

  if (length(missing_catch_stra_names) > 0) {
    addLogMessages("WARNING-Missing catch strata columns: {str_c(missing_catch_stra_names, collapse=\",\")}")
    catch_strata_col_names <- dplyr::intersect(catch_strata_col_names, colnames(survey_data))
  }

  survey_sum <-
    survey_data %>%
    filter(!is.na(did_not_fish)) %>%
    group_by_at(c(LicStrataColNames, catch_strata_col_names)) %>%
    summarize_at(getCatchColNames(.), sum, na.rm = TRUE)

  survey_resp_totals <-
    calcResponseTotals(survey_data,
                       participate_id,
                       LicStrataColNames)
  result_list$survey_specific_variance <-
    surveySpecificVariance(survey_data,
                           survey_resp_totals,
                           lic_uniq_col_name = participate_id,
                           strata_col_names = LicStrataColNames,
                           non_var_col_names = catch_strata_col_names)

  survey_full_result <-
    pop_size  %>%
    full_join(survey_sum,
              by = LicStrataColNames) %>%
    full_join(survey_resp_totals,
              by = LicStrataColNames)

  samp_size_col_names <- getSampSizeColNames(survey_full_result)
  catch_col_names <- getCatchColNames(survey_full_result)

  result_list$survey_summary <-
    survey_full_result %>%
    select(-one_of(samp_size_col_names))

  result_list$survey_sample_sizes <-
    survey_full_result %>%
    select(-one_of(catch_col_names))

  expand_rate <-
    survey_full_result %>%
    mutate_at(samp_size_col_names,
              function(., lic_total) {
                return(coalesce(lic_total, 0L) / .)
              },
              .$lic_total)

  expandCatch <- function(catch_col_name, sample_col_name, expand_rate) {
    catch_col <- expand_rate[[catch_col_name]]
    sample_col <- expand_rate[[sample_col_name]]
    est_catch_col <-
      catch_col * sample_col %>%
      tibble()

    colnames(est_catch_col) <- catch_col_name
    return(est_catch_col)
  }

  catch_estimate_detail <- map2_dfc(catch_col_names,
                                    samp_size_col_names,
                                    expandCatch,
                                    expand_rate)

  catch_estimate_detail <-
    expand_rate %>%
    select(which(colnames(.) %notin% c(samp_size_col_names, catch_col_names))) %>%
    bind_cols(catch_estimate_detail)

  result_list$detail_estimated_catch <- catch_estimate_detail

  catch_estimate_summary <-
    catch_estimate_detail %>%
    group_by(area, method) %>%
    summarize_at(catch_col_names,
                 sum,
                 na.rm = TRUE) %>%
    ungroup()

  result_list$summary_estimated_catch <- catch_estimate_summary

  result_list$total_variance <-
    surveyTotalVariance(result_list$survey_specific_variance,
                        result_list$survey_sample_sizes,
                        pop_size,
                        LicStrataColNames,
                        catch_strata_col_names)

  result_list$log <- getLogMessages()

  return(result_list)
}
