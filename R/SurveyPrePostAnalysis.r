
#' Run Pre/Post Survey Analysis
#'
#' Processes survey results based on the pre/post sampled licences
#' (licence years 2012 to 2017)
#'
#' @param config Survey configuration list
#' @param elic_data Electronic licence data
#'
#' @return The list provided as data_list is returned (allows writing multiple worksheets in a pipe)
#'
#' @importFrom dplyr summarize_if group_by anti_join transmute
#'
runPrePostSurveyAnalysis <- function(config, elic_data) {
  result_list <- list()

  if (is.null(config$survey_result_filename) == TRUE ||
      is.na(config$survey_result_filename) ||
      str_length(config$survey_result_filename) == 0) {
    stop("No survey result file name provided using the \"survey_result_filename\" parameter.")
  }


  survey_data <-
    loadSurveyResults(config$survey_result_filename,
                      config$survey_start_date,
                      config$exclude_filename,
                      config$survey_adj_filename)


  remove_col_names <-
    c("first_name", "last_name", "email", "year", "month", "day") |>
    intersect(names(survey_data))

  survey_data <-
    survey_data |>
    select(-remove_col_names)

  vendor_sales_data <- loadVendorSalesData(config$vendor_sales_filename,
                                           config$survey_month,
                                           config$annual_expire_date)
  non_resident_split <-
    vendor_sales_data %>%
    filter(resident_status == ResStatusNon,
           age_category == AgeCatAdultSenior,
           licence_type == LicTypeAnnual) %>%
    pull(month_total)

  if (length(non_resident_split) > 0 && non_resident_split > 0) {
    #The vendor sales data is not split into Adult/Senior, so all Adult and Senior non-resident
    #licences need to be grouped into a combined Adult/Senior non-resident licence category
    addLogMessages("WARNING - Vendor sales group adult and senior non-resident license, so survey results are similarly grouped")
    elic_data <-
      elic_data %>%
      mutate(age_category = if_else(resident_status == ResStatusNon &
                                      licence_type == LicTypeAnnual,
                                    AgeCatAdultSenior,
                                    age_category))
  }

  elic_data <-
    elic_data %>%
    addDrawType(config$survey_dates)

  survey_data <- mergeELicSurveyData(survey_data, elic_data)

  invalid_response <-
    survey_data %>%
    filter(end_date < config$survey_dates[1] |
             start_date > config$survey_dates[2]) %>%
    select(full_licence_id)

  if (nrow(invalid_response) > 0) {
    survey_data <-
      anti_join(survey_data, invalid_response, by = "full_licence_id")

    addLogMessages("The following survey participant was invalid for the survey period: {getTruncText(pull(invalid_response))}")
  }

  #Sum up the survey records with a effort days count set for the various strata
  survey_result <-
    survey_data %>%
    filter(effort_days > 0 | did_not_fish == TRUE) %>%
    select(-stamp) %>%
    group_by(area,
             draw_type,
             resident_status,
             licence_type,
             age_category,
             method,
             lodge,
             guided) %>%
    summarize_if(is.numeric,
                 sum,
                 na.rm = TRUE) %>%
    ungroup()

  #Check that none of the rows without areas have and reported catch
  invalid_result <-
    survey_result %>%
    filter(is.na(area)) %>%
    select_at(getCatchColNames(.)) %>%
    mutate_all(coalesce, 0) %>%
    transmute(row_sum = rowSums(.)) %>%
    filter(row_sum > 0)

  if (nrow(invalid_result)) {
    stop("Invalid survey responses with catch and no area provided.")
  }

  survey_result <-
      filter(survey_result, !is.na(area))

  #Calculate Response Totals for each category
  survey_response <-
    calcResponseTotals(survey_data)

  lic_total <-
    summarizeLicPop(elic_data, vendor_sales_data, config$survey_dates) %>%
    filter(age_category != AgeCatJuvenile)

  result_list$licence_summary <- lic_total

  lic_total <-
    lic_total %>%
    select(draw_type,
           resident_status,
           licence_type,
           age_category,
           lic_total)

  result_list$survey_specific_variance <-
    surveySpecificVariance(survey_data, survey_response)

  survey_result <-
    lic_total  %>%
    full_join(survey_result,
              by = c("draw_type",
                     "resident_status",
                     "licence_type",
                     "age_category")) %>%
    full_join(survey_response,
              by = c("draw_type",
                     "resident_status",
                     "licence_type",
                     "age_category"))


  samp_size_col_names <- getSampSizeColNames(survey_result)
  catch_col_names <- getCatchColNames(survey_result)

  result_list$survey_summary <-
    survey_result %>%
    select(-one_of(samp_size_col_names))

  result_list$survey_sample_sizes <-
    survey_result %>%
    select(-one_of(catch_col_names))

  #because there is both a data frame and column called lic_total, the mutate_at call
  #needs to use .$lic_total to identify that it is refering to the column in survey_result
  expand_rate <-
    survey_result %>%
    mutate_at(samp_size_col_names,
              function(., lic_total) {
                return(coalesce(lic_total, 0) / .)
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
                        lic_total)


  result_list$log <- getLogMessages()
  return(result_list)
}
