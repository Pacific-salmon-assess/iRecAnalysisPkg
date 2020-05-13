
#' Export Instantaniace Survey Raw Results
#'
#' Export the raw survey results for an instantaniace survey
#'
#' @param licence_filename Licence file name
#' @param survey_result_filename Survey result file name
#' @param output_data_filename Output file name
#'
#' @return The Excel document file name that contains all the annual data
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom fs dir_exists
#' @importFrom openxlsx createWorkbook saveWorkbook
#' @importFrom readr write_csv
#'
exportInstRawResult <- function(licence_filename,
                                survey_result_filename,
                                output_data_filename) {
  survey_result <-
    haven::read_spss(survey_result_filename) %>%
    as_tibble() %>%
    select_all(tolower) %>%
    mutate_all(haven::zap_labels)


  licence_file <-
    readr::read_csv(licence_filename) %>%
    select_all(tolower)

  full_data_set <-
    licence_file %>%
    full_join(survey_result, by=c(survey_access_key = "surveykey"))


  write_csv(full_data_set, output_data_filename, na = "")
}

