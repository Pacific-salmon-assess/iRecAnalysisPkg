
#' Parse user provided licence ID text based on
#' comma or space delimited list of Licence IDs
#'
#' @param licence_id_txt Text of licence IDs to parse
#'
#' @return character vector of unique licences IDs or an empty vector if on were provided
#'
#' @importFrom stringr str_split
#'
parseLicenceIds <- function(licence_id_txt) {
  adj_lic_text <- str_trim(licence_id_txt)

  adj_lic_id <- c()

  if (nchar(adj_lic_text) > 0) {
    adj_lic_id <-
      adj_lic_text %>%
      str_split(",") %>%
      unlist() %>%
      str_trim() %>%
      str_split(" ") %>%
      unlist() %>%
      unique()

    adj_lic_id <- adj_lic_id[nchar(adj_lic_id) > 0 & !is.na(adj_lic_id)]
  }

  return (adj_lic_id)
}

#' Create Adjustment File
#'
#' Create an adjustment file based on specific licence ids
#'
#' @param adj_lic_id licences ids to export in the adjustment file
#' @param config Configuration list
#'
#' @return The file name that the adjustment file is created
#'
#' @importFrom readr write_csv
#'
#' @export
#'
createAdjFile <- function(adj_lic_id, config) {
  survey_data <-
    loadSurveyResults(config$survey_result_filename,
                      config$survey_start_date,
                      config$exclude_filename,
                      NA)

  if (length(adj_lic_id) > 0) {
    if ("licence_id" %in% colnames(survey_data)) {
      valid_lic_id <-
        survey_data %>%
        pull(licence_id) %>%
        unique()

      invalid_lic_id <-
        adj_lic_id %>%
        setdiff(valid_lic_id)

      if (length(invalid_lic_id) > 0) {
        invalid_lic_text <-
          str_c(invalid_lic_id, collapse=",")

        cat("\nWARNING: The following Licence IDs your provided do not appear in the survey responses\n")
        cat(invalid_lic_text)
        cat("\n\n")

        #Exclude invalid licence IDs
        adj_lic_id <-
          adj_lic_id %>%
          setdiff(invalid_lic_id)
      }

      survey_data <-
        survey_data %>%
        filter(licence_id %in% adj_lic_id)
    } else {
      valid_survey_key <-
        survey_data %>%
        pull(surveykey) %>%
        unique()

      invalid_survey_key<-
        adj_lic_id %>%
        setdiff(valid_survey_key)

      if (length(invalid_survey_key) > 0) {
        invalid_key_text <-
          str_c(invalid_survey_key, collapse=",")

        cat("\nWARNING: The following survey keys your provided do not appear in the survey responses\n")
        cat(invalid_key_text)
        cat("\n\n")

        #Exclude invalid licence IDs
        adj_lic_id <-
          adj_lic_id %>%
          setdiff(invalid_survey_key)
      }

      survey_data <-
        survey_data %>%
        filter(surveykey %in% adj_lic_id)
    }
  } else {
    #no licence IDs selected for adjustment, so drop all rows and only keep header structure
    survey_data <-
      survey_data %>%
      filter(FALSE)
  }

  survey_data <-
    survey_data %>%
    mutate(did_not_fish = if_else(did_not_fish, "yes", "no"))


  adj_file_name <-
    path(config$survey_data_path,
         glue("survey_adj_{getTimeStampText()}.csv")) %>%
    path_norm()

  write_csv(survey_data, adj_file_name)

  return(adj_file_name)

}
