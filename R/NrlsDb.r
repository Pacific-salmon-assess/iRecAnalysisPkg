
#' Gets log-in information and sets up a connection to Oracle.
#'
#' @param db_user_name The user name to log into Oracle with
#' @param db_conn_text Database connection text
#' @param db_pass Password for user, if not provided with prompt user to provide
#'
#' @return An ROracle connection
#'
#' @importFrom glue glue
#' @importFrom DBI dbDriver dbGetQuery dbConnect
#' @importFrom askpass askpass
#'
setupNrlsConn <- function(db_user_name, db_conn_text, db_pass = NULL) {
  db_driver <- dbDriver("Oracle")
  print(glue("Connecting to NRLS DB: {db_conn_text}"))

  if (is.null(db_pass)) {
    db_pass <-
      askpass(glue("Enter database password for '{db_user_name}':"))
  }

  db_conn <-
    dbConnect(
      db_driver,
      username = db_user_name,
      password = db_pass,
      dbname = db_conn_text
    )

  return(db_conn)
}

#' Gets licence records from NRLS
#'
#' @param db_conn Database connection to NRLS
#' @param lic_year Licence year to retrieve
#'
#' @return A data frame of licence records
#'
#' @importFrom glue glue
#' @importFrom DBI dbGetQuery
#' @importFrom lubridate as_date
#'
getNrlsLicences <- function(db_conn, lic_year) {
  sql <-
    glue(
      "SELECT lic_year, lic_no as full_licence_id, survey_access_key, survey_definition_id, survey_start_dt, ",
      "survey_end_dt, survey_emailed_dtt, purchase_dtt, stamp_purchase_dtt, ",
      "start_dt, expiry_dt, lic_type_id, lic_type_cat_code, paper_yn, age_grp_id, fisher_id ",
      "from NRLS.irec_survey_vw WHERE lic_year = {lic_year}"
    )

  nrls_lic_df <-
    dbGetQuery(db_conn, sql) %>%
    as_tibble() %>%
    select_all(tolower) %>%
    mutate_at(c("lic_year", "survey_definition_id"),
              as.integer) %>%
    mutate_at(
      c(
        "start_dt",
        "purchase_dtt",
        "stamp_purchase_dtt",
        "survey_start_dt",
        "survey_end_dt"
      ),
      as_date
    )

  return(nrls_lic_df)
}


#' Refresh NRLS licence file
#'
#' Connect with NRLS database and retrieve the latest licence records
#'
#' @param lic_year Licence year to refresh licence records for
#' @param config Licence year configuration list
#' @param irec_dir_root Root directory that contains iRec Analysis
#' @param password The password used to connect with the NRLS database
#'
#' @return File name that was refreshed
#'
#' @importFrom fs file_exists
#' @importFrom DBI dbDisconnect
#'
#' @export
#'
refreshNrlsLicenceFile <- function(lic_year,
                                   config = NULL,
                                   irec_dir_root = getiRecAnalysisDir(),
                                   password = NULL) {
  if (is.null(config)) {
    config <-
      loadAnalysisYearConfig(lic_year, irec_dir_root = irec_dir_root)
  }

  if (is.null(config$nrls_user_name) |
      is.null(config$nrls_db_conn)) {
    stop(
      "To refers the NRLS licence file the nrls_user_name and nrls_db_conn must be set in the year_config.yml"
    )
  }

  if (is.null(config$licence_filename)) {
    stop(
      "To refers the NRLS licence file the licence_filename must be set in the year_config.yml"
    )
  }

  print("Connecting to NRLS Database")
  db_conn <-
    setupNrlsConn(config$nrls_user_name, config$nrls_db_conn, password)

  print(glue("Retrieving to NRLS Licence records for {lic_year}"))
  lic_df <- getNrlsLicences(db_conn, lic_year)

  if (file_exists(config$licence_filename)) {
    #check to see if there is any missing licence IDs compared against the previous file
    missing_lic_id <-
      loadNrlsLicenceFile(
        config$licence_filename,
        config$annual_date_range[1],
        config$annual_date_range[2]
      ) %>%
      pull(full_licence_id) %>%
      setdiff(pull(lic_df, full_licence_id))

    if (length(missing_lic_id)) {
      error_msg <-
        glue(
          "{length(missing_lic_id)} licence ids are missing:",
          getTruncText(missing_lic_id),
          "To fix this issue you need to remove the previous licence file.",
          .sep = "\n"
        )
      stop(error_msg)
    }
  }

  print(glue(
    "Writing {nrow(lic_df)} licence records to: {config$licence_filename}"
  ))
  writeProtectedCsv(lic_df, config$licence_filename)

  dbDisconnect(db_conn)

  return(config$licence_filename)
}

#' Load NRLS Licence File
#'
#' The main method used to load the licence data from NRLS system.  This function selects the
#' appropriate version of the file format to load based on the survey year
#' (starting April 1st)
#'
#' @param licence_filename File name that licence records are saved in CSV format
#' @param lic_start_date Licence start date
#' @param lic_end_date Licence end date
#'
#' @return A data frame with the licence records
#'
#' @importFrom dplyr select_all select mutate mutate_at tally pull case_when count
#' @importFrom magrittr %<>%
#' @importFrom readr read_csv
#' @importFrom lubridate dmy
#' @importFrom purrr map_chr
#' @importFrom stringr str_sub
#'
loadNrlsLicenceFile <- function(licence_filename,
                                lic_start_date,
                                lic_end_date) {
  lic_data <-
    read_csv(licence_filename) %>%
    select_all(tolower) %>%
    mutate_at(c("lic_year", "survey_definition_id"),
              as.integer) %>%
    select(
      full_licence_id,
      start_date = start_dt,
      purchase_date = purchase_dtt,
      stamp_purchase_date = stamp_purchase_dtt,
      lic_type_cat_code,
      survey_access_key,
      survey_definition_id,
      lic_year,
      fisher_id
    )

  #Parse the partial licence ID, because some historic survey results use it
  lic_split_char <- "-"
  lic_data <-
    lic_data %>%
    mutate(licence_id = map_chr(strsplit(full_licence_id, lic_split_char),
                                function(x) {
                                  if (length(x) >= 4) {
                                    return(as.character(as.integer(x[4])))
                                  } else {
                                    return(paste(x, collapse = lic_split_char))
                                  }
                                }))


  #The Licence pre start date is one month before licence start date because licences
  #can be purchased typically up to a month before the licence start date
  lic_pre_start_date <- lic_start_date - months(1)

  #Licences on sale at most 1 month before licence year: lic_start_date - months(1)
  #If the licence purchase date is invalid (not in licence year), then set to NA and use the start date
  lic_data  %<>%
    mutate(
      purchase_date = if_else(
        purchase_date < lic_pre_start_date |
          purchase_date > lic_end_date,
        as_date(NA),
        purchase_date
      )
    )

  invalid_purchase_dates <-
    lic_data %>%
    tally(is.na(purchase_date)) %>%
    pull()

  if (invalid_purchase_dates > 0) {
    addLogMessages(
      "WARNING - {invalid_purchase_dates} invalid purchase dates ignore (outside potential licence year)"
    )
  }

  invalid_stamp_dates <-
    lic_data %>%
    tally(
      !is.na(stamp_purchase_date) &
        (
          stamp_purchase_date < lic_pre_start_date |
            stamp_purchase_date > lic_end_date
        )
    ) %>%
    pull()

  if (invalid_stamp_dates > 0) {
    addLogMessages(
      "WARNING - {invalid_stamp_dates} invalid stamps start dates replaced (licence start date used if outside licence year)"
    )
    lic_data %<>%
      mutate(
        stamp_purchase_date = case_when(
          is.na(stamp_purchase_date) ~ as_date(NA),
          stamp_purchase_date < lic_pre_start_date |
            stamp_purchase_date > lic_end_date  ~ start_date,
          TRUE ~ stamp_purchase_date
        )
      )
  }

  all_lic_total <- nrow(lic_data)
  lic_data %<>%
    filter(fisher_id != 1)

  if (nrow(lic_data) < all_lic_total) {
    addLogMessages("{all_lic_total - nrow(lic_data)} licences were dropped as unsold (fisher ID = 1)")
  }

  lic_data %<>%
    mutate(start_date = pmax(start_date, purchase_date, na.rm = TRUE))

  blank_lic_lines <-
    count(lic_data, wt = is.na(lic_type_cat_code) |
            lic_type_cat_code == "")

  addLogMessages("Number of blank records in the licence file: {pull(blank_lic_lines)}")


  full_lic_total <- nrow(lic_data)
  lic_data <-
    filter(lic_data,
           lic_start_date <= start_date,
           start_date <= lic_end_date)

  drop_lic_total <- full_lic_total - nrow(lic_data)
  if (drop_lic_total > 0) {
    addLogMessages(
      "WARNING: Removed {drop_lic_total} because the start date was not in the correct licence year"
    )
  }

  lic_data <-
    filter(lic_data,
           !is.na(lic_type_cat_code) & lic_type_cat_code != "")

  addLogMessages("Number of records in the licence file: {nrow(lic_data)}")

  lic_data <-
    lic_data %>%
    mutate(
      res_status_chr = str_sub(lic_type_cat_code, end = 1),
      age_status_chr = str_sub(lic_type_cat_code, start = 3, end = 3)
    )


  family_lic_total <-
    lic_data %>%
    count(wt = res_status_chr == "F") %>%
    pull()

  if (family_lic_total > 0) {
    addLogMessages("Number of family licences removed: {family_lic_total}")
    lic_data <-
      filter(lic_data, res_status_chr != "F")
  }

  lic_data <-
    lic_data %>%
    mutate(
      resident_status = case_when(
        res_status_chr == "N" ~ ResStatusNon,
        res_status_chr == "R" ~ ResStatusRes
      ),
      age_category = case_when(
        age_status_chr == "A" ~ AgeCatAdult,
        age_status_chr == "S" ~ AgeCatSenior,
        age_status_chr == "" ~ AgeCatAdultSenior
      ),
      annual_flag = coalesce(substr(lic_type_cat_code, 2, 2) == "A", FALSE)
    )

  invalidStatus <-
    lic_data %>%
    filter(is.na(resident_status)) %>%
    distinct(res_status_chr) %>%
    pull()

  if (length(invalidStatus) > 0) {
    stop(
      glue(
        "Invalid resident status codes in licence category codes: {str_c(invalidStatus, collapse=\", \")}"
      )
    )
  }

  invalidAge <-
    lic_data %>%
    filter(is.na(age_category)) %>%
    distinct(age_status_chr) %>%
    pull()

  if (length(invalidAge) > 0) {
    stop(
      glue(
        "Invalid age codes in licence category codes: {str_c(invalidAge, collapse=\", \")}"
      )
    )
  }

  lic_data <-
    lic_data %>%
    mutate(
      res_status_chr = NULL,
      age_status_chr = NULL,
      lic_type_chr = str_sub(lic_type_cat_code, 2, 2)) %>%
    mutate(
      licence_type = case_when(
        lic_type_chr == "A" ~ LicTypeAnnual,
        lic_type_chr == "5" ~ LicType5Day,
        lic_type_chr == "3" ~ LicType3Day,
        lic_type_chr == "1" ~ LicType1Day
      )
    ) %>%
    mutate(
      end_date = case_when(
        licence_type == LicTypeAnnual ~ lic_end_date,
        licence_type == LicType5Day ~ start_date + 4,
        licence_type == LicType3Day ~ start_date + 2,
        licence_type == LicType1Day ~ start_date
      )
    )

  invalidLicType <-
    lic_data %>%
    filter(is.na(licence_type)) %>%
    distinct(lic_type_chr) %>%
    pull()

  if (length(invalidLicType) > 0) {
    stop(
      glue(
        "Invalid licence type codes in licence category codes: {str_c(invalidLicType, collapse=\", \")}"
      )
    )
  }

  lic_data <-
    select(lic_data,-lic_type_chr)

  return(lic_data)
}
