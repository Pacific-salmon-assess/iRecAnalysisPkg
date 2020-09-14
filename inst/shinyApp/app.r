library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(stringr)
library(glue)
library(fs)

LicYears <- 2012:as.integer(format(Sys.Date(), "%Y"))
LicYears <-
  iRecAnalysisPkg::getLicenceYearText(LicYears) %>%
  sort(decreasing = TRUE)


ui <- fluidPage(
  titlePanel('iRec Survey Processing Interface'),
  sidebarPanel(
    selectInput('lic_year',
                'Licence Year',
                LicYears),
    textInput("nrls_pw", "NRLS Password", ""),
    actionButton(inputId = "annual_analysis",
                 label = "Run Annual Analysis"),
    actionButton(inputId = "refresh_licences",
                 label = "Retrieve NRLS Licences"),
    selectInput('month', 'Month',
                c(month.name[iRecAnalysisPkg:::LicenceMonthStart:12],
                  month.name[1:(iRecAnalysisPkg:::LicenceMonthStart - 1)]),
                selected = month.name[iRecAnalysisPkg:::LicenceMonthStart]),
    actionButton(inputId = "month_analysis",
                 label = "Run Month Analysis"),
    actionButton(inputId = "export",
                 label = "Export Survey Responses"),
    tags$hr(),
    textInput("adj_lic_text", "Adjust Licence IDs", ""),
    actionButton(inputId = "create_adj",
                 label = "Create Adjustment File"),
    tags$hr(),
    tags$p(glue('Version ', paste0(packageVersion("iRecAnalysisPkg"), collapse = ".")))
  ),
  mainPanel(
    tags$h2("Run Log"),
    textOutput('run_month_analysis_log'),
    textOutput('run_annual_analysis_log'),
    textOutput('refresh_nrls_lic_log'),
    textOutput('create_adj_file_log'),
    textOutput('export_file_log'),
    tags$h2("Survey Parameters"),
    tableOutput('survey_param')
  )
)

server <- function(input, output) {

  output$survey_param <- renderTable({
    survey_params <-
      tibble(Parameter = c("iRec Analysis Directory",
                           "Licence Year",
                           "Survey Month"),
             Value = c(iRecAnalysisPkg:::getiRecAnalysisDir(),
                       input$lic_year,
                       input$month))

    year_path <- iRecAnalysisPkg:::getSurveyYearPath(input$lic_year)
    if (dir_exists(year_path) == FALSE) {
      return(glue("The survey year directory '{year_path}' does not exist"))
    }

    year_config <- iRecAnalysisPkg:::loadAnalysisYearConfig(input$lic_year)

    survey_dir <- iRecAnalysisPkg:::getSurveyPath(input$month,
                                                  year_config$year_data_path,
                                                  year_config$annual_expire_date)

    if (dir_exists(survey_dir) == FALSE) {
      return(glue("The survey directory '{survey_dir}' does not exist"))
    }

    config <- iRecAnalysisPkg::loadAnalysisConfig(input$lic_year,
                                                   input$month,
                                                   year_config = year_config,
                                                   irec_dir_root = iRecAnalysisPkg:::getiRecAnalysisDir())
    if (is.list(config)) {
      config <-
        config %>%
        map(paste0, collapse = ",")

      survey_params <-
        tibble(Parameter = names(config), Value = unlist(config)) %>%
        bind_rows(survey_params)
    }
    return(survey_params)
  })

  run_month_analysis_func <- eventReactive(input$month_analysis, {
    lic_year <- as.integer(str_sub(input$lic_year,1,4))
    result <- iRecAnalysisPkg::runSingleMonthAnalysis(lic_year, input$month)
    return(result)
  })

  run_annual_analysis_func <- eventReactive(input$annual_analysis, {
    lic_year <- as.integer(str_sub(input$lic_year,1,4))
    result <- iRecAnalysisPkg::runAnnualAnalysis(lic_year)
    return(result)
  })

  refresh_nrls_lic_func <- eventReactive(input$refresh_licences, {
    lic_year <- as.integer(str_sub(input$lic_year,1,4))
    result <- iRecAnalysisPkg::refreshNrlsLicenceFile(lic_year,
                                                      password = input$nrls_pw)
    return(result)
  })

  create_adj_func <- eventReactive(input$create_adj, {
    config <- iRecAnalysisPkg::loadAnalysisConfig(input$lic_year, input$month)
    licence_ids <- iRecAnalysisPkg:::parseLicenceIds(input$adj_lic_text)
    return(iRecAnalysisPkg::createAdjFile(licence_ids, config))
  })

  export_func <- eventReactive(input$export, {
    lic_year <- as.integer(str_sub(input$lic_year,1,4))
    export_filename <- exportSingleMonthResults(lic_year, input$month)
    return(export_filename)
  })

  output$run_month_analysis_log <- renderText({
    analysis_filename <- run_month_analysis_func()
    glue("Analysis result at: {analysis_filename}")
  })

  output$run_annual_analysis_log <- renderText({
    analysis_filename <- run_annual_analysis_func()
    glue("Analysis result at: {analysis_filename}")
  })

  output$refresh_nrls_lic_log <- renderText({
    refresh_log <- refresh_nrls_lic_func()
    glue("Refresh NRLS licence result: {refresh_log}")
  })

  output$create_adj_file_log <- renderText({
    adj_filename <- create_adj_func()
    glue("Adjustment file created at: {adj_filename}")
  })

  output$export_file_log <- renderText({
    export_filename <- export_func()
    glue("Export file created at: {export_filename}")
  })
}

shinyApp(ui = ui, server = server)

