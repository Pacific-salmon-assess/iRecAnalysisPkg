% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SurveyDb.r
\name{loadSurveyResults}
\alias{loadSurveyResults}
\title{LOad Survey Results}
\usage{
loadSurveyResults(
  survey_result_filename,
  survey_start_date,
  exclude_lic_filename,
  survey_adj_filename
)
}
\arguments{
\item{survey_result_filename}{Survey result file name}

\item{survey_start_date}{Survey start data}

\item{exclude_lic_filename}{Exclusion licence file name}

\item{survey_adj_filename}{Survey adjustment file name (no adjustment applied if file name is excluded)}
}
\value{
A data frame with survey results with common stratification columns in common format
}
\description{
Load Survey Results from either EKOS or PRA into a common format.
}
