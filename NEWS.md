# iRecAnalysisPkg 1.1.2

* Add fishing year, month, and day to export and adjustment files for EKOS responses
* Fix export of single month survey results
* Remove empty columns when load adjustment file, likely added by Excel

# iRecAnalysisPkg 1.1.1

* Fix log messages to translate Unicode to ASCII to prevent excel file corruption

# iRecAnalysisPkg 1.1.0

* Implement "Export" for a single survey
* Remove deprecated `GetTimeStampText()` function, use the `getTimeStampText()` function
* Add option to stratify estimates by salmon stamp and purchase periods (e.g. in survey, early season, late season)

NOTE: New configuration for stratification: `stamp_stratify`, `period_stratify`, and `period_stratify_date`.  
 * Stratify by salmon stamp purchase by setting `stamp_stratify` to `yes`.
 * Stratify survey results based on licences purchased during the survey by setting `period_stratify` to `yes`.   
 * Stratify based on early and late season by setting `period_stratify` to `yes` and `period_stratify_date` to the date to split (e.g. 20200701 to identify July 1st 2020 as the start of the late season results)

# iRecAnalysisPkg 1.0.2

* Set R and Oracle driver time zones to the Pacific Time.
* Fix misspelling in survey results "rrawn" -> "prawn"
* Show package version number in shiny UI (bottom left panel)

# iRecAnalysisPkg 1.0.1

* Add alias for `getTimeStampText()` that provides warning to update the function call
* Add parameter to specify the port for the shiny server with `runUI()` call
* Text box on shiny UI to provide login info
* Handle new checkcrabsprawns questions starting April 2020 survey

# iRecAnalysisPkg 1.0.0

* Initial release of the updated iRec Analysis Package
* Standardized on a single licence file format for all years
* Add ability to refresh licence file directly from licencing system (DFO internal)
