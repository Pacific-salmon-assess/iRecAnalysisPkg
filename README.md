
# Internet Recreational Catch Analysis Package

<!-- badges: start -->
[![R build status](https://github.com/nick-komick/iRecAnalysisPkg/workflows/R-CMD-check/badge.svg)](https://github.com/nick-komick/iRecAnalysisPkg/actions)
<!-- badges: end -->

The goal of iRecAnalysisPkg is to analyse the results of catch reporting from the internet survey of recreational licence holders.

For the lastest notes on package modifications check the [NEWS Page](https://github.com/Pacific-salmon-assess/iRecAnalysisPkg/blob/master/NEWS.md)

## Installation

You can install the released version of iRecAnalysisPkg from [GitHub](https://github.com/Pacific-salmon-assess/iRecAnalysisPkg) with:

``` r
install.packages("remotes")
remotes::install_git("https://github.com/Pacific-salmon-assess/iRecAnalysisPkg.git")
```

## Example

To open the user interface to carry out iRec Functions:

``` r
library(iRecAnalysisPkg)
library(ROracle) #Needed to access licence database
runUI()
```

