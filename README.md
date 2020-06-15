
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

It is important to note that to retrieve licence data, you need to have `ROracle` package installed.  Because the CRAN version of this package is problematic to install directly, it is not a required package.  This allows for automated package testing through GitHub Actions.   However, if you want to retrieve the licence data (only within the DFO network) you must install the `ROracle` package directly from the Oracle website and also install the Oracle client software.

## Example

To open the user interface to carry out iRec Functions:

``` r
library(iRecAnalysisPkg)
library(ROracle) #Needed to access licence database
runUI()
```

