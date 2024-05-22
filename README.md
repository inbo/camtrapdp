
<!-- README.md is generated from README.Rmd. Please edit that file -->

# camtrapdp

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/camtraptor)](https://CRAN.R-project.org/package=camtrapdp)
[![CRAN
checks](https://badges.cranchecks.info/worst/camtrapdp.svg)](https://cran.r-project.org/web/checks/check_results_camtrapdp.html)
[![R-CMD-check](https://github.com/inbo/camtrapdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inbo/camtrapdp/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/inbo/camtraptor/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/camtrapdp/)
[![repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

Camtrapdp is the R interface to [Camera Trap Data Package (Camtrap
DP)](https://camtrap-dp.tdwg.org), a data exchange format for camera
trap data. It is designed to read, filter and transform data (including
to [Darwin Core](https://dwc.tdwg.org)) before further analysis in
e.g. [camtraptor](https://inbo.github.io/camtraptor/) or
[camtrapR](https://cran.r-project.org/package=camtrapR).

To get started, see:

- [Function
  reference](https://inbo.github.io/camtrapdp/reference/index.html):
  overview of all functions.

## Installation

You can install the development version of camtrapdp from
[GitHub](https://github.com/inbo/camtrapdp) with:

``` r
# install.packages("devtools")
devtools::install_github("inbo/camtrapdp")
```

## Usage

With camtrapdp you can **read** a (downloaded) Camtrap DP dataset into
your R environment:

``` r
library(camtrapdp)

file <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
x <- read_camtrapdp(file)
x
#> A Data Package with 4 resources:
#> • deployments
#> • media
#> • observations
#> • individuals
#> Use `unclass()` to print the Data Package as a list.
```

`read_camtrapdp()` will automatically **convert** an older version of
Camtrap DP to the latest version. It will also make the data easier to
use, by assigning taxonomic information (found in the metadata) to the
observations and `eventID`s (found in the observations) to the media.

To access the data, use one of the [accessor
functions](https://inbo.github.io/camtrapdp/reference/index.html#accessor-functions)
like `locations()`:

``` r
locations(x)
#> # A tibble: 4 × 5
#>   locationID locationName               latitude longitude coordinateUncertainty
#>   <chr>      <chr>                         <dbl>     <dbl>                 <dbl>
#> 1 e254a13c   B_HS_val 2_processiepark       51.5      4.77                   187
#> 2 2df5259b   B_DL_val 5_beek kleine vi…     51.2      5.66                   187
#> 3 ff1535c0   B_DL_val 3_dikke boom          51.2      5.66                   187
#> 4 ce943ced   B_DM_val 4_'t WAD              50.7      4.01                   187
```

One can also **filter** data, which will automatically filter the
related data. For example, here are all the event-based observations
that have a media file that was marked as favourite:

``` r
x %>%
  filter_observations(observationLevel == "event") %>%
  filter_media(favorite == TRUE) %>%
  observations()
#> # A tibble: 1 × 32
#>   observationID deploymentID mediaID eventID  eventStart         
#>   <chr>         <chr>        <chr>   <chr>    <dttm>             
#> 1 f5707f70      29b7d356     <NA>    45ee3031 2020-08-02 05:00:14
#> # ℹ 27 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, classificationMethod <fct>,
#> #   classifiedBy <chr>, classificationTimestamp <dttm>, …
```

## Meta

- We welcome
  [contributions](https://inbo.github.io/camtrapdp/CONTRIBUTING.html)
  including bug reports.
- License: MIT
- Get [citation
  information](https://inbo.github.io/camtrapdp/authors.html#citation)
  for camtrapdp in R doing `citation("camtrapdp")`.
- Please note that this project is released with a [Contributor Code of
  Conduct](https://inbo.github.io/camtrapdp/CODE_OF_CONDUCT.html). By
  participating in this project you agree to abide by its terms.
