
<!-- README.md is generated from README.Rmd. Please edit that file -->

# camtrapdp

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/camtrapdp)](https://CRAN.R-project.org/package=camtrapdp)
[![CRAN
checks](https://badges.cranchecks.info/worst/camtrapdp.svg)](https://cran.r-project.org/web/checks/check_results_camtrapdp.html)
[![R-CMD-check](https://github.com/inbo/camtrapdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inbo/camtrapdp/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/inbo/camtrapdp/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/camtrapdp/)
[![repo
status](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11490269.svg)](https://doi.org/10.5281/zenodo.11490269)
<!-- badges: end -->

Camtrapdp is an R package to read and manipulate Camera Trap Data
Packages (Camtrap DP). [Camtrap DP](https://camtrap-dp.tdwg.org) is a
data exchange format for camera trap data. With camtrapdp you can read,
filter and transform data (including to [Darwin
Core](https://dwc.tdwg.org)) before further analysis in
e.g. [camtraptor](https://inbo.github.io/camtraptor/) or
[camtrapR](https://cran.r-project.org/package=camtrapR).

To get started, see:

- [Function
  reference](https://inbo.github.io/camtrapdp/reference/index.html):
  overview of all functions.

## Installation

Install the latest released version from CRAN:

``` r
install.packages("camtrapdp")
```

Or the development version from
[GitHub](https://github.com/inbo/camtrapdp):

``` r
# install.packages("devtools")
devtools::install_github("inbo/camtrapdp")
```

## Usage

With camtrapdp you can **read** a Camtrap DP dataset into your R
environment:

``` r
library(camtrapdp, warn.conflicts = FALSE)

file <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0/example/datapackage.json"
x <- read_camtrapdp(file)
x
#> A Camera Trap Data Package "camtrap-dp-example-dataset" with 3 tables:
#> • deployments: 4 rows
#> • media: 423 rows
#> • observations: 549 rows
#> 
#> And 1 additional resource:
#> • individuals
#> Use `unclass()` to print the Data Package as a list.
```

`read_camtrapdp()` will automatically **upgrade** an older version of
Camtrap DP to the latest version. It will also make the data easier to
use, by assigning taxonomic information (found in the metadata) to the
observations and `eventID`s (found in the observations) to the media.

To **access** the data, use one of the [accessor
functions](https://inbo.github.io/camtrapdp/reference/index.html#accessor-and-assignment-functions)
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

You can also **filter** data with one of the [filter
functions](https://inbo.github.io/camtrapdp/reference/index.html#filter-functions),
which automatically filter the related data. For example, here we filter
observations on scientific name(s) and return the associated events in
that subset:

``` r
x %>%
  filter_observations(
    scientificName %in% c("Martes foina", "Mustela putorius")
  ) %>%
  events()
#> # A tibble: 4 × 4
#>   deploymentID eventID  eventStart          eventEnd           
#>   <chr>        <chr>    <dttm>              <dttm>             
#> 1 577b543a     976129e2 2020-06-19 22:31:51 2020-06-19 22:31:56
#> 2 577b543a     b4b39b00 2020-06-23 23:33:53 2020-06-23 23:33:58
#> 3 577b543a     5be4f4ed 2020-06-28 22:01:12 2020-06-28 22:01:18
#> 4 577b543a     a60816f2 2020-06-28 23:33:16 2020-06-28 23:33:22
```

For more functionality, see the [function
reference](https://inbo.github.io/camtrapdp/reference/index.html).

## GBIF pipeline

The Global Biodiversity Information Facility
([GBIF](https://www.gbif.org/)) uses camtrapdp to process Camera Trap
Data Packages published with the Integrated Publishing Toolkit
([IPT](https://www.gbif.org/ipt)). Datasets are first read with
`read_camtrapdp()` and then converted to Darwin Core with `write_dwc()`
and EML with `write_eml()`. See the [Camtrap DP pipeline
repository](https://github.com/gbif/camtrap-dp-pipeline/tree/master/docker)
for details.

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
