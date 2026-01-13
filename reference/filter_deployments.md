# Filter deployments

Subsets deployments in a Camera Trap Data Package object, retaining all
rows that satisfy the conditions.

- Media are filtered on associated `deploymentID`.

- Observations are filtered on associated `deploymentID`.

- Metadata (`x$spatial`, `x$temporal` and `x$taxonomic`) are updated to
  match the filtered deployments.

## Usage

``` r
filter_deployments(x, ...)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- ...:

  Filtering conditions, see
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

## Value

`x` filtered.

## See also

Other filter functions:
[`filter_media()`](https://inbo.github.io/camtrapdp/reference/filter_media.md),
[`filter_observations()`](https://inbo.github.io/camtrapdp/reference/filter_observations.md)

## Examples

``` r
x <- example_dataset()

# Filtering returns x, so pipe with deployments() to see the result
x %>%
  filter_deployments(deploymentID == "62c200a9") %>%
  deployments()
#> # A tibble: 1 × 24
#>   deploymentID locationID locationName  latitude longitude coordinateUncertainty
#>   <chr>        <chr>      <chr>            <dbl>     <dbl>                 <dbl>
#> 1 62c200a9     ce943ced   B_DM_val 4_'…     50.7      4.01                   187
#> # ℹ 18 more variables: deploymentStart <dttm>, deploymentEnd <dttm>,
#> #   setupBy <chr>, cameraID <chr>, cameraModel <chr>, cameraDelay <dbl>,
#> #   cameraHeight <dbl>, cameraDepth <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, detectionDistance <dbl>, timestampIssues <lgl>,
#> #   baitUse <lgl>, featureType <fct>, habitat <chr>, deploymentGroups <chr>,
#> #   deploymentTags <chr>, deploymentComments <chr>

# Filtering on deployments also affects associated media and observations
x_filtered <- filter_deployments(x, deploymentID == "62c200a9")
media(x_filtered)
#> # A tibble: 60 × 12
#>    mediaID  deploymentID captureMethod   timestamp           filePath filePublic
#>    <chr>    <chr>        <fct>           <dttm>              <chr>    <lgl>     
#>  1 fb58a2b9 62c200a9     activityDetect… 2021-03-27 20:38:18 https:/… TRUE      
#>  2 0bb2566e 62c200a9     activityDetect… 2021-03-27 20:38:18 https:/… TRUE      
#>  3 a6a7a04c 62c200a9     activityDetect… 2021-03-27 20:38:19 https:/… TRUE      
#>  4 f47b05e6 62c200a9     activityDetect… 2021-03-27 20:38:20 https:/… TRUE      
#>  5 8c541e7b 62c200a9     activityDetect… 2021-03-27 20:38:20 https:/… TRUE      
#>  6 e6c2cb72 62c200a9     activityDetect… 2021-03-27 20:38:21 https:/… TRUE      
#>  7 3945bb1f 62c200a9     activityDetect… 2021-03-27 20:38:21 https:/… TRUE      
#>  8 ad62d7b9 62c200a9     activityDetect… 2021-03-27 20:38:22 https:/… TRUE      
#>  9 7b95c483 62c200a9     activityDetect… 2021-03-27 20:38:23 https:/… TRUE      
#> 10 f24826a2 62c200a9     activityDetect… 2021-03-27 20:38:23 https:/… TRUE      
#> # ℹ 50 more rows
#> # ℹ 6 more variables: fileName <chr>, fileMediatype <chr>, exifData <chr>,
#> #   favorite <lgl>, mediaComments <chr>, eventID <chr>
observations(x_filtered)
#> # A tibble: 65 × 32
#>    observationID deploymentID mediaID  eventID  eventStart         
#>    <chr>         <chr>        <chr>    <chr>    <dttm>             
#>  1 a0431321      62c200a9     NA       16537357 2021-03-27 20:38:18
#>  2 fb58a2b9_1    62c200a9     fb58a2b9 16537357 2021-03-27 20:38:18
#>  3 0bb2566e_1    62c200a9     0bb2566e 16537357 2021-03-27 20:38:18
#>  4 a6a7a04c_1    62c200a9     a6a7a04c 16537357 2021-03-27 20:38:19
#>  5 f47b05e6_1    62c200a9     f47b05e6 16537357 2021-03-27 20:38:20
#>  6 8c541e7b_1    62c200a9     8c541e7b 16537357 2021-03-27 20:38:20
#>  7 e6c2cb72_1    62c200a9     e6c2cb72 16537357 2021-03-27 20:38:21
#>  8 3945bb1f_1    62c200a9     3945bb1f 16537357 2021-03-27 20:38:21
#>  9 ad62d7b9_1    62c200a9     ad62d7b9 16537357 2021-03-27 20:38:22
#> 10 7b95c483_1    62c200a9     7b95c483 16537357 2021-03-27 20:38:23
#> # ℹ 55 more rows
#> # ℹ 27 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, …

# Filtering on multiple conditions (combined with &)
x %>%
  filter_deployments(latitude > 51.0, longitude > 5.0) %>%
  deployments()
#> # A tibble: 2 × 24
#>   deploymentID locationID locationName  latitude longitude coordinateUncertainty
#>   <chr>        <chr>      <chr>            <dbl>     <dbl>                 <dbl>
#> 1 29b7d356     2df5259b   B_DL_val 5_b…     51.2      5.66                   187
#> 2 577b543a     ff1535c0   B_DL_val 3_d…     51.2      5.66                   187
#> # ℹ 18 more variables: deploymentStart <dttm>, deploymentEnd <dttm>,
#> #   setupBy <chr>, cameraID <chr>, cameraModel <chr>, cameraDelay <dbl>,
#> #   cameraHeight <dbl>, cameraDepth <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, detectionDistance <dbl>, timestampIssues <lgl>,
#> #   baitUse <lgl>, featureType <fct>, habitat <chr>, deploymentGroups <chr>,
#> #   deploymentTags <chr>, deploymentComments <chr>

# Filtering on dates is easiest with lubridate
library(lubridate, warn.conflicts = FALSE)
x %>%
  filter_deployments(
    deploymentStart >= lubridate::as_date("2020-06-19"),
    deploymentEnd <= lubridate::as_date("2020-08-30")
  ) %>%
  deployments()
#> # A tibble: 2 × 24
#>   deploymentID locationID locationName  latitude longitude coordinateUncertainty
#>   <chr>        <chr>      <chr>            <dbl>     <dbl>                 <dbl>
#> 1 29b7d356     2df5259b   B_DL_val 5_b…     51.2      5.66                   187
#> 2 577b543a     ff1535c0   B_DL_val 3_d…     51.2      5.66                   187
#> # ℹ 18 more variables: deploymentStart <dttm>, deploymentEnd <dttm>,
#> #   setupBy <chr>, cameraID <chr>, cameraModel <chr>, cameraDelay <dbl>,
#> #   cameraHeight <dbl>, cameraDepth <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, detectionDistance <dbl>, timestampIssues <lgl>,
#> #   baitUse <lgl>, featureType <fct>, habitat <chr>, deploymentGroups <chr>,
#> #   deploymentTags <chr>, deploymentComments <chr>
```
