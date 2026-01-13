# Filter observations

Subsets observations in a Camera Trap Data Package object, retaining all
rows that satisfy the conditions.

- Deployments are not filtered.

- Media are filtered on associated `mediaID` (for media-based
  observations) and `eventID` (for event-based observations). Filter on
  `observationLevel == "media"` to only retain directly linked media.

- Metadata (`x$taxonomic`) are updated to match the filtered
  observations.

## Usage

``` r
filter_observations(x, ...)
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
[`filter_deployments()`](https://inbo.github.io/camtrapdp/reference/filter_deployments.md),
[`filter_media()`](https://inbo.github.io/camtrapdp/reference/filter_media.md)

## Examples

``` r
x <- example_dataset()

# Filtering returns x, so pipe with observations() to see the result
x %>%
  filter_observations(observationType == "animal") %>%
  observations()
#> # A tibble: 366 × 32
#>    observationID deploymentID mediaID  eventID  eventStart         
#>    <chr>         <chr>        <chr>    <chr>    <dttm>             
#>  1 705e6036      00a2c20d     NA       4bb69c45 2020-05-30 02:57:37
#>  2 07840dcc_1    00a2c20d     07840dcc 4bb69c45 2020-05-30 02:57:37
#>  3 401386c7_1    00a2c20d     401386c7 4bb69c45 2020-05-30 02:57:40
#>  4 ca3ff293_1    00a2c20d     ca3ff293 4bb69c45 2020-05-30 02:57:40
#>  5 e8b8e44c_1    00a2c20d     e8b8e44c 4bb69c45 2020-05-30 02:57:41
#>  6 b8690a15_1    00a2c20d     b8690a15 4bb69c45 2020-05-30 02:57:41
#>  7 64734615_1    00a2c20d     64734615 4bb69c45 2020-05-30 02:57:42
#>  8 12ce5004_1    00a2c20d     12ce5004 4bb69c45 2020-05-30 02:57:43
#>  9 f372acaf_1    00a2c20d     f372acaf 4bb69c45 2020-05-30 02:57:43
#> 10 97140835_1    00a2c20d     97140835 4bb69c45 2020-05-30 02:57:44
#> # ℹ 356 more rows
#> # ℹ 27 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, …

# Filtering on observations also affects associated media, but not deployments
x %>%
  filter_observations(
    scientificName == "Vulpes vulpes",
    observationLevel == "event"
  ) %>%
  media()
#> # A tibble: 10 × 12
#>    mediaID  deploymentID captureMethod   timestamp           filePath filePublic
#>    <chr>    <chr>        <fct>           <dttm>              <chr>    <lgl>     
#>  1 f011d0e6 577b543a     activityDetect… 2020-06-26 02:09:25 https:/… TRUE      
#>  2 b3374ef1 577b543a     activityDetect… 2020-06-26 02:09:25 https:/… TRUE      
#>  3 1c601054 577b543a     activityDetect… 2020-06-26 02:09:26 https:/… TRUE      
#>  4 b43f1608 577b543a     activityDetect… 2020-06-26 02:09:27 https:/… TRUE      
#>  5 81523d1f 577b543a     activityDetect… 2020-06-26 02:09:27 https:/… TRUE      
#>  6 f4a5ec0f 577b543a     activityDetect… 2020-06-26 02:09:28 https:/… TRUE      
#>  7 5e8f4538 577b543a     activityDetect… 2020-06-26 02:09:28 https:/… TRUE      
#>  8 ee7f52c4 577b543a     activityDetect… 2020-06-26 02:09:29 https:/… TRUE      
#>  9 825e9af1 577b543a     activityDetect… 2020-06-26 02:09:30 https:/… TRUE      
#> 10 2e611f13 577b543a     activityDetect… 2020-06-26 02:09:30 https:/… TRUE      
#> # ℹ 6 more variables: fileName <chr>, fileMediatype <chr>, exifData <chr>,
#> #   favorite <lgl>, mediaComments <chr>, eventID <chr>
x %>%
  filter_observations(
    scientificName == "Vulpes vulpes",
    observationLevel == "media"
  ) %>%
  media()
#> # A tibble: 6 × 12
#>   mediaID  deploymentID captureMethod    timestamp           filePath filePublic
#>   <chr>    <chr>        <fct>            <dttm>              <chr>    <lgl>     
#> 1 f011d0e6 577b543a     activityDetecti… 2020-06-26 02:09:25 https:/… TRUE      
#> 2 b3374ef1 577b543a     activityDetecti… 2020-06-26 02:09:25 https:/… TRUE      
#> 3 1c601054 577b543a     activityDetecti… 2020-06-26 02:09:26 https:/… TRUE      
#> 4 b43f1608 577b543a     activityDetecti… 2020-06-26 02:09:27 https:/… TRUE      
#> 5 81523d1f 577b543a     activityDetecti… 2020-06-26 02:09:27 https:/… TRUE      
#> 6 f4a5ec0f 577b543a     activityDetecti… 2020-06-26 02:09:28 https:/… TRUE      
#> # ℹ 6 more variables: fileName <chr>, fileMediatype <chr>, exifData <chr>,
#> #   favorite <lgl>, mediaComments <chr>, eventID <chr>

# Filtering on multiple conditions (combined with &)
x %>%
  filter_observations(
    deploymentID == "577b543a",
    scientificName %in% c("Martes foina", "Mustela putorius")
  ) %>%
  observations()
#> # A tibble: 23 × 32
#>    observationID deploymentID mediaID  eventID  eventStart         
#>    <chr>         <chr>        <chr>    <chr>    <dttm>             
#>  1 e622b65c      577b543a     NA       976129e2 2020-06-19 22:31:51
#>  2 a8848796_1    577b543a     a8848796 976129e2 2020-06-19 22:31:51
#>  3 e470918a_1    577b543a     e470918a 976129e2 2020-06-19 22:31:51
#>  4 5ff74dc7_1    577b543a     5ff74dc7 976129e2 2020-06-19 22:31:52
#>  5 fb662fab      577b543a     NA       b4b39b00 2020-06-23 23:33:53
#>  6 91de8c9e_1    577b543a     91de8c9e b4b39b00 2020-06-23 23:33:53
#>  7 7f0758ac_1    577b543a     7f0758ac b4b39b00 2020-06-23 23:33:54
#>  8 7fc6cb24_1    577b543a     7fc6cb24 b4b39b00 2020-06-23 23:33:54
#>  9 dfdc4c48      577b543a     NA       5be4f4ed 2020-06-28 22:01:12
#> 10 e9c1a382_1    577b543a     e9c1a382 5be4f4ed 2020-06-28 22:01:12
#> # ℹ 13 more rows
#> # ℹ 27 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, …

# Filtering on datetimes is easiest with lubridate
library(lubridate, warn.conflicts = FALSE)
x %>%
  filter_observations(
    eventStart >= lubridate::as_datetime("2020-06-19 22:00:00"),
    eventEnd <= lubridate::as_datetime("2020-06-19 22:10:00")
  ) %>%
  observations()
#> # A tibble: 12 × 32
#>    observationID deploymentID mediaID  eventID  eventStart         
#>    <chr>         <chr>        <chr>    <chr>    <dttm>             
#>  1 72a7147a_1    577b543a     72a7147a a80896b5 2020-06-19 22:00:00
#>  2 d350d2bc      577b543a     NA       5fbf69a4 2020-06-19 22:05:55
#>  3 44201e9e_1    577b543a     44201e9e 5fbf69a4 2020-06-19 22:05:55
#>  4 4c8086c7_1    577b543a     4c8086c7 5fbf69a4 2020-06-19 22:05:55
#>  5 aea55534_1    577b543a     aea55534 5fbf69a4 2020-06-19 22:05:56
#>  6 7329cccb_1    577b543a     7329cccb 5fbf69a4 2020-06-19 22:05:57
#>  7 46fb91af_1    577b543a     46fb91af 5fbf69a4 2020-06-19 22:05:57
#>  8 8608b7ef_1    577b543a     8608b7ef 5fbf69a4 2020-06-19 22:05:58
#>  9 6709f244_1    577b543a     6709f244 5fbf69a4 2020-06-19 22:05:58
#> 10 d1f2810a_1    577b543a     d1f2810a 5fbf69a4 2020-06-19 22:05:59
#> 11 fe0b0683_1    577b543a     fe0b0683 5fbf69a4 2020-06-19 22:06:00
#> 12 a4a2edd4_1    577b543a     a4a2edd4 5fbf69a4 2020-06-19 22:06:00
#> # ℹ 27 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, classificationMethod <fct>,
#> #   classifiedBy <chr>, classificationTimestamp <dttm>, …
```
