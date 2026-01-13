# Filter media

Subsets media in a Camera Trap Data Package object, retaining all rows
that satisfy the conditions.

- Deployments are not filtered.

- Observations are filtered on associated `mediaID` (for media-based
  observations) and `eventID` (for event-based observations).

- Metadata (`x$taxonomic`) are updated to match the filtered
  observations.

## Usage

``` r
filter_media(x, ...)
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
[`filter_observations()`](https://inbo.github.io/camtrapdp/reference/filter_observations.md)

## Examples

``` r
x <- example_dataset()

# Filtering returns x, so pipe with media() to see the result
x %>%
  filter_media(captureMethod == "timeLapse") %>%
  media()
#> # A tibble: 3 × 12
#>   mediaID  deploymentID captureMethod timestamp           filePath    filePublic
#>   <chr>    <chr>        <fct>         <dttm>              <chr>       <lgl>     
#> 1 b42ddc97 577b543a     timeLapse     2020-06-19 21:00:00 https://mu… TRUE      
#> 2 72a7147a 577b543a     timeLapse     2020-06-19 22:00:00 https://mu… TRUE      
#> 3 b1d97da9 577b543a     timeLapse     2020-06-28 23:00:00 https://mu… TRUE      
#> # ℹ 6 more variables: fileName <chr>, fileMediatype <chr>, exifData <chr>,
#> #   favorite <lgl>, mediaComments <chr>, eventID <chr>

# Filtering on media also affects associated observations, but not deployments
x_filtered <- filter_media(x, favorite == TRUE)
observations(x_filtered)
#> # A tibble: 4 × 32
#>   observationID deploymentID mediaID  eventID  eventStart         
#>   <chr>         <chr>        <chr>    <chr>    <dttm>             
#> 1 f5707f70      29b7d356     NA       45ee3031 2020-08-02 05:00:14
#> 2 59b38bc6_1    29b7d356     59b38bc6 45ee3031 2020-08-02 05:00:16
#> 3 cc50edaa_1    29b7d356     cc50edaa 45ee3031 2020-08-02 05:00:18
#> 4 0966e552_1    29b7d356     0966e552 45ee3031 2020-08-02 05:00:19
#> # ℹ 27 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, classificationMethod <fct>,
#> #   classifiedBy <chr>, classificationTimestamp <dttm>, …

# Filtering on multiple conditions (combined with &)
x %>%
  filter_media(captureMethod == "activityDetection", filePublic == FALSE) %>%
  media()
#> # A tibble: 60 × 12
#>    mediaID  deploymentID captureMethod   timestamp           filePath filePublic
#>    <chr>    <chr>        <fct>           <dttm>              <chr>    <lgl>     
#>  1 075f9dfd 00a2c20d     activityDetect… 2020-07-01 09:40:42 https:/… FALSE     
#>  2 f99b3751 00a2c20d     activityDetect… 2020-07-01 09:40:43 https:/… FALSE     
#>  3 70ec9558 00a2c20d     activityDetect… 2020-07-01 09:40:45 https:/… FALSE     
#>  4 534a79c0 00a2c20d     activityDetect… 2020-07-01 09:40:45 https:/… FALSE     
#>  5 c66bcfd1 00a2c20d     activityDetect… 2020-07-01 09:40:46 https:/… FALSE     
#>  6 11e92a6f 00a2c20d     activityDetect… 2020-07-01 09:40:46 https:/… FALSE     
#>  7 15b7b50e 00a2c20d     activityDetect… 2020-07-01 09:40:47 https:/… FALSE     
#>  8 9a886c5b 00a2c20d     activityDetect… 2020-07-01 09:40:48 https:/… FALSE     
#>  9 a98fde19 00a2c20d     activityDetect… 2020-07-01 09:40:48 https:/… FALSE     
#> 10 6b57b8cd 00a2c20d     activityDetect… 2020-07-01 09:40:49 https:/… FALSE     
#> # ℹ 50 more rows
#> # ℹ 6 more variables: fileName <chr>, fileMediatype <chr>, exifData <chr>,
#> #   favorite <lgl>, mediaComments <chr>, eventID <chr>

# Filtering on datetimes is easiest with lubridate
library(lubridate, warn.conflicts = FALSE)
x %>%
  filter_media(
    timestamp >= lubridate::as_datetime("2020-08-02 05:01:00"),
    timestamp <= lubridate::as_datetime("2020-08-02 05:02:00")
  ) %>%
  media()
#> # A tibble: 10 × 12
#>    mediaID  deploymentID captureMethod   timestamp           filePath filePublic
#>    <chr>    <chr>        <fct>           <dttm>              <chr>    <lgl>     
#>  1 27c6cb3d 29b7d356     activityDetect… 2020-08-02 05:01:00 https:/… TRUE      
#>  2 d32c67ed 29b7d356     activityDetect… 2020-08-02 05:01:01 https:/… TRUE      
#>  3 be5d44fd 29b7d356     activityDetect… 2020-08-02 05:01:01 https:/… TRUE      
#>  4 08b56231 29b7d356     activityDetect… 2020-08-02 05:01:02 https:/… TRUE      
#>  5 ccb03b7e 29b7d356     activityDetect… 2020-08-02 05:01:02 https:/… TRUE      
#>  6 fbf8292b 29b7d356     activityDetect… 2020-08-02 05:01:03 https:/… TRUE      
#>  7 5b71377d 29b7d356     activityDetect… 2020-08-02 05:01:04 https:/… TRUE      
#>  8 6c4db631 29b7d356     activityDetect… 2020-08-02 05:01:04 https:/… TRUE      
#>  9 a6a07d02 29b7d356     activityDetect… 2020-08-02 05:01:05 https:/… TRUE      
#> 10 73efdfb6 29b7d356     activityDetect… 2020-08-02 05:01:05 https:/… TRUE      
#> # ℹ 6 more variables: fileName <chr>, fileMediatype <chr>, exifData <chr>,
#> #   favorite <lgl>, mediaComments <chr>, eventID <chr>
```
