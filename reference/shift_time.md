# Shift date-times

Shifts date-times for selected deployments (and associated media and
observations) by a specified duration. This function can be used to
correct date-time issues such as incorrectly set time zones.

- Deployments: `deploymentStart` and `deploymentEnd` are updated and
  `timestampIssues` is set to `FALSE`.

- Media: `timestamp` is updated.

- Observations: `eventStart` and `eventEnd` are updated.

- Metadata (`x$temporal`) are updated to match the new temporal scope.

## Usage

``` r
shift_time(x, deployment_id, duration)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- deployment_id:

  One or more deploymentIDs.

- duration:

  Difference between the current and new date-times. Provide as a
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  or [difftime](https://rdrr.io/r/base/difftime.html).

## Value

`x` with shifted date-times.

## See also

Other transformation functions:
[`merge_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/merge_camtrapdp.md),
[`round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.md),
[`update_taxon()`](https://inbo.github.io/camtrapdp/reference/update_taxon.md),
[`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md),
[`write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.md)

## Examples

``` r
# Set desired duration between current and new date-times (e.g. 4 hours earlier)
library(lubridate, warn.conflicts = FALSE)
duration(-4, units = "hours")
#> [1] "-14400s (~-4 hours)"

# Or calculate one based on two date-times
current <- ymd_hms("2024-04-01T04:00:00", tz = "UTC")
new <- ymd_hms("2024-04-01T00:00:00", tz = "UTC")
duration <- as.duration(interval(current, new))

# Shift date-times for 2 deployments
x <- example_dataset()
x_shifted <- shift_time(x, c("00a2c20d", "29b7d356"), duration)
#> ✔ Date-times in selected deployments, media and observations were shifted by
#>   -14400s (~-4 hours). E.g. 2020-05-30 02:57:37 is now 2020-05-29 22:57:37.

# Inspect results
deployments(x)[, c("deploymentID", "deploymentStart", "deploymentEnd")]
#> # A tibble: 4 × 3
#>   deploymentID deploymentStart     deploymentEnd      
#>   <chr>        <dttm>              <dttm>             
#> 1 00a2c20d     2020-05-30 02:57:37 2020-07-01 09:41:41
#> 2 29b7d356     2020-07-29 05:29:41 2020-08-08 04:20:40
#> 3 577b543a     2020-06-19 21:00:00 2020-06-28 23:33:22
#> 4 62c200a9     2021-03-27 20:38:18 2021-04-18 21:25:00
deployments(x_shifted)[, c("deploymentID", "deploymentStart", "deploymentEnd")]
#> # A tibble: 4 × 3
#>   deploymentID deploymentStart     deploymentEnd      
#>   <chr>        <dttm>              <dttm>             
#> 1 00a2c20d     2020-05-29 22:57:37 2020-07-01 05:41:41
#> 2 29b7d356     2020-07-29 01:29:41 2020-08-08 00:20:40
#> 3 577b543a     2020-06-19 21:00:00 2020-06-28 23:33:22
#> 4 62c200a9     2021-03-27 20:38:18 2021-04-18 21:25:00
```
