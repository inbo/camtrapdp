# Get events

Gets the (unique) events from the observations of a Camera Trap Data
Package object. Only observations with `observationLevel == "event"` are
considered.

## Usage

``` r
events(x)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with the events, containing the following columns:

- `deploymentID`

- `eventID`

- `eventStart`

- `eventEnd`

## See also

Other accessor functions:
[`contributors()`](https://inbo.github.io/camtrapdp/reference/contributors.md),
[`deployments()`](https://inbo.github.io/camtrapdp/reference/deployments.md),
[`individuals()`](https://inbo.github.io/camtrapdp/reference/individuals.md),
[`locations()`](https://inbo.github.io/camtrapdp/reference/locations.md),
[`media()`](https://inbo.github.io/camtrapdp/reference/media.md),
[`observations()`](https://inbo.github.io/camtrapdp/reference/observations.md),
[`taxa()`](https://inbo.github.io/camtrapdp/reference/taxa.md)

## Examples

``` r
x <- example_dataset()
events(x)
#> # A tibble: 31 × 4
#>    deploymentID eventID  eventStart          eventEnd           
#>    <chr>        <chr>    <dttm>              <dttm>             
#>  1 00a2c20d     4bb69c45 2020-05-30 02:57:37 2020-05-30 02:57:44
#>  2 00a2c20d     f99bfff4 2020-05-31 04:05:10 2020-05-31 04:05:16
#>  3 00a2c20d     45abeadc 2020-05-31 20:06:43 2020-05-31 20:07:36
#>  4 00a2c20d     ea72c74f 2020-06-05 02:49:20 2020-06-05 02:49:32
#>  5 00a2c20d     52107a58 2020-06-06 04:11:07 2020-06-06 04:11:13
#>  6 00a2c20d     7363b68a 2020-06-09 03:16:11 2020-06-09 03:16:17
#>  7 00a2c20d     79204343 2020-06-12 04:04:29 2020-06-12 04:04:55
#>  8 00a2c20d     14059fd2 2020-06-22 02:11:14 2020-06-22 02:11:20
#>  9 00a2c20d     780c49bd 2020-06-27 01:19:06 2020-06-27 01:19:11
#> 10 00a2c20d     99880973 2020-07-01 09:40:42 2020-07-01 09:41:41
#> # ℹ 21 more rows
```
