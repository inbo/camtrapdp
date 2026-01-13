# Get or set observations

`observations()` gets the observations from a Camera Trap Data Package
object.

`observations()<-` is the assignment equivalent.

- It should only be used within other functions, where the expected data
  structure can be guaranteed.

- Metadata (`x$taxonomic`) are updated to match the assigned
  observations.

## Usage

``` r
observations(x)

observations(x) <- value
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- value:

  A data frame to assign as observations.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with observations.

## See also

Other accessor functions:
[`contributors()`](https://inbo.github.io/camtrapdp/reference/contributors.md),
[`deployments()`](https://inbo.github.io/camtrapdp/reference/deployments.md),
[`events()`](https://inbo.github.io/camtrapdp/reference/events.md),
[`individuals()`](https://inbo.github.io/camtrapdp/reference/individuals.md),
[`locations()`](https://inbo.github.io/camtrapdp/reference/locations.md),
[`media()`](https://inbo.github.io/camtrapdp/reference/media.md),
[`taxa()`](https://inbo.github.io/camtrapdp/reference/taxa.md)

## Examples

``` r
x <- example_dataset()
# Get the observations
observations(x)
#> # A tibble: 549 × 32
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
#> # ℹ 539 more rows
#> # ℹ 27 more variables: eventEnd <dttm>, observationLevel <fct>,
#> #   observationType <fct>, cameraSetupType <fct>, scientificName <chr>,
#> #   count <dbl>, lifeStage <fct>, sex <fct>, behavior <chr>,
#> #   individualID <chr>, individualPositionRadius <dbl>,
#> #   individualPositionAngle <dbl>, individualSpeed <dbl>, bboxX <dbl>,
#> #   bboxY <dbl>, bboxWidth <dbl>, bboxHeight <dbl>, …

# Set observations (not recommended outside a function)
observations(x) <- head(observations(x), 1)
```
