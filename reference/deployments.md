# Get or set deployments

`deployments()` gets the deployments from a Camera Trap Data Package
object.

`deployments()<-` is the assignment equivalent.

- It should only be used within other functions, where the expected data
  structure can be guaranteed.

- Metadata (`x$spatial` and `x$temporal`) are updated to match the
  assigned deployments.

## Usage

``` r
deployments(x)

deployments(x) <- value
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- value:

  A data frame to assign as deployments.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with deployments.

## See also

Other accessor functions:
[`contributors()`](https://inbo.github.io/camtrapdp/reference/contributors.md),
[`events()`](https://inbo.github.io/camtrapdp/reference/events.md),
[`individuals()`](https://inbo.github.io/camtrapdp/reference/individuals.md),
[`locations()`](https://inbo.github.io/camtrapdp/reference/locations.md),
[`media()`](https://inbo.github.io/camtrapdp/reference/media.md),
[`observations()`](https://inbo.github.io/camtrapdp/reference/observations.md),
[`taxa()`](https://inbo.github.io/camtrapdp/reference/taxa.md)

## Examples

``` r
x <- example_dataset()
# Get deployments
deployments(x)
#> # A tibble: 4 × 24
#>   deploymentID locationID locationName  latitude longitude coordinateUncertainty
#>   <chr>        <chr>      <chr>            <dbl>     <dbl>                 <dbl>
#> 1 00a2c20d     e254a13c   B_HS_val 2_p…     51.5      4.77                   187
#> 2 29b7d356     2df5259b   B_DL_val 5_b…     51.2      5.66                   187
#> 3 577b543a     ff1535c0   B_DL_val 3_d…     51.2      5.66                   187
#> 4 62c200a9     ce943ced   B_DM_val 4_'…     50.7      4.01                   187
#> # ℹ 18 more variables: deploymentStart <dttm>, deploymentEnd <dttm>,
#> #   setupBy <chr>, cameraID <chr>, cameraModel <chr>, cameraDelay <dbl>,
#> #   cameraHeight <dbl>, cameraDepth <dbl>, cameraTilt <dbl>,
#> #   cameraHeading <dbl>, detectionDistance <dbl>, timestampIssues <lgl>,
#> #   baitUse <lgl>, featureType <fct>, habitat <chr>, deploymentGroups <chr>,
#> #   deploymentTags <chr>, deploymentComments <chr>

# Set deployments (not recommended outside a function)
deployments(x) <- head(deployments(x), 1)
```
