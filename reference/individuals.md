# Get individuals

Gets the (unique) individuals from the observations of a Camera Trap
Data Package object.

## Usage

``` r
individuals(x)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with the individuals that have an `individualID`, containing
the following columns:

- `individualID`

- `scientificName`

- `lifeStage`

- `sex`

## See also

Other accessor functions:
[`contributors()`](https://inbo.github.io/camtrapdp/reference/contributors.md),
[`deployments()`](https://inbo.github.io/camtrapdp/reference/deployments.md),
[`events()`](https://inbo.github.io/camtrapdp/reference/events.md),
[`locations()`](https://inbo.github.io/camtrapdp/reference/locations.md),
[`media()`](https://inbo.github.io/camtrapdp/reference/media.md),
[`observations()`](https://inbo.github.io/camtrapdp/reference/observations.md),
[`taxa()`](https://inbo.github.io/camtrapdp/reference/taxa.md)

## Examples

``` r
x <- example_dataset()
individuals(x)
#> # A tibble: 0 × 4
#> # ℹ 4 variables: individualID <chr>, scientificName <chr>, lifeStage <fct>,
#> #   sex <fct>
```
