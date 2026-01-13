# Round coordinates to generalize camera trap locations

Rounds deployment coordinates to a certain number of digits to
fuzzy/generalize camera trap locations. This function can be used before
publishing data in order to protect sensitive species and/or prevent
theft of active cameras.

## Usage

``` r
round_coordinates(x, digits)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- digits:

  Number of decimal places to round coordinates to (`1`, `2` or `3`).

## Value

`x` with chosen `coordinatePrecision` in metadata and rounded
coordinates and calculated `coordinateUncertainty` in deployments.

## Details

Rounding coordinates is a recommended method to generalize sensitive
biodiversity information (see [Section
4.2](https://docs.gbif.org/sensitive-species-best-practices/master/en/#s-generalization)
in Chapman 2020). Use this function to do so for your data. Determine
the category of sensitivity (see [Section
2.2](https://docs.gbif.org/sensitive-species-best-practices/master/en/#table-06)
in Chapman 2020) and choose the associated number of digits :

|               |               |                                |
|---------------|---------------|--------------------------------|
| category      | sensitivity   | digits                         |
| category 1    | extreme       | (do not publish)               |
| category 2    | high          | 1                              |
| category 3    | medium        | 2                              |
| category 4    | low           | 3                              |
| not sensitive | not sensitive | all (do not use this function) |

The function will:

1.  Set the `coordinatePrecision` in the metadata (original values will
    be overwritten):

    |        |                     |
    |--------|---------------------|
    | digits | coordinatePrecision |
    | 1      | 0.1                 |
    | 2      | 0.01                |
    | 3      | 0.001               |

2.  Round all coordinates in the deployments to the selected number of
    digits.

3.  Update the `coordinateUncertainy` (in meters) in the deployments.
    This uncertainty is based on the number of digits and the latitude,
    following [Table
    3](https://docs.gbif.org/georeferencing-best-practices/1.0/en/#table-uncertainty)
    in Chapman & Wieczorek 2020:

    |        |             |              |              |              |
    |--------|-------------|--------------|--------------|--------------|
    | digits | 0° latitude | 30° latitude | 60° latitude | 85° latitude |
    | 1      | 15691 m     | 14697 m      | 12461 m      | 11211 m      |
    | 2      | 1570 m      | 1470 m       | 1246 m       | 1121 m       |
    | 3      | 157 m       | 147 m        | 125 m        | 112 m        |

    If a `coordinatePrecision` is already present, the function will
    subtract the `coordinateUncertainty` associated with it before
    setting a new uncertainty (e.g. `0.001` to `0.01` =
    `original value - 157 + 1570 m`). If `original value` is `NA`, the
    function will assume the coordinates were obtained by GPS and set
    `original value = 30`.

## See also

Other transformation functions:
[`merge_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/merge_camtrapdp.md),
[`shift_time()`](https://inbo.github.io/camtrapdp/reference/shift_time.md),
[`update_taxon()`](https://inbo.github.io/camtrapdp/reference/update_taxon.md),
[`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md),
[`write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.md)

## Examples

``` r
x <- example_dataset()

# Original precision
x$coordinatePrecision
#> [1] 0.001

# Original coordinates and uncertainty
deployments(x)[c("latitude", "longitude", "coordinateUncertainty")]
#> # A tibble: 4 × 3
#>   latitude longitude coordinateUncertainty
#>      <dbl>     <dbl>                 <dbl>
#> 1     51.5      4.77                   187
#> 2     51.2      5.66                   187
#> 3     51.2      5.66                   187
#> 4     50.7      4.01                   187

# Round coordinates to 1 digit
x_rounded <- round_coordinates(x, 1)

# Updated coordinatePrecision
x_rounded$coordinatePrecision
#> [1] 0.1

# Updated coordinates and uncertainty (original 187 - 147 + 14697 = 14737)
deployments(x_rounded)[c("latitude", "longitude", "coordinateUncertainty")]
#> # A tibble: 4 × 3
#>   latitude longitude coordinateUncertainty
#>      <dbl>     <dbl>                 <dbl>
#> 1     51.5       4.8                 14737
#> 2     51.2       5.7                 14737
#> 3     51.2       5.7                 14737
#> 4     50.7       4                   14737
```
