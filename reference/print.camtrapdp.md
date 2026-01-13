# Print a Camera Trap Data Package

Prints a human-readable summary of a Camera Trap Data Package, as an
extension of
[`frictionless::print.datapackage()`](https://docs.ropensci.org/frictionless/reference/print.datapackage.html).

## Usage

``` r
# S3 method for class 'camtrapdp'
print(x, ...)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- ...:

  Further arguments, they are ignored by this function.

## Value

[`print()`](https://rdrr.io/r/base/print.html) with a summary of the
Camera Trap Data Package object.

## Examples

``` r
x <- example_dataset()

# Print a summary
print(x)
#> A Camera Trap Data Package "camtrap-dp-example-dataset" with 3 tables:
#> • deployments: 4 rows
#> • media: 423 rows
#> • observations: 549 rows
#> 
#> And 1 additional resource:
#> • individuals
#> Use `unclass()` to print the Data Package as a list.

# Print a summary after filtering
filter_deployments(x, deploymentID == "62c200a9")
#> A Camera Trap Data Package "camtrap-dp-example-dataset" with 3 tables:
#> • deployments: 1 rows
#> • media: 60 rows
#> • observations: 65 rows
#> 
#> And 1 additional resource:
#> • individuals
#> Use `unclass()` to print the Data Package as a list.
```
