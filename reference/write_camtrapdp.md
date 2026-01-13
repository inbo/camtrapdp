# Write a Camera Trap Data Package to disk

Writes a Camera Trap Data Package and its related Data Resources to disk
as a `datapackage.json` and CSV files.

## Usage

``` r
write_camtrapdp(x, directory, ...)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- directory:

  Path to local directory to write files to.

- ...:

  Further arguments, passed to
  [`frictionless::write_package()`](https://docs.ropensci.org/frictionless/reference/write_package.html)
  (e.g. `compress = TRUE`).

## Value

`datapackage.json` and CSV files written to disk.

## Examples

``` r
x <- example_dataset()

# Filter (and therefore change) the dataset
x <- filter_deployments(x, deploymentID == "00a2c20d")

# Write the Camera Trap Data Package to disk
write_camtrapdp(x, directory = "my_directory")

# Check files
list.files("my_directory")
#> [1] "datapackage.json" "deployments.csv"  "media.csv"        "observations.csv"

# Clean up (don't do this if you want to keep your files)
unlink("my_directory", recursive = TRUE)
```
