# Merge two Camera Trap Data Packages

Merges two Camera Trap Data Package objects into one. Repeat to merge
multiple datasets.

## Usage

``` r
merge_camtrapdp(x, y)
```

## Arguments

- x, y:

  Camera Trap Data Package objects, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

## Value

A single Camera Trap Data Package object that is the combination of `x`
and `y`.

## Transformation details

Both `x` and `y` must have a unique dataset name `x$name` and `y$name`.
This name is used to prefix identifiers in the data that occur in both
datasets. For example:

- `x` contains `deploymentID`s `c("a", "b")`.

- `y` contains `deploymentID`s `c("b", "c")`.

- Then merged `xy` will contain `deploymentID`s
  `c("a", "x_b", "y_b", "c")`.

Data are merged as follows:

- Deployments are combined, with `deploymentID` kept unique.

- Media are combined, with `mediaID`, `deploymentID` and `eventID` kept
  unique.

- Observations are combined, with `observationID`, `deploymentID`,
  `mediaID` and `eventID` kept unique.

- Additional resources are retained, with the resource name kept unique.

Metadata properties are merged as follows:

- **name**: Removed.

- **id**: Removed.

- **created**: Set to current timestamp.

- **title**: Removed.

- **contributors**: Combined, with duplicates removed.

- **description**: Combined as two paragraphs.

- **version**: Set to `1.0`.

- **keywords**: Combined, with duplicates removed.

- **image**: Removed.

- **homepage**: Removed.

- **sources**: Combined, with duplicates removed.

- **licenses**: Combined, with duplicates removed.

- **bibliographicCitation**: Removed.

- **project\$id**: Removed.

- **project\$title**: Combined.

- **project\$acronym**: Removed.

- **project\$description**: Combined as two paragraphs.

- **project\$path**: Removed.

- **project\$samplingDesign**: Sampling design of `x`.

- **project\$captureMethod**: Combined, with duplicates removed.

- **project\$individuals**: `TRUE` if one of the datasets has `TRUE`.

- **project\$observationLevel**: Combined, with duplicates removed.

- **coordinatePrecision**: Set to the least precise
  `coordinatePrecision`.

- **spatial**: Reset based on the new deployments.

- **temporal**: Reset based on the new deployments.

- **taxonomic**: Combined, with duplicates removed.

- **relatedIdentifiers**: Combined, with duplicates removed.

- **references**: Combined, with duplicates removed.

- Custom properties of `x` are also retained.

## See also

Other transformation functions:
[`round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.md),
[`shift_time()`](https://inbo.github.io/camtrapdp/reference/shift_time.md),
[`update_taxon()`](https://inbo.github.io/camtrapdp/reference/update_taxon.md),
[`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md),
[`write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.md)

## Examples

``` r
x <- example_dataset() %>%
  filter_deployments(deploymentID %in% c("00a2c20d", "29b7d356"))
y <- example_dataset() %>%
  filter_deployments(deploymentID %in% c("577b543a", "62c200a9"))
x$name <- "x"
y$name <- "y"
merge_camtrapdp(x, y)
#> A Camera Trap Data Package with 3 tables:
#> • deployments: 4 rows
#> • media: 423 rows
#> • observations: 549 rows
#> 
#> And 2 additional resources:
#> • x_individuals
#> • y_individuals
#> Use `unclass()` to print the Data Package as a list.
```
