# Update a taxon

Updates taxonomic information in data and metadata for a provided taxon.
This allows to:

1.  Update a taxon: provide the same name in `to` and
    `from$scientificName`.

2.  Replace a taxon: provide a new name in `from$scientificName`.

3.  Lump a taxon: provide a name in `from$scientificName` that is
    already present in the dataset. In all cases, existing information
    will be overwritten with the provided information.

## Usage

``` r
update_taxon(x, from, to)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- from:

  `scientificName` of the taxon to update.

- to:

  Named list with taxon information, e.g.
  `list(scientificName = "Ardea", taxonRank = "genus", vernacularname.eng = "great herons")`.

## Value

`x` with updated taxon information.

## See also

Other transformation functions:
[`merge_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/merge_camtrapdp.md),
[`round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.md),
[`shift_time()`](https://inbo.github.io/camtrapdp/reference/shift_time.md),
[`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md),
[`write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.md)

## Examples

``` r
x <- example_dataset()

# Update taxonomic information for "Anas platyrhynchos"
updated_x <- update_taxon(
  x,
  from = "Anas platyrhynchos",
  to = list (
    scientificName = "Anas platyrhynchos",
    taxonID = "https://www.checklistbank.org/dataset/COL2023/taxon/DGP6",
    taxonRank = "species",
    vernacularNames.fra = "canard colvert"
  )
)
#> ℹ Taxon "Anas platyrhynchos" is replaced by:
#> scientificName: Anas platyrhynchos
#> taxonID: https://www.checklistbank.org/dataset/COL2023/taxon/DGP6
#> taxonRank: species
#> vernacularNames.eng: NA
#> vernacularNames.nld: NA
#> vernacularNames.fra: canard colvert

# Lump "Ardea cinerea" into already present "Ardea", using the provided info
updated_x <- update_taxon(
  x,
  from = "Ardea cinerea",
  to = list(scientificName = "Ardea", vernacularname.fra = "grands hérons")
)
#> ℹ Taxon "Ardea cinerea" is replaced by:
#> scientificName: Ardea
#> taxonID: NA
#> taxonRank: NA
#> vernacularNames.eng: NA
#> vernacularNames.nld: NA
#> vernacularname.fra: grands hérons
```
