# Changelog

## camtrapdp 0.5.0

CRAN release: 2026-01-14

- [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md)
  now upgrades datasets to Camtrap DP 1.0.2
  ([\#183](https://github.com/inbo/camtrapdp/issues/183)) and provides
  help for unsupported versions
  ([\#198](https://github.com/inbo/camtrapdp/issues/198)).
- [`write_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/write_camtrapdp.md)
  now removes properties with `NA` values from `x$taxonomic` and
  `x$contributors` which caused `datapackage.json` to be invalid
  ([\#186](https://github.com/inbo/camtrapdp/issues/186)).
- [`write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.md)’s
  derived paragraph is now formatted as DocBook rather than HTML
  ([\#188](https://github.com/inbo/camtrapdp/issues/188)).

## camtrapdp 0.4.0

CRAN release: 2025-06-11

### Reading and writing data

- [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md)
  now updates (or creates) the spatial, temporal and taxonomic scope in
  the metadata based on the data
  ([\#130](https://github.com/inbo/camtrapdp/issues/130),
  [\#164](https://github.com/inbo/camtrapdp/issues/164)).
- [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md)
  now upgrades datasets to Camtrap DP 1.0.1. The internal function
  `convert()` has been renamed to
  [`upgrade()`](https://rdrr.io/r/utils/upgrade.html)
  ([\#113](https://github.com/inbo/camtrapdp/issues/113)).
- New
  [`write_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/write_camtrapdp.md)
  writes a Camera Trap Data Package to disk as a `datapackage.json` and
  CSV files ([\#137](https://github.com/inbo/camtrapdp/issues/137)).
  This means you can now read, update and write Camtrap DP datasets.

### Accessing data

- New
  [`contributors()`](https://inbo.github.io/camtrapdp/reference/contributors.md)
  returns a tibble with contributors
  ([\#140](https://github.com/inbo/camtrapdp/issues/140)).
- New
  [`individuals()`](https://inbo.github.io/camtrapdp/reference/individuals.md)
  returns a data frame with unique individuals
  ([\#149](https://github.com/inbo/camtrapdp/issues/149)).
- [`taxa()`](https://inbo.github.io/camtrapdp/reference/taxa.md) now
  removes duplicates
  ([\#130](https://github.com/inbo/camtrapdp/issues/130)).

### Filtering data

- [`filter_deployments()`](https://inbo.github.io/camtrapdp/reference/filter_deployments.md)
  and `deployments()<-` now update the spatial, temporal and taxonomic
  scope in the metadata based on the returned data
  ([\#100](https://github.com/inbo/camtrapdp/issues/100),
  [\#132](https://github.com/inbo/camtrapdp/issues/132)).
- [`filter_observations()`](https://inbo.github.io/camtrapdp/reference/filter_observations.md),
  [`filter_media()`](https://inbo.github.io/camtrapdp/reference/filter_media.md),
  `media()<-` and `observations()<-` now update the taxonomic scope in
  the metadata based on the returned data
  ([\#89](https://github.com/inbo/camtrapdp/issues/89),
  [\#100](https://github.com/inbo/camtrapdp/issues/100),
  [\#130](https://github.com/inbo/camtrapdp/issues/130)).

### Transforming data

- [`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md)
  now adds `identificationVerificationStatus` for observations
  classified by humans with 100% certainty
  ([\#158](https://github.com/inbo/camtrapdp/issues/158)).
- [`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md)
  now allows to create occurrences from media-based observations
  ([\#172](https://github.com/inbo/camtrapdp/issues/172)).
- New
  [`write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.md)
  transforms Camtrap DP metadata to EML
  ([\#99](https://github.com/inbo/camtrapdp/issues/99)). This function
  is used by GBIF to create metadata for a dataset page.
- New
  [`merge_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/merge_camtrapdp.md)
  allows to merge two datasets
  ([\#112](https://github.com/inbo/camtrapdp/issues/112)). This can be
  useful to combine data from multiple studies.
- New
  [`round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.md)
  allows to fuzzy/generalize location information by rounding deployment
  `latitude` and `longitude`. It also updates `coordinateUncertainty` in
  the deployments and `coordinatePrecision` and spatial scope in the
  metadata ([\#106](https://github.com/inbo/camtrapdp/issues/106)).
- New
  [`shift_time()`](https://inbo.github.io/camtrapdp/reference/shift_time.md)
  allows to shift/correct date-times in data and metadata for specified
  deploymentIDs and duration
  ([\#108](https://github.com/inbo/camtrapdp/issues/108)).
- New
  [`update_taxon()`](https://inbo.github.io/camtrapdp/reference/update_taxon.md)
  allows to update taxonomic information in data and metadata
  ([\#159](https://github.com/inbo/camtrapdp/issues/159)).

### Miscellaneous

- camtrapdp now relies on (and is tested for) R version 3.6.0 or higher
  ([\#138](https://github.com/inbo/camtrapdp/issues/138)).
- Internal function `build_taxa()` is renamed to `taxonomic()`
  ([\#130](https://github.com/inbo/camtrapdp/issues/130)).

## camtrapdp 0.3.1

CRAN release: 2024-07-05

- Fix CRAN note ([\#102](https://github.com/inbo/camtrapdp/issues/102)).

## camtrapdp 0.3.0

- New function [`print()`](https://rdrr.io/r/base/print.html) prints a
  human-readable summary of the Camera Trap Data Package
  ([\#8](https://github.com/inbo/camtrapdp/issues/8)).
- Fix `fieldsEnclosedBy` issue in `meta.xml`, so GBIF occurrence
  processing correctly handles commas in fields
  ([\#95](https://github.com/inbo/camtrapdp/issues/95)).
- Fix CRAN note ([\#94](https://github.com/inbo/camtrapdp/issues/94)).

## camtrapdp 0.2.1

CRAN release: 2024-06-05

- First release on [CRAN](https://cran.r-project.org/package=camtrapdp).
  🎉
- [`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md)
  no longer writes to `"."` by default, since this is not allowed by
  CRAN policies. The user needs to explicitly define a directory
  ([\#79](https://github.com/inbo/camtrapdp/issues/79)).

## camtrapdp 0.2.0

- New function
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md)
  reads data files from a Camtrap DP into memory
  ([\#9](https://github.com/inbo/camtrapdp/issues/9)). It will make the
  data easier to use, by assigning taxonomic information (found in the
  metadata) to the observations and `eventID`s (found in the
  observations) to the media
  ([\#37](https://github.com/inbo/camtrapdp/issues/37)).
- New accessor functions
  [`deployments()`](https://inbo.github.io/camtrapdp/reference/deployments.md),
  [`media()`](https://inbo.github.io/camtrapdp/reference/media.md) and
  [`observations()`](https://inbo.github.io/camtrapdp/reference/observations.md)
  return a data frame with the deployments, media and observations
  respectively ([\#29](https://github.com/inbo/camtrapdp/issues/29)).
  These functions also have an assignment equivalent
  ([\#50](https://github.com/inbo/camtrapdp/issues/50)).
- New accessor functions
  [`locations()`](https://inbo.github.io/camtrapdp/reference/locations.md),
  [`events()`](https://inbo.github.io/camtrapdp/reference/events.md) and
  [`taxa()`](https://inbo.github.io/camtrapdp/reference/taxa.md) return
  a data frame with unique locations, events and taxa respectively
  ([\#22](https://github.com/inbo/camtrapdp/issues/22),
  [\#57](https://github.com/inbo/camtrapdp/issues/57),
  [\#17](https://github.com/inbo/camtrapdp/issues/17)).
- New functions
  [`filter_deployments()`](https://inbo.github.io/camtrapdp/reference/filter_deployments.md),
  [`filter_observations()`](https://inbo.github.io/camtrapdp/reference/filter_observations.md)
  and
  [`filter_media()`](https://inbo.github.io/camtrapdp/reference/filter_media.md)
  allow to filter data. They work similarly to
  [dplyr](https://cran.r-project.org/package=dplyr)’s
  [`filter()`](https://rdrr.io/r/stats/filter.html)
  ([\#23](https://github.com/inbo/camtrapdp/issues/23)).
- New function
  [`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md)
  transforms a Camera Trap Data Package to a Darwin Core Archive
  ([\#55](https://github.com/inbo/camtrapdp/issues/55)).
- New function `example_package()` returns the latest Camtrap DP example
  dataset and caches the result
  ([\#24](https://github.com/inbo/camtrapdp/issues/24),
  [\#67](https://github.com/inbo/camtrapdp/issues/67)).
- New function
  [`version()`](https://inbo.github.io/camtrapdp/reference/version.md)
  allows to get the version of a camtrapdp object.
- New internal function
  [`check_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/check_camtrapdp.md)
  validates a camtrapdp object
  ([\#34](https://github.com/inbo/camtrapdp/issues/34)).
- New internal function `convert()` converts camtrapdp objects to the
  latest version. This function is currently not used, as the only
  supported version is Camtrap DP 1.0
  ([\#9](https://github.com/inbo/camtrapdp/issues/9)).
- [dplyr](https://cran.r-project.org/package=dplyr)’s pipe (`%>%`) is
  included in NAMESPACE, so you don’t have to load dplyr (or magrittr)
  to use it ([\#56](https://github.com/inbo/camtrapdp/issues/56)). `%>%`
  and `.data` are imported at package level, so they can be used in
  functions without namespace
  ([\#37](https://github.com/inbo/camtrapdp/issues/37)).
