# camtrapdp (development version)

* `read_camtrapdp()` now upgrades datasets to Camtrap DP 1.0.2 (#183) and provides help for unsupported versions.
* `write_camtrapdp()` now removes properties with `NA` values from `x$taxonomic` and `x$contributors` which caused `datapackage.json` to be invalid (#186).
* `write_eml()`'s derived paragraph is now formatted as DocBook rather than HTML (#188).

# camtrapdp 0.4.0

## Reading and writing data

* `read_camtrapdp()` now updates (or creates) the spatial, temporal and taxonomic scope in the metadata based on the data (#130, #164).
* `read_camtrapdp()` now upgrades datasets to Camtrap DP 1.0.1. The internal function `convert()` has been renamed to `upgrade()` (#113).
* New `write_camtrapdp()` writes a Camera Trap Data Package to disk as a `datapackage.json` and CSV files (#137). This means you can now read, update and write Camtrap DP datasets.

## Accessing data

* New `contributors()` returns a tibble with contributors (#140).
* New `individuals()` returns a data frame with unique individuals (#149).
* `taxa()` now removes duplicates (#130).

## Filtering data

* `filter_deployments()` and `deployments()<-` now update the spatial, temporal and taxonomic scope in the metadata based on the returned data (#100, #132).
* `filter_observations()`, `filter_media()`, `media()<-` and `observations()<-` now update the taxonomic scope in the metadata based on the returned data (#89, #100, #130).

## Transforming data

* `write_dwc()` now adds `identificationVerificationStatus` for observations classified by humans with 100% certainty (#158).
* `write_dwc()` now allows to create occurrences from media-based observations (#172).
* New `write_eml()` transforms Camtrap DP metadata to EML (#99). This function is used by GBIF to create metadata for a dataset page.
* New `merge_camtrapdp()` allows to merge two datasets (#112). This can be useful to combine data from multiple studies.
* New `round_coordinates()` allows to fuzzy/generalize location information by rounding deployment `latitude` and `longitude`. It also updates `coordinateUncertainty` in the deployments and `coordinatePrecision` and spatial scope in the metadata (#106).
* New `shift_time()` allows to shift/correct date-times in data and metadata for specified deploymentIDs and duration (#108).
* New `update_taxon()` allows to update taxonomic information in data and metadata (#159).  

## Miscellaneous

* camtrapdp now relies on (and is tested for) R version 3.6.0 or higher (#138).
* Internal function `build_taxa()` is renamed to `taxonomic()` (#130).

# camtrapdp 0.3.1

* Fix CRAN note (#102).

# camtrapdp 0.3.0

* New function `print()` prints a human-readable summary of the Camera Trap Data Package (#8).
* Fix `fieldsEnclosedBy` issue in `meta.xml`, so GBIF occurrence processing correctly handles commas in fields (#95).
* Fix CRAN note (#94).

# camtrapdp 0.2.1

* First release on [CRAN](https://cran.r-project.org/package=camtrapdp). 🎉
* `write_dwc()` no longer writes to `"."` by default, since this is not allowed by CRAN policies. The user needs to explicitly define a directory (#79).

# camtrapdp 0.2.0

* New function `read_camtrapdp()` reads data files from a Camtrap DP into memory (#9). It will make the data easier to use, by assigning taxonomic information (found in the metadata) to the observations and `eventID`s (found in the observations) to the media (#37).
* New accessor functions `deployments()`, `media()` and `observations()` return a data frame with the deployments, media and observations respectively (#29). These functions also have an assignment equivalent (#50).
* New accessor functions `locations()`, `events()` and `taxa()` return a data frame with unique locations, events and taxa respectively (#22, #57, #17).
* New functions `filter_deployments()`, `filter_observations()` and `filter_media()` allow to filter data. They work similarly to [dplyr](https://cran.r-project.org/package=dplyr)'s `filter()` (#23).
* New function `write_dwc()` transforms a Camera Trap Data Package to a Darwin Core Archive (#55).
* New function `example_package()` returns the latest Camtrap DP example dataset and caches the result (#24, #67).
* New function `version()` allows to get the version of a camtrapdp object.
* New internal function `check_camtrapdp()` validates a camtrapdp object (#34).
* New internal function `convert()` converts camtrapdp objects to the latest version. This function is currently not used, as the only supported version is Camtrap DP 1.0 (#9).
* [dplyr](https://cran.r-project.org/package=dplyr)'s pipe (`%>%`) is included in NAMESPACE, so you don’t have to load dplyr (or magrittr) to use it (#56). `%>%` and `.data` are imported at package level, so they can be used in functions without namespace (#37).
