# camtrapdp (development version)

* New function `write_camtrapdp()` writes a Camera Trap Data Package to disk as a `datapackage.json` and CSV files (#137).
* New function `merge_camtrapdp()` allows to merge two datasets (#112).
* New function `write_eml()` transforms Camtrap DP metadata to EML (#99).
* New function `round_coordinates()` allows to fuzzy/generalize location information by rounding deployment `latitude` and `longitude`. It also updates `coordinateUncertainty` in the deployments and `coordinatePrecision` and spatial scope in the metadata (#106).
* New function `shift_time()` allows to shift/correct date-times in data and metadata for specified deploymentIDs and duration (#108).
* New function `individuals()` returns a data frame with unique individuals (#149).
* `filter_deployments()` and `deployments()<-` now update the spatial, temporal and taxonomic scope in the metadata based on the returned data (#100, #132).
* `filter_observations()`, `filter_media()`, `media()<-` and `observations()<-` now update the taxonomic scope in the metadata based on the returned data (#89, #100, #130).
* `read_camtrapdp()` now updates the spatial, temporal and taxonomic scope in the metadata based on the data (#130, #164).
* `read_camtrapdp()` now upgrades datasets to Camtrap DP 1.0.1. The internal function `convert()` has been renamed to `upgrade()` (#113).
* Internal function `build_taxa()` is renamed to `taxonomic()` (#130).
* `taxa()` now removes duplicates (#130).
* `write_dwc()` now adds `identificationVerificationStatus` for observations classified by humans with 100% certainty (#158).
* `write_dwc()` now also allows to write media-based observations (#172).

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
