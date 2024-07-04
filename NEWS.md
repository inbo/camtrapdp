# camtrapdp (development version)

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

# camtrapdp 0.3.0

* The taxonomic metadata is updated when observations are filtered with `filter_observation()` and `filter_media()` (#73)
