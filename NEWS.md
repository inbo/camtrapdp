# camtrapdp (development version)

* New function `read_camtrapdp()` reads data files into memory (#9).
* New functions `deployments()`, `media()` and `observations()` return a data frame with respectively deployments, media and observations (#29).
* New function `locations()` returns the unique locations from the deployments (#22). 
* New function `taxa()` returns the unique scientific names and associated taxonomic information from the observations (#17). This information is derived from the metadata and added the observations with column prefix `taxon.` in `read_camtrap_dp()` (#37).
* New internal function `example_package()` returns the latest Camtrap DP example dataset (#24).
* New function `version()` allows to get the version of a camtrapdp object.
* New internal function `check_camtrapdp()` validates a camtrapdp object (#34).
* New internal function `convert()` converts camtrapdp objects to the latest version. This function is currently not used, as the only supported version is Camtrap DP 1.0 (#9).
* Pipe (`%>%`) is imported from [dplyr](https://cran.r-project.org/package=dplyr) at package level, so it can be used in functions (#37). Same for `.data`.
* New functions allow filtering at package level: `filter_deployments()`, `filter_observations()` and `filter_media()`). They work similarly to [dplyr](https://cran.r-project.org/package=dplyr)'s `filter()` (#23).
