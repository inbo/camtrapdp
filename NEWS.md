# camtrapdp (development version)

* New function `read_camtrapdp()` reads data files into memory (#9).
* New function `version()` allows to get the version of a camtrapdp object.
* New functions `deployments()`, `media()` and `observations()` return a data frame with respectively deployments, media and observations (#29).
* New internal function `example_package()` returns the latest Camtrap DP example dataset (#24).
* New internal function `check_camtrapd()` validates a camtrapdp object (#34).
* New internal function `convert()` converts camtrapdp objects to the latest version. This function is currently not used, as the only supported version is Camtrap DP 1.0 (#9).
* Pipe (`%>%`) is imported from [dplyr](https://cran.r-project.org/package=dplyr) at package level, so it can be used in functions (#37).
