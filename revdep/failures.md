# ct ()

* GitHub: <https://github.com/inbo/camtrapdp>
* Email: <mailto:peter.desmet@inbo.be>

Run `revdepcheck::revdep_details(, "ct")` for more info

## Error before installation

### Devel

```



Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package ‘BH’ failed
Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package ‘BH’ failed
Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.5/BH_1.90.0-1.tgz': Timeout of 60 seconds was reached
2: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.5/BH_1.90.0-1.tgz': Timeout of 60 seconds was reached
3: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  some files were not downloaded
4: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  some files were not downloaded
5: In if (length(block) < 512L) stop("incomplete block on file") :
  closing unused connection 3 (/var/folders/dl/xs4s859d7077r33qyzfd6bx80000gn/T//RtmpuhcHvr/downloaded_packages/BH_1.90.0-1.tgz)


```
### CRAN

```



Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package ‘BH’ failed
Warning in download.packages(pkgs, destdir = tmpd, available = available,  :
  download of package ‘BH’ failed
Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.5/BH_1.90.0-1.tgz': Timeout of 60 seconds was reached
2: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.5/BH_1.90.0-1.tgz': Timeout of 60 seconds was reached
3: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  some files were not downloaded
4: In download.file(urls, destfiles, "libcurl", mode = "wb", ...) :
  some files were not downloaded
5: In if (length(block) < 512L) stop("incomplete block on file") :
  closing unused connection 3 (/var/folders/dl/xs4s859d7077r33qyzfd6bx80000gn/T//RtmpuhcHvr/downloaded_packages/BH_1.90.0-1.tgz)


```
