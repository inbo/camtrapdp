# R2camtrapdp (2.0.0)

* GitHub: <https://github.com/kfukasawa37/R2camtrapdp>
* Email: <mailto:terayama.kana@nies.go.jp>
* GitHub mirror: <https://github.com/cran/R2camtrapdp>

Run `revdepcheck::revdep_details(, "R2camtrapdp")` for more info

## Newly broken

*   checking running R code from vignettes ...
     ```
       ‘Vignette_R2camtrapdp.Rmd’ using ‘UTF-8’... failed
       ‘Vignette_R2camtrapdp_Audio.Rmd’ using ‘UTF-8’... OK
       ‘Vignette_R2camtrapdp_Audio_ja.Rmd’ using ‘UTF-8’... OK
       ‘Vignette_R2camtrapdp_SchemaDriven.Rmd’ using ‘UTF-8’... OK
       ‘Vignette_R2camtrapdp_SchemaDriven_ja.Rmd’ using ‘UTF-8’... OK
       ‘Vignette_R2camtrapdp_SingleCamera.Rmd’ using ‘UTF-8’... OK
      ERROR
     Errors in running code in vignettes:
     when running code in ‘Vignette_R2camtrapdp.Rmd’
       ...
     > datapackage$set_taxon()
     duckdb is storing downloaded extensions and secrets under ~/.duckdb:
     ℹ /Users/peter_desmet/.duckdb
     This persists across sessions and is shared with the DuckDB CLI and other clients.
     ℹ Run duckdb(shared_home = FALSE) to use a temporary directory instead.
     ℹ See ?duckdb_storage for details and alternatives.
     
       When sourcing ‘Vignette_R2camtrapdp.R’:
     Error: [EACCES] Failed to make directory '/Users/peter_desmet/Library/Application Support/org.R-project.R/R/contentid/sha256/23/d5': permission denied
     Execution halted
     ```

