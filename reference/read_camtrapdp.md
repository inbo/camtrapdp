# Read a Camera Trap Data Package

Reads a [Camera Trap Data Package (Camtrap
DP)](https://camtrap-dp.tdwg.org) dataset into memory.

## Usage

``` r
read_camtrapdp(file)
```

## Arguments

- file:

  Path or URL to a `datapackage.json` file.

## Value

A Camera Trap Data Package object.

## Older versions

The `read_camtrapdp()` function supports older versions of Camtrap DP
and will automatically **upgrade** such datasets to the latest version
of the standard. It currently supports versions 1.0, 1.0.1 and 1.0.2
(latest).

## Events

Observations can contain classifications at two levels:

- **Media-based** observations (`observationLevel = "media"`) are based
  on a single media file and are directly linked to it via `mediaID`.

- **Event-based** observations (`observationLevel = "event"`) are based
  on an event, defined as a combination of `eventID`, `eventStart` and
  `eventEnd`. This event can consist of one or more media files, but is
  not directly linked to these.

The `read_camtrapdp()` function **will automatically assign `eventID`s
to media**, using `media.deploymentID = observations.deploymentID` and
`observations.eventStart <= media.timestamp <= observations.eventEnd`.
Note that this can result in media being linked to multiple events (and
thus being duplicated), for example when events and sub-events were
defined.

## Taxonomic information

Camtrap DP metadata has a `taxonomic` property that can contain extra
information for each `scientificName` found in observations. Such
information can include higher taxonomy (`family`, `order`, etc.) and
vernacular names in multiple languages.

The `read_camtrapdp()` function **will automatically include this
taxonomic information in observations**, as extra columns starting with
`taxon.`. It will then update the `taxonomic` scope in the metadata to
the unique
[`taxa()`](https://inbo.github.io/camtrapdp/reference/taxa.md) found in
the data.

## Spatial/temporal coverage

Camtrap DP metadata has a `spatial` and `temporal` property that
contains the spatial and temporal coverage of the package respectively.

The `read_camtrapdp()` function **will automatically update (or create)
the spatial and temporal scopes** in the metadata based on the data. It
also does this for the taxonomic scope (see higher).

## Additional resources

A Camtrap DP can contain Data Resources not described by the standard.
Those are listed with the tables supported by the standard (i.e.
deployments, media, observations) in the `resources` property.

The `read_camtrapdp()` function will **ignore these additional
resources** and only read the tables described by the standard.
Additional resources can be read with
[`frictionless::read_resource()`](https://docs.ropensci.org/frictionless/reference/read_resource.html)
if they are tabular.

## Examples

``` r
file <- "https://raw.githubusercontent.com/tdwg/camtrap-dp/1.0.2/example/datapackage.json"
x <- read_camtrapdp(file)
x
#> A Camera Trap Data Package "camtrap-dp-example-dataset" with 3 tables:
#> • deployments: 4 rows
#> • media: 423 rows
#> • observations: 549 rows
#> 
#> And 1 additional resource:
#> • individuals
#> Use `unclass()` to print the Data Package as a list.
```
