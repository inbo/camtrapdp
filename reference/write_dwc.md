# Transform a Camera Trap Data Package to a Darwin Core Archive

Transforms a Camera Trap Data Package object to a [Darwin Core
Archive](https://dwc.tdwg.org/text/).

## Usage

``` r
write_dwc(x, directory)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- directory:

  Path to local directory to write files to.

## Value

CSV and `meta.xml` files written to disk. And invisibly, a list of data
frames with the transformed data.

## Transformation details

This function **follows recommendations** in Reyserhove et al. (2023)
[doi:10.35035/doc-0qzp-2x37](https://doi.org/10.35035/doc-0qzp-2x37) and
transform data to:

- An [Occurrence
  core](https://docs.gbif.org/camera-trap-guide/en/#section-occurrence-core).

- An [Audubon/Audiovisual Media Description
  extension](https://docs.gbif.org/camera-trap-guide/en/#section-ac-extension).

- A `meta.xml` file.

Key features of the Darwin Core transformation:

- The Occurrence core contains one row per observation
  (`dwc:occurrenceID = observationID`).

- Only observations with `observationType = "animal"` and are included,
  thus excluding observations that are (of) humans, vehicles, blanks,
  unknowns and unclassified.

- Either observations with `observationLevel = "event"` or `"media"` are
  used, never both to avoid duplicates. The level be defined with
  `x$gbifIngestion$observationLevel`, with `"event"` as default.

- Observations classified by humans with 100% certainty get a
  `dwc:identificationVerificationStatus = "verified using recorded media"`.

- Deployment information is included in the Occurrence core, such as
  location, habitat, `dwc:samplingProtocol`, deployment duration in
  `dwc:samplingEffort` and `dwc:parentEventID = deploymentID` as
  grouping identifier.

- Event information is included in the Occurrence core, as event
  duration in `dwc:eventDate` and `dwc:eventID = eventID` as grouping
  identifier.

- Media files are included in the Audubon/Audiovisual Media Description
  extension, with a foreign key to the observation. A media file that is
  used for more than one observation is repeated.

- Metadata are used to set the following record-level terms:

  - `dwc:datasetID`: `x$id`.

  - `dwc:datasetName`: `x$title`.

  - `dwc:collectionCode`: first source in `x$sources`.

  - `dcterms:license`: license `name` (e.g. `CC0-1.0`) in `x$licenses`
    with scope `data`. The license `name` with scope `media` is used as
    `dcterms:rights` in the Audubon Media Description extension.

  - `dcterms:rightsHolder`: first contributor in `x$contributors` with
    role `rightsHolder`.

  - `dwc:dataGeneralizations`: set if `x$coordinatePrecision` is
    defined.

## See also

Other transformation functions:
[`merge_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/merge_camtrapdp.md),
[`round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.md),
[`shift_time()`](https://inbo.github.io/camtrapdp/reference/shift_time.md),
[`update_taxon()`](https://inbo.github.io/camtrapdp/reference/update_taxon.md),
[`write_eml()`](https://inbo.github.io/camtrapdp/reference/write_eml.md)

## Examples

``` r
x <- example_dataset()
write_dwc(x, directory = "my_directory")
#> 
#> ── Transforming data to Darwin Core (event-based observations) ──
#> 
#> ── Writing files ──
#> 
#> • my_directory/occurrence.csv
#> • my_directory/multimedia.csv
#> • my_directory/meta.xml

# Clean up (don't do this if you want to keep your files)
unlink("my_directory", recursive = TRUE)
```
