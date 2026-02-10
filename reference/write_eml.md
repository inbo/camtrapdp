# Transform a Camera Trap Data Package to EML

Transforms the metadata of a Camera Trap Data Package object to an
[Ecological Metadata Language (EML)](https://eml.ecoinformatics.org/)
file.

## Usage

``` r
write_eml(x, directory, derived_paragraph = TRUE)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

- directory:

  Path to local directory to write files to.

- derived_paragraph:

  If `TRUE`, a paragraph will be added to the abstract, indicating that
  data have been transformed using
  [`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md).

## Value

`eml.xml` file written to disk. And invisibly, an
[EML::eml](https://docs.ropensci.org/EML/reference/eml.html) object.

## Transformation details

Metadata are derived from what is provided in `x`. The following
properties are set:

- **title**: Title as provided in `x$title`.

- **type**: Set to `Occurrence` in keywords.

- **subtype**: Set to `Observation` in keywords.

- **update frequency**: Set to `unknown`.

- **description**: Description as provided in `x$description`. If
  `derived_paragraph = TRUE` a generated paragraph is added, e.g.:

  Data have been standardized to Darwin Core using the
  [camtrapdp](https://inbo.github.io/camtrapdp/) R package and only
  include observations (and associated media) of animals. Excluded are
  records that document blank or unclassified media, vehicles and
  observations of humans.

- **license**: License with scope `data` as provided in `x$licenses`.

- **creators**: Contributors as provided in `x$contributors`, excluding
  those with roles `rightsHolder` and `publisher`.

- **contact**: Contributors with role `contact`. If none exist, first
  creator.

- **metadata provider**: Same as `contact`.

- **keywords**: Keywords as provided in `x$keywords`.

- **geographic coverage**: Bounding box as provided in `x$spatial`.

- **taxonomic coverage**: Taxa as provided in `x$taxonomic`.

- **temporal coverage**: Date range as provided in `x$temporal`.

- **project data**: Title, acronym as identifier, description, and
  sampling design as provided in `x$project`.

- **alternative identifier**: Identifier as provided in `x$id`. If this
  is a DOI, no new DOI will be created when publishing to GBIF.

- **external link**: URL of the project as provided in `x$project$path`.

The following properties are not set:

- **publishing organization**

- **associated parties**

- **sampling methods**

- **citations**

- **collection data**: not applicable.

## See also

Other transformation functions:
[`merge_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/merge_camtrapdp.md),
[`round_coordinates()`](https://inbo.github.io/camtrapdp/reference/round_coordinates.md),
[`shift_time()`](https://inbo.github.io/camtrapdp/reference/shift_time.md),
[`update_taxon()`](https://inbo.github.io/camtrapdp/reference/update_taxon.md),
[`write_dwc()`](https://inbo.github.io/camtrapdp/reference/write_dwc.md)

## Examples

``` r
x <- example_dataset()
(write_eml(x, directory = "my_directory"))
#> 
#> ── Writing file ──
#> 
#> • my_directory/eml.xml
#> $packageId
#> [1] "c8514796-396e-43b6-a8de-ada15a842424"
#> 
#> $system
#> [1] "uuid"
#> 
#> $dataset
#> $dataset$title
#> [1] "Sample from: MICA - Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany"
#> 
#> $dataset$abstract
#> $dataset$abstract$para
#> [1] "MICA - Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany is an occurrence dataset published by the Research Institute of Nature and Forest (INBO). It is part of the LIFE project MICA, in which innovative techniques are tested for a more efficient control of muskrat and coypu populations, both invasive species. This dataset is a sample of the original dataset and serves as an example of a Camera Trap Data Package (Camtrap DP)."
#> [2] "Data have been standardized to Darwin Core using the <ulink url=\"https://inbo.github.io/camtrapdp/\"><citetitle>camtrapdp</citetitle></ulink> R package and only include observations (and associated media) of animals. Excluded are records that document blank or unclassified media vehicles and observations of humans."                                                                                                                                                 
#> 
#> 
#> $dataset$maintenance
#> $dataset$maintenance$description
#> $dataset$maintenance$description$para
#> [1] ""
#> 
#> 
#> $dataset$maintenance$maintenanceUpdateFrequency
#> [1] "unknown"
#> 
#> 
#> $dataset$creator
#> $dataset$creator[[1]]
#> '@id': ~
#> address: ~
#> electronicMailAddress: axel.neukermans@inbo.be
#> individualName:
#>   givenName: Axel
#>   surName: Neukermans
#> onlineUrl: .na.character
#> organizationName: Research Institute for Nature and Forest (INBO)
#> phone: ~
#> positionName: ~
#> userId:
#>   directory: https://orcid.org/
#>   '': 0000-0003-0272-9180
#> 
#> $dataset$creator[[2]]
#> '@id': ~
#> address: ~
#> electronicMailAddress: daniel.vanderbeeck@gmail.com
#> individualName:
#>   givenName: Danny
#>   surName: Van der beeck
#> onlineUrl: .na.character
#> organizationName: .na.character
#> phone: ~
#> positionName: ~
#> userId: ~
#> 
#> $dataset$creator[[3]]
#> '@id': ~
#> address: ~
#> electronicMailAddress: emma.cartuyvels@inbo.be
#> individualName:
#>   givenName: Emma
#>   surName: Cartuyvels
#> onlineUrl: .na.character
#> organizationName: Research Institute for Nature and Forest (INBO)
#> phone: ~
#> positionName: ~
#> userId: ~
#> 
#> $dataset$creator[[4]]
#> '@id': ~
#> address: ~
#> electronicMailAddress: peter.desmet@inbo.be
#> individualName:
#>   givenName: Peter
#>   surName: Desmet
#> onlineUrl: .na.character
#> organizationName: Research Institute for Nature and Forest (INBO)
#> phone: ~
#> positionName: ~
#> userId:
#>   directory: https://orcid.org/
#>   '': 0000-0002-8442-8025
#> 
#> 
#> $dataset$contact
#> $dataset$contact[[1]]
#> '@id': ~
#> address: ~
#> electronicMailAddress: peter.desmet@inbo.be
#> individualName:
#>   givenName: Peter
#>   surName: Desmet
#> onlineUrl: .na.character
#> organizationName: Research Institute for Nature and Forest (INBO)
#> phone: ~
#> positionName: ~
#> userId:
#>   directory: https://orcid.org/
#>   '': 0000-0002-8442-8025
#> 
#> 
#> $dataset$metadataProvider
#> $dataset$metadataProvider[[1]]
#> '@id': ~
#> address: ~
#> electronicMailAddress: peter.desmet@inbo.be
#> individualName:
#>   givenName: Peter
#>   surName: Desmet
#> onlineUrl: .na.character
#> organizationName: Research Institute for Nature and Forest (INBO)
#> phone: ~
#> positionName: ~
#> userId:
#>   directory: https://orcid.org/
#>   '': 0000-0002-8442-8025
#> 
#> 
#> $dataset$keywordSet
#> $dataset$keywordSet[[1]]
#> $dataset$keywordSet[[1]]$keywordThesaurus
#> [1] "GBIF Dataset Type Vocabulary: http://rs.gbif.org/vocabulary/gbif/dataset_type_2015-07-10.xml"
#> 
#> $dataset$keywordSet[[1]]$keyword
#> [1] "Occurrence"
#> 
#> 
#> $dataset$keywordSet[[2]]
#> $dataset$keywordSet[[2]]$keywordThesaurus
#> [1] "GBIF Dataset Subtype Vocabulary: http://rs.gbif.org/vocabulary/gbif/dataset_subtype.xml"
#> 
#> $dataset$keywordSet[[2]]$keyword
#> [1] "Observation"
#> 
#> 
#> $dataset$keywordSet[[3]]
#> $dataset$keywordSet[[3]]$keywordThesaurus
#> [1] "n/a"
#> 
#> $dataset$keywordSet[[3]]$keyword
#>  [1] "camera traps"              "public awareness campaign"
#>  [3] "flood protection"          "flood control"            
#>  [5] "damage prevention"         "animal damage"            
#>  [7] "pest control"              "invasive alien species"   
#>  [9] "muskrat"                   "coypu"                    
#> 
#> 
#> 
#> $dataset$intellectualRights
#> $dataset$intellectualRights$para
#> [1] "CC0-1.0"
#> 
#> 
#> $dataset$coverage
#> geographicCoverage:
#>   geographicDescription: Geographic description not provided for this dataset.
#>   boundingCoordinates:
#>     westBoundingCoordinate: 4.013
#>     eastBoundingCoordinate: 5.659
#>     northBoundingCoordinate: 51.496
#>     southBoundingCoordinate: 50.699
#>     boundingAltitudes:
#>       altitudeMinimum: []
#>       altitudeMaximum: []
#>       altitudeUnits: []
#> taxonomicCoverage:
#>   taxonomicClassification:
#>   - taxonRankName: species
#>     taxonRankValue: Anas platyrhynchos
#>   - taxonRankName: species
#>     taxonRankValue: Anas strepera
#>   - taxonRankName: genus
#>     taxonRankValue: Ardea
#>   - taxonRankName: species
#>     taxonRankValue: Ardea cinerea
#>   - taxonRankName: class
#>     taxonRankValue: Aves
#>   - taxonRankName: species
#>     taxonRankValue: Martes foina
#>   - taxonRankName: species
#>     taxonRankValue: Mustela putorius
#>   - taxonRankName: species
#>     taxonRankValue: Rattus norvegicus
#>   - taxonRankName: species
#>     taxonRankValue: Vulpes vulpes
#> temporalCoverage:
#>   rangeOfDates:
#>     beginDate:
#>       calendarDate: '2020-05-30'
#>     endDate:
#>       calendarDate: '2021-04-18'
#> 
#> $dataset$project
#> $dataset$project$id
#> [1] "MICA"
#> 
#> $dataset$project$title
#> [1] "Management of Invasive Coypu and muskrAt in Europe"
#> 
#> $dataset$project$abstract
#> $dataset$project$abstract$para
#> [1] "Invasive alien species such as the coypu and muskrat pose a major threat to biodiversity and cost millions of euros annually. By feeding on rushes and reeds, these animals cause serious damage to the environment in which they live and endangered species suffer from habitat loss. The disappearance of reeds and digging in dikes represents a safety risk for humans in the lowland areas. With the LIFE project MICA (<https://lifemica.eu>), the partners from the participating countries want to develop a transnational plan for the management of coypu and muskrat populations in Europe and aim to reduce their population. The objective of an effective population control of coypu and muskrat is to protect lowlands from flooding, to prevent crop damage and loss of biodiversity. The objective of the project is to serve as a pilot and demonstration project in which ‘best practices’ are tested and new techniques are developed for a more efficient control of muskrat and coypu populations. By involving organisations from Belgium, Germany and the Netherlands, the project also promotes international cooperation and knowledge exchange in the field of muskrat and coypu management."
#> 
#> 
#> $dataset$project$designDescription
#> $dataset$project$designDescription$description
#> $dataset$project$designDescription$description$para
#> [1] "This project uses a targeted sampling design. Animals are unmarked and camera traps are triggered with activity detection and time lapse."
#> 
#> 
#> 
#> $dataset$project$personnel
#> '@id': ~
#> address: ~
#> electronicMailAddress: axel.neukermans@inbo.be
#> individualName:
#>   givenName: Axel
#>   surName: Neukermans
#> onlineUrl: .na.character
#> organizationName: Research Institute for Nature and Forest (INBO)
#> phone: ~
#> positionName: ~
#> userId:
#>   directory: https://orcid.org/
#>   '': 0000-0003-0272-9180
#> 
#> 
#> $dataset$distribution
#> $dataset$distribution$scope
#> [1] "document"
#> 
#> $dataset$distribution$online
#> $dataset$distribution$online$url
#> $dataset$distribution$online$url$`function`
#> [1] "information"
#> 
#> $dataset$distribution$online$url[[2]]
#> [1] "https://lifemica.eu"
#> 
#> 
#> 
#> 
#> $dataset$pubDate
#> [1] "2023-02-06"
#> 
#> $dataset$alternateIdentifier
#> [1] "7cca70f5-ef8c-4f86-85fb-8f070937d7ab"
#> 
#> 
#> $additionalMetadata
#> $additionalMetadata$metadata
#> $additionalMetadata$metadata$gbif
#> $additionalMetadata$metadata$gbif$citation
#> [1] "Desmet P, Neukermans A, Van der beeck D, Cartuyvels E (2022). Sample from: MICA - Muskrat and coypu camera trap observations in Belgium, the Netherlands and Germany. Version 1.0.2. Research Institute for Nature and Forest (INBO). Dataset. https://camtrap-dp.tdwg.org/example/"
#> 
#> 
#> 
#> 

# Clean up (don't do this if you want to keep your files)
unlink("my_directory", recursive = TRUE)
```
