#' Transform a Camera Trap Data Package to EML
#'
#' Transforms the metadata of a Camera Trap Data Package object to an
#' [Ecological Metadata Language (EML)](https://eml.ecoinformatics.org/) file.
#'
#' @inheritParams print.camtrapdp
#' @param directory Path to local directory to write files to.
#' @param derived_paragraph If `TRUE`, a paragraph will be added to the
#'   abstract, indicating that data have been transformed using `write_dwc()`.
#' @return `eml.xml` file written to disk.
#'   And invisibly, an [EML::eml] object.
#' @family transformation functions
#' @export
#' @section Transformation details:
#' Metadata are derived from what is provided in `x`.
#' The following properties are set:
#' - **title**: Title as provided in `x$title`.
#' - **type**: Set to `Occurrence` in keywords.
#' - **subtype**: Set to `Observation` in keywords.
#' - **update frequency**: Set to `unknown`.
#' - **description**: Description as provided in `x$description`.
#'   If `derived_paragraph = TRUE` a generated paragraph is added, e.g.:
#'
#'   Data have been standardized to Darwin Core using the [camtrapdp](
#'   https://inbo.github.io/camtrapdp/) R package and only include observations
#'   (and associated media) of animals. Excluded are records that document blank
#'   or unclassified media, vehicles and observations of humans.
#' - **license**: License with scope `data` as provided in `x$licenses`.
#' - **creators**: Contributors as provided in `x$contributors`, excluding those
#' with roles `rightsHolder` and `publisher`.
#' - **contact**: Contributors with role `contact`. If none exist, first
#' creator.
#' - **metadata provider**: Same as `contact`.
#' - **keywords**: Keywords as provided in `x$keywords`.
#' - **geographic coverage**: Bounding box as provided in `x$spatial`.
#' - **taxonomic coverage**: Taxa as provided in `x$taxonomic`.
#' - **temporal coverage**: Date range as provided in `x$temporal`.
#' - **project data**: Title, acronym as identifier, description, and sampling
#'   design as provided in `x$project`.
#' - **alternative identifier**: Identifier as provided in `x$id`.
#'   If this is a DOI, no new DOI will be created when publishing to GBIF.
#' - **external link**: URL of the project as provided in
#'   `x$project$path`.
#'
#' The following properties are not set:
#' - **publishing organization**
#' - **associated parties**
#' - **sampling methods**
#' - **citations**
#' - **collection data**: not applicable.
#' @examples
#' x <- example_dataset()
#' (write_eml(x, directory = "my_directory"))
#'
#' # Clean up (don't do this if you want to keep your files)
#' unlink("my_directory", recursive = TRUE)
write_eml <- function(x, directory, derived_paragraph = TRUE) {
  # Filter dataset on observations (also affects media)
  x <- filter_observations(
    x,
    .data$observationLevel == "event",
    .data$observationType == "animal"
  )

  # Initiate EML
  eml <- list(
    packageId = uuid::UUIDgenerate(),
    system = "uuid",
    dataset = list()
  )

  # Get properties
  project <- x$project

  # Set title
  eml$dataset$title <- x$title

  # Set abstract, with optional extra paragraph
  para <- x$description %>%
    # Add <p></p> tags to each paragraph
    purrr::map_chr(~ paste0("<p>", ., "</p>"))
  if (derived_paragraph) {
    last_para <- paste0(
      "<p>Data have been standardized to Darwin Core using the ",
      "<a href=\"https://inbo.github.io/camtrapdp/\">camtrapdp</a> R package ",
      "and only include observations (and associated media) of animals. ",
      "Excluded are records that document blank or unclassified media, ",
      "vehicles and observations of humans.</p>"
    )
    para <- append(para, last_para)
  }
  eml$dataset$abstract$para <- paste0(para, collapse = "")

  # Set update frequency (requires a description, even if empty)
  eml$dataset$maintenance <- list(
    description = list(para = ""),
    maintenanceUpdateFrequency = "unknown"
  )

  # Convert contributors to a data frame
  orcid_regex <- "(\\d{4}-){3}\\d{3}(\\d|X)"
  creators <-
    purrr::map_dfr(
      x$contributors,
      ~ as.data.frame(., stringsAsFactors = FALSE)
    ) %>%
    dplyr::filter(!.data$role %in% c("rightsHolder", "publisher")) %>%
    mutate_when_missing(path = character()) %>% # Guarantee path col
    dplyr::mutate(
      first_name = purrr::map_chr(
        .data$title,
        ~ strsplit(.x, " ", fixed = TRUE)[[1]][1] # First string before space
      ),
      last_name = purrr::map_chr(
        .data$title,
        ~ sub("^\\S* ", "", .x) # Remove string up until first space
      ),
      orcid = ifelse( # Move ORCID from path to separate column
        !is.na(regexpr(orcid_regex, .data$path)),
        regmatches(.data$path, regexpr(orcid_regex, .data$path)),
        NA_character_
      ),
      path = ifelse(
        grepl(orcid_regex, .data$path),
        NA_character_,
        .data$path
      ),
      role = .data$role
    )

  # Create creators list
  creator_list <- purrr::transpose(creators)

  # Set creators
  eml$dataset$creator <- create_eml_contributors(creator_list)

  # Set contacts
  contact_df <- dplyr::filter(creators, .data$role == "contact")
  contact_list <- purrr::transpose(contact_df)
  if (length(contact_list) != 0) {
    contacts <- create_eml_contributors(contact_list)
  } else {
    contacts <- purrr::pluck(eml, "dataset", "creator", 1) # First creator
  }
  eml$dataset$contact <- contacts
  eml$dataset$metadataProvider <- contacts

  # Set keywords
  eml$dataset$keywordSet <-
    list(
      list(
        keywordThesaurus = paste(
          "GBIF Dataset Type Vocabulary:",
          "http://rs.gbif.org/vocabulary/gbif/dataset_type_2015-07-10.xml"
        ),
        keyword = "Occurrence"
      ),
      list(
        keywordThesaurus = paste(
          "GBIF Dataset Subtype Vocabulary:",
          "http://rs.gbif.org/vocabulary/gbif/dataset_subtype.xml"
        ),
        keyword = "Observation"
      ),
      list(
        keywordThesaurus = "n/a",
        keyword = x$keywords
      )
    )

  # Set license
  eml$dataset$intellectualRights$para <-
    purrr::keep(x$licenses, ~ .$scope == "data")[[1]]$name

  # Set temporal and geographic coverage
  coordinates <- x$spatial$coordinates
  eml$dataset$coverage <-
    EML::set_coverage(
      beginDate = x$temporal$start,
      endDate = x$temporal$end,
      geographicDescription =
        "Geographic description not provided for this dataset.",
      westBoundingCoordinate = coordinates[1,1,1], # long_min
      southBoundingCoordinate = coordinates[1,1,2], # lat_min
      eastBoundingCoordinate = coordinates[1,3,1], # long_max
      northBoundingCoordinate = coordinates[1,3,2] # lat_max
    )

  # Set taxonomic coverage
  taxa <- taxonomic(x)
  eml$dataset$coverage$taxonomicCoverage <-
    list(
      taxonomicClassification =
        purrr::map(1:nrow(taxa), function(i) {
          current_row <- taxa[i, ]
          list(
            taxonRankName = current_row$taxonRank,
            taxonRankValue = current_row$scientificName
          )
        }
      )
    )

  # Set project
  type_samplingDesign <-
    dplyr::case_when(
      project$samplingDesign == "simpleRandom" ~ "simple random",
      project$samplingDesign == "systematicRandom" ~ "systematic random",
      project$samplingDesign == "clusteredRandom" ~ "clustered random",
      .default = project$samplingDesign
    )
  marked_or_unmarked <-
    ifelse(project$individualAnimals, "marked", "unmarked")
  mapped_captureMethod <-
    dplyr::case_when(
      project$captureMethod == "activityDetection" ~ "activity detection",
      project$captureMethod == "timeLapse" ~ "time lapse"
    )
  captureMethod <- paste(mapped_captureMethod, collapse = " and ")
  design_para <- paste0(
    "This project uses a ", type_samplingDesign, " sampling design. ",
    "Animals are ", marked_or_unmarked, " and camera traps are triggered with ",
    captureMethod, "."
  )
  eml$dataset$project <- list(
    id = project$acronym, # Can be NULL, assigned as <project id="id">
    title = project$title,
    abstract = list(para = project$description), # Can be NULL
    designDescription = list(description = list(para = design_para)),
    personnel = eml$dataset$creator[[1]]
  )

  # Set bibliographic citation (can be NULL)
  eml$additionalMetadata$metadata$gbif$bibliography$citation <-
    x$bibliographicCitation

  # Set external link = project URL (can be NULL)
  project_url <- x$project$path
  if (!is.null(project_url)) {
    eml$dataset$distribution <- list(
      scope = "document", online = list(
        url = list("function" = "information", project_url)
      )
    )
  }

  # Set publication date = created date
  eml$dataset$pubDate <- as.Date(x$created)

  # Set alternative identifier = package id (can be DOI)
  eml$dataset$alternateIdentifier <- x$id

  # Write files
  eml_path <- file.path(directory, "eml.xml")
  cli::cli_h2("Writing file")
  cli::cli_ul(c(
    "{.file {eml_path}}"
  ))
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  EML::write_eml(eml, eml_path)

  # Return EML list invisibly
  invisible(eml)
}
