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
#' - **description**: Description as provided in `x$description`.
#'   If `derived_paragraph = TRUE` a generated paragraph is added, e.g.:
#'
#'   Data have been standardized to Darwin Core using the [camtrapdp](
#'   https://inbo.github.io/camtrapdp/) R package and only include observations
#'   (and associated media) of animals. Excluded are records that document blank
#'   or unclassified media, vehicles and observations of humans.
#' - **license**: License with scope `data` as provided in `x$licenses`.
#' - **creators**: Contributors (all roles) as provided in `x$contributors`.
#' - **contact**: First creator.
#' - **metadata provider**: First creator.
#' - **keywords**: Keywords as provided in `x$keywords`.
#' - **geographic coverage**: Bounding box as provided in `x$spatial`.
#' - **taxonomic coverage**: Species (no other ranks) as provided in
#'   `x$taxonomic`.
#' - **temporal coverage**: Date range as provided in `x$temporal`.
#' - **project data**: Title, acronym as identifier, description, and sampling
#'   design as provided in `x$project`.
#' - **alternative identifier**: Identifier as provided in `x$id`.
#'   If this is a DOI, no new DOI will be created when publishing to GBIF.
#' - **external link**: URL of the project as provided in
#'   `x$project$path`.
#'
#' The following properties are not set:
#' - **type**
#' - **subtype**
#' - **update frequency**
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

  # Set description
  eml$dataset$abstract$para <- x$description

  # Add extra paragraph to description
  if (derived_paragraph) {
    last_para <- paste0(
      # Add span to circumvent https://github.com/ropensci/EML/issues/342
      "<span></span>Data have been standardized to Darwin Core using the ",
      "<a href=\"https://inbo.github.io/camtrapdp/\">camtrapdp</a> R package ",
      "and only include observations (and associated media) of animals. ",
      "Excluded are records that document blank or unclassified media, ",
      "vehicles and observations of humans."
    )
    eml$dataset$abstract$para <- append(
      eml$dataset$abstract$para,
      paste0("<![CDATA[", last_para, "]]>")
    )
  }

  # Convert contributors to a data frame
  orcid_regex <- "(\\d{4}-){3}\\d{3}(\\d|X)"
  creators <-
    purrr::map_dfr(
      x$contributors, ~ as.data.frame(., stringsAsFactors = FALSE)
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
      )
    ) %>%
    dplyr::arrange(.data$last_name)

  # Create creators list
  creator_list <- purrr::transpose(creators)

  # Set creators
  eml$dataset$creator <- purrr::map(creator_list, ~ EML::set_responsibleParty(
    givenName = .$first_name,
    surName = .$last_name,
    organizationName = .$organization, # Discouraged by EML, but used by IPT
    email = .$email,
    userId = if (!is.na(.$orcid)) {
      list(directory = "https://orcid.org/", .$orcid)
    } else {
      NULL
    },
    onlineUrl = .$path
  ))
  eml$dataset$contact <- eml$dataset$creator[[1]]
  eml$dataset$metadataProvider <- eml$dataset$creator[[1]]

  # Set keywords
  eml$dataset$keywordSet <-
    list(list(keywordThesaurus = "n/a", keyword = c("camera traps")))

  # Set license
  eml$dataset$intellectualRights$para <-
    purrr::keep(x$licenses, ~ .$scope == "data")[[1]]$name

  # Set coverage
  bbox <- x$spatial$bbox
  taxa <- taxa(x)
  if ("taxonRank" %in% names(taxa)) {
    taxa <- dplyr::filter(taxa, .data$taxonRank == "species")
  }
  sci_names <-
    dplyr::select(taxa, "scientificName") %>%
    dplyr::rename(Species = "scientificName") # Column name should be rank
  eml$dataset$coverage <-
    EML::set_coverage(
      begin = x$temporal$start,
      end = x$temporal$end,
      west = bbox[1],
      south = bbox[2],
      east = bbox[3],
      north = bbox[4],
      sci_names = sci_names
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
  if (!is.null(x$project$path)) {
    eml$dataset$distribution <- list(
      scope = "document", online = list(
        url = list("function" = "information", x$project$path)
      )
    )
  }

  # Set publication date = created date
  eml$dataset$pubDate <- as.Date(x$created)

  # Set alternative identifier = package id (can be DOI)
  eml$dataset$alternateIdentifier <- x$id

  # Write filess
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
