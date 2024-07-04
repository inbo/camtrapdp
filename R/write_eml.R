#' Transform Camtrap DP metadata to EML
#'
#' Transforms the metadata of a [Camera Trap Data Package](
#' https://camtrap-dp.tdwg.org) to an [EML](https://eml.ecoinformatics.org/)
#' file that can be uploaded to a [GBIF IPT](https://www.gbif.org/ipt) for
#' publication.
#'
#' @param x A Camtrap DP, as read by [read_camtrap_dp()].
#' @param directory Path to local directory to write file to.
#'   If `NULL`, then the EML object is returned instead, which can be useful
#'   for extended/adapting the EML before writing with [EML::write_eml()].
#' @return `eml.xml` file written to disk or `EML` object when
#'   `directory = NULL`.
#' @family publication functions
#' @export
#' @importFrom dplyr %>% .data
#' @section Transformation details:
#' Metadata is derived from what is provided in `x` and in the function
#' parameters.
#' The following properties are set:
#' - **title**: Title as provided in `x$title`.
#' - **description**: Description as provided in `x$description`.
#'   The description is preceded by an automatically generated paragraph
#'   describing from which project and platform the dataset is derived, and
#'   to which extend coordinates are rounded (`x$coordinatePrecision`).
#' - **license**: License with scope `data` as provided in `x$licenses`.
#' - **creators**: Contributors (all roles) as provided in `x$contributors`.
#' - **contact**: First creator.
#' - **metadata provider**: First creator.
#' - **keywords**: Keywords as provided in `keywords`.
#' - **associated parties**: Organizations as provided in
#'   `x$organizations`.
#' - **geographic coverage**: Bounding box as provided in `x$spatial`.
#' - **taxonomic coverage**: Species (no other ranks) as provided in
#'   `x$taxonomic`.
#' - **temporal coverage**: Date range as provided in `x$temporal`.
#' - **project data**: Title, acronym as identifier, description, and sampling
#'   design as provided in `x$project`.
#'   The first creator is set as project personnel.
#' - **alternative identifier**: Identifier as provided in `x$id`.
#'   If this is a DOI, no new DOI will be created when publishing to GBIF.
#' - **external link**: URL of the project as provided in
#'   `x$project$path`.
#'
#' To be set manually in the GBIF IPT: **type**, **subtype**,
#' **update frequency** and **publishing organization**.
#'
#' Not set: **sampling methods** and **citations**.
#'
#' Not applicable: **collection data**.
write_eml <- function(x, directory) {

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

  # Set abstract
  last_para <-
    # Add span to circumvent https://github.com/ropensci/EML/issues/342
    "<span></span>Data have been standardized to Darwin Core using the https://inbo.github.io/camtrapdp/\">camtrapdp</a> R package and only include observations (and associated media) of animals. Excluded are records that document blank or unclassified media, vehicles and observations of humans."
  eml$dataset$abstract$para <- append(
    x$description,
    paste0("<![CDATA[", last_para, "]]>")
  )

  # Convert contributors to data frame
  orcid_regex <- "(\\d{4}-){3}\\d{3}(\\d|X)"
  creators <-
    purrr::map_dfr(x$contributors, ~ as.data.frame(., stringsAsFactors = FALSE)) %>%
    mutate_when_missing(path = character()) %>% # Guarantee path col
    tidyr::separate(
      title,
      c("first_name", "last_name"),
      sep = " ",
      extra = "merge",
      remove = FALSE
    ) %>%
    # Move ORCID from path to separate column
    dplyr::mutate(
      orcid = stringr::str_extract(.data$path, orcid_regex),
      path = ifelse(
        stringr::str_detect(.data$path, orcid_regex),
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
  taxonomy <- build_taxonomy(x)
  if ("taxonRank" %in% names(taxonomy)) {
    taxonomy <- dplyr::filter(taxonomy, .data$taxonRank == "species")
  }
  sci_names <-
    dplyr::rename(taxonomy, Species = "scientificName") %>%
    dplyr::select("Species")

  eml$dataset$coverage <- EML::set_coverage(
    begin = x$temporal$start,
    end = x$temporal$end,
    west = bbox[1],
    south = bbox[2],
    east = bbox[3],
    north = bbox[4],
    sci_names = sci_names
  )

  # Set project
  design_para <- glue::glue(
    "This project uses a {project$samplingDesign} sampling design. ",
    "Animals are {glue::glue_collapse(project$animalTypes, last = ' and ')} ",
    "and camera traps are triggered with ",
    "{glue::glue_collapse(project$captureMethod, last = ' and ')}. ",
    "Media are classified at {project$classificationLevel} level."
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

  # Return object or write file
  if (is.null(directory)) {
    eml
  } else {
    eml_path <- file.path(directory, "eml.xml")
    message(glue::glue(
      "Writing metadata to:",
      eml_path,
      .sep = "\n"
    ))
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
    EML::write_eml(eml, eml_path)
  }
}
