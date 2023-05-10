
# set of utility functions used in the Open Data Detection

#---------------------------------------------------------------------
# 1 - PDF conversion
#---------------------------------------------------------------------

#' @noRd
.create_output_filename <- function(PDF_filename, output_folder)
{
  output_filename <- PDF_filename |>
    stringr::str_replace(stringr::fixed(".pdf"),
                         stringr::fixed(".txt"))
  output_filename <- utils::tail(stringr::str_split(output_filename, stringr::fixed("/"))[[1]],1)
  output_filename <- paste0(output_folder, output_filename)

  return(output_filename)
}

#' convert PDF file to txt file and saves it to output_folder (can now deal with multiple column output)
#' @noRd


.pdf_to_text <- function(PDF_filename, output_folder, overwriteExistingFiles = FALSE) {
  success <- FALSE
  output_filename <- .create_output_filename(PDF_filename, output_folder)

  if(!file.exists(PDF_filename)) {
    print("PDF file does not exist!")
  } else if(file.exists(output_filename) && overwriteExistingFiles == FALSE) {
    print("Output file already exists!")
    success <- TRUE
  } else  {
    tryCatch({
      text <- pdftools::pdf_data(PDF_filename, font_info = TRUE) |>
        purrr::map_chr(\(x) .textbox_to_str(x, PDF_filename))
      cat(text, file = output_filename)
      success <- TRUE
    }, error = function(e) {
      print("Could not convert pdf to text.")
    })
  }

  names(success) <- PDF_filename
  return(success)
}

#' calculate the estimated number of columns, based on the mean return symbol per line
#'
#' @noRd
.est_col_n <- function(text_data) {

  dac <- space <- text <- y <- x <- line_n <- NULL
  dac_statement_present <- text_data |>
    dplyr::mutate(dac = dplyr::if_else(dplyr::lag(space) == FALSE & text == "Data" &
                                  dplyr::lead(text) == "and" &
                                  dplyr::lead(text, n = 2) == "code" &
                                  dplyr::lead(text, n = 3) == "availability" &
                                  dplyr::lead(space, n = 3) == FALSE,
                                1, 0)) |>
    dplyr::summarise(dac = sum(dac)) |>
    dplyr::pull(dac) |>
    as.logical()

  if (dac_statement_present == TRUE) return (1)

  cols <- text_data |>
    dplyr::arrange(y, x) |>
    .add_line_n() |>
    dplyr::group_by(line_n) |>
    dplyr::summarise(ret_per_line = sum(space == FALSE))

  stats::median(cols$ret_per_line)

}



#' Obtain the most frequent value in a vector
#' @noRd
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' for the enragingly awful layout in frontiers and royal publishing journals, remove references first
#' @noRd
.reformat_columns <- function(text_data) {

  y_ref <- text_data |>
    dplyr::filter(stringr::str_detect(text, "REFERENCES|References"),
                  space == FALSE) |>
    dplyr::pull(y) |>
    min(743) # if no references found, set to y value of footer

  text_data |>
    dplyr::filter(y < y_ref) |>
    dplyr::arrange(y, x)
}


#' add line numbers, but check if rearranging needed first
#' @noRd
.add_line_n <- function(text_data) {

  text_data |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0),
                  line_n = abs(jump_size) > 5,
                  # line_n = dplyr::lag(space == FALSE, default = 0) & jump_size > 5,
                  line_n = cumsum(line_n))
}


#' convert the dataframe extracted by pdftools::pdf_data into a one-column string
#' to be saved as a txt for further processing
#' @noRd
.textbox_to_str <- function(text_data, PDF_filename) {

  if (nrow(text_data) == 0) return("")

  # remove hidden text tags in Elsevier Journals
  if (stringr::str_detect(PDF_filename, "10\\.1016\\+j\\.ecl")) {
    text_data <- text_data |>
      dplyr::filter(font_name != "DOHGPO+AdvP48722B",
                    font_size > 7) |>
      dplyr::mutate(text = stringr::str_remove(text, "Tag$"))

  } else if (stringr::str_detect(PDF_filename, "10\\.1038\\+s41531"))
    text_data <- text_data |>
      dplyr::filter(font_name != "BBKNAK+AdvTT6780a46b")
  # special case for the frontiers, royal society special 2col formatting
  if (stringr::str_detect(PDF_filename, "10\\.3389\\+f|10\\.1098\\+rspb")) {
    text_data <- .reformat_columns(text_data)
    cols <- 2
  } else {
    cols <- .est_col_n(text_data)
  }

  col2_x <- max(text_data$x, 700) # initial estimate is the maximum, works for single column layouts
  min_x <- min(text_data$x, 0) + 100


  if (cols == 2 | is.na(cols)) {

    if (nrow(text_data) < 2) return("")

    # estimate the x coordinate of the second column, usually around 303 - 306
    col2_x_est <- text_data |>
      dplyr::filter(x > min_x & dplyr::lag(space) == FALSE) |>
      dplyr::count(x) |>
      dplyr::filter(n > 5) |>
      dplyr::pull(x) |>
      min()

    # if above fails, take the max estimate, reducing in effect to a single column
    if (is.na(col2_x_est)) col2_x_est <- col2_x
    # if max is at least 100 to the right of estimate, then estimate is good
    if (col2_x - col2_x_est > 100) col2_x <- col2_x_est

 }

  min_y <- min(35, min(text_data$y)) # remove header in most journals

  text_data <- text_data |>
    # remove page numbers, textboxes with citation numbers, line numbers, etc.
    # as well as the very first line on top of the page
    dplyr::filter(!(stringr::str_detect(text, "^\\d+\\.*$") & space == FALSE), y > min_y) |>
    # remove the references section because it often collides with DAS
    dplyr::filter(!(stringr::str_detect(tolower(text), "references$") & space == FALSE)) |>
    dplyr::mutate(x_jump_size = x - dplyr::lag(x, default = 0),
           column = dplyr::if_else(x >= col2_x, 2, 1))


  if (cols == 2) text_data <- text_data |>
    dplyr::arrange(column, y, x)

  text_data <- .add_line_n(text_data) |>
    dplyr::arrange(column, line_n, x) |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0))



  section_jump <- text_data$jump_size[text_data$jump_size > 0] |>
    Mode() * 1.4

  # regular_font_size <- text_data |>
  #   filter(font_size > 7) |>
  #   pull(font_size) |>
  #   Mode()
  # str_detect("[B,b]old|\\.[B,b]|PSHN-H$|PA5D1$")

  res <- text_data |>
    dplyr::mutate(
      heading_font = dplyr::if_else(
        abs(font_size - dplyr::lag(font_size)) > 1.4 |
          !stringr::str_detect(text, "[[:lower:]]") | # only caps
        # font_size - regular_font_size > 1 |
          stringr::str_detect(font_name, "[B,b]old|\\.[B,b]|AdvPSHN-H$|AdvPA5D1$|AdvOTd67905e7$"), TRUE, FALSE),
      newline_heading = line_n == 1 & is.na(heading_font) |
        line_n > dplyr::lag(line_n) &
        stringr::str_detect(dplyr::lag(text), "\\.$") &
        (font_name != dplyr::lag(font_name)),
      paragraph_start = abs(jump_size) > section_jump,
      section_start = (paragraph_start & heading_font) | newline_heading,
      text = dplyr::if_else(section_start == TRUE, paste("\n<section>", text), text)) |>
      # text = ifelse(section_start == TRUE, paste(" .\n<Section>", text), text)) |>
    dplyr::group_by(line_n) |>
    dplyr::summarise(text = paste(text, collapse = " ")) |>
    dplyr::summarise(text = paste(text, collapse = "\n ")) |>
    dplyr::pull(text)

  res
}
#--------------------------------------------------------------------------------------
# 2 - text data loading
#--------------------------------------------------------------------------------------

#' search for sentences that were falsely split on abbreviations like accession nr.
#' and pastes them together again
#' @noRd

.correct_tokenization <- function(PDF_text)
{
  PDF_text_corrected <- PDF_text
  sentence_paste_idx <- PDF_text  |>
    stringr::str_sub(-13, -1) |>
    stringr::str_detect("accession nr\\.|acc(ession)? no\\.|ccession nos\\.|ccession nrs\\.") |>
    which()

  #for all indices do a pairwise pasting
  if(length(sentence_paste_idx) > 0)
  {
    for(i in 1:length(sentence_paste_idx))
    {
      PDF_text_corrected <- .paste_idx(PDF_text_corrected, sentence_paste_idx[i]-(i-1))
    }
  }

  return(PDF_text_corrected)
}

#' paste together sentences where tokenization needs to be corrected by index
#' @noRd

.paste_idx <- function(PDF_text, idx)
{
  #create dummy sentences such that the indexing always works correctly,
  #even with only one element in PDF_text
  PDF_text_pasted <- c("x", PDF_text, "x")
  idx <- idx + 1 #shift idx due to dummy sentence

  PDF_text_pasted <- c(PDF_text_pasted[1:(idx-1)],
                       paste(PDF_text_pasted[idx], PDF_text_pasted[idx+1]),
                       PDF_text_pasted[(idx+2):length(PDF_text_pasted)])
  #remove dummy elemets
  PDF_text_pasted <- PDF_text_pasted[c(-1, -length(PDF_text_pasted))]

  return(PDF_text_pasted)
}

#' format
#' @noRd
# textfile <- paste0(pdf_text_folder, txt_filenames)
.tokenize_sections <- function(textfile)
{
  readr::read_lines(textfile) |>
    paste(collapse = " ") |>
    stringr::str_replace_all("\n", ".") |>
    stringr::str_squish() |>
    tokenizers::tokenize_sentences(simplify = TRUE, lowercase = TRUE) |>
    tokenizers::tokenize_regex(pattern = " (?=<section>)", simplify = TRUE) |>
    purrr::list_c() |>
    stringr::str_replace_all(pattern = ",", replacement = "") |>
    stringr::str_replace_all(pattern = "- ", replacement = "") |>
    .correct_tokenization()
}


#--------------------------------------------------------------------------------------
# 3 - Open data identification keywords
#--------------------------------------------------------------------------------------

#' Search for several categories of similar keywords in a sentence.
#' Multiple categories must match to trigger a detection.
#' @noRd
.create_keyword_list <- function()
{
  keyword_list <- list()

  available <- c("included",
                 "deposited",
                 "archived",
                 "released",
                 "(is|are) provided",
                 "(is|are) hosted",
                 "contained in",
                 "listed in",
                 "lodged with",
                 "(?<!the) available",
                 "doi of the data(set)? (is|are)",
                 "reproduce",
                 "accessible",
                 "(can|may) be (freely )?accessed",
                 "(can|may) be retrieved",
                 "submitted",
                 "(can|may) be downloaded",
                 "(can|may) be found",
                 "held in",
                 "reported in",
                 "uploaded",
                 "are public on",
                 "added",
                 "(?<=data) at",
                 "doi: ") |>
    .format_keyword_vector()
  keyword_list[["available"]] <- available

  was_available <- c("(was|were) provided",
                     "(was|were)? previously published",
                     "(was|were) contained in",
                     "(was|were) available",
                     "(was|were) accessible",
                     "(was|were) deposited by",
                     "(was|were) reproduced",
                     "were open source data",
                     "(previous|prior) study",
                     "were (used|analyzed) in this study",
                     "were downloaded and analyzed",
                     "this study used .* publicly available",
                     "existing publicly available",
                     "data .* reanalyzed here") |>
    .format_keyword_vector()
  keyword_list[["was_available"]] <- was_available

  not_produced <- c("not developed",
                    "not produced",
                    "not generated") |>
    .format_keyword_vector()

  keyword_list[["not_produced"]] <- not_produced


  not_available <- c("not included",
                     "not deposited",
                     "not released",
                     "not provided",
                     "not contained in",
                     "not available",
                     "not accessible",
                     "not submitted",
                     "no \\w.* (is|are) available") |>
    .format_keyword_vector()
  keyword_list[["not_available"]] <- not_available

# stringr::str_detect("Proteome exchange", "Proteome *[E,e][X,x]change")
  # field_specific_repo |> sort() |> paste(collapse = ",\n")
  field_specific_repo <- c(
    "10xgenomics",
    "23andme",
    "accession codes*",
    "accession numbers*",
    "ACDC",
    "Addgene",
    "ADHD *200",
    "AIBL",
    "Array[ ,-]*Express",
    "BBMRI.nl",
    "bbmri.nl",
    "Biological General Repository for Interaction Datasets",
    "Biological Magnetic Resonance Data Bank",
    "BioProject",
    "BMRB",
    "Broad Institute",
    "Cambridge Crystallographic Data Centre",
    "caNanoLab",
    "Cancer Imaging Archive",
    "CATS",
    "CCDC",
    "CCMS",
    "Cell Image Library",
    "ChEMBL",
    "clinicaltrials.gov",
    "ClinVar",
    "Coherent X-ray Imaging Data Bank",
    "Computational Chemistry Datasets",
    "ConnectomeDB",
    "CPTAC",
    "Crystallography Open Database",
    "CXIDB",
    "Database of Interacting Proteins",
    "dbGaP",
    "dbSNP",
    "dbVar",
    "DDBJ",
    "DECIPHER",
    "DepMap Portal",
    "DNA Data Bank of Japan",
    "DZNE",
    "EBI",
    "EBRAINS",
    "Electron microscopy data bank",
    "EMBL-EBI",
    "emdb",
    "EMIDEC",
    "ega",
    "ENA",
    "ENCODE",
    "Environmental Data Initiative",
    "Eukaryotic Pathogen Database Resources",
    "EuPathDB",
    "European Genome[ ,-]*phenome Archive",
    "European Molecular Biology Laboratory",
    "European Nucleotide Archive",
    "European Variation Archive",
    "FlowRepository",
    "FASTGenomics",
    "G-Node",
    "GBIF",
    "GenBank",
    "Gene Expression Omnibus",
    "GenomeRNAi",
    "Genomic Expression Archive",
    "GEO",
    "GESIS",
    "GHDx",
    "GigaDB",
    "Global Biodiversity Information Facility",
    "gmgc",
    "GPCRMD",
    "GWAS",
    "HIV Data Archive Program",
    "Image Data Resource",
    "ImmPort",
    "Influenza Research Database",
    "INDI",
    "IntAct",
    "Integrated Taxonomic Information System",
    "IoChem-BD",
    "ITIS",
    "Japanese Genotype-phenotype Archive",
    "KiMoSys",
    "Kinetic Models of Biological Systems",
    "Knowledge Network for Biocomplexity",
    "LCSB",
    "LTER Network Information System Data Portal",
    "MassIVE",
    "MetaboLights",
    "Metagenomics Rapid Annotation using Subsystem Technology",
    "Metabolomics Workbench",
    "MG-RAST",
    "Mgnify",
    "MiDAS",
    "Morphobank",
    "Mouse Genome Informatics",
    "NAHDAP",
    "National Center for Biotechnology Information",
    "National Database for Autism Research",
    "National Database for Clinical Trials related to Mental Illness",
    "NCBI",
    "NDAR",
    "NDCT",
    "NDEx",
    "Neuroimaging Informatics Tools and Resources Collaboratory",
    "NeuroMorpho",
    "NIMH Data Archive",
    "NIST",
    "NITRC",
    "OAR[-,\\.]PTB",
    "Open *Agrar",
    "OpenNeuro",
    "PCDDB",
    "PDB",
    "PeptideAtlas",
    "PGS Catalog",
    "PhysioNet",
    "PRIDE",
    "Protein Circular Dichroism Data Bank",
    "Protein Data *Bank",
    "proteindiffraction.org",
    "Proteome *EXchange",
    "PubChem",
    "Rat Genome Database",
    "RdoCdb",
    "Research Domain Criteria Database",
    "SAMS",
    "Sequence Read Archive",
    "SICAS Medical Image Repository",
    "SRA",
    "STRENDA",
    "St. Jude Cloud",
    "stjude.cloud",
    "Structural Biology Data Grid",
    "Synapse",
    "TCGA",
    "The Network Data Exchange",
    "Treebase",
    "ucdr.be/data",
    "UK Data Service",
    "UniProt[KB]*",
    "VectorBase",
    "Virtual Trials Archive",
    "Worldwide Protein Data Bank",
    "wwPDB",
    "Xenbase",
    "Zebrafish Model Organism Database",
    "ZFIN"
  ) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["field_specific_repo"]] <- field_specific_repo

  accession_nr <- c("G(SE|SM|DS|PL)[[:digit:]]{2,}", #GEO
                    "PRJ(E|D|N|EB|DB|NB)[:digit:]+",
                    "SAM(E|D|N)[A-Z]?[:digit:]+",
                    "[A-Z]{1}[:digit:]{4}", #GenBank
                    "[A-Z]{2}[:digit:]{6}",
                    "[A-Z]{3}[:digit:]{5}",
                    "[A-Z]{4,6}[:digit:]{3,}",
                    "GCA_[:digit:]{9}\\.[:digit:]+",
                    "SR(P|R|X|S|Z)[[:digit:]]{3,}",
                    "(E|P)-[A-Z]{4}-[:digit:]{1,}",
                    "(?<!\\.)[:digit:]{1}[a-z]{1}[[:alnum:]]{2}",
                    "MTBLS[[:digit:]]{2,}",
                    "10.17590",
                    "10.5073",
                    "10.25493",
                    "10.6073",
                    "10.15468",
                    "10.5063",
                    "fcon_1000\\.projects\\.nitrc\\.org", # URL for INDI
                    "[[:digit:]]{6}",
                    "[A-Z]{2,3}_[:digit:]{5,}",
                    "[A-Z]{2,3}-[:digit:]{4,}",
                    "[A-Z]{2}[:digit:]{5}-[A-Z]{1}",
                    "DIP:[:digit:]{3}",
                    "FR-FCM-[[:alnum:]]{4}",
                    "ICPSR [:digit:]{4}",
                    "SN [:digit:]{4}",
                    "key resources table") |>
    .format_keyword_vector()
  keyword_list[["accession_nr"]] <- accession_nr

  repositories <- c("available online *(\\(|\\[)+",
                    "10\\.17617/3.", # Edmond
                    "10\\.18452", # edoc HU
                    "10\\.13130", # UNIMI Dataverse
                    "10\\.1038 */s41597-", # scientific data
                    "code ocean *(capsule)?",
                    "cyverse",
                    "dataverse(\\.harvard.\\edu)?",
                    "DataverseNL",
                    "dryad",
                    "datadryad",
                    "(fi|\\ufb01)gshare",
                    "GIGADB",
                    "GigaScience database",
                    "harvard dataverse",
                    "heidata",
                    "mendeley data",
                    "OpenNeuro",
                    "open science framework",
                    "osf",
                    "tu datalib",
                    "UNIMI",
                    "zivahub",
                    "zenodo"
                    ) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["repositories"]] <- repositories

  github <- c("github") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["github"]] <- github


  data <- c("data",
            "dataset",
            "datasets") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["data"]] <- data


  all_data <- c("all data",
                "all array data",
                "raw data",
                "source data",
                "full data set",
                "full dataset",
                # "crystallographic data",
                "subject-level data") |>
    .format_keyword_vector()
  keyword_list[["all_data"]] <- all_data


  not_data <- c("not all data",
                "not all array data",
                "no raw data",
                "no full data set",
                "no full dataset") |>
    .format_keyword_vector()
  keyword_list[["not_data"]] <- not_data


  source_code <- c("source code",
                   "analysis script",
                   "data and code",
                   "github",
                   "gitlab",
                   "code ocean",
                   "SAS script",
                   "SPSS script",
                   "R script",
                   "R code",
                   "python script",
                   "python code",
                   "matlab script",
                   "matlab code",
                   "macro",
                   "software") |>
    .format_keyword_vector()
  keyword_list[["source_code"]] <- source_code


  supplement <- c("supporting information",
                  "supplement",
                  "supplementa(l|ry) data",
                  "supplementa(l|ry) material",
                  "supplementa(l|ry) information"
                  # "provided with this paper",
                  # "(with)?in th(is|e) article",
                  ) |>
    .format_keyword_vector()
  keyword_list[["supplement"]] <- supplement


  file_formats <- c("csv",
                    "zip",
                    "xls",
                    "xlsx",
                    "sav",
                    "cif",
                    "fasta") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["file_formats"]] <- file_formats


  upon_request <- c("(up)?on( reasonable)? request",
                    "without undue reservation",
                    "the corresponding author",
                    "the lead contact",
                    "requests.* should be (directed|submitted) to",
                    "data requests?")|>
    .format_keyword_vector()
  keyword_list[["upon_request"]] <- upon_request


  data_availability <- c("Software and Data Availability",
                         "Statistical Analysis and Data Availability",
                         "Data sharing statement",
                         "Data sharing",
                         "Data A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y S ?t ?a ?t ?e ?m ?e ?n ?t",
                         "Data A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y P ?o ?l ?i ?c ?y",
                         "Data A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y",
                         "Data availability statement & Data deposition",
                         "Data and code availability",
                         "Data deposition",
                         "Deposited Data",
                         "Data Archiving",
                         "Accessibility of data and materials",
                         "Availability of data and materials",
                         "Availability of data",
                         "Data Accessibility",
                         "Accessibility of data",
                         "Accessibility of data and code",
                         "Accession codes") |>
    .format_keyword_vector()
  keyword_list[["data_availability"]] <- data_availability


  code_availability <- c("Code sharing",
                         "Code availability statement",
                         "Code availability",
                         "Data and code availability",
                         "Accessibility of data and code",
                         "Source code available from:") |>
    .format_keyword_vector()
  keyword_list[["code_availability"]] <- code_availability


  supplemental_table_name <- c("supplementa(l|ry) tables?",
                          "tables?",
                          "additional file",
                          "file", "files",
                          "article/supplementa(l|ry) material",
                          "supplementa(l|ry) information",
                          "supplementa(l|ry) file")
  supplemental_table_number <- c("S[[:digit:]]", "[[:digit:]]", "[A-Z]{2}[[:digit:]]")
  supplemental_table <- .outer_str(supplemental_table_name, supplemental_table_number) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["supplemental_table"]] <- supplemental_table


  supplemental_dataset <- c("supplementary data [[:digit:]]{1,2}",
                      "supplementary dataset [[:digit:]]{1,2}",
                      "supplementary data set [[:digit:]]{1,2}",
                      "supplemental data [[:digit:]]{1,2}",
                      "supplemental dataset [[:digit:]]{1,2}",
                      "supplemental data set [[:digit:]]{1,2}")
  dataset_name <- c("data", "dataset", "datasets", "data set", "data sets")
  dataset_number <- c("S[[:digit:]]{1,2}")
  dataset <- .outer_str(dataset_name, dataset_number)
  dataset <- c(dataset, supplemental_dataset) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["dataset"]] <- dataset

  protocol <- c("protocol") |>
    .format_keyword_vector()
  keyword_list[["protocol"]] <- protocol


  data_journal_dois <- c(
    # "10.1038/s41597-019-", # Scientific Data (2019)
                         "10.3390/data", # Data
                         "10.1016/j.dib", # Data in Brief
                         "10.1038/s41597-", # Scientific Data (all years)
                         "10.3897/BDJ.", # BioDiversity Data Journal
                         "10.1016/j.cdc.", # Chemical Data Collection
                         "10.5194/essd-", # Earth System Science Data
                         "10.1002/gdj3.", # GeoScience Data Journal
                         "10.1016/j.gdata.", # Genomics Data
                         "10.5334/joad.", # Journal of Open Archaeology Data
                         "10.5334/johd." # Journal of Open Humanities Data
                         ) |>
    .format_keyword_vector()
  keyword_list[["data_journal_dois"]] <- data_journal_dois


  #special regex pattern that looks for word closeness instead of words being in the same sentence
  #effect: all_data & file_format words are at most 10 words apart from each other
  all_data_file_formats <- .near_wd_sym(all_data, file_formats, dist = 10)
  keyword_list[["all_data_file_formats"]] <- all_data_file_formats


  supp_table_data <- .near_wd_sym(supplemental_table,
                                 paste(file_formats, all_data, sep = "|"),
                                 dist = 10)
  keyword_list[["supp_table_data"]] <- supp_table_data

  # data_availability_statement <- .near_wd(data_availability,
  #                                        paste("doi", accession_nr, repositories, "online", sep = "|"),
  #                                        dist = 30)
  # keyword_list[["data_availability_statement"]] <- data_availability_statement
  keyword_list[["data_availability_statement"]] <- data_availability

  return(keyword_list)
}


#' standardize the format of different keyword vectors
#' @noRd
.format_keyword_vector <- function(keywords, end_boundary = FALSE) {
  #typically word boundaries are added in the beginning only to allow for different possible endings
  if(end_boundary) {
    keywords_formatted <- paste0("\\b", keywords, "\\b")
  } else {
    keywords_formatted <- paste0("\\b", keywords)
  }
  #collapse keywords into one string with OR symbol between them and convert to lowercase
  keywords_formatted <- paste(keywords_formatted, collapse = "|") |> tolower()

  return(keywords_formatted)
}


# return all the pasted combinations of two strings coming from different vectors
# used to make all possible text combinations of two related keyword categories
#' @noRd
.outer_str <- function(x, y)
{
  outer_1 <- outer(x, y, FUN = "paste") |> as.vector()
  outer_2 <- outer(y, x, FUN = "paste") |> as.vector()
  outer_sym <- c(outer_1, outer_2)

  return(outer_sym)
}


#' create Regex that searches for cases where words x and y are at max dist words apart
#' @noRd
.near_wd_sym <- function(x, y, dist = 10)
{
  combined <- paste0("\\b(",
                     x,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     y,
                     ")\\b|\\b(",
                     y,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     x,
                     ")\\b")

  return(combined)
}


#' create Regex that searches for cases where words x and y are at max dist words apart,
#' assymetric version where only the case with x before y is checked
#' @noRd
.near_wd <- function(x, y, dist = 10)
{
  # combined <- paste0("\\b(",
  combined <- paste0("(",
                     x,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     y,
                     ")\\b")

  return(combined)
}



#---------------------------------------------------------------------
# 4 - Open data identification
#---------------------------------------------------------------------

# identify the text fragments in which the Open Data keywords were detected
#' @noRd
.text_fragments <- function(open_data_tibble) {

  tibble_cols <- dim(open_data_tibble)[2]
  sentence_idx <- apply(open_data_tibble[2:tibble_cols], 2, which)

  if(length(sentence_idx) == 0) {
    return(rep("", dim(open_data_tibble)[2]-1))
  }

  text_frag <- vector()
  for(idx in sentence_idx)
  {
    if(length(idx) == 0) {
      text_frag <- c(text_frag, "")
    } else {
      current_txt <- open_data_tibble[[1]][idx] |> paste(collapse = ";     ")
      text_frag <- c(text_frag, current_txt)
    }
  }

  return(text_frag)
}

# publ_sentences <- "data availability source data are provided with this paper."
# publ_sentences <- PDF_text_sentences[[1]]

#' apply all keyword category searches onto each sentence of the publications
#' @noRd
.map_keywords <- function(publ_sentences)
{
  keyword_list <- .create_keyword_list()

  # search for all relevant keyword categories
  # not all keyword categories are used for the sentence search
  sentence_search_keywords <- c("available", "was_available", "not_available",
                                "field_specific_repo", "accession_nr", "repositories",
                                "github", "data", "all_data",
                                "not_data", "source_code", "supplement",
                                "file_formats", "upon_request", "dataset", "protocol")

  # search for all relevant keyword categories
  publ_keywords <- sentence_search_keywords  |>
    furrr::future_map(.search_keyword_cat, publ_sentences, keyword_list)
  names(publ_keywords) <- sentence_search_keywords

  #gather results
  publ_keywords <- do.call(cbind, publ_keywords) |>
    dplyr::as_tibble()
  publ_keywords <- cbind(publ_sentences, publ_keywords) |>
    dplyr::as_tibble()
  publ_keywords$publ_sentences <- as.character(publ_keywords$publ_sentences)

  return(publ_keywords)
}
# sentences <- publ_sentences
# sentences <- paste("data availability", publ_sentences) |> tolower()
# keyword <- "repositories"
# keyword <- "data_availability_statement"
#' search for all keyword categories in the sentences
#' @noRd
.search_keyword_cat <- function(keyword, sentences, keyword_list)
{
  detection_col <- sentences |>
    furrr::future_map_lgl(stringr::str_detect, pattern = keyword_list[[keyword]])

  return(detection_col)
}
# sections_v <- PDF_text_sentences[DAS_start:DAS_end]
#' search plos DAS when spread over two pages
#' @noRd
.splice_twopager <- function(sections_v) {

  # if (length(sections_v) == 1) return(sections_v)
  if (any(stringr::str_detect(sections_v, "<section> plos"))) {

    if (which.max(stringr::str_detect(sections_v, "<section> plos")) == length(sections_v)) {
      return(sections_v[-length(sections_v)])
    } else {
      return(c(sections_v[1], sections_v[length(sections_v)]))
    }

  } else {
    return(sections_v)
  }


}

#' test if text contains data availability statement
#' @noRd
.has_DAS <- function(sentence, keyword_list) {

  data_availability <- keyword_list[["data_availability"]]

  data_availability <- paste0("(<section>)\\W+(", data_availability, ")\\b")
  stringr::str_detect(sentence, data_availability)
}

# sentence <- "<section> data availability"
# .has_DAS(sentence)

#' extract data availability statement
#' @noRd
.extract_DAS <- function(PDF_text_sentences) {

  keyword_list <- .create_keyword_list()

  data_availability <- keyword_list[["data_availability"]]

  # lowered_text_sentences <- tolower(PDF_text_sentences)

  # check if meta-science article containing >5 mentions of DAS, then return whole text
  # DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
  #                     \(sentence) stringr::str_detect(sentence, data_availability))
  # if (sum(DAS_detections) > 5) return(PDF_text_sentences)

  # data_availability <- .near_wd("section", data_availability, 1)
  # data_availability <- paste0("(<section>)\\W+(", data_availability, ")\\b")
  DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
                      \(sentence) .has_DAS(sentence, keyword_list))
  # check if meta-science article containing >5 mentions of DAS, then return whole text
  # if (sum(DAS_detections) > 2) return(PDF_text_sentences)
  DAS_start <- which(DAS_detections)

  # if more than one detections of DAS were made, then return full document
  if (length(DAS_start) == 2) {
    if (diff(DAS_start) == 1) { # if the DAS starts with <section> and a DAS keyphrase
      DAS_start <- min(DAS_start)
    } else {
      return(PDF_text_sentences)
    }
  } else if (length(DAS_start) != 1) {
    return(PDF_text_sentences)
  }


  # stringr::str_detect("<section> abstract", "^<section> (?!abstract)")
  str_DAS <- PDF_text_sentences[DAS_start] |>
    stringr::str_trim()
  str_DAS_sameline <- str_DAS |> stringr::str_remove(data_availability)

  # if (stringr::str_length(str_DAS_sameline) > 5) {
  #
    DAS_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + 1):length(PDF_text_sentences)],
                                         \(sentence) stringr::str_detect(sentence, "section> (?!d )|^\\d\\.")) |>
      which() - 1
    DAS_end <- DAS_end_candidates[1]

    if (DAS_start / length(DAS_detections) < 0.1) { # for plos journals with DAS on first page
      # ti <- tibble(text = ti )
      DAS_second_part <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + DAS_end + 1):length(PDF_text_sentences)],
                                               \(sentence) stringr::str_detect(sentence, "<section> funding:"))
      # "^<section> (?!(abstract|author|plos))"))
      if (sum(DAS_second_part) == 0) {
        DAS_end <- DAS_end
        } else {
          DAS_end <- DAS_end + which(DAS_second_part) - 1
          }
      } else if ((stringr::str_length(str_DAS_sameline) < 5 & DAS_end == 0) |
                 !stringr::str_detect(PDF_text_sentences[DAS_start + DAS_end], "\\.$") ) {
        # DAS_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + 2):length(PDF_text_sentences)],
        #                           \(sentence) stringr::str_detect(sentence, "section"))
        # DAS_end <- which.max(DAS_end_candidates)
        DAS_end <- DAS_end_candidates[2]
#
#         if (!stringr::str_detect(PDF_text_sentences[DAS_start + DAS_end], "\\.$")) {
#           DAS_end <- DAS_end + DAS_end_candidates[(DAS_end + 1):length(DAS_end_candidates)] |> which.max()
#         }

      }
#
#     if (!stringr::str_detect(PDF_text_sentences[DAS_start + DAS_end], "\\.$")) {
#       DAS_end <- DAS_end_candidates[2]
#     }

      # tibe <- tibble(text = PDF_text_sentences[(DAS_start + DAS_end + 1):length(PDF_text_sentences)],
      #                detection = furrr::future_map_lgl(PDF_text_sentences[(DAS_start + DAS_end + 1):length(PDF_text_sentences)],
      #                                             \(sentence) stringr::str_detect(sentence,
      #                                                                             "^<section> (?!(abstract|author))")))


  # } else {
  #   DAS_end <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + 1):length(PDF_text_sentences)],
  #                             \(sentence) stringr::str_detect(sentence, "section")) |>
  #     which.max() - 1
  # }

  DAS_end <- DAS_start + DAS_end
  # str_detect("https://", "/")
  # str_remove_all("https://doi. org/10.5281/zenodo.4498214.", "(?<=https?://\\w{0,10}\\.?) ")
  # PDF_text_sentences <-
  PDF_text_sentences[DAS_start:DAS_end] |>
    .splice_twopager() |>
    paste(collapse = " ") |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim() |>
    # in some cases the references interpolate between DAS title and DAS text
    stringr::str_remove("<section> references.*") |> # remove references
    # stringr::str_remove("<section> plos.*(?=<)") |> # remove plos remainder
    stringr::str_remove_all("(?<=\\d\\.) (?=\\w)") |> # remove space separating URL or similar
    stringr::str_remove_all("(?<=www\\.(\\w){0,10}\\.?) ") |> # remove space separating URL or similar
    stringr::str_remove_all("(?<=https?://\\w{0,10}\\.?) (?=\\w)") |> # remove space separating URL or similar
    stringr::str_remove_all("(?<=et al)\\.") |> # remove dot after et al.
    stringr::str_replace("(?<=repository)\\.", ":") |>  # for the weird cases when after repository a . and not : follows
    stringr::str_replace("(?<=were analyzed in this study)\\.", ":") |>  # for the standard phrasing of data re-use
    tokenizers::tokenize_regex(pattern = "(?<=\\.) ", simplify = TRUE) |> # tokenize sentences
    .correct_tokenization()

  # PDF_text_sentences |> unlist() |> paste(collapse = " ") |>
#   str_remove_all("(?<=et al)\\.")
#
#   DAS_start <- stringr::str_locate(PDF_DAS_sentences, data_availability)[1]
#   stringr::str_sub(PDF_DAS_sentences,
#                    start = DAS_start,
#                    stringr::str_length(PDF_DAS_sentences))

}

# tib <- tibble(text = PDF_text_sentences)


#'
#' @noRd
.keyword_search_full <- function(PDF_text_sentences)
{
  # if (length(PDF_text_sentences) == 1)
# PDF_text_sentences <- "The crystallographic data and structure were deposited in the Protein Data Bank under ID: 5AHK." |> tolower()
  #search for open data keywords in the full texts
  open_data_categories <- furrr::future_map(PDF_text_sentences, .map_keywords, .progress = TRUE)

  #combine columns for the different open data keywords
  keyword_results_combined <- open_data_categories  |>
    purrr::map(dplyr::mutate, com_specific_repo = field_specific_repo & accession_nr & available & !not_available & !was_available) |>
    purrr::map(dplyr::mutate, com_general_repo = repositories & available & !not_available & !was_available) |>
    purrr::map(dplyr::mutate, com_github_data = data & github & available & !not_available & !was_available) |>
    purrr::map(dplyr::mutate, com_code = source_code & available & !not_available & !was_available & !upon_request) |>
    purrr::map(dplyr::mutate, com_suppl_code = supplement & source_code) |>
    purrr::map(dplyr::mutate, com_reuse = was_available & data & (field_specific_repo | repositories )) |>
    purrr::map(dplyr::mutate, com_request = upon_request) |>
    purrr::map(dplyr::select, publ_sentences, com_specific_repo, com_general_repo,
                com_github_data, dataset, com_code, com_suppl_code, com_reuse, com_request)

  return(keyword_results_combined)
}


#' keyword search on the tokenized sentences
#' @noRd
.keyword_search_tokenized <- function(keyword_results_combined)
{
  #summarze results over all sentences of each publication to see if any match was found for each keyword category
  keyword_results_tokenized <- keyword_results_combined |>
    purrr::map(\(x) dplyr::select(x, -publ_sentences)) |>
    purrr::map(\(x) apply(x, MARGIN = 2, FUN = any))
  keyword_results_tokenized <- do.call(rbind, keyword_results_tokenized) |>
    tibble::as_tibble()

  return(keyword_results_tokenized)
}


#' search for nearby words on the full text, part of the keyword search
#' @noRd
.keyword_search_near_wd <- function(PDF_text_sentences, extract_text = FALSE)
{
  keyword_list <- .create_keyword_list()

  #needs text input in two formats: split into sentences, and one long string for
  PDF_text_full <- PDF_text_sentences |>
    furrr::future_map_if(.p = \(sentence) length(sentence) > 1,
                         .f = \(sentence) paste(sentence, collapse = " "))

  #search for the last combination in the non-tokenized text and add it to the results table for the publications
  #either give out TRUE/FALSE or return detected string
  if(extract_text) {
    str_function <- stringr::str_extract
    map_function <- furrr::future_map_chr
  } else {
    str_function <- stringr::str_detect
    map_function <- furrr::future_map_lgl
  }
  keyword_results_near_wd <- dplyr::tibble(
    com_file_formats = map_function(PDF_text_full, str_function,
                                    pattern = keyword_list[["all_data_file_formats"]]),
    com_supplemental_data = map_function(PDF_text_full, str_function,
                                         pattern = keyword_list[["supp_table_data"]])
    #
    # ,
    # com_data_availability = unlist(PDF_text_full)
    # com_data_availability = map_function(PDF_text_full, str_function,
    #                                      pattern = keyword_list[["data_availability_statement"]])
    )

  return(keyword_results_near_wd)
}


#' helper function for the testing of the keywords with testthat
#' @noRd
.detect_keywords <- function(string, keyword_category)
{
  keywords <- .create_keyword_list()

  string <- tolower(string)
  detected <- stringr::str_detect(string, keywords[[keyword_category]])

  return(detected)
}


#' additional Open Data check: check if PDF DOI stems from a list of Data Journals
#' requires that the PDF filename is the DOI
#' @noRd
.check_journal_doi <- function(PDF_text_sentences)
{
  keyword_list <- .create_keyword_list()

  dois <- names(PDF_text_sentences) |>
    stringr::str_replace_all(stringr::fixed("+"), stringr::fixed("/")) |>
    stringr::str_remove(stringr::fixed(".txt"))

  data_journal_doi <- dplyr::tibble(
    is_data_journal = purrr::map_lgl(dois, stringr::str_detect, pattern = keyword_list[["data_journal_dois"]]))

  return(data_journal_doi)

}


#' assign the detected Open Data cases to certain categories depending
#' on the keyword category in which it was found
#' @noRd
.OD_category <- function(specific_repo,
                         general_repo,
                         suppl,
                         reuse,
                         request,
                         # DAS,
                         data_journal)
{
  category = vector()

  # should give out several categories
  # if there are multipe matches
  if(specific_repo == TRUE) {
    category <- category |> c("field-specific repository")
  }
  if(general_repo == TRUE) {
    category <- category |> c("general-purpose repository")
  }
  if(suppl == TRUE) {
    category <- category |> c("supplement")
  }
  if(reuse == TRUE) {
    category <- category |> c("re-use")
  }
  if(request == TRUE) {
    category <- category |> c("upon request")
  }
  # if(DAS == TRUE) {
  #   category <- category |> c("data availability statement")
  # }
  if(data_journal == TRUE) {
    category <- category |> c("data journal")
  }
  category <- category |> paste(collapse = ", ")

  return(category)
}



#---------------------------------------------------------------------
# 5 - Combine search steps to obtain Open Data status &
#     detected sentences
#---------------------------------------------------------------------


#' @noRd
.open_data_detection <- function(PDF_text_sentences, keyword_results)
{
  #one part of the keyword search acts on the tokenized sentences while another part acts on the full text
  keyword_results_tokenized <- .keyword_search_tokenized(keyword_results)
  keyword_results_near_wd <- .keyword_search_near_wd(PDF_text_sentences)
  data_journal_doi <- .check_journal_doi(PDF_text_sentences)

  keyword_results_combined <- cbind(keyword_results_tokenized,
                                    keyword_results_near_wd,
                                    data_journal_doi) |>
    dplyr::as_tibble()

  #check if any of the combined columns was positive to determine if the publication has Open Data or Open Code
  open_data_publication <- keyword_results_combined |>
    dplyr::mutate(is_open_data = com_specific_repo | com_general_repo |
                    com_file_formats | com_github_data | dataset | is_data_journal) |>
    dplyr::mutate(is_open_code = com_code | com_suppl_code) |>
    dplyr::mutate(is_supplement = dataset | com_file_formats | com_supplemental_data) |>
    dplyr::mutate(is_general_purpose = com_general_repo | com_github_data ) |>
    dplyr::mutate(is_reuse = com_reuse) |>
    dplyr::mutate(is_request = com_request) |>
    dplyr::mutate(open_data_category = furrr::future_pmap_chr(list(com_specific_repo, is_general_purpose,
                                                                   is_supplement, is_reuse, is_request,
                                                                   is_data_journal), .OD_category)) |>
    tibble::add_column(article = names(PDF_text_sentences)) |>
    dplyr::select(article, is_open_data, open_data_category, is_reuse, is_open_code)

  return(open_data_publication)
}


#' @noRd
.open_data_sentences <- function(PDF_text_sentences, keyword_results)
{
  keyword_list <- .create_keyword_list()
  #add simple TRUE/FALSE for the categories where the whole text is searched for nearby words
  keyword_results_near_wd <- .keyword_search_near_wd(PDF_text_sentences, extract_text = TRUE)

  DAS_sentences <- PDF_text_sentences |>
    furrr::future_map_chr(.f = \(sentences) dplyr::if_else(!any(.has_DAS(sentences, keyword_list)),
                                            NA,
                                            sentences |>
                                              paste(collapse = " ") |>
                                              unlist()))

  #identifies the text fragments in which the Open Data keywords were detected
  open_data_sentences <- furrr::future_map(keyword_results, .text_fragments)
  open_data_sentences <- do.call(rbind, open_data_sentences)
  open_data_sentences <- cbind(names(keyword_results), open_data_sentences,
                               keyword_results_near_wd, DAS_sentences) |>
    dplyr::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  colnames(open_data_sentences) <- c("article", "com_specific_repo", "com_general_repo",
                                     "com_github_data", "dataset", "com_code", "com_suppl_code",
                                     "com_reuse",
                                     "com_request",
                                     "com_file_formats", "com_supplemental_data", "com_data_availability")
  open_data_sentences[is.na(open_data_sentences)] = "" #unify empty fields

  #collapse the found statements into one column for Open Data and one for Open Code
  open_data_sentences <- open_data_sentences |>
    dplyr::mutate(open_data_statements =
                    paste(com_specific_repo, com_general_repo,
                          com_github_data, dataset, com_file_formats,
                          com_supplemental_data, com_reuse, sep = " ") |>
                    trimws()
                  ) |>
    dplyr::mutate(open_data_statements = dplyr::if_else(com_data_availability == "",
                                                open_data_statements,
                                                com_data_availability)) |>
    dplyr::mutate(open_code_statements =
                    paste(com_code, com_suppl_code, sep = " ") |>
                    trimws()
                  ) |>
    dplyr::select(article, open_data_statements, open_code_statements)


  return(open_data_sentences)
}


