
# Open data identification keywords
#--------------------------------------------------------------------------------------

#' Search for several categories of similar keywords in a sentence.
#' Multiple categories must match to trigger a detection.
#' @noRd
.create_keyword_list <- function()
{
  keyword_list <- list()

  available <- c("(?<!which )included(?! (\\d|only))", # research this
                 "deposited",
                 "archived",
                 "released",
                 "(is|are) provided",
                 "(is|are) hosted",
                 "contained in",
                 "contains",
                 "listed in",
                 "lodged with",
                 "assign(ed)? a doi",
                 "(?<!the )available",
                 "doi of the data(set)? (is|are)",
                 "(with regards to|for) data availability.*see:?",
                 "reproduce",
                 "accessible",
                 "(can|may) be (freely )?accessed",
                 "(can|may) be retrieved",
                 "submitted",
                 "(can|may) be downloaded",
                 "(can|may) be .*found",
                 "held in",
                 "reported in",
                 "uploaded",
                 "are public on",
                 "we (have )?added",
                 "(?<=data) at",
                 "(?<!\\d(-|\\))?(\\.|;) )doi: ") |> # exclude dois in references
    .format_keyword_vector()
  keyword_list[["available"]] <- available

  was_available <- c("(was|were) provided",
                     "(was|were )?previously published",
                     "(was|were|have been )?published previously",
                     "(was|were) included in",
                     "(was|were) contained in",
                     "(was|were|(?<!((has|have) been)|are (now)?) made) available(?! for)",
                     "(was|were) accessible",
                     "(was|were) deposited by",
                     "(has (been )?)?previously (been )?deposited",
                     "(was|were) reproduced",
                     "(was|were) open source data( ?sets?)?",
                     "(previous|prior) study",
                     "made their .* available",
                     "from( a)? publicly available data( ?sets?)?",
                     "(were|was) (used|analy[z,s]ed)",
                     "(were|was) downloaded( and analy[z,s]ed)?",
                     "(were|was) derived from",
                     "this study used .* publicly available",
                     "(existing|made use of) publicly available",
                     "done using( the)? publicly available",
                     "publicly available data(sets)? were analy(z|s)ed",
                     "used .* publicly accessible",
                     "data ?set used previously",
                     "data .*reanaly[z,s]ed",
                     "used data from a public",
                     "pre-existing",
                     "\\boriginal data accession",
                     "package was used to",
                     "using .* functions? in .* package",
                     "all data we used are public",
                     "covers public",
                     "(machine learning )?frameworks? used",
                     "available in .* previous",
                     "(?<!(approvals?|permissions?|permit|consent) )(we|was|were) obtained(?! (with|using))",
                     "(?<!code )we used .* data",
                     "checked .* on .* freely available") |>
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
                     "when available",
                     "not accessible",
                     "not submitted",
                     "did not .* generate",
                     "no .* (were|was) generated",
                     "(few|no|not enough) \\w.* (is|are) available",
                     "includes no",
                     "does not report original",
                     "(N|n)ot applicable"
                     ) |>
    .format_keyword_vector()
  keyword_list[["not_available"]] <- not_available
# str_detect("no custom codes were generated", not_available)
  # str_detect(publ_sentences, accession_nr[1])field_specific_repo
 # stringr::str_detect("http://www.ncbi. nlm.nih.gov/ pubmed", "ncbi(?!\\. ?nlm\\. ?nih\\. ?gov/ ?pubmed)")
  # field_specific_repo |> sort() |> paste(collapse = ",\n")
  field_specific_repo <- c(
    "10xgenomics",
    "23andme",
    "4D Nucleome Consortium",
    "accession codes*",
    "accession numbers*",
    "ACDC",
    "Addgene",
    "ADHD *200",
    "AIBL",
    "Aphasia Resource",
    "aphasiatrials\\.org",
    "Array[ ,-]*Express",
    "BBMRI.nl",
    "bbmri.nl",
    "biocollections",
    "Biological General Repository for Interaction Datasets",
    "Biological Magnetic Resonance Data Bank",
    "BioProject",
    "BioSample",
    "BMRB",
    "BONARES",
    "Broad Institute",
    "Cambridge Crystallographic Data Centre",
    "caNanoLab",
    "Cancer Imaging Archive",
    "cancergenome\\. ?nih\\. ?gov",
    "CATS",
    "CCDC",
    "CCMS",
    "cdc\\. ?gov",
    "Cell Image Library",
    "ChEMBL",
    "civicdb",
    # "clinicaltrials.gov", does not cover our data criteria
    # "ClinVar", does not cover our data criteria
    "Coherent X-ray Imaging Data Bank",
    "Computational Chemistry Datasets",
    "ConnectomeDB",
    "Conserved domains",
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
    "Gin",
    "Global Biodiversity Information Facility",
    "gmgc",
    "GPCRMD",
    "GWAS(?! summary)",
    "HIV Data Archive Program",
    "Human Connectome Project",
    "hPSCreg",
    "Image Data Resource",
    "ImmPort",
    "Influenza Research Database",
    "INDI",
    "IntAct",
    "Integrated Taxonomic Information System",
    "IoChem-BD",
    "ITIS",
    "Japanese Genotype-phenotype Archive",
    "JCB DataViewer",
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
    "ncbi(?!\\. ?nlm\\. ?nih\\. ?gov/ ?(pubmed|pmc|clinvar|books))", # exclude some repositories
    "NDAR",
    "NDCT",
    "NDEx",
    "Neuroimaging Informatics Tools and Resources Collaboratory",
    "NeuroMorpho",
    "NeuroVault",
    "NIMH Data Archive",
    "NIST",
    "NITRC",
    "OAR[-,\\.]PTB",
    "Open *Agrar",
    "OpenNeuro",
    "PCDDB",
    "PDB",
    "PeptideAtlas",
    "pdgenetics",
    "PGS Catalog",
    "PhysioNet",
    "PRIDE",
    "Protein Circular Dichroism Data Bank",
    "Protein Data *Bank",
    "proteindiffraction.org",
    "Proteome ?e?Xchange",
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
                    "(?<!(\\.|<))[:digit:]{1}[a-z]{1}[[:alnum:]]{2}",
                    "MTBLS[[:digit:]]{2,}",
                    "10.17590",
                    "10.5073",
                    "10.25493",
                    "10.6073",
                    "10.15468",
                    "10.5063",
                    "10.12751", #GIN
                    "fcon_ ?1000\\.projects\\.nitrc\\.org", # URL for INDI
                    "()[[:digit:]]{6}",
                    "[A-Z]{2,3}_[:digit:]{5,}",
                    "[A-Z]{2,3}-[:digit:]{4,}",
                    "[A-Z]{2}[:digit:]{5}-[A-Z]{1}",
                    "DIP:[:digit:]{3}",
                    "FR-FCM-[[:alnum:]]{4}",
                    "Collections?(:|\\/)[:digit:]{4}", # Neurovault
                    "ICPSR [:digit:]{4}",
                    "SN [:digit:]{4}",
                    "key resources table",
                    "accession numbers") |>
    .format_keyword_vector()
  keyword_list[["accession_nr"]] <- accession_nr
# publ_sentences <- tolower("Complete tandem mass tag proteomics data as uploaded to PRIDE [accession N◦.: PXD014572] included 93 significantly enriched immunomodulatory and angiogenic proteins.")
  # string_sup <- "<section> data availability all data are available in the supplementary tables (tables s1–s3)."
  # str_detect("<section> data availability all data are available in the supplementary tables (table 1 3)", "data(?! (availability|accessibility)).* (is|are) available *(\\w+\\W+){0,3}(\\(|\\[)?.*\\d{1,4}(\\)|\\])+")
  # str_detect("all data are available in the supplementary tables (s1–s3)", "data(?! (availability|accessibility)).* (is|are) available *(\\(|\\[)?[^=]*\\d{1,4}(\\)|\\])+")

  repositories <- c("data(?! (availability|accessibility)).* available online *(\\(|\\[)?.*\\d{1,4}(\\)|\\])+",
                    "open data repository (\\(|\\[)\\d{1,4}",
                    "data(?! (availability|accessibility)).* (is|are) available *(\\(|\\[)+[^=s]*\\d{1,4}(\\)|\\])+",
                    "data(?! (availability|accessibility)).* (is|are) available .*\\d{4}\\)",
                    "10\\.17617/3.", # Edmond
                    "10\\.17632", # Mendeley Data
                    "10\\.18452", # edoc HU
                    "10\\.13130", # UNIMI Dataverse
                    "10\\.1038 */s41597-", # scientific data
                    "10\\.11588", # hei DATA
                    # "doi\\.org/[[:graph:]]* (?!received)",
                    "code ocean ?(capsule)?.{2,}", # at least two symbols before end of string
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
                    "mediatum",
                    "mendeley data",
                    "OpenNeuro",
                    "open science framework",
                    "osf(?! group)",
                    "purl\\.stanford",
                    "Stanford Digital Repository",
                    "tu datalib",
                    "UNIMI",
                    "zivahub",
                    "zenodo"
                    ) |>
    .format_keyword_vector()
  keyword_list[["repositories"]] <- repositories

  github <- c("github") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["github"]] <- github


  data <- c("data(?! (availability|accessibility))",
            "datasets?",
            # "databases?",
            "annotations",
            "sequences",
            "responses",
            "materials") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["data"]] <- data


  all_data <- c("all data",
                "all array data",
                "raw data",
                "source data",
                "full data set",
                "full dataset",
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

  source_code <- c("source codes?",
                   "code files?",
                   "analysis (script|codes?\\w)",
                   "data (and )?codes?(?! availabili)",
                   "github",
                   "gitlab",
                   "bitbucket",
                   "preprocessing software",
                   "code ocean",
                   "the (script|codes?) for",
                   "code for .* (analys|model)",
                   "(?<!icd )codes? used for",
                   "SAS scripts?",
                   "SPSS scripts?",
                   "r[-, ]+script",
                   "r[-, ]+codes?",
                   "(?<!(using|via|through)( the)? )r[-, ]+package",
                   "python script",
                   "python codes?",
                   # "software",
                   "matlab script",
                   "matlab codes?",
                   "macros?\\s(?!(group|level))") |>
    .format_keyword_vector()
  keyword_list[["source_code"]] <- source_code

# stringr::str_detect("analysis code-fh", "analysis (script|codes?\\w)")
# str_detect("my.email@haha.com", weblink)
  weblink <- "(((https?|ftp|smtp):\\/\\/)|(www\\.?))[a-z0-9]+\\.[a-z ]+(\\/[a-zA-Z0-9#]+\\/?)*"

  citation <- "\\(.*\\d{4}\\)|\\[\\d{1,3}\\]|cited as reference"
  grant <- c("grant",
             "funding",
             "support (was provided )?by") |>
    .format_keyword_vector()

  keyword_list[["grant"]] <- grant

  keyword_list[["weblink"]] <- weblink

  reuse <- .near_wd(was_available,
                    paste(
                      accession_nr,
                      field_specific_repo,
                      repositories,
                      weblink,
                      citation,
                      github,
                      # "(here|in (the present|this) study)",
                      sep = "|"),
                    dist = 30)
  keyword_list[["reuse"]] <- reuse

  supplement <- c("supporting information",
                  "supplement",
                  "supplementa(l|ry) data",
                  "supplementa(l|ry) material",
                  "supplementa(l|ry) information",
                  "in the additional file"
                  # "provided with this paper",
                  # "(with)?in th(is|e) article",
                  ) |>
    .format_keyword_vector()
  keyword_list[["supplement"]] <- supplement

  file_formats <- c("csv",
                    "zip",
                    "xls",
                    "xlsx",
                    "Excel (table|spreadsheet)",
                    "sav",
                    "cif",
                    "fasta") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["file_formats"]] <- file_formats

  upon_request <- c("(up)?on( reasonable)? request",
                    "by request",
                    "via a direct request to",
                    "without undue reservation",
                    "the corresponding author",
                    "the lead contact",
                    "the techincal contact",
                    "requests.* should be (directed|submitted) (to|through|via)",
                    "data requests?")|>
    .format_keyword_vector()
  keyword_list[["upon_request"]] <- upon_request

  data_availability <- c("Data and (Software|Resource) Availability",
                         "Software and Data Availability",
                         "Statistical Analysis and Data Availability",
                         "Data.sharing statement",
                         "Data.sharing",
                         "Data availability statement & Data deposition",
                         "D ?a ?t ?a A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y S ?t ?a ?t ?e ?m ?e ?n ?t",
                         "D ?a ?t ?a A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y P ?o ?l ?i ?c ?y",
                         "D ?a ?t ?a A ?v ?a ?i ?l ?a ?b ?i ?l ?i? ?t ?y",
                         "Data and code availability",
                         "Data and ma-*te-*ri-*als a-*vai-*la-*bi-*li-*ty",
                         "Information on author access to data",
                         "Data Materials and Software Availability",
                         "Data Transparency Statement",
                         "Data deposition",
                         "Deposited Data",
                         "Data Archiving",
                         "(Accessi|availa)bility of data and materials",
                         "Availability of data",
                         "Data Accessibility",
                         "Data Access",
                         "Accessibility of data and code",
                         "Accessibility of data",
                         "Code and data availability statement",
                         "Availability and implementation",
                         "Accession codes",
                         "Accession numbers sequence data",
                         "Open practices") |>
    .format_keyword_vector()
  keyword_list[["data_availability"]] <- data_availability

  code_availability <- c("Code sharing",
                         "Code availability statement",
                         "Code availability",
                         # "(Data and)? Software availability",
                         # "Data and code availability",
                         # "Data and materials availability",
                         # "Accessibility of data and code",
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
  supplemental_table_number <- c("S[[:digit:]]",
                                 "[[:digit:]]",
                                 "[A-Z]{2}[[:digit:]]")
  supplemental_table <- .outer_str(supplemental_table_name, supplemental_table_number) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["supplemental_table"]] <- supplemental_table

  supplemental_dataset <- c("supplementary data [[:digit:]]{1,2}",
                      "supplementary dataset [[:digit:]]{1,2}",
                      "supplementary data set [[:digit:]]{1,2}",
                      "supplemental data [[:digit:]]{1,2}",
                      "supplemental dataset [[:digit:]]{1,2}",
                      "supplemental data set [[:digit:]]{1,2}",
                      "supplemental data can be found online")
  dataset_name <- c("data", "dataset", "datasets", "data set", "data sets")
  dataset_number <- c("S[[:digit:]]{1,2}")
  dataset <- .outer_str(dataset_name, dataset_number)
  dataset <- c(dataset, supplemental_dataset) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["dataset"]] <- dataset

  protocol <- c("protocols?",
                "procedures can be found") |>
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
  #effect: all_data & file_format words are at most 12 words apart from each other
  all_data_file_formats <- .near_wd_sym(all_data, file_formats, dist = 12)
  keyword_list[["all_data_file_formats"]] <- all_data_file_formats


  supp_table_data <- .near_wd_sym(supplemental_table,
                                 paste(file_formats, all_data, sep = "|"),
                                 dist = 12)
  keyword_list[["supp_table_data"]] <- supp_table_data

  # data_in_DAS <- .near_wd(data_availability,
  #                                         paste(
  #                                           accession_nr, repositories,
  #                                           weblink,
  #                                           sep = "|"),
  #                                         dist = 30)
  # keyword_list[["data_in_DAS"]] <- data_in_DAS

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
# publ_sentences <- DAS

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
                                "reuse",
                                "grant",
                                "file_formats", "upon_request", "dataset", "protocol", "weblink")

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
# sections_v <- DAS
#' search plos DAS when spread over two pages
#' @noRd
.splice_plos_twopager <- function(sections_v) {

  # if (any(stringr::str_detect(sections_v, "<section> plos"))) {
    sections <- stringr::str_detect(sections_v, "^<section> ")

    if (sum(sections) == 1) return(sections_v)

    splice_start_piece <- which(sections)[2] - 1
    splice_end_piece <- sections[length(sections):1] |> which.max() # invert vector to find last occurrence of section
    splice_end_piece <- length(sections) - splice_end_piece + 1 # last occurence of section

    c(sections_v[1:splice_start_piece], sections_v[splice_end_piece:length(sections_v)])
    # plos_line <- which.max(stringr::str_detect(sections_v, "<section> plos"))

    # if (plos_line == length(sections_v)) {
      # return(sections_v[-length(sections_v)])
      # if plos line at the end of DAS there is no page break
    # } else if (plos_line == which(sections)[2] & stringr::str_detect(sections_v[plos_line - 1], "\\.$")) {
      # return(sections_v[1:(plos_line - 1)])
    # } else {
      # splice_start_piece <- which(sections)[2] - 1
      # splice_end_piece <- sections[length(sections):1] |> which.max() # invert vector to find last occurrence of section
      # splice_end_piece <- length(sections) - splice_end_piece + 1 # last occurence of section
      # return(c(sections_v[1:splice_start_piece], sections_v[splice_end_piece:length(sections_v)]))
    # }

  # } else {
  #   return(sections_v)
  # }

}
# sentence <- PDF_text_sentences[927]
#' test if text contains data availability statement
#' @noRd
.has_DAS <- function(sentence, keyword_list) {

  data_availability <- keyword_list[["data_availability"]]

  data_availability <- paste0("(<section>)\\W+[\\d,\\W]*(", data_availability, ")\\b")
  stringr::str_detect(sentence, data_availability)
}

# sentence <- "<section> data availability"
# .has_DAS(sentence)

#' extract data availability statement
#' @noRd
.extract_DAS <- function(PDF_text_sentences) {

  keyword_list <- .create_keyword_list()

  data_availability <- keyword_list[["data_availability"]]


  DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
                      \(sentence) stringr::str_detect(sentence, data_availability))

  if (sum(DAS_detections) > 0) {
    DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
                                            \(sentence) .has_DAS(sentence, keyword_list))
  }

  DAS_start <- which(DAS_detections)

  if (length(DAS_start) == 2) {
    # select detection with statement
    statement_detections <- PDF_text_sentences[DAS_start] |>
      stringr::str_detect("statement")
    # select detection without "data
    quotation_detections <- !stringr::str_detect(PDF_text_sentences[DAS_start], '\\"data')
    if (sum(quotation_detections) == 1) {
      DAS_start <- DAS_start[quotation_detections] # select detection without "data
    } else if (sum(statement_detections) == 1) {
      DAS_start <- DAS_start[statement_detections] # select detection with statement
    } else {
      DAS_start <- min(DAS_start)
    }

    # if more than two detections of DAS were made, then return full document
  } else if (length(DAS_start) != 1) {
    return(PDF_text_sentences)
  }


  str_DAS <- PDF_text_sentences[DAS_start] |>
    stringr::str_trim()
  str_DAS_sameline <- str_DAS |>
    stringr::str_remove(data_availability) |>
    stringr::str_remove("<section> ")

  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  DAS_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + 1):length(PDF_text_sentences)],
                                         \(sentence) stringr::str_detect(sentence, "(section|insert|iend)> (?!d )|^\\d\\.")) |>
    which() - 1

  # if (length(PDF_text_sentences) - DAS_start <= 2) return(PDF_text_sentences[DAS_start:length(PDF_text_sentences)])
  if (
    length(DAS_end_candidates) == 0 |
    (length(DAS_end_candidates) == 1 & DAS_end_candidates[1] == 0) |
    purrr::is_empty(DAS_end_candidates)
      ) return(PDF_text_sentences[DAS_start:length(PDF_text_sentences)])

  # check if candidates are full sentences ending in full stop. This achieves splicing if section continues on next page
  completed_sentences <- furrr::future_map_lgl(PDF_text_sentences[DAS_start + DAS_end_candidates],
                                               \(sentence) stringr::str_detect(sentence, "\\..?$"))

  if (stringr::str_length(str_DAS_sameline) < 5 & str_DAS_sameline != "." ) {
    # first_sentence <- DAS_start + 1

    DAS_end <- DAS_end_candidates[-1][completed_sentences[-1]][1]#



  } else {
    DAS_end <- DAS_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section

    if (DAS_start / length(DAS_detections) < 0.1 & DAS_end != 0) { # for plos journals with DAS on first page

      DAS_second_part <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + DAS_end + 1):length(PDF_text_sentences)],
                                               \(sentence) stringr::str_detect(sentence, "<section> funding:"))

      if (sum(DAS_second_part) == 0) {
        DAS_end <- DAS_end
        } else {
          DAS_end <- DAS_end + which(DAS_second_part) - 1
        }
    }
  }

  if (is.na(DAS_end)) {

    if (!any(completed_sentences) | is.na(completed_sentences)[1]) {
      DAS_end <- length(PDF_text_sentences) - DAS_start
    } else {
      DAS_end <- min(DAS_end_candidates[DAS_end_candidates > 0], length(PDF_text_sentences) - DAS_start)
    }

  }

  if (DAS_start + DAS_end > length(PDF_text_sentences)) {
    DAS_end <- DAS_start
  }

  DAS_end <- DAS_start + DAS_end

  DAS <- PDF_text_sentences[DAS_start:DAS_end]

  if (DAS_start < 50 & any(stringr::str_detect(PDF_text_sentences[1:10], "plos"))) {
    DAS <- .splice_plos_twopager(DAS)
  }
   DAS |>
     paste(collapse = " ") |>
     stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
     stringr::str_trim() |>
     # in some cases the references or other sections interpolate between DAS title and DAS text
     stringr::str_remove("<section> references.*") |> # remove references
     stringr::str_remove("<section> authors.*") |> # remove author contributions
     stringr::str_remove("<section> additional contr.*") |> # remove author contributions
     stringr::str_remove_all(" <section>") |>
     stringr::str_replace("(?<=repository)\\. ", ": ") |>  # for the weird cases when after repository a . and not : follows
     stringr::str_replace("(?<=were analy(z|s)ed in this study)\\.", ":") |>  # for the standard phrasing of data re-use
     tokenizers::tokenize_regex(pattern = "(?<=\\.) ", simplify = TRUE) |> # tokenize sentences
     .correct_tokenization()

}


#' extract code availability statement
#' @noRd
.extract_CAS <- function(PDF_text_sentences) {

  keyword_list <- .create_keyword_list()

  code_availability <- keyword_list[["code_availability"]]

  CAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
                                          \(sentence) stringr::str_detect(sentence, code_availability))

  CAS_start <- which(CAS_detections)

  if (length(CAS_start) == 2) {
    CAS_start <- max(CAS_start)
  } else if (length(CAS_start) != 1 ) {
    return("")
  }

  str_CAS <- PDF_text_sentences[CAS_start] |>
    stringr::str_trim()
  str_CAS_sameline <- str_CAS |>
    stringr::str_remove(code_availability) |>
    stringr::str_remove("<section> ")


  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  CAS_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(CAS_start + 1):length(PDF_text_sentences)],
                                              \(sentence) stringr::str_detect(sentence, "section> (?!d )|^\\d\\.")) |>
    which() - 1
  # check if candidates are full sentences ending in full stop. This achieves splicing if section contines on next page
  completed_sentences <- furrr::future_map_lgl(PDF_text_sentences[CAS_start + CAS_end_candidates],
                                               \(sentence) stringr::str_detect(sentence, "\\.$"))

  if (stringr::str_length(str_CAS_sameline) < 5) {
    # first_sentence <- DAS_start + 1

    CAS_end <- CAS_end_candidates[-1][completed_sentences[-1]][1]#

  } else {
    CAS_end <- CAS_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section
  }

  if (is.na(CAS_end)) {
    CAS_end <- min(CAS_end_candidates)

    if (!any(completed_sentences) | is.na(completed_sentences)[1]) {
      CAS_end <- length(PDF_text_sentences) - CAS_start
    } else {
      CAS_end <- min(CAS_end_candidates[CAS_end_candidates > 0], length(PDF_text_sentences) - CAS_start)
    }
  }

  if (CAS_start + CAS_end > length(PDF_text_sentences)) {
    CAS_end <- length(PDF_text_sentences) - CAS_start
  }

  CAS_end <- CAS_start + CAS_end

  PDF_text_sentences[CAS_start:CAS_end] |>
    # .splice_plos_twopager() |>
    paste(collapse = " ") |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim() |>
    # in some cases the references interpolate between CAS title and DAS text
    stringr::str_remove("<section> references.*") |> # remove references
    stringr::str_remove("funding.*") |> # remove funding
    tokenizers::tokenize_regex(pattern = "(?<=\\.) ", simplify = TRUE) |> # tokenize sentences
    .correct_tokenization()

}

#'
#' @noRd
.remove_references <- function(PDF_text_sentences)
{
  line_before_refs <- PDF_text_sentences |>
    stringr::str_detect("<section> r ?e ?f ?e ?r ?e ?n ?c ?e ?s(?! and notes)") |>
    which() - 1

  if (length(line_before_refs) > 1) line_before_refs <- line_before_refs[length(line_before_refs)]

  if (sum(line_before_refs) == 0) {
    # if no references detected
    return(PDF_text_sentences)
  }

  # for journals that print useful information after the references (Elsevier, Science, etc.)
  line_after_ref <- suppressWarnings(
    PDF_text_sentences |>
      stringr::str_detect("<section> (star\\+methods|acknowledgments:|open data)") |>
      which() |>
      max()
  )

  if (sum(line_after_ref) <= 0) line_after_ref <- 0

  # excise references for special case Elsevier journals
  if (line_after_ref > line_before_refs) {
    return(c(PDF_text_sentences[1:line_before_refs],
             PDF_text_sentences[line_before_refs:length(PDF_text_sentences)]))
  }
  # excise references for most journals
  PDF_text_sentences[1:line_before_refs]

}
# kw <- keyword_results_combined[[1]]
#'
#' @noRd
.keyword_search_full <- function(PDF_text_sentences)
{
# odc <- open_data_categories[[1]]
  # PDF_text_sentences <- publ_sentences
  # search for open data keywords in the full texts
  open_data_categories <- furrr::future_map(PDF_text_sentences, .map_keywords, .progress = TRUE)

  # combine columns for the different open data keywords
  keyword_results_combined <- open_data_categories  |>
    purrr::map(dplyr::mutate, com_specific_repo =
                 field_specific_repo &
                 (accession_nr | weblink) & available & !not_available & !was_available &
                 (!protocol & !supplement | data) & !grant
               )|>
    purrr::map(dplyr::mutate, com_general_repo = repositories & available &
                 !not_available & !was_available & (!protocol & !supplement | data)) |>
    purrr::map(dplyr::mutate, com_github_data = data & github & available &
                 !not_available & !was_available) |>
    purrr::map(dplyr::mutate, com_code = source_code & available &
                 !not_available & !was_available & (!upon_request|stringr::str_detect(publ_sentences, "git|www|http"))) |>
    purrr::map(dplyr::mutate, com_suppl_code = supplement & source_code) |>
    purrr::map(dplyr::mutate, com_reuse = reuse & !grant) |>
    purrr::map(dplyr::mutate, com_request = upon_request) |>
    purrr::map(dplyr::mutate, com_n_weblinks = stringr::str_count(publ_sentences, "www|http")) |>
    purrr::map(dplyr::mutate, com_unknown_source = dplyr::case_when(
      com_n_weblinks > 1 ~ data & available & weblink &
        !not_available & !accession_nr & !com_general_repo, # maybe exclude supplements here as well
      .default = data & available & weblink &
        !not_available & !accession_nr & !com_general_repo & !com_github_data & !supplement
    )) |>
    purrr::map(dplyr::select, publ_sentences, com_specific_repo, com_general_repo,
                com_github_data, dataset, com_code, com_suppl_code, com_reuse, com_request, com_unknown_source)

  return(keyword_results_combined)
}

#' keyword search on the tokenized sentences
#' @noRd
.keyword_search_tokenized <- function(keyword_results_combined)
{
  # summarize results over all sentences of each publication to see if any match was found for each keyword category
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

  # needs text input in two formats: split into sentences, and one long string for
  PDF_text_full <- PDF_text_sentences |>
    furrr::future_map_if(.p = \(sentence) length(sentence) > 1,
                         .f = \(sentence) paste(sentence, collapse = " "))

  # search for the last combination in the non-tokenized text and add it to the results table for the publications
  # either give out TRUE/FALSE or return detected string
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
                         github,
                         unknown_source,
                         data_journal
                         )
{
  category = vector()

  # should give out several categories
  # if there are multipe matches
  if (specific_repo == TRUE) {
    category <- category |> c("field-specific repository")
  }
  if (general_repo == TRUE) {
    category <- category |> c("general-purpose repository")
  }
  if (suppl == TRUE) {
    category <- category |> c("supplement")
  }
  if (reuse == TRUE) {
    category <- category |> c("re-use")
  }
  if (request == TRUE) {
    category <- category |> c("upon request")
  }
  if (github == TRUE) {
    category <- category |> c("github")
  }
  if (unknown_source == TRUE) {
    category <- category |> c("unknown url")
  }
  if (data_journal == TRUE) {
    category <- category |> c("data journal")
  }
  category <- category |> paste(collapse = ", ")

  return(category)
}

#' check if data availability statement (das) contains a url that is not a git url
#' @noRd
.has_url <- function(das){
  stringr::str_detect(das, "(((https?|ftp|smtp):\\/\\/(?! ?git)|(www\\.?))[a-z0-9]+\\.[a-z ]+(\\/[a-z0-9#]+\\/?)*)|(?<!git(hub|lab)?)\\.(com|org)")
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
                    is_data_journal,
                  is_open_code = com_code | com_suppl_code,
                  is_supplement = dataset | com_file_formats | com_supplemental_data,
                  is_general_purpose = com_general_repo,
                  is_reuse = com_reuse,
                  open_data_category = furrr::future_pmap_chr(list(com_specific_repo, is_general_purpose,
                                                                   is_supplement, is_reuse, com_request,
                                                                   com_github_data,
                                                                   com_unknown_source,
                                                                   is_data_journal), .OD_category)) |>
    tibble::add_column(article = names(PDF_text_sentences)) |>
    dplyr::select(article, is_open_data, open_data_category, is_reuse, is_open_code)

  return(open_data_publication)
}


#' @noRd
.open_data_sentences <- function(PDF_text_sentences, DAS_sentences, sentences_with_DAS, CAS_sentences, keyword_results)
{
  keyword_list <- .create_keyword_list()
  #add simple TRUE/FALSE for the categories where the whole text is searched for nearby words
  keyword_results_near_wd <- .keyword_search_near_wd(PDF_text_sentences, extract_text = TRUE)

  DAS_sentences <- DAS_sentences |>
    furrr::future_imap_chr(.f = \(x, idx) dplyr::case_when(
      # !any(.has_DAS(x, keyword_list)) ~ "",
      # length(sentences) > 30 ~ "multiple DAS mentions",
      !idx %in% names(sentences_with_DAS) ~ "",
      .default =  x |>
        paste(collapse = " ") |>
        unlist()))

  CAS_sentences <- CAS_sentences |>
    furrr::future_map_chr(\(x) paste(x, collapse = " ") |>  unlist())
  # DAS_sentences <- CAS_sentences <- ""
# tib <- tibble::tibble(text = DAS_sentences)
  #identifies the text fragments in which the Open Data keywords were detected
  open_data_sentences <- furrr::future_map(keyword_results, .text_fragments)
  open_data_sentences <- do.call(rbind, open_data_sentences)
  open_data_sentences <- cbind(names(keyword_results), open_data_sentences,
                               keyword_results_near_wd, DAS_sentences, CAS_sentences) |>
    dplyr::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  colnames(open_data_sentences) <- c("article", "com_specific_repo", "com_general_repo",
                                     "com_github_data", "dataset", "com_code", "com_suppl_code",
                                     "com_reuse",
                                     "com_request",
                                     "com_unknown_source",
                                     "com_file_formats", "com_supplemental_data", "das", "cas")
  open_data_sentences[is.na(open_data_sentences)] = "" #unify empty fields

  #collapse the found statements into one column for Open Data and one for Open Code
  open_data_sentences <- open_data_sentences |>
    dplyr::mutate(
      open_data_statements =
        paste(com_specific_repo, com_general_repo,
              com_github_data, dataset, com_file_formats,
              com_supplemental_data, com_reuse, com_unknown_source, sep = " ") |>
        trimws()
                  ) |>
    # copy over das to cas if das is actually also a cas
    dplyr::mutate(
      cas = ifelse(stringr::str_detect(stringr::str_sub(das, 1, 30), "(?<!accession) code|software|materials"), das, cas),
      open_code_statements =
        paste(com_code, com_suppl_code, sep = " ") |>
        trimws()
      ) |>
    dplyr::select(article, das, open_data_statements, cas, open_code_statements)



  return(open_data_sentences)
}
# categories <- "re-use"
# Remove repeated categories listed in a string separated by commas
#' @noRd
.remove_repeats <- function(categories) {
  strsplit(categories, ", ") |>
    unlist() |>
    unique() |>
    paste0(collapse = ", ")
}
