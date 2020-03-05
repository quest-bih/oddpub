
# set of utility functions used in the Open Data Detection

#---------------------------------------------------------------------
# 1 - PDF conversion
#---------------------------------------------------------------------

#converts PDF file to txt file and saves it to output_folder
#requires the pdftotext program that is run via a terminal command
.pdf_to_text <- function(PDF_filename, output_folder, overwriteExistingFiles = FALSE)
{
  success <- FALSE
  output_filename <- .create_output_filename(PDF_filename, output_folder)

  if(!file.exists(PDF_filename)) {
    print("PDF file does not exist!")
  } else if(file.exists(output_filename) && overwriteExistingFiles == FALSE) {
    print("Output file already exists!")
    success <- TRUE
  } else  {
    tryCatch({
      com <- paste0('pdftotext ', '\"', PDF_filename, '\" ',
                    '\"', output_filename, '\"')
      system(com, wait = T)
      success <- TRUE
    }, error=function(e){
      print("Could not convert pdf to text.")
    })
  }

  names(success) <- PDF_filename
  return(success)
}


.create_output_filename <- function(PDF_filename, output_folder)
{
  output_filename <- sub('.pdf', '.txt', PDF_filename)
  output_filename <- tail(strsplit(output_filename, "/")[[1]],1)
  output_filename <- paste0(output_folder, output_filename)

  return(output_filename)
}


#--------------------------------------------------------------------------------------
# 2 - text data loading
#--------------------------------------------------------------------------------------

#function that search for sentences that were falsely split on abbreviations like accession nr.
#and pastes them together again
.correct_tokenization <- function(PDF_text)
{
  PDF_text_corrected <- PDF_text
  sentence_paste_idx <- PDF_text %>%
    stringr::str_sub(-13, -1) %>%
    stringr::str_detect("accession nr.|accession no.|ccession nos.|ccession nrs.") %>%
    which()

  #for all indicies do a pairwise pasting
  if(length(sentence_paste_idx) > 0)
  {
    for(i in 1:length(sentence_paste_idx))
    {
      PDF_text_corrected <- .paste_idx(PDF_text_corrected, sentence_paste_idx[i]-(i-1))
    }
  }

  return(PDF_text_corrected)
}

#helper function for .correct_tokenization
#pastes together sentences where tokenization needs to be corrected by index
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


#--------------------------------------------------------------------------------------
# 3 - Open data identification keywords
#--------------------------------------------------------------------------------------

#Several categories of similar keywords are searched for in a sentence.
#Multiple categories have to match to trigger a detection.
.create_keyword_list <- function()
{
  keyword_list <- list()

  available <- c("included",
                 "deposited",
                 "released",
                 "is provided",
                 "are provided",
                 "contained in",
                 "available",
                 "reproduce",
                 "accessible",
                 "can be accessed",
                 "submitted",
                 "can be downloaded",
                 "reported in",
                 "uploaded",
                 "are public on") %>%
    .format_keyword_vector()
  keyword_list[["available"]] <- available


  was_available <- c("was provided",
                     "were provided",
                     "was contained in",
                     "were contained in",
                     "was available",
                     "were available",
                     "was accessible",
                     "were accessible",
                     "deposited by",
                     "were reproduced") %>%
    .format_keyword_vector()
  keyword_list[["was_available"]] <- was_available


  not_available <- c("not included",
                     "not deposited",
                     "not released",
                     "not provided",
                     "not contained in",
                     "not available",
                     "not accessible",
                     "not submitted") %>%
    .format_keyword_vector()
  keyword_list[["not_available"]] <- not_available


  field_specific_db <- c("GEO",
               "Gene Expression Omnibus",
               "European Nucleotide Archive",
               "National Center for Biotechnology Information",
               "European Molecular Biology Laboratory",
               "EMBL-EBI",
               "BioProject",
               "Sequence Read Archive",
               "SRA",
               "ENA",
               "MassIVE",
               "ProteomeXchange",
               "Proteome Exchange",
               "ProteomeExchange",
               "MetaboLights",
               "Array-Express",
               "ArrayExpress",
               "Array Express",
               "PRIDE",
               "DNA Data Bank of Japan",
               "DDBJ",
               "Genbank",
               "Protein Databank",
               "Protein Data Bank",
               "PDB",
               "Metagenomics Rapid Annotation using Subsystem Technology",
               "MG-RAST",
               "metabolights",
               "OpenAgrar",
               "Open Agrar",
               "Electron microscopy data bank",
               "emdb",
               "Cambridge Crystallographic Data Centre",
               "CCDC",
               "Treebase",
               "dbSNP",
               "dbGaP",
               "IntAct",
               "ClinVar",
               "accession number",
               "accession code",
               "accession numbers",
               "accession codes") %>%
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["field_specific_db"]] <- field_specific_db



  accession_nr <- c("GSE[[:digit:]]{2,8}", #GEO
                    "GSM[[:digit:]]{2,8}",
                    "GDS[[:digit:]]{2,8}",
                    "GPL[[:digit:]]{2,8}",
                    "(E|D|S)RP[:digit:]{6,}",
                    "PRJ(E|D|N|EB|DB|NB)[:digit:]+",
                    "ERS[:digit:]{6,}",
                    "SAM(E|D|N)[A-Z]?[:digit:]+",
                    "[A-Z]{1}[:digit:]{5}\\.[:digit:]+",
                    "[A-Z]{2}[:digit:]{6}\\.[:digit:]+",
                    "[A-Z]{4}S?[:digit:]{8,9}\\.[:digit:]+",
                    "[A-Z]{3}[:digit:]{5}\\.[:digit:]+",
                    "[A-Z]{5}[:digit:]{7}\\.[:digit:]+",
                    "[A-Z]{1}[:digit:]{5}", #GenBank
                    "[A-Z]{2}[:digit:]{6}",
                    "[A-Z]{3}[:digit:]{5}",
                    "[A-Z]{4}[:digit:]{8}",
                    "[A-Z]{5}[:digit:]{7}",
                    "[A-Z]{6}[:digit:]{9}",
                    "(E|D|S)RX[:digit:]{6,}",
                    "(E|D|S)RR[:digit:]{6,}",
                    "GCA_[:digit:]{9}\\.[:digit:]+",
                    "PRJNA[[:digit:]]{3,8}",
                    "SRP[[:digit:]]{3,8}",
                    "SRR[[:digit:]]{3,8}",
                    "SRX[[:digit:]]{3,8}",
                    "SRS[[:digit:]]{3,8}",
                    "SRZ[[:digit:]]{3,8}",
                    "MSV[[:digit:]]{7,9}",
                    "PDX[[:digit:]]{5,6}",
                    "E-[A-Z]{4}-[:digit:]{1,6}",
                    "PXD[[:digit:]]{5,6}",
                    "PRD[[:digit:]]{5,6}",
                    "DRA[[:digit:]]{6}",
                    "ERA[[:digit:]]{6}",
                    "DRP[[:digit:]]{6}",
                    "ERP[[:digit:]]{6}",
                    "DRX[[:digit:]]{6}",
                    "DRS[[:digit:]]{6}",
                    "DRR[[:digit:]]{6}",
                    "[:digit:]{1}[A-Z]{1}[[:alnum:]]{2}",
                    "MTBLS[[:digit:]]{2,4}",
                    "10.17590",
                    "10.5073",
                    "EMD-[[:digit:]]{4,5}",
                    "[[:digit:]]{7}",
                    "[A-Z]{2}_[:digit:]{6,7}",
                    "[A-Z]{2}-[:digit:]{4,5}") %>%
    .format_keyword_vector()
  keyword_list[["accession_nr"]] <- accession_nr


  repositories <- c("figshare",
                    "dryad",
                    "zenodo",
                    "dataverse",
                    "DataverseNL",
                    "osf",
                    "open science framework",
                    "mendeley data",
                    "GIGADB",
                    "GigaScience database",
                    "OpenNeuro") %>%
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["repositories"]] <- repositories


  github <- c("github") %>%
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["github"]] <- github


  data <- c("data",
            "dataset",
            "datasets") %>%
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["data"]] <- data


  all_data <- c("all data",
                "all array data",
                "raw data",
                "full data set",
                "full dataset",
                "crystallographic data",
                "subject-level data") %>%
    .format_keyword_vector()
  keyword_list[["all_data"]] <- all_data


  not_data <- c("not all data",
                "not all array data",
                "no raw data",
                "no full data set",
                "no full dataset") %>%
    .format_keyword_vector()
  keyword_list[["not_data"]] <- not_data


  source_code <- c("source code",
                   "analysis script",
                   "github",
                   "SAS script",
                   "SPSS script",
                   "R script",
                   "R code",
                   "python script",
                   "python code",
                   "matlab script",
                   "matlab code") %>%
    .format_keyword_vector()
  keyword_list[["source_code"]] <- source_code


  supplement <- c("supporting information",
                  "supplement",
                  "supplementary data") %>%
    .format_keyword_vector()
  keyword_list[["supplement"]] <- supplement


  file_formats <- c("csv",
                    "zip",
                    "xls",
                    "xlsx",
                    "sav",
                    "cif",
                    "fasta") %>%
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["file_formats"]] <- file_formats


  upon_request <- c("upon request",
                    "on request",
                    "upon reasonable request") %>%
    .format_keyword_vector()
  keyword_list[["upon_request"]] <- upon_request


  data_availability <- c("Data sharing",
                        "Data Availability Statement",
                        "Data Availability",
                        "Data deposition",
                        "Deposited Data",
                        "Data Archiving",
                        "Availability of data and materials",
                        "Availability of data",
                        "Data Accessibility",
                        "Accessibility of data") %>%
    .format_keyword_vector()
  keyword_list[["data_availability"]] <- data_availability


  supplemental_table_name <- c("supplementary table",
                          "supplementary tables",
                          "supplemental table",
                          "supplemental tables",
                          "table", "tables",
                          "additional file",
                          "file", "files")
  supplemental_table_number <- c("S[[:digit:]]", "[[:digit:]]", "[A-Z]{2}[[:digit:]]")
  supplemental_table <- .outer_str(supplemental_table_name, supplemental_table_number) %>%
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
  dataset <- c(dataset, supplemental_dataset) %>%
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["dataset"]] <- dataset


  data_journal_dois <- c("10.1038/s41597-019-", "10.3390/data", "10.1016/j.dib") %>%
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


  data_availibility_statement <- .near_wd(data_availability,
                                         paste("doi", accession_nr, repositories, sep = "|"),
                                         dist = 30)
  keyword_list[["data_availibility_statement"]] <- data_availibility_statement

  return(keyword_list)
}


#standard formatting for the different keyword vectors
.format_keyword_vector <- function(keywords, end_boundary = FALSE) {
  #typically word boundaries are added in the beginning only to allow for different possible endings
  if(end_boundary) {
    keywords_formatted <- paste0("\\b", keywords, "\\b")
  } else {
    keywords_formatted <- paste0("\\b", keywords)
  }
  #collapse keywords into one string with OR symbol between them and convert to lowercase
  keywords_formatted <- paste(keywords_formatted, collapse = "|") %>% tolower()

  return(keywords_formatted)
}


#function that returns all the pasted combinations of two strings coming from different vectors
#used to make all possible text combinations of two related keyword categories
.outer_str <- function(x, y)
{
  outer_1 <- outer(x, y, FUN = "paste") %>% as.vector()
  outer_2 <- outer(y, x, FUN = "paste") %>% as.vector()
  outer_sym <- c(outer_1, outer_2)

  return(outer_sym)
}


#function that creates Regex that searches for cases where words x and y are at max dist words apart
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


#assymetric version where only the case with x before y is checked
.near_wd <- function(x, y, dist = 10)
{
  combined <- paste0("\\b(",
                     x,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     y,
                     ")\\b")

  return(combined)
}



#---------------------------------------------------------------------
# 4 - Open data identification
#---------------------------------------------------------------------

#identifies the text fragments in which the Open Data keywords were detected
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
      current_txt <- open_data_tibble[[1]][idx] %>% paste(collapse = ";     ")
      text_frag <- c(text_frag, current_txt)
    }
  }

  return(text_frag)
}


#function that applies all keyword category searches onto each sentence of the publications
.map_keywords <- function(publ_sentences)
{
  keyword_list <- .create_keyword_list()

  #not all keyword categories are used for the sentence search
  sentence_search_keywords <- c("available", "was_available", "not_available",
                                "field_specific_db", "accession_nr", "repositories",
                                "github", "data", "all_data",
                                "not_data", "source_code", "supplement",
                                "file_formats", "upon_request", "dataset")

  #search for all relevant keyword categories
  publ_keywords <- sentence_search_keywords %>%
    map(.search_keyword_cat, publ_sentences, keyword_list)
  names(publ_keywords) <- sentence_search_keywords

  #gather results
  publ_keywords <- do.call(cbind, publ_keywords) %>%
    as_tibble()
  publ_keywords <- cbind(publ_sentences, publ_keywords) %>%
    as_tibble()
  publ_keywords$publ_sentences <- as.character(publ_keywords$publ_sentences)

  return(publ_keywords)
}


#helper function to search for all keyword categories in the sentences
.search_keyword_cat <- function(keyword, sentences, keyword_list)
{
  detection_col <- sentences %>%
    map_lgl(stringr::str_detect, pattern = keyword_list[[keyword]])

  return(detection_col)
}


#part of the keyword search on the tokenized sentences
.keyword_search_tokenized <- function(PDF_text_sentences)
{
  #search for open data keywords in the full texts
  open_data_categories <- map(PDF_text_sentences, .map_keywords)

  #combine columns for the different open data keywords
  keyword_results_combined <- open_data_categories %>%
    map(mutate, com_specific_db = field_specific_db & accession_nr & available & !not_available & !was_available) %>%
    map(mutate, com_general_db = repositories & available & !not_available & !was_available) %>%
    map(mutate, com_github_data = data & github & available & !not_available & !was_available) %>%
    map(mutate, com_code = source_code & available & !not_available & !was_available & !upon_request) %>%
    map(mutate, com_suppl_code = supplement & source_code) %>%
    map(select, com_specific_db, com_general_db, com_github_data, dataset, com_code, com_suppl_code)

  #summarze results over all sentences of each publication to see if any match was found for each keyword category
  keyword_results_tokenized <- keyword_results_combined %>%
    map(apply, MARGIN = 2, FUN = any)
  keyword_results_tokenized <- do.call(rbind, keyword_results_tokenized) %>%
    as_tibble()

  return(keyword_results_tokenized)
}


#part of the keyword search that searches for nearby words on the full text
.keyword_search_near_wd <- function(PDF_text_sentences, extract_text = FALSE)
{
  keyword_list <- .create_keyword_list()

  #needs text input in two formats: split into sentences, and one long string for
  PDF_text_full <- PDF_text_sentences %>% map(paste, collapse = " ")

  #search for the last combination in the non-tokenized text and add it to the results table for the publications
  #either give out TRUE/FALSE or return detected string
  if(extract_text) {
    str_function <- stringr::str_extract
    map_function <- map_chr
  } else {
    str_function <- stringr::str_detect
    map_function <- map_lgl
  }
  keyword_results_near_wd <- tibble(
    com_file_formats = map_function(PDF_text_full, str_function, pattern = keyword_list[["all_data_file_formats"]]),
    com_supplemental_data = map_function(PDF_text_full, str_function, pattern = keyword_list[["supp_table_data"]]),
    com_data_availibility = map_function(PDF_text_full, str_function, pattern = keyword_list[["data_availibility_statement"]]))

  return(keyword_results_near_wd)
}


#helper function for the testing of the keywords with testthat
.detect_keywords <- function(string, keyword_category)
{
  keywords <- .create_keyword_list()

  string <- tolower(string)
  detected <- stringr::str_detect(string, keywords[[keyword_category]])

  return(detected)
}


#additional Open Data check: checks if PDF DOI stems from a list of Data Journals
#requires that the PDF filename is the DOI
.check_journal_doi <- function(PDF_text_sentences)
{
  keyword_list <- .create_keyword_list()

  dois <- names(PDF_text_sentences) %>%
    stringr::str_replace_all(stringr::fixed("+"), stringr::fixed("/")) %>%
    stringr::str_remove(stringr::fixed(".txt"))

  data_journal_doi <- tibble(
    is_data_journal = map_lgl(dois, stringr::str_detect, pattern = keyword_list[["data_journal_dois"]]))

  return(data_journal_doi)

}