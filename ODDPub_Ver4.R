#--------------------------------------------------------------------------------------
# Open Data Detection in Publications (ODDPub) -
# text mining algorithm that parses a set of publications
# and outputs in which of the publications Open Data or Open Code were detected
# this algoritm is tailored towards biomedical literature
#
# Author: Nico Riedel
# This code is licensed under MIT license (see LICENSE.txt for details)
#--------------------------------------------------------------------------------------

require(tokenizers) # used for the sentence tokenizing
require(tidyverse)
require(foreach)
require(doParallel)

#determines how many parallel processes are used for the pdf conversion
cl <- makeCluster(10, outfile="")
registerDoParallel(cl)

source("ODDPub_Ver4_functions.R")

#needs folder with saved PDFs as input, outputs converted PDFs in output_folder
save_folder <- "PDFs/"
output_folder <- "PDF_to_text/"


#--------------------------------------------------------------------------------------
# 1 - PDF conversion
#--------------------------------------------------------------------------------------

PDF_filenames <- list.files(save_folder, recursive = TRUE)
PDF_filenames <- paste0(save_folder, PDF_filenames)

#converts PDF file to txt file and saves it to output_folder
#requires the pdftotext program that is run via a terminal command
conversion_success <- foreach(i=1:length(PDF_filenames)) %dopar% {
  PDF_to_text(PDF_filenames[i], output_folder)
}
conversion_success <- unlist(conversion_success)


#--------------------------------------------------------------------------------------
# 2 - read in PDF text
#--------------------------------------------------------------------------------------

#read in full text from .txt files
txt_filenames <- list.files(output_folder)
txt_filenames <- txt_filenames[!str_detect(txt_filenames,fixed(".pdf"))]
txt_filenames_short <- txt_filenames #only used as names for the text/results tables later
txt_filenames <- paste0(output_folder, txt_filenames)

#produce version of the full texts where all sentences are seperate vector elements
PDF_text_sentences <- map(txt_filenames, readLines, warn = FALSE, encoding = "UTF-8") %>%
  map(paste, collapse = " ") %>%
  map(tokenize_sentences, simplify = TRUE) %>%
  map(tolower) %>%
  map(str_replace_all, pattern = ",", replacement = "")
names(PDF_text_sentences) <- txt_filenames_short


#--------------------------------------------------------------------------------------
# 3 - Open data identification keywords
#--------------------------------------------------------------------------------------

#Several categories of similar keywords are searched for in a sentence.
#Multiple categories have to match to trigger a detection.

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
               "uploaded to")
#word boundaries are added in the beginning only to allow for different possible endings
available <- paste0("\\b", available)
available <- paste(available, collapse = "|") %>% tolower()


was_available <- c("was provided",
                   "were provided",
                   "was contained in",
                   "were contained in",
                   "was available",
                   "were available",
                   "was accessible",
                   "were accessible",
                   "available from",
                   "deposited by",
                   "were reproduced")
was_available <- paste0("\\b", was_available)
was_available <- paste(was_available, collapse = "|") %>% tolower()


not_available <- c("not included",
                   "not deposited",
                   "not released",
                   "not provided",
                   "not contained in",
                   "not available",
                   "not accessible",
                   "not submitted")
not_available <- paste0("\\b", not_available)
not_available <- paste(not_available, collapse = "|") %>% tolower()


gene_db <- c("GEO",
            "Gene Expression Omnibus",
            "European Nucleotide Archive",
            "National Center for Biotechnology Information",
            "NCBI",
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
            "PRIDE",
            "DNA Data Bank of Japan",
            "DDBJ",
            "Genbank",
            "Protein Databank",
            "PDB",
            "Metagenomics Rapid Annotation using Subsystem Technology",
            "MG-RAST",
            "accession number",
            "accession code",
            "accession numbers",
            "accession codes")
gene_db <- paste0("\\b", gene_db, "\\b") #for all the abbreviations probably need an explicit end of word boundary as well, but needs to be tested
gene_db <- paste(gene_db, collapse = "|") %>% tolower()


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
                  "[:digit:]{1}[A-Z]{1}[[:alnum:]]{2}") #Protein Data Bank
accession_nr <- paste0("\\b", accession_nr)
accession_nr <- paste(accession_nr, collapse = "|") %>% tolower()


repositories <- c("figshare",
                  "dryad",
                  "zenodo",
                  "dataverse",
                  "osf")
repositories <- paste0("\\b", repositories)
repositories <- paste(repositories, collapse = "|") %>% tolower()


all_data <- c("all data",
              "all array data",
              "raw data",
              "full data set",
              "full dataset")
all_data <- paste0("\\b", all_data)
all_data <- paste(all_data, collapse = "|") %>% tolower()


not_data <- c("not all data",
              "not all array data",
              "no raw data",
              "no full data set",
              "no full dataset")
not_data <- paste0("\\b", not_data)
not_data <- paste(not_data, collapse = "|") %>% tolower()


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
                 "matlab code")
source_code <- paste0("\\b", source_code)
source_code <- paste(source_code, collapse = "|") %>% tolower()


supplement <- c("supporting information",
                "supplement",
                "supplementary data")
supplement <- paste0("\\b", supplement)
supplement <- paste(supplement, collapse = "|") %>% tolower()


file_formats <- c("csv",
                  "zip",
                  "xls",
                  "xlsx")
file_formats <- paste0("\\b", file_formats)
file_formats <- paste(file_formats, collapse = "|") %>% tolower()


upon_request <- c("upon request",
                  "on request",
                  "upon reasonable request")
upon_request <- paste0("\\b", upon_request)
upon_request <- paste(upon_request, collapse = "|") %>% tolower()


#special regex pattern that looks for word closeness instead of words being in the same sentence
#effect: all_data & file_format words are at most 10 words apart from each other
all_data_file_formats <- paste0("\\b(",
                                   all_data,
                                   ")(?:\\W+\\w+){0,10}?\\W+(",
                                   file_formats,
                                   ")\\b|\\b(",
                                   file_formats,
                                   ")(?:\\W+\\w+){0,10}?\\W+(",
                                   all_data,
                                   ")\\b")


#--------------------------------------------------------------------------------------
# 4 - Open data identification
#--------------------------------------------------------------------------------------

#actual search for open data & open code keywords
open_data_search <- function(PDF_text_sentences)
{
  #needs text input in two formats: split into sentences, and one long string for
  PDF_text_full <- PDF_text_sentences %>% map(paste, collapse = " ")

  #search for open data keywords in the full texts
  open_data_categories <- map(PDF_text_sentences, map_keywords)

  #combine columns for the different open data keywords
  open_data_combined <- open_data_categories %>%
    map(mutate, com_specific_db = gene_db & accession_nr & available & !not_available & !was_available) %>%
    map(mutate, com_general_db = repositories & available & !not_available & !was_available) %>%
    map(mutate, com_code = source_code & available & !not_available & !was_available & !upon_request) %>%
    map(mutate, com_suppl_code = supplement & source_code) %>%
    map(select, com_specific_db, com_general_db, com_code, com_suppl_code)

  #summarze results over all sentences of each publication to see if any match was found for each keyword category
  open_data_publication <- open_data_combined %>%
    map(apply, MARGIN = 2, FUN = any)
  open_data_publication <- do.call(rbind, open_data_publication)

  #search for the last combination in the non-tokenized text and add it to the results table for the publications
  com_file_formats <- map_lgl(PDF_text_full, str_detect, pattern = all_data_file_formats)
  open_data_publication <- open_data_publication %>%
    cbind(com_file_formats) %>%
    as_tibble()

  #check if any of the combined columns was positive to determine if the publication has Open Data or Open Code
  open_data_publication <- open_data_publication %>%
    mutate(is_open_data = com_specific_db | com_general_db | com_file_formats) %>%
    mutate(is_open_code = com_code | com_suppl_code) %>%
    add_column(article = names(open_data_combined)) %>%
    select(article, is_open_data, is_open_code)

  return(open_data_publication)
}


#function that applies all keyword category searches onto each sentence of the publications
map_keywords <- function(publ_sentences)
{
  keywd_names <- c("available", "was_available", "not_available", "gene_db", "accession_nr", "repositories",
                   "all_data", "not_data", "source_code", "supplement", "file_formats", "upon_request")
  publ_keywords <- as_tibble(publ_sentences) %>%
    add_column(available = map_lgl(publ_sentences, str_detect, pattern = available)) %>%
    add_column(was_available = map_lgl(publ_sentences, str_detect, pattern = was_available)) %>%
    add_column(not_available = map_lgl(publ_sentences, str_detect, pattern = not_available)) %>%
    add_column(gene_db = map_lgl(publ_sentences, str_detect, pattern = gene_db)) %>%
    add_column(accession_nr = map_lgl(publ_sentences, str_detect, pattern = accession_nr)) %>%
    add_column(repositories = map_lgl(publ_sentences, str_detect, pattern = repositories)) %>%
    add_column(all_data = map_lgl(publ_sentences, str_detect, pattern = all_data)) %>%
    add_column(not_data = map_lgl(publ_sentences, str_detect, pattern = not_data)) %>%
    add_column(source_code = map_lgl(publ_sentences, str_detect, pattern = source_code)) %>%
    add_column(supplement = map_lgl(publ_sentences, str_detect, pattern = supplement)) %>%
    add_column(file_formats = map_lgl(publ_sentences, str_detect, pattern = file_formats)) %>%
    add_column(upon_request = map_lgl(publ_sentences, str_detect, pattern = upon_request))

  return(publ_keywords)
}


#funtion that detects and outputs the sentences in which the Open Data keywords were detected
open_data_sentences <- function(PDF_text_sentences)
{
  #search for open data keywords in the full texts
  open_data_categories <- map(PDF_text_sentences, map_keywords)

  open_data_combined <- open_data_categories %>%
    map(mutate, com_specific_db = gene_db & accession_nr & available & !not_available & !was_available) %>%
    map(mutate, com_general_db = repositories & available & !not_available & !was_available) %>%
    map(mutate, com_code = source_code & available & !not_available & !was_available & !upon_request) %>%
    map(mutate, com_suppl_code = supplement & source_code) %>%
    map(select, value, com_specific_db, com_general_db, com_code, com_suppl_code)

  #identifies the text fragments in which the Open Data keywords were detected
  open_data_sentences <- map(open_data_combined, text_fragments)
  open_data_sentences <- do.call(rbind, open_data_sentences)
  open_data_sentences <- cbind(names(open_data_categories), open_data_sentences) %>%
    as_tibble()
  colnames(open_data_sentences) <- c("article", "com_specific_db", "com_general_db", "com_code", "com_suppl_code")

  return(open_data_sentences)
}


#--------------------------------------------------------------------------------------
# 5 - Results of the Open Data Search
#--------------------------------------------------------------------------------------

open_data_results <- open_data_search(PDF_text_sentences)
write_csv(open_data_results, "open_data_detection.csv")

open_data_sentences_detected <- open_data_sentences(PDF_text_sentences)
write_csv(open_data_sentences_detected, "open_data_sentences.csv")

