require(tm)
require(XML)
require(tokenizers)
require(tidyverse)
require(foreach)
require(doParallel)
library(tictoc)

cl <- makeCluster(10, outfile="") #determines how many parallel processes are used for the pdf downloading
registerDoParallel(cl)

source("T:/Dokumente/Projekte/Text mining survey/Pubmed Text Mining/2 - PDF retrieval & Conversion/PDF_conversion.R")
source("T:/Dokumente/Projekte/Text mining survey/Pubmed Text Mining/3 - full text cleaning & risk of bias/Cleaning_RoB.R")
source("functions.R")

save_folder_full <- "C:/Datenablage/Status Quo PDFs/Open Data Papers/PDF/"
output_folder_full <- "C:/Datenablage/Status Quo PDFs/Open Data Papers/PDF_to_text/"

#save_folder_full <- "../Stichprobe PDFs/"
#output_folder_full <- "../Stichprobe_PDF_to_text/"

#---------------------------------------------------------------------
# 3 - PDF conversion
#---------------------------------------------------------------------

PDF_filenames <- list.files(save_folder_full, recursive = TRUE)
PDF_filenames <- paste0(save_folder_full, PDF_filenames)


conversion_success <- foreach(i=1:length(PDF_filenames)) %dopar% {
  PDF_to_text(PDF_filenames[i], output_folder_full)
}
conversion_success <- unlist(conversion_success)


#---------------------------------------------------------------------
# 4 - read in PDF text
#---------------------------------------------------------------------

#read in full text from .txt files
txt_filenames <- list.files(output_folder_full)
txt_filenames <- txt_filenames[!str_detect(txt_filenames,fixed(".pdf"))]
txt_filenames <- paste0(output_folder_full, txt_filenames)

#produce version of the full texts where all sentences are seperate vector elements
PDF_text_body <- map(txt_filenames, readLines, warn = FALSE, encoding = "UTF-8") %>%
  map(paste, collapse = " ") %>%
  map(tokenize_sentences, simplify = TRUE) %>%
  map(tolower) %>%
  map(str_replace_all, pattern = ",", replacement = "")
names(PDF_text_body) <- sapply(txt_filenames, get_pmid_from_filename, USE.NAMES = FALSE)

#second version of the full text without sentence splitting (used for the )
PDF_text_full <- map(txt_filenames, readLines, warn = FALSE, encoding = "UTF-8") %>%
  map(paste, collapse = " ") %>%
  map(tolower) %>%
  map(str_replace_all, pattern = ",", replacement = "")
names(PDF_text_full) <- sapply(txt_filenames, get_pmid_from_filename, USE.NAMES = FALSE)



#alternative way to read in documents
# pdf_reader <- readPDF(engine = "xpdf")
# PDF_dir <- DirSource(save_folder, encoding = "UTF-8")
# PDF_corpus <- VCorpus(PDF_dir, readerControl = list(reader = pdf_reader, language = "en"))
#
# PDF_meta_tm <- lapply(PDF_corpus, "[[", "meta")
# PDF_text_body_tm <- lapply(PDF_corpus, "[[", "content")
# PDF_text_body_tm <- lapply(PDF_text_body, paste, collapse = " ")


#---------------------------------------------------------------------
# 4 - Open data identification keywords
#---------------------------------------------------------------------

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

accession_nr <- c("GSE[[:digit:]]{2,8}",
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
com_suppl_raw_all_data_2 <- paste0("\\b(",
                                   all_data,
                                   ")(?:\\W+\\w+){0,10}?\\W+(",
                                   file_formats,
                                   ")\\b|\\b(",
                                   file_formats,
                                   ")(?:\\W+\\w+){0,10}?\\W+(",
                                   all_data,
                                   ")\\b")


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


#---------------------------------------------------------------------
# 5 - Open data identification
#---------------------------------------------------------------------


text_fragments <- function(open_data_tibble) {
  open_data_tibble <- select(open_data_tibble, c(1, 14:17))
  sentence_idx <- apply(open_data_tibble[2:5], 2, which)

  if(length(sentence_idx) == 0) {
    return(c("", "", "", ""))
  }

  text_frag <- vector()
  for(idx in sentence_idx)
  {
    if(length(idx) == 0) {
      text_frag <- c(text_frag, "")
    } else {
      current_txt <- open_data_tibble$value[idx] %>% paste(collapse = ";     ")
      text_frag <- c(text_frag, current_txt)
    }
  }

  return(text_frag)
}

#search for open data keywords in the full texts
tic()
open_data <- map(PDF_text_body, map_keywords)
toc()

#combine columns for the different open data keywords
open_data <- open_data %>% map(mutate, com_specific_db = gene_db & accession_nr & available & !not_available & !was_available) %>%
  map(mutate, com_general_db = repositories & available & !not_available & !was_available) %>%
  map(mutate, com_code = source_code & available & !not_available & !was_available & !upon_request) %>%
  map(mutate, com_suppl_code = supplement & source_code)

#summarze results to see if any match was found for each keyword category
open_data_detected <- map(open_data, select, c(14:17)) %>%
  map(apply, MARGIN = 2, FUN = any)

open_data_detected <- do.call(rbind, open_data_detected)

#add the last keyword combination that was
open_data_near_wd <- map_lgl(PDF_text_full, str_detect, pattern = com_suppl_raw_all_data_2)
open_data_detected <- open_data_detected %>% cbind(open_data_near_wd)
colnames(open_data_detected)[5] <- "com_suppl_raw_all_data"
open_data_detected <- as_tibble(cbind(names(open_data), as_tibble(open_data_detected)))
colnames(open_data_detected)[1] <- "article"
open_data_detected$article <- as.character(open_data_detected$article)

write_csv(as_tibble(open_data_detected), "../ver4_results/open_data_detection_Ver4_upd_3.csv")
open_data_detected <- read_csv("../ver4_results/open_data_detection_Ver4_upd_3.csv")

is_open_data <- apply(open_data_detected[,c(2,3,6)], 1, any)
names(is_open_data) <- open_data_detected$article



#get the sentences in which the keywords were detected
open_data_sentences <- map(open_data, text_fragments)
open_data_sentences <- do.call(rbind, open_data_sentences)
open_data_sentences <- cbind(names(open_data), open_data_sentences) %>%
  as_tibble()
open_data_sentences <- cbind(open_data_sentences, open_data_detected[,6])
open_data_sentences <- open_data_sentences %>% as_tibble()
colnames(open_data_sentences) <- c("paper", colnames(open_data_detected) %>% head(-1))

write_csv(open_data_sentences, "../ver4_results/open_data_sentences_ver4_upd_3.csv")
open_data_sentences <- read_csv("../ver4_results/open_data_sentences_ver4_upd_3.csv")





