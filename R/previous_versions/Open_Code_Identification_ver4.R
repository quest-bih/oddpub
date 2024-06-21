library(tm)
library(XML)
library(tokenizers)
library(tidyverse)
library(foreach)
library(doParallel)
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

source_code <- c("source code",
                 "analysis script",
                 "github",
                 "gitlab",
                 "SAS script",
                 "SPSS script",
                 "R script",
                 "R code",
                 "python script",
                 "python code",
                 "matlab script",
                 "matlab code",
                 "c++ code",
                 "julia code",
                 "julia script")
source_code <- paste0("\\b", source_code)
source_code <- paste(source_code, collapse = "|") %>% tolower()

supplement <- c("supporting information",
                "supplement",
                "supplementary data")
supplement <- paste0("\\b", supplement)
supplement <- paste(supplement, collapse = "|") %>% tolower()

upon_request <- c("upon request",
                  "on request",
                  "upon reasonable request")
upon_request <- paste0("\\b", upon_request)
upon_request <- paste(upon_request, collapse = "|") %>% tolower()



map_keywords <- function(publ_sentences)
{
  keywd_names <- c("available", "was_available", "not_available", "gene_db", "accession_nr", "repositories",
                   "all_data", "not_data", "source_code", "supplement", "file_formats", "upon_request")
  publ_keywords <- as_tibble(publ_sentences) %>%
    add_column(available = map_lgl(publ_sentences, str_detect, pattern = available)) %>%
    add_column(was_available = map_lgl(publ_sentences, str_detect, pattern = was_available)) %>%
    add_column(not_available = map_lgl(publ_sentences, str_detect, pattern = not_available)) %>%
    add_column(source_code = map_lgl(publ_sentences, str_detect, pattern = source_code)) %>%
    add_column(supplement = map_lgl(publ_sentences, str_detect, pattern = supplement)) %>%
    add_column(upon_request = map_lgl(publ_sentences, str_detect, pattern = upon_request))

  return(publ_keywords)
}


#---------------------------------------------------------------------
# 5 - Open data identification
#---------------------------------------------------------------------


text_fragments <- function(open_data_tibble) {
  open_data_tibble <- select(open_data_tibble, c(1, 8:9))
  sentence_idx <- map(as.list(open_data_tibble[2:3]), which)

  if(length(sentence_idx) == 0) {
    return(c("", ""))
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
open_code <- map(PDF_text_body, map_keywords)
toc()

#combine columns for the different open data keywords
open_code <- open_code %>% map(mutate, com_code = source_code & available & !not_available & !was_available & !upon_request) %>%
  map(mutate, com_suppl_raw_all_data_1 = supplement & source_code)


#summarze results to see if any match was found for each keyword category
open_code_detected <- map(open_code, select, c(8:9)) %>%
  map(apply, MARGIN = 2, FUN = any)

open_code_detected <- do.call(rbind, open_code_detected)

#add the last keyword combination that was
open_code_detected <- as_tibble(cbind(names(open_code), as_tibble(open_code_detected)))
colnames(open_code_detected)[1] <- "article"
open_code_detected$article <- as.character(open_code_detected$article)

write_csv(as_tibble(open_code_detected), "../ver4_results/open_code_detection_Ver4_upd_1.csv")
open_code_detected <- read_csv("../ver4_results/open_code_detection_Ver4_upd_1.csv")

is_open_code <- apply(open_code_detected[,c(2,3)], 1, any)
names(is_open_code) <- open_code_detected$article



#get the sentences in which the keywords were detected
open_code_sentences <- map(open_code, text_fragments)
open_code_sentences <- do.call(rbind, open_code_sentences)
open_code_sentences <- cbind(names(open_code), open_code_sentences) %>%
  as_tibble()
colnames(open_code_sentences) <- c("paper", colnames(open_code_detected) %>% tail(-1))

write_csv(open_code_sentences, "../ver4_results/open_code_sentences_ver4_upd_1.csv")
open_code_sentences <- read_csv("../ver4_results/open_code_sentences_ver4_upd_1.csv")



open_code_results_table <- cbind(open_code_sentences, is_open_code) %>%
  as_tibble() %>%
  filter(is_open_code)
write_csv(open_code_results_table, "../ver4_results/open_code_detection_Ver4_upd_1_table.csv")


#copy PDFs of the new sample to separate folder
sample_open_code_files_pos <- open_code_results_table$paper %>% str_split("\\.") %>% map_chr(1)
PDF_filenames_pos <- PDF_filenames[map_lgl(PDF_filenames,
                                           function(x) str_detect(x, fixed(sample_open_code_files_pos)) %>% any())]

file.copy(as.list(PDF_filenames_pos), to = "../ver4_results/open code papers")
