require(foreach)
require(doParallel)
require(tm)
require(XML)
require(tokenizers)
require(tidyverse)

cl <- makeCluster(10, outfile="") #determines how many parallel processes are used for the pdf downloading
registerDoParallel(cl)

source("T:/Dokumente/Projekte/Text mining survey/Pubmed Text Mining/2 - PDF retrieval & Conversion/PDF_conversion.R")
source("T:/Dokumente/Projekte/Text mining survey/Pubmed Text Mining/3 - full text cleaning & risk of bias/Cleaning_RoB.R")
source("functions.R")

save_folder <- "../PDF/"
output_folder <- "../PDF_to_text/"

#save_folder_full <- "C:/Datenablage/Status Quo PDFs/statusquo.Data/PDF/"
#output_folder_full <- "C:/Datenablage/Status Quo PDFs/statusquo.Data/PDF_to_text/"

save_folder_full <- "../Stichprobe PDFs/"
output_folder_full <- "../Stichprobe_PDF_to_text/"

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

#read from .txt files
txt_filenames <- list.files(output_folder_full)
txt_filenames <- paste0(output_folder_full, txt_filenames)

PDF_text_body <- map(txt_filenames, readLines, warn = FALSE)
PDF_text_body <- map(PDF_text_body, paste, collapse = " ")
#PDF_text_body <- map(PDF_text_body, tokenize_sentences, simplify = TRUE)
PDF_text_body <- map(PDF_text_body, tolower)
names(PDF_text_body) <- sapply(txt_filenames, get_pmid_from_filename, USE.NAMES = FALSE)


#alternative way to read in documents
# pdf_reader <- readPDF(engine = "xpdf")
# PDF_dir <- DirSource(save_folder, encoding = "UTF-8")
# PDF_corpus <- VCorpus(PDF_dir, readerControl = list(reader = pdf_reader, language = "en"))
#
# PDF_meta_tm <- lapply(PDF_corpus, "[[", "meta")
# PDF_text_body_tm <- lapply(PDF_corpus, "[[", "content")
# PDF_text_body_tm <- lapply(PDF_text_body, paste, collapse = " ")


#---------------------------------------------------------------------
# 4 - ???
#---------------------------------------------------------------------


#---------------------------------------------------------------------
# 5 - Open data identification
#---------------------------------------------------------------------

open_data_string <- c("zenodo",
                      "figshare",
                      "dryad",
                      "data and material availability",
                      "availability of data",
                      "data availability",
                      "data can be accessed",
                      "data are available",
                      "data are publicly available",
                      "data is available",
                      "data for figure",
                      "data for table",
                      "data to reproduce",
                      "data to replicate",
                      "data set [[:lower:]]{1,3}[[:digit:]]{1,2}",
                      "dataset [[:lower:]]{1,3}[[:digit:]]{1,2}",
                      "source data [[:lower:]]{1,3}[[:digit:]]{1,2}",
                      "source data [[:digit:]]{1,2}",
                      "raw data",
                      "full data",
                      "data repository")
open_data_regex <- paste(open_data_string, collapse = "|")

#seach all full text documents for the open data keywords
open_data <- unlist(lapply(PDF_text_body, RegularExpressionIdentification, pattern = open_data_regex))
#additionally pick all sentences in which any of the keywords appear
open_data_sample_sentence <- lapply(PDF_text_body[open_data], regex_sample_sentence, pattern = open_data_regex)

#save all keyword sentences to text file
sample_save <- "../Open Data/Open_Date_sample_sentences.txt"
for(i in 1:length(open_data_sample_sentence))
{
  write(names(open_data_sample_sentence[i]), sample_save, append = TRUE)
  write(open_data_sample_sentence[[i]], sample_save, append = TRUE)
  write("\n", sample_save, append = TRUE)
}

#we discovered one special phrase that is used in >120 publications
#now pick all pdfs for these publications
open_data_string_special <- "all relevant data are within the paper and its supporting information files"
open_data_special <- unlist(lapply(PDF_text_body, RegularExpressionIdentification, pattern = open_data_string_special))

#search PDF filenames for those cases
open_data_special_files <- names(open_data_special)[open_data_special == TRUE]
open_data_special_PDFs <-PDF_filenames[sapply(open_data_special_files, grep, x=PDF_filenames, fixed=TRUE)]
for(PDF in open_data_special_PDFs)
{
  file.copy(from=PDF, to="../Open Data/Open Data Papers/")
}

#draw random subset of data
is_open_data <- open_data[open_data == TRUE]
n_sample <- ceiling(length(is_open_data)/10)
random_subset <- sample(1:length(is_open_data), n_sample, replace=F)

#save those subset PDF to separate file
random_subset_files <- names(is_open_data)[random_subset]
random_subset_PDFs <-PDF_filenames[sapply(random_subset_files, grep, x=PDF_filenames, fixed=TRUE)]
for(PDF in random_subset_PDFs)
{
  file.copy(from=PDF, to="../Open Data/Stichprobe PDFs/")
}
