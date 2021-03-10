#' Conversion of PDFs to text.
#'
#' Converts PDFs to text before the text can be loaded and text-mined.
#' Requires the poppler library (\url{https://poppler.freedesktop.org/})
#' that is run via a terminal command to do the conversion.
#'
#' @param PDF_folder String of the folder name in which the PDFs are located.
#' @param output_folder String of the folder name in which the converted files will be saved.
#' @param clusters Number of parallel threads used to speed up conversion (using packages parallel, doParallel and foreach)
#'
#' @return Logical vector describing the conversion success for each PDF file.
#'
pdf_convert <- function(PDF_folder, output_folder, clusters = 10)
{
  #parallelize pdf conversion
  cl <- parallel::makeCluster(clusters, outfile="")
  doParallel::registerDoParallel(cl)

  #check if dir path has final /, otherwise add
  if(PDF_folder %>% stringr::str_sub(-1) != "/") {
    PDF_folder <- paste0(PDF_folder, "/")
  }
  if(output_folder %>% stringr::str_sub(-1) != "/") {
    output_folder <- paste0(output_folder, "/")
  }

  PDF_filenames <- list.files(PDF_folder, recursive = TRUE)
  PDF_filenames <- paste0(PDF_folder, PDF_filenames)

  #converts PDF file to txt file and saves it to output_folder
  #requires the pdftotext program that is run via a terminal command
  conversion_success <- foreach::foreach(i=1:length(PDF_filenames)) %dopar% {
    .pdf_to_text(PDF_filenames[i], output_folder)
  }
  conversion_success <- unlist(conversion_success)

  parallel::stopCluster(cl)

  return(conversion_success)
}


#' Load converted PDFs.
#'
#' Loads the text files into a list of string vectors that can subsequently be searched by the text-mining algorithm.
#' The returned object has a list structure with one list element per document.
#' Each list element is a vector of strings containing the sentences of the document.
#' First use the function pdf_convert to create the converted files, if you have them in PDF format.
#'
#' @param pdf_text_folder String of the folder name from which the converted files will be loaded.
#'
#' @return List with one element per document.
#' Each document is split into its sentences and saved as a vector of strings.
#'
#' @examples pdf_load("examples/")
#'
pdf_load <- function(pdf_text_folder)
{

  #read in full text from .txt files
  txt_filenames <- list.files(pdf_text_folder)
  txt_filenames <- txt_filenames[!stringr::str_detect(txt_filenames,stringr::fixed(".pdf"))]
  txt_filenames_short <- txt_filenames #only used as names for the text/results tables later
  txt_filenames <- paste0(pdf_text_folder, txt_filenames)

  #produce version of the full texts where all sentences are seperate vector elements
  PDF_text_sentences <- map(txt_filenames, readr::read_lines) %>%
    map(paste, collapse = " ") %>%
    map(tokenizers::tokenize_sentences, simplify = TRUE) %>%
    map(tolower) %>%
    map(stringr::str_replace_all, pattern = ",", replacement = "") %>%
    map(.correct_tokenization)
  names(PDF_text_sentences) <- txt_filenames_short

  return(PDF_text_sentences)
}


#' Seach for open data & open code keywords.
#'
#' The algorithm searches for several categories of similar keywords in each sentence.
#' Multiple categories have to match for a single sentence to trigger a detection.
#' Among keyword categories are categories for specific biomedical databases as well as
#' their corresponding accession numbers (as regular expressions), general-purpose repositories
#' or different file formats typically used to distribute raw data in the supplement.
#' Additionally, Open Code dissemination is detected using keywords categories for source code or code repositories.
#'
#' @param PDF_text_sentences Document corpus loaded with the pdf_load function.
#'
#' @param detected_sentences Logical parameter. If TRUE, the sentences in which the Open Data
#' statements were detected are added to the results table as well.
#'
#' @return Tibble with one row per screened document and the filename and logical values for open data
#' and open code detection as columns plus additional columns containing the identified open data/code categories
#' as well as the sentences in which open data/code was detected.
#'
#' @examples open_data_search(pdf_load("examples/"))
#'
open_data_search <- function(PDF_text_sentences, detected_sentences = TRUE)
{
  #search for open data keywords in the full texts
  #do this only once, as this is the most time consuming step
  keyword_results <- .keyword_search_full(PDF_text_sentences)

  open_data_results <- .open_data_detection(PDF_text_sentences, keyword_results)

  #extract detected sentences as well
  if(detected_sentences)
  {
    detected_sentences <- .open_data_sentences(PDF_text_sentences, keyword_results)
    open_data_results <- cbind(open_data_results, detected_sentences[,-1]) %>%
      as_tibble()
  }

  return(open_data_results)
}



#' Parallelizied of the open data & open code keyword search
#'
#' The algorithm searches for several categories of similar keywords in each sentence.
#' Multiple categories have to match for a single sentence to trigger a detection.
#' Among keyword categories are categories for specific biomedical databases as well as
#' their corresponding accession numbers (as regular expressions), general-purpose repositories
#' or different file formats typically used to distribute raw data in the supplement.
#' Additionally, Open Code dissemination is detected using keywords categories for source code or code repositories.
#'
#' @param PDF_text_sentences Document corpus loaded with the pdf_load function.
#'
#' @param detected_sentences Logical parameter. If TRUE, the sentences in which the Open Data
#' statements were detected are added to the results table as well.
#'
#' @param cluster_num Number of parallel processes that are started. The keyword search is parallelized
#' with respect to the documents to speed up the calculations.
#'
#' @return Tibble with one row per screened document and the filename and logical values for open data
#' and open code detection as columns plus additional columns containing the identified open data/code categories
#' as well as the sentences in which open data/code was detected.
#'
#' @examples open_data_search_parallel(pdf_load("examples/"))
#'
open_data_search_parallel <- function(PDF_text_sentences, detected_sentences = TRUE, cluster_num = 4)
{
  cl <- parallel::makeCluster(cluster_num, outfile="")
  doParallel::registerDoParallel(cl)

  open_data_results <- foreach::foreach(i=1:length(PDF_text_sentences)) %dopar% {
    open_data_search(PDF_text_sentences[i], detected_sentences)
  }
  open_data_results <- do.call(rbind, open_data_results)

  parallel::stopCluster(cl)

  return(open_data_results)
}


