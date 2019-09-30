#' @title ODDPub_v%
#'
#' @description Detection of Open Data & Open Code statements in biomedical publications
#'
#' @param PDF_text_sentences
#'
#' @return open_data_publication
#'
#' @export open_data_search


#converts PDFs to text
#requires the pdftotext program that is run via a terminal command
pdf_convert <- function(PDF_folder, output_folder, clusters = 10)
{
  #parallelize pdf conversion
  cl <- parallel::makeCluster(clusters, outfile="")
  doParallel::registerDoParallel(cl)

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


#loads converted PDFs
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


#actual search for open data & open code keywords
open_data_search <- function(PDF_text_sentences)
{
  #one part of the keyword search acts on the tokenized sentences while another part acts on the full text
  keyword_results_tokenized <- .keyword_search_tokenized(PDF_text_sentences)
  keyword_results_near_wd <- .keyword_search_near_wd(PDF_text_sentences)
  data_journal_doi <- .check_journal_doi(PDF_text_sentences)

  keyword_results_combined <- cbind(keyword_results_tokenized, keyword_results_near_wd, data_journal_doi) %>%
    as_tibble()

  #check if any of the combined columns was positive to determine if the publication has Open Data or Open Code
  open_data_publication <- keyword_results_combined %>%
    mutate(is_open_data = com_specific_db | com_general_db | com_file_formats | com_github_data | dataset | com_supplemental_data | com_data_availibility | is_data_journal) %>%
    mutate(is_open_code = com_code | com_suppl_code) %>%
    tibble::add_column(article = names(PDF_text_sentences)) %>%
    select(article, is_open_data, is_open_code)

  return(open_data_publication)
}


#funtion that detects and outputs the sentences in which the Open Data keywords were detected
open_data_sentences <- function(PDF_text_sentences)
{
  #search for open data keywords in the full texts
  open_data_categories <- map(PDF_text_sentences, .map_keywords)

  open_data_combined <- open_data_categories %>%
    map(mutate, com_specific_db = field_specific_db & accession_nr & available & !not_available & !was_available) %>%
    map(mutate, com_general_db = repositories & available & !not_available & !was_available) %>%
    map(mutate, com_github_data = data & github & available & !not_available & !was_available) %>%
    map(mutate, com_code = source_code & available & !not_available & !was_available & !upon_request) %>%
    map(mutate, com_suppl_code = supplement & source_code) %>%
    map(select, value, com_specific_db, com_general_db, com_github_data, dataset, com_code, com_suppl_code)

  #add simple TRUE/FALSE for the categories where the whole text is searched for nearby words
  keyword_results_near_wd <- .keyword_search_near_wd(PDF_text_sentences, extract_text = TRUE)


  #identifies the text fragments in which the Open Data keywords were detected
  open_data_sentences <- map(open_data_combined, .text_fragments)
  open_data_sentences <- do.call(rbind, open_data_sentences)
  open_data_sentences <- cbind(names(open_data_categories), open_data_sentences, keyword_results_near_wd) %>%
    as_tibble()
  colnames(open_data_sentences) <- c("article", "com_specific_db", "com_general_db", "com_github_data", "dataset", "com_code", "com_suppl_code",
                                     "com_file_formats", "com_supplemental_data", "com_data_availibility")

  return(open_data_sentences)
}
