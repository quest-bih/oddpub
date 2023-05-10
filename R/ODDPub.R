#' Conversion of PDFs to text.
#'
#' Converts PDFs to text before the text can be loaded and text-mined.
#' For the PDFconversion requires the pdftools library.
#' for parallelisation requires the future and furrr libraries.
#'
#' @param PDF_folder String of the folder name in which the PDFs are located.
#' @param output_folder String of the folder name in which the converted files will be saved.
#'
#' @return Logical vector describing the conversion success for each PDF file.
#'
#' @export
pdf_convert <- function(PDF_folder, output_folder)
{

  #check if dir path has final /, otherwise add
  if(PDF_folder |> stringr::str_sub(-1) != "/") {
    PDF_folder <- paste0(PDF_folder, "/")
  }
  if(output_folder |> stringr::str_sub(-1) != "/") {
    output_folder <- paste0(output_folder, "/")
  }

  PDF_filenames <- list.files(PDF_folder, recursive = TRUE)
  PDF_filenames <- paste0(PDF_folder, PDF_filenames)

  # converts PDF file to txt file and saves it to output_folder
  # requires the pdftools library
  # some PDFs make take a very long time to process!
  conversion_success <-
    furrr::future_map_lgl(PDF_filenames,
                          \(x) .pdf_to_text(x, output_folder), .progress = TRUE)

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
#' @examples
#' \dontrun{
#' pdf_load("examples/")
#' }
#' @export
pdf_load <- function(pdf_text_folder)
{

  # read in full text from .txt files
  txt_filenames <- list.files(pdf_text_folder)
  txt_filenames <- txt_filenames[!stringr::str_detect(txt_filenames,stringr::fixed(".pdf"))]
  txt_filenames_short <- txt_filenames # used as names for the text/results tables later
  txt_filenames <- paste0(pdf_text_folder, txt_filenames)

  # produce version of the full texts where all sentences are separate vector elements
  PDF_text_sentences <- txt_filenames |>
    furrr::future_map(.tokenize_sections, .progress = TRUE)

  names(PDF_text_sentences) <- txt_filenames_short

  return(PDF_text_sentences)
}


#' Seach for open data & open code keywords.
#'
#' The algorithm first searchers for a Data Availability Statement (DAS) and extracts it.
#' If no DAS was found, the algorithm searches for several categories of similar keywords
#' in each sentence. Multiple categories have to match for a single sentence to trigger a detection.
#' Among keyword categories are categories for specific biomedical databases as well as
#' their corresponding accession numbers (as regular expressions), general-purpose repositories
#' or different file formats typically used to distribute raw data in the supplement.
#' Additionally, Open Code dissemination is detected using keywords categories for source code or
#' code repositories.
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
#' @examples
#' \dontrun{
#' open_data_search(pdf_load("examples/"))
#' }
#'
#' @export
open_data_search <- function(PDF_text_sentences, detected_sentences = TRUE)
{

  PDF_text_sentences <- PDF_text_sentences |>
    furrr::future_map(.extract_DAS)


  # PDF_data_sentences <- PDF_text_sentences |>
  #   furrr::future_map(.extract_DAS)

  # PDF_code_sentences <- PDF_text_sentences |>
  #   furrr::future_map(.extract_CAS)



  # PDF_text_sentences <- c(PDF_data_sentences, PDF_code_sentences)
  # extract a data availability statement and replace full text with it,
  # otherwise take full text

  # search for open data keywords in the full texts or DAS
  # do this only once, as this is the most time consuming step
  keyword_results <- .keyword_search_full(PDF_text_sentences)
  open_data_results <- .open_data_detection(PDF_text_sentences, keyword_results)

  #extract detected sentences as well
  if(detected_sentences)
  {
    detected_sentences <- .open_data_sentences(PDF_text_sentences, keyword_results)
    open_data_results <- cbind(open_data_results, detected_sentences[, -1]) |>
      dplyr::as_tibble()
  }

  return(open_data_results)
}
