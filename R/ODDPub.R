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
#' @param extract_sentences Logical parameter. If TRUE, the sentences in which the Open Data
#' statements were detected are extracted and added to the results table as well.
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
open_data_search <- function(PDF_text_sentences, extract_sentences = TRUE)
{
  PDF_text_sentences <- furrr::future_map(PDF_text_sentences, .remove_references)

  # PDF_text_sentences <- DAS_text_sentences
  DAS_text_sentences <- PDF_text_sentences |>
    furrr::future_map(.extract_DAS)

  sentences_with_DAS <- DAS_text_sentences |>
    purrr::map_lgl(\(x) length(x) < 31) |> # It is assumed a DAS will not have more than 30 sentences
    which()

  CAS_text_sentences <- PDF_text_sentences |>
    furrr::future_map(.extract_CAS)

  DAS_CAS <- furrr::future_map2(DAS_text_sentences, CAS_text_sentences, vctrs::vec_c)
  # search for open data keywords in the full texts or DAS
  print("Extracting and screening Data and Code Availability Statements:")
  keyword_results <- .keyword_search_full(DAS_CAS)
  open_data_results <- .open_data_detection(DAS_CAS, keyword_results)

  sentences_second_pass <- open_data_results |>
    dplyr::filter(is_open_data == FALSE & is_reuse == FALSE) |> # consider second pass also for software?
    dplyr::pull(article) # extract the not open data cases for double-check in second pass

  # restrict to cases with DAS only, since full texts were already screened for rest
  sentences_second_pass <- sentences_second_pass[sentences_second_pass %in% names(sentences_with_DAS)]
  # kw <- keyword_second_pass[[1]]
  # screen full text of second-pass cases
  # do this only for subset of cases, as this is the most time-consuming step
  if (length(sentences_second_pass) > 0) {
    print("Screening full text of articles with uninformative Data Availability Statements:")
    keyword_second_pass <- .keyword_search_full(PDF_text_sentences[sentences_second_pass])
    open_data_second_pass <- .open_data_detection(PDF_text_sentences[sentences_second_pass], keyword_second_pass)
    # |>
    #   dplyr::bind_cols(open_data_cat_old = open_data_results$open_data_category) |>
    #   dplyr::mutate(open_data_category = ifelse(open_data_cat_old != "", paste0(open_data_cat_old, ", ", open_data_category),
    #                 open_data_category)) |>
    #   dplyr::select(-open_data_cat_old) # test if this is necessary (if it overwrites categories at all)

    keyword_results <- keyword_results |>
      purrr::list_assign(!!!keyword_second_pass)


    open_data_results <- open_data_results |>
      dplyr::rows_upsert(open_data_second_pass, by = "article")
  }

  #extract detected sentences as well
  if(extract_sentences == TRUE)
  {
    detected_sentences <- .open_data_sentences(PDF_text_sentences, DAS_text_sentences, sentences_with_DAS,
                                               CAS_text_sentences, keyword_results)
    open_data_results <- cbind(open_data_results, detected_sentences[, -1]) |>
      dplyr::as_tibble() |>
      dplyr::mutate(has_only_unknown = .has_url(das) & !is_open_data & !is_reuse, # check for unknown website
        open_data_category = dplyr::case_when(
          has_only_unknown & stringr::str_length(open_data_category) > 0 ~ paste0(open_data_category, ", unknown url"),
          has_only_unknown & stringr::str_length(open_data_category) == 0 ~ "unknown url",
          .default = open_data_category)) |>
      dplyr::select(-has_only_unknown)
  }

  return(open_data_results)
}

