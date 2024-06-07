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
pdf_convert <- function(PDF_folder, output_folder, recursive = TRUE)
{

  #check if dir path has final /, otherwise add
  if(PDF_folder |> stringr::str_sub(-1) != "/") {
    PDF_folder <- paste0(PDF_folder, "/")
  }
  if(output_folder |> stringr::str_sub(-1) != "/") {
    output_folder <- paste0(output_folder, "/")
  }

  # PDF_filenames <- list.files(PDF_folder, recursive = recursive)
  PDF_filenames <- list.files(PDF_folder, recursive = recursive, full.names = TRUE)

  # converts PDF file to txt file and saves it to output_folder
  # requires the pdftools library
  # some PDFs make take a very long time to process!
  conversion_success <-
    suppressWarnings(furrr::future_map_lgl(PDF_filenames,
                          \(x) .pdf_to_text(x, output_folder), .progress = TRUE))

  return(conversion_success)
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
#' @param extract_sentences Boolean. If TRUE, the sentences in which the Open Data
#' statements were detected are extracted and added to the results table as well.
#'
#' @param stop_if_hit_in_DAS Boolean. If TRUE, the default setting, then first
#'  the Data availability and Code availability statement or statements (DAS and CAS)
#'  will be screened and then the full text (without references) will only be screened
#'  if no open data or data re-use or github data were detected in the DAS and CAS.
#'  If FALSE, then the full text (without references) will be screened immediately.
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
open_data_search <- function(PDF_text_sentences, extract_sentences = TRUE, stop_if_hit_in_DAS = TRUE)
{
  PDF_text_sentences <- furrr::future_map(PDF_text_sentences, .remove_references)

  # PDF_text_sentences <- DAS_text_sentences
  DAS_text_sentences <- PDF_text_sentences |>
    furrr::future_map(.extract_DAS)

  sentences_with_DAS <- DAS_text_sentences |>
    purrr::imap_lgl(\(x, idx) length(x) < 31 & length(x) != length(PDF_text_sentences[[idx]])) |> # It is assumed a DAS will not have more than 30 sentences
    which()

  CAS_text_sentences <- PDF_text_sentences |>
    furrr::future_map(.extract_CAS)

  if (stop_if_hit_in_DAS == TRUE) {
    DAS_CAS <- furrr::future_map2(DAS_text_sentences, CAS_text_sentences, vctrs::vec_c) |>
      furrr::future_map(unique)
    # search for open data keywords in the full texts or DAS
    print("Screening Data and Code Availability Statements:")
    keyword_results <- .keyword_search_full(DAS_CAS)
    open_data_results <- .open_data_detection(DAS_CAS, keyword_results)

    # kw <- keyword_results[[1]]
    sentences_second_pass <- open_data_results |>
      dplyr::filter(is_open_data == FALSE & is_reuse == FALSE &
                      !stringr::str_detect(open_data_category, "github")) |> # consider second pass also for software?
      dplyr::pull(article) # extract the not open data cases for double-check in second pass
    # restrict to cases with DAS only, since full texts were already screened for rest
    sentences_full_screen <- sentences_second_pass[sentences_second_pass %in% names(sentences_with_DAS)]
  } else {
    sentences_full_screen <- rep(TRUE, length(PDF_text_sentences))
    open_data_results <- NULL
  }
  # screen full text of second-pass cases
  # do this only for subset of cases, as this is the most time-consuming step
  if (length(sentences_full_screen) > 0) {
    print("Screening full text of articles:")

    keyword_full_screen <- .keyword_search_full(PDF_text_sentences[sentences_full_screen])
    open_data_full_screen <- .open_data_detection(PDF_text_sentences[sentences_full_screen], keyword_full_screen)

    if (!rlang::is_null(open_data_results)) {
      open_data_cat_old <- open_data_results |>
        dplyr::select(article, open_data_cat_old = open_data_category)
      open_data_full_screen <- open_data_full_screen |>
        dplyr::left_join(open_data_cat_old, by = "article") |>
        dplyr::mutate(open_data_category =
                        dplyr::case_when(
                          open_data_cat_old != "" & open_data_category != "" ~ paste0(open_data_cat_old, ", ", open_data_category),
                          .default = paste0(open_data_cat_old, open_data_category)),
                      open_data_category = purrr::map_chr(open_data_category, .remove_repeats)) |>
        dplyr::select(-open_data_cat_old)

      keyword_results <- keyword_results |>
        purrr::list_assign(!!!keyword_full_screen)

      open_data_results <- open_data_results |>
        dplyr::rows_upsert(open_data_full_screen, by = "article")
    } else {
      open_data_results <- open_data_full_screen
    }

  }
  print("Consolidating data:")

  #extract detected sentences as well
  if(extract_sentences == TRUE)
  {
    detected_sentences <- .open_data_sentences(PDF_text_sentences, DAS_text_sentences, sentences_with_DAS,
                                               CAS_text_sentences, keyword_results)
    open_data_results <- cbind(open_data_results, detected_sentences[, -1]) |>
      dplyr::as_tibble()
    # |>
    #   dplyr::mutate(has_only_unknown = .has_url(das) & !is_open_data & !is_reuse, # check for unknown website
    #     open_data_category = dplyr::case_when(
    #       has_only_unknown & stringr::str_length(open_data_category) > 0 ~ paste0(open_data_category, ", unknown url/accnr"),
    #       has_only_unknown & stringr::str_length(open_data_category) == 0 ~ "unknown url/accnr",
    #       .default = open_data_category)) |>
    #   dplyr::select(-has_only_unknown)
  }

  return(open_data_results)
}


