#' Conversion of PDFs to text.
#'
#' Converts PDFs to text before the text can be loaded and text-mined.
#' For the PDFconversion requires the pdftools library.
#' for parallelisation requires the future and furrr libraries.
#'
#' @param pdf_folder String of the folder name in which the PDFs are located.
#' @param output_folder String of the folder name in which the converted files will be saved.
#' @param recursive Boolean. If TRUE (the default), then search all the subfolders of the given folder for PDF files.
#' @param overwrite_existing_files Boolean. If FALSE (the default) does not overwrite already existing files.
#' @param add_section_tags Boolean. If TRUE (the default), adds '<section>' tags at the beginning of potential text sections.
#' This needs to be set to TRUE for later recognition of Data and Code Availability Statements.
#'
#' @return Logical vector describing the conversion success for each PDF file.
#'
#' @export
pdf_convert <- function(pdf_folder, output_folder, recursive = TRUE,
                        overwrite_existing_files = FALSE, add_section_tags = TRUE) {

  #check if dir path has final /, otherwise add
  # if(pdf_folder |> stringr::str_sub(-1) != "/") {
  #   pdf_folder <- paste0(pdf_folder, "/")
  # }
  # if(output_folder |> stringr::str_sub(-1) != "/") {
  #   output_folder <- paste0(output_folder, "/")
  # }

  # pdf_filenames <- list.files(pdf_folder, recursive = recursive)
  pdf_filenames <- list.files(pdf_folder, pattern = "\\.(pdf|PDF)", recursive = recursive, full.names = TRUE)

  # converts PDF file to txt file and saves it to output_folder
  # requires the pdftools library
  # some PDFs make take a very long time to process!
  p <- progressr::progressor(along = pdf_filenames)
  conversion_success <-
    suppressWarnings(furrr::future_map_lgl(pdf_filenames, \(x) {
      p()
      .pdf_to_text(x, output_folder,
                   overwrite_existing_files = overwrite_existing_files,
                   add_section_tags = add_section_tags)
      }))
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
#' @param pdf_text_sentences Document corpus loaded with the pdf_load function.
#'
#' @param extract_sentences Boolean. If TRUE, the sentences in which the Open Data
#' statements were detected are extracted and added to the results table as well.
#'
#' @param screen_das How to screen Data availability and Code availability
#'  statement or statements (DAS and CAS). The default, "priority"
#'  screens DAS and CAS first if available and then the full text (without references)
#'  will only be screened if no open data or data re-use or github data were detected
#'  in the DAS and CAS.
#'  If set to "extra", the DAS and CAS are screened first, if available, and then
#'  the full text is also always screened. This is to allow for the detection of
#'  data re-use, open data, or open code in the main text with an uninformative
#'  DAS or CAS.
#'  With both of the above options the output generates two new columns,
#'  `is_open_data_das` and `is_open_code_cas`, which give the detections for only
#'  the DAS and CAS, respectively, as opposed to the detections based on the full
#'  screen (`is_open_data`, `is_open_code`).
#'  If set to "legacy", then the full text (without references) will be screened
#'  immediately. Caution is to be used with this setting, as the special detection
#'  algorithm for DAS is not applied here and DAS in papers with complex layouts,
#'  e.g. PLoS, will most likely cause references to data sets to be missed.
#'
#'
#' @return Tibble with one row per screened document and the filename and logical values for open data
#' and open code detection as columns plus additional columns containing the identified open data/code categories
#' as well as the sentences in which open data/code was detected.
#'
#' @examples
#' \dontrun{
#' open_data_search(pdf_load("examples/"))
#' open_data_search(pdf_load("examples/"), screen_das = "extra")
#'
#' future::plan(multisession) # add this for parallelization
#' progressr::handlers(global = TRUE) # add this for visualizing the progress
#'
#' text_corpus <- pdf_load("examples/")
#' results <- open_data_search(text_corpus)
#' }
#'
#' @export
open_data_search <- function(pdf_text_sentences, extract_sentences = TRUE, screen_das = "priority") {
# pdf_text_sentences <- pdf_text_corpus
  screen_das <- match.arg(screen_das, c("priority", "extra", "legacy"))
  is_open_data <- is_reuse <- open_data_category <- article <-
    is_open_code <- is_open_data_das <- is_open_code_cas <-
    is_code_reuse <- is_code_supplement <-
    is_code_reuse_old <- is_code_supplement_old <- NULL

  message("Removing References:\n")
  p <- progressr::progressor(along = pdf_text_sentences)
  pdf_text_sentences <- furrr::future_map(pdf_text_sentences, \(x) {
    p()
    .remove_references(x)
  })

  # pdf_text_sentences <- das_text_sentences

  ########## extract das and cas if available, if not, the full text is extracted instead
  message("Extracting Data Availability Statements:\n")
  p <- progressr::progressor(along = pdf_text_sentences)
  das_text_sentences <- pdf_text_sentences |>
    furrr::future_map(\(x) {
     p()
      .extract_cdas(x, type = "das")
    })

  # get the index of articles with das
  sentences_with_das <- das_text_sentences |>
    purrr::map_lgl(\(x) length(x) < 31 & x[1] != "") |> # It is assumed a DAS will not have more than 30 sentences
    which()

  message("Extracting Code Availability Statements:\n")
  p <- progressr::progressor(along = pdf_text_sentences)
  cas_text_sentences <- pdf_text_sentences |>
    furrr::future_map(\(x) {
      p()
      .extract_cdas(x, type = "cas")
    })

  # get the index of articles with cas
  sentences_with_cas <- cas_text_sentences |>
    purrr::map_lgl(\(x) length(x) < 31 & x[1] != "") |> # It is assumed a CAS will not have more than 30 sentences
    which()

  # get names for articles with das and also a cas to enable priority and extra screening
  # sentences_with_das_cas <- intersect(names(sentences_with_das),
  #                                     names(sentences_with_cas))

  # i_sentences_with_das_cas <- union(sentences_with_das, sentences_with_cas)

# screen_das = "extra"
  if (screen_das == "legacy") {
    # screen the full text without special treatment of das_cas
    sentences_full_screen <- rep(TRUE, length(pdf_text_sentences))
    open_data_results <- NULL

  } else {
    # when das_cas is to be screened separately
    das_cas <- furrr::future_map2(das_text_sentences, cas_text_sentences, vctrs::vec_c) |>
      furrr::future_map(unique)
    # search for open data keywords in the full texts or DAS
    message("Screening Data and Code Availability Statements:\n")
    keyword_results <- .keyword_search_full(das_cas)
    open_data_results <- .open_data_detection(das_cas, keyword_results) |>
      dplyr::mutate(is_open_data_das = ifelse(article %in% names(sentences_with_das),
                                              is_open_data, FALSE),
                    is_open_code_cas = ifelse(article %in% names(sentences_with_cas),
                                              is_open_code, FALSE))
    # kw <- keyword_results[[1]]

    if (screen_das == "priority") { # with "priority" only screen cases where no open data
      # re-use or github was detected in the das_cas
      sentences_full_screen <- open_data_results |>
        dplyr::filter(is_open_data == FALSE & is_reuse == FALSE &
                        !stringr::str_detect(open_data_category, "github")) |>
        dplyr::pull(article) # extract the not open data cases for double-check in second pass

      # sentences_full_screen <- sentences_second_pass[sentences_second_pass %in% names(sentences_with_das)]
    } else { # with "extra", screen all articles again, this time in full
      sentences_full_screen <- rep(TRUE, length(pdf_text_sentences))

    }
  }

  # screen full text of second-pass cases

  if (length(sentences_full_screen) > 0) {
    message("\nScreening full text of articles:\n")

    keyword_full_screen <- .keyword_search_full(pdf_text_sentences[sentences_full_screen])
    open_data_full_screen <- .open_data_detection(pdf_text_sentences[sentences_full_screen], keyword_full_screen)

    if (!rlang::is_null(open_data_results)) {
      open_data_cat_old <- open_data_results |>
        dplyr::select(article, open_data_cat_old = open_data_category,
                      is_code_reuse_old = is_code_reuse, is_code_supplement_old = is_code_supplement)
      open_data_full_screen <- open_data_full_screen |>
        dplyr::left_join(open_data_cat_old, by = "article") |>
        dplyr::mutate(open_data_category =
                        dplyr::case_when(
                          open_data_cat_old != "" & open_data_category != "" ~
                            paste0(open_data_cat_old, ", ", open_data_category),
                          .default = paste0(open_data_cat_old, open_data_category)),
                      open_data_category = purrr::map_chr(open_data_category, .remove_repeats),
                      is_code_reuse = is_code_reuse_old | is_code_reuse,
                      is_code_supplement = is_code_supplement_old | is_code_supplement) |>
        dplyr::select(-dplyr::contains("_old"))

      keyword_results <- keyword_results |>
        purrr::list_assign(!!!keyword_full_screen)

      open_data_results <- open_data_results |>
        dplyr::rows_upsert(open_data_full_screen, by = "article") |>
        dplyr::mutate(is_open_data = is_open_data | is_open_data_das,
                      is_open_code = is_open_code | is_open_code_cas)
    } else {
      open_data_results <- open_data_full_screen
    }

  }
  message("\nConsolidating data:\n")

  #extract detected sentences as well
  if(extract_sentences == TRUE) {
    detected_sentences <- .open_data_sentences(pdf_text_sentences, das_text_sentences,
                                               sentences_with_das,
                                               cas_text_sentences, keyword_results)
    open_data_results <- cbind(open_data_results, detected_sentences[, -1]) |>
      dplyr::as_tibble()
  }

  return(open_data_results)
}