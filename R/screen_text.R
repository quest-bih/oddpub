
# Open data identification keywords
#--------------------------------------------------------------------------------------

#' Search for several categories of similar keywords in a sentence.
#' Multiple categories must match to trigger a detection.
#' @noRd
.create_keyword_list <- function()
{
  # read regular expression dictionaries from yaml file
  yaml_path <- system.file("extdata", "keywords_patterns.yaml", package = "oddpub")
  keyword_list <- yaml::read_yaml(file.path(yaml_path))
  # add conditional formating for some dictionaries
  keyword_list <- keyword_list |>
    purrr::map2(names(keyword_list), \(x, y) switch(y,
      "field_specific_repo" = .format_keyword_vector(x, end_boundary = TRUE),
      "data" = .format_keyword_vector(x, end_boundary = TRUE),
      "file_formats" = .format_keyword_vector(x, end_boundary = TRUE),
      "github" = .format_keyword_vector(x, end_boundary = TRUE),
      "ownership_claim" = .format_keyword_vector(x, end_boundary = TRUE),
      "supplemental_table" = x,
      "supplemental_table_number" = x,
      "weblink" = x,
      "dataset_name" = x,
      "dataset_number" = x,
      "supplemental_dataset" = x,
      .format_keyword_vector(x)
    ))
  # add complex categories
  keyword_list[["reuse"]] <- paste(keyword_list$reuse_statements,
                                   .near_wd_sym(keyword_list$was_available,
                                                paste(
                                                  keyword_list$field_specific_repo,
                                                  keyword_list$repositories,
                                                  keyword_list$github,
                                                  sep = "|"),
                                                dist = 30, add_boundary = FALSE),
                                   sep = "|")

  keyword_list[["supplemental_table"]] <-
    .outer_str(keyword_list$supplemental_table, keyword_list$supplemental_table_number) |>
    .format_keyword_vector(end_boundary = TRUE)

  dataset <- .outer_str(keyword_list$dataset_name, keyword_list$dataset_number)
  keyword_list[["dataset"]] <- c(dataset, keyword_list$supplemental_dataset) |>
    .format_keyword_vector(end_boundary = TRUE)

  keyword_list[["all_data_file_formats"]] <- .near_wd_sym(keyword_list$all_data,
                                                          keyword_list$file_formats,
                                                          dist = 12)

  keyword_list[["supp_table_data"]] <- .near_wd_sym(keyword_list$supplemental_table,
                                                    paste(keyword_list$file_formats,
                                                          keyword_list$all_data,
                                                          sep = "|"),
                                                    dist = 12)

  return(keyword_list)
}

#' standardize the format of different keyword vectors
#' @noRd
.format_keyword_vector <- function(keywords, end_boundary = FALSE) {
  #typically word boundaries are added in the beginning only to allow for different possible endings
  if(end_boundary) {
    keywords_formatted <- paste0("\\b", keywords, "\\b")
  } else {
    keywords_formatted <- paste0("\\b", keywords)
  }
  #collapse keywords into one string with OR symbol between them and convert to lowercase
  keywords_formatted <- paste(keywords_formatted, collapse = "|") |> tolower()

  return(keywords_formatted)
}


# return all the pasted combinations of two strings coming from different vectors
# used to make all possible text combinations of two related keyword categories
#' @noRd
.outer_str <- function(x, y)
{
  outer_1 <- outer(x, y, FUN = "paste") |> as.vector()
  outer_2 <- outer(y, x, FUN = "paste") |> as.vector()
  outer_sym <- c(outer_1, outer_2)

  return(outer_sym)
}


#' create Regex that searches for cases where words x and y are at max dist words apart
#' @noRd
.near_wd_sym <- function(x, y, dist = 10, add_boundary = TRUE)
{
  if (add_boundary == TRUE) {
    boundary <- "\\b"
  } else {
    boundary <- ""
  }
  combined <- paste0(boundary, "(",
                     x,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     y,
                     ")", boundary, "|", boundary, "(",
                     y,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     x,
                     ")", boundary)

  return(combined)
}

#' create Regex that searches for cases where words x and y are at max dist words apart,
#' assymetric version where only the case with x before y is checked
#' @noRd
.near_wd <- function(x, y, dist = 10)
{
  # combined <- paste0("\\b(",
  combined <- paste0("(",
                     x,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     y,
                     ")\\b")

  return(combined)
}

#---------------------------------------------------------------------
# 4 - Open data identification
#---------------------------------------------------------------------

# identify the text fragments in which the Open Data keywords were detected
#' @noRd
.text_fragments <- function(open_data_tibble) {

  tibble_cols <- dim(open_data_tibble)[2]
  sentence_idx <- apply(open_data_tibble[2:tibble_cols], 2, which)

  if(length(sentence_idx) == 0) {
    return(rep("", dim(open_data_tibble)[2]-1))
  }

  text_frag <- vector()
  for(idx in sentence_idx)
  {
    if(length(idx) == 0) {
      text_frag <- c(text_frag, "")
    } else {
      current_txt <- open_data_tibble[[1]][idx] |> paste(collapse = ";     ")
      text_frag <- c(text_frag, current_txt)
    }
  }

  return(text_frag)
}

# publ_sentences <- "data availability source data are provided with this paper."
# publ_sentences <- pdf_text_sentences[[1]]
# publ_sentences <- DAS

#' apply all keyword category searches onto each sentence of the publications
#' @noRd
.map_keywords <- function(publ_sentences)
{
  keyword_list <- .create_keyword_list()

  # search for all relevant keyword categories
  # not all keyword categories are used for the sentence search
  sentence_search_keywords <- c("available", "was_available", "not_available",
                                "field_specific_repo", "accession_nr", "repositories",
                                "github", "data", "all_data",
                                "not_data", "source_code", "supplement",
                                "reuse", "software_use",  "ownership_claim", "grant",
                                "file_formats", "upon_request", "dataset", "protocol", "weblink", "misc_non_data")

  # search for all relevant keyword categories
  publ_keywords <- sentence_search_keywords  |>
    furrr::future_map(.search_keyword_cat, publ_sentences, keyword_list)
  names(publ_keywords) <- sentence_search_keywords

  #gather results
  publ_keywords <- do.call(cbind, publ_keywords) |>
    dplyr::as_tibble()
  publ_keywords <- cbind(publ_sentences, publ_keywords) |>
    dplyr::as_tibble()
  publ_keywords$publ_sentences <- as.character(publ_keywords$publ_sentences)

  return(publ_keywords)
}
# sentences <- publ_sentences
# sentences <- paste("data availability", publ_sentences) |> tolower()
# keyword <- "repositories"
# keyword <- "source_code"
#' search for all keyword categories in the sentences
#' @noRd
.search_keyword_cat <- function(keyword, sentences, keyword_list)
{
  detection_col <- sentences |>
    furrr::future_map_lgl(stringr::str_detect, pattern = keyword_list[[keyword]])

  return(detection_col)
}
# sections_v <- pdf_text_sentences[das_start:das_end]
# sections_v <- DAS
# #'Splice sentences from e.g. DAS that were separated due to interpolated
# #' sections in PLoS-formatted articles.
# #'@param sections_v Character vector of sentences that may contain
# #'interpolated sentences from abstract or other sections.
# #' @returns Character vector with sentences with interpolated sentences
# #' removed.
# #'
# #' @export
# splice_plos_twopager <- function(sections_v) {
#
#   # needs properly placed section tags in order to work!
#   # if (any(stringr::str_detect(sections_v, "<section> plos"))) {
#   sections <- stringr::str_detect(sections_v, "^(<section>|##+) ")
#
#   if (sum(sections) == 1) return(sections_v)
#
#   splice_start_piece <- which(sections)[2] - 1
#   splice_end_piece <- sections[length(sections):1] |> which.max() # invert vector to find last occurrence of section
#   splice_end_piece <- length(sections) - splice_end_piece + 1 # last occurence of section
#
#   return(
#     c(sections_v[1:splice_start_piece],
#       sections_v[splice_end_piece:length(sections_v)])
#   )
# }

# sentence <- pdf_text_sentences[927]
#' test if text contains data availability statement
#' @noRd
.has_das <- function(sentence, keyword_list) {

  data_availability <- keyword_list$data_availability

  data_availability <- paste0("(<section>|##+)\\W+[\\d,\\W]*(", "(<margin> )?", data_availability, ")\\b")
  stringr::str_detect(sentence, data_availability)
}

# sentence <- "<section> data availability"
# .has_das(sentence)

#' extract data and or code availability statement
#' @noRd
.extract_cdas <- function(pdf_text_sentences, type = "das") {

  text <- NULL

  type <- match.arg(type, c("das", "cas"))

  keyword_list <- .create_keyword_list()

  pdf_text_tib <- splice_margin_text(pdf_text_sentences) |>
    dplyr::mutate(text = stringr::str_remove_all(text, "\\s+<(margin|mend)>"))

  pdf_text_sentences <- pdf_text_tib$text

  if (type == "das") {
    cdas_detections <- purrr::map_lgl(pdf_text_sentences,
                                     \(sentence) stringr::str_detect(sentence, keyword_list$data_availability))

    if (sum(cdas_detections) > 0) {
      cdas_detections <- purrr::map_lgl(pdf_text_sentences,
                                       \(sentence) .has_das(sentence, keyword_list))
    }
    keyword_list$section_stopwords <- paste0(keyword_list$section_stopwords, "|\\bcode availability")

  } else {

    cdas_detections <- furrr::future_map_lgl(pdf_text_sentences,
                                            \(sentence) stringr::str_detect(sentence, keyword_list$code_availability))
  }

  cdas_start <- which(cdas_detections)

  if (length(cdas_start) == 2) {
    # select detection with statement
    statement_detections <- pdf_text_sentences[cdas_start] |>
      stringr::str_detect("statement|availability of data(?! from)")
    # select detection without "data
    quotation_detections <- !stringr::str_detect(pdf_text_sentences[cdas_start], '\\"data')
    if (sum(quotation_detections) == 1) {
      cdas_start <- cdas_start[quotation_detections] # select detection without "data
    } else if (sum(statement_detections) == 1) {
      cdas_start <- cdas_start[statement_detections] # select detection with statement
    } else {
      cdas_start <- min(cdas_start)
    }

    # if more than two detections of CDAS were made, then return the last one
  } else if (length(cdas_start) > 2) {
    cdas_start <- max(cdas_start)
  } else if (length(cdas_start) == 0) {
    # return empty string if no CDAS was detected
    return("")
  }

  # cdas_str <- pdf_text_sentences[cdas_start] |>
  #   stringr::str_trim()
  # cdas_str_sameline <- cdas_str |>
  #   stringr::str_remove(keyword_list$data_availability) |>
  #   stringr::str_remove(keyword_list$code_availability) |>
  #   stringr::str_remove("(<section>|##+) ")
  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  cdas_end_candidates <- purrr::map_lgl(pdf_text_sentences[(cdas_start + 1):length(pdf_text_sentences)],
                                         \(sentence) stringr::str_detect(sentence, "((section|insert|iend)>|#+) (?!d )|^\\d\\.") &
                                         stringr::str_detect(sentence, keyword_list$section_stopwords)
                                        # &
                                        #   !stringr::str_detect(sentence, "data")&
                                        #   !stringr::str_detect(sentence, keyword_list$source_code)
                                        ) |>
    which() - 1

  # if (length(pdf_text_sentences) - das_start <= 2) return(pdf_text_sentences[das_start:length(pdf_text_sentences)])
  if (
    length(cdas_end_candidates) == 0 |
    # (length(das_end_candidates) == 1 & das_end_candidates[1] == 0) |
    purrr::is_empty(cdas_end_candidates)
      ) return(pdf_text_sentences[cdas_start:length(pdf_text_sentences)])

  # check if candidates are full sentences ending in full stop. This achieves splicing if section continues on next page
  completed_sentences <- purrr::map_lgl(pdf_text_sentences[cdas_start + cdas_end_candidates],
                                               \(sentence) stringr::str_detect(sentence, "(?<!www)\\..?$"))

  # if (stringr::str_length(str_das_sameline) < 5 & str_das_sameline != ".") {
  #   # first_sentence <- das_start + 1
  #
  #   das_end <- das_end_candidates[-1][completed_sentences[-1]][1]#
  #
  # } else {
  cdas_end <- cdas_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section

  # if (cdas_start / length(cdas_detections) < 0.1 & cdas_end != 0) { # for plos journals with DAS on first page
  #
  #     cdas_second_part <- furrr::future_map_lgl(pdf_text_sentences[(cdas_start + cdas_end + 1):length(pdf_text_sentences)],
  #                                              \(sentence) stringr::str_detect(sentence, "(<section>|##+) funding:"))
  #
  #     if (sum(cdas_second_part) == 0) {
  #       cdas_end <- cdas_end
  #       } else {
  #         cdas_end <- cdas_end + which(cdas_second_part) - 1
  #       }
  #   }
  # }
  #
  if (is.na(cdas_end)) {

    if (!any(completed_sentences) | is.na(completed_sentences)[1]) {
      cdas_end <- length(pdf_text_sentences) - cdas_start
    } else {
      cdas_end <- min(cdas_end_candidates[cdas_end_candidates > 0], length(pdf_text_sentences) - cdas_start, na.rm = TRUE)
    }

  }

  if ((cdas_start + cdas_end) > length(pdf_text_sentences)) {
    cdas_end <- cdas_start
  }

  cdas_end <- cdas_start + cdas_end

  CDAS <- pdf_text_sentences[cdas_start:cdas_end]

  # if (cdas_start < 50 & any(stringr::str_detect(pdf_text_sentences[1:10], "plos"), na.rm = TRUE)) {
  #   CDAS <- splice_plos_twopager(CDAS)
  # }
   CDAS |>
     stats::na.omit() |>
     paste(collapse = " ") |>
     stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
     stringr::str_trim() |>
     stringr::str_remove_all(" (<section>|##+)") |> # remove interim tags
     stringr::str_replace("(?<=repository)\\. ", ": ") |>  # for the weird cases when after repository a . and not : follows
     stringr::str_replace("(?<=were analy(z|s)ed in this study)\\.", ":") |>  # for the standard phrasing of data re-use
     tokenizers::tokenize_regex(pattern = "(?<=\\.) ", simplify = TRUE) |> # tokenize sentences
     .correct_tokenization()

}

#' #' extract code availability statement
#' #' @noRd
# .extract_cas <- function(pdf_text_sentences) {
#
#   keyword_list <- .create_keyword_list()
#
#   # code_availability <- keyword_list[["code_availability"]]
#
#   cas_detections <- furrr::future_map_lgl(pdf_text_sentences,
#                                           \(sentence) stringr::str_detect(sentence, keyword_list$code_availability))
#
#   cas_start <- which(cas_detections)
#
#   if (length(cas_start) > 2) {
#     cas_start <- max(cas_start)
#   } else if (length(cas_start) != 1 ) {
#     return("")
#   }
#
#   str_cas <- pdf_text_sentences[cas_start] |>
#     stringr::str_trim()
#   str_cas_sameline <- str_cas |>
#     stringr::str_remove(keyword_list$code_availability) |>
#     stringr::str_remove("(<section>|##+) ")
#
#
#   # candidates are sentences after the first section but before the next
#   # which begin with <section> or digit. (reference number at start of line)
#   cas_end_candidates <- furrr::future_map_lgl(pdf_text_sentences[(cas_start + 1):length(pdf_text_sentences)],
#                                               \(sentence) stringr::str_detect(sentence, "(section>|##+) (?!d )|^\\d\\.|\\u2750") &
#                                                 stringr::str_detect(sentence, keyword_list$section_stopwords) &
#                                                 !stringr::str_detect(sentence, keyword_list$source_code)) |>
#     which() - 1
#   # check if candidates are full sentences ending in full stop. This achieves splicing if section continues on next page
#   completed_sentences <- furrr::future_map_lgl(pdf_text_sentences[cas_start + cas_end_candidates],
#                                                \(sentence) stringr::str_detect(sentence, "(?<!www)\\..?$"))
#
#   if (stringr::str_length(str_cas_sameline) < 5) {
#     # first_sentence <- das_start + 1
#
#     cas_end <- cas_end_candidates[-1][completed_sentences[-1]][1]#
#
#   } else {
#     cas_end <- cas_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section
#   }
#
#   if (is.na(cas_end)) {
#     cas_end <- min(cas_end_candidates)
#
#     if (!any(completed_sentences) | is.na(completed_sentences)[1]) {
#       cas_end <- length(pdf_text_sentences) - cas_start
#     } else {
#       cas_end <- min(cas_end_candidates[cas_end_candidates > 0], length(pdf_text_sentences) - cas_start)
#     }
#   }
#
#   if (cas_start + cas_end > length(pdf_text_sentences)) {
#     cas_end <- length(pdf_text_sentences) - cas_start
#   }
#
#   cas_end <- cas_start + cas_end
#
#   pdf_text_sentences[cas_start:cas_end] |>
#     stats::na.omit() |>
#     paste(collapse = " ") |>
#     stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
#     stringr::str_trim() |>
#     stringr::str_remove_all(" (<section>|##+)") |>
#     tokenizers::tokenize_regex(pattern = "(?<=\\.) ", simplify = TRUE) |> # tokenize sentences
#     .correct_tokenization()
#
# }

#' @noRd
.remove_references <- function(pdf_text_sentences) {

  line_before_refs <- pdf_text_sentences |>
    stringr::str_detect("(<section>|##+) r ?e ?f ?e ?r ?e ?n ?c ?e ?s(?! and notes)") |>
    which() - 1

  if (length(line_before_refs) > 1) line_before_refs <- line_before_refs[length(line_before_refs)]

  if (sum(line_before_refs) == 0) {
    # if no references detected
    return(pdf_text_sentences)
  }

  # for journals that print useful information after the references (Elsevier, Science, etc.)
  line_after_refs <- suppressWarnings(
    pdf_text_sentences |>
      stringr::str_detect("(<section>|##+) (star\\+methods|open data)|<insert> key resources table") |>
      which() |>
      max()
  )
  if (sum(line_after_refs) <= 0) {
    line_after_refs <- suppressWarnings(
      pdf_text_sentences[line_before_refs:length(pdf_text_sentences)] |>
      stringr::str_detect("(<section>|##+) ((methods$)|acknowledge?ments:?|additional|author)") |>
      which() |>
      min()
    )
    line_after_refs <- line_after_refs + line_before_refs - 1
  }

  if (sum(line_after_refs) <= 0 | is.infinite(line_after_refs)) line_after_refs <- 0

  # excise references for special case Elsevier journals
  if (line_after_refs > line_before_refs) {
    return(c(pdf_text_sentences[1:line_before_refs],
             pdf_text_sentences[line_after_refs:length(pdf_text_sentences)]))
  }
  # excise references for most journals
  pdf_text_sentences[1:line_before_refs]

}
# kw <- keyword_results_combined[[1]]
#'
#' @noRd
.keyword_search_full <- function(pdf_text_sentences)
{

  data <- grant <- weblink <- reuse <- available <- not_available <-
    was_available <- misc_non_data <- field_specific_repo <- accession_nr <-
    repositories <- protocol <- supplement <-
    source_code <- software_use <- github <- ownership_claim <-
    upon_request <- publ_sentences <- com_general_repo <- com_specific_repo <-
    com_github_data <- dataset <- com_code <- com_suppl_code <- com_reuse <-
    com_request <- com_unknown_source <- com_code_reuse <- NULL

# odc <- open_data_categories[[1]]
  # pdf_text_sentences <- publ_sentences
  # search for open data keywords in the full texts
  p <- progressr::progressor(along = pdf_text_sentences)
  open_data_categories <- furrr::future_map(pdf_text_sentences, \(x) {
    p()
    .map_keywords(x)
  })

  # combine columns for the different open data keywords
  keyword_results_combined <- open_data_categories  |>
    purrr::map(dplyr::mutate, com_specific_repo =
                 field_specific_repo &
                 (accession_nr | weblink) & available &
                 !not_available & !was_available & (!reuse | ownership_claim) &
                 ((!misc_non_data & !protocol & !supplement & !source_code & !grant) | data)
               )|>
    purrr::map(dplyr::mutate, com_general_repo = repositories & available &
                 !not_available & !was_available & (!reuse | ownership_claim) &
                 (!misc_non_data & !protocol & !supplement & !source_code | data)) |>
    purrr::map(dplyr::mutate, com_github_data = data & github & available &
                 !not_available & !was_available) |>
    purrr::map(dplyr::mutate, com_code = source_code &
                 !not_available & ((!was_available & !reuse & !software_use) | ownership_claim) &
                 ((!upon_request & !supplement & !dataset & available)|stringr::str_detect(publ_sentences, "www|http")|github)) |>
    purrr::map(dplyr::mutate, com_suppl_code = source_code & (supplement | dataset)) |>
    purrr::map(dplyr::mutate, com_reuse = reuse &
                 ((!misc_non_data & !protocol & !supplement & !grant & !source_code) | data)) |>
    purrr::map(dplyr::mutate, com_code_reuse = (reuse | software_use) & source_code &
                 (!not_available & (available | stringr::str_detect(publ_sentences, "www|http")|github))) |>
    purrr::map(dplyr::mutate, com_request = upon_request) |>
    purrr::map(dplyr::mutate, com_n_weblinks = stringr::str_count(publ_sentences, "www|http")) |>
    purrr::map(dplyr::mutate, com_unknown_source = dplyr::case_when(
      com_n_weblinks > 1 ~ data & available & weblink &
        !not_available & !accession_nr & !com_general_repo & !reuse & !com_specific_repo, # maybe exclude supplements here as well
      .default = data & available & weblink &
        !not_available & !accession_nr & !com_general_repo & !reuse &
        !com_specific_repo & !com_github_data & !supplement & !misc_non_data
    )) |>
    purrr::map(dplyr::select, publ_sentences, com_specific_repo, com_general_repo,
                com_github_data, dataset, com_code, com_suppl_code, com_reuse, com_code_reuse, com_request, com_unknown_source)

  return(keyword_results_combined)
}

#' keyword search on the tokenized sentences
#' @noRd
.keyword_search_tokenized <- function(keyword_results_combined)
{
  publ_sentences <- NULL
  # summarize results over all sentences of each publication to see if any match was found for each keyword category
  keyword_results_tokenized <- keyword_results_combined |>
    purrr::map(\(x) dplyr::select(x, -publ_sentences)) |>
    purrr::map(\(x) apply(x, MARGIN = 2, FUN = any))
  keyword_results_tokenized <- do.call(rbind, keyword_results_tokenized) |>
    tibble::as_tibble()

  return(keyword_results_tokenized)
}


#' search for nearby words on the full text, part of the keyword search
#' @noRd
.keyword_search_near_wd <- function(pdf_text_sentences, extract_text = FALSE)
{
  keyword_list <- .create_keyword_list()

  # needs text input in two formats: split into sentences, and one long string for
  pdf_text_full <- pdf_text_sentences |>
    furrr::future_map_if(.p = \(sentence) length(sentence) > 1,
                         .f = \(sentence) paste(sentence, collapse = " "))

  # search for the last combination in the non-tokenized text and add it to the results table for the publications
  # either give out TRUE/FALSE or return detected string
  if (extract_text) {
    str_function <- stringr::str_extract
    map_function <- furrr::future_map_chr
  } else {
    str_function <- stringr::str_detect
    map_function <- furrr::future_map_lgl
  }
  keyword_results_near_wd <- dplyr::tibble(
    com_file_formats = map_function(pdf_text_full, str_function,
                                    pattern = keyword_list[["all_data_file_formats"]]),
    com_supplemental_data = map_function(pdf_text_full, str_function,
                                         pattern = keyword_list[["supp_table_data"]])
    )

  return(keyword_results_near_wd)
}


#' helper function for the testing of the keywords with testthat
#' @noRd
.detect_keywords <- function(string, keyword_category)
{
  keywords <- .create_keyword_list()

  string <- tolower(string)
  detected <- stringr::str_detect(string, keywords[[keyword_category]])

  return(detected)
}


#' additional Open Data check: check if PDF DOI stems from a list of Data Journals
#' requires that the PDF filename is the DOI
#' @noRd
.check_journal_doi <- function(pdf_text_sentences)
{
  keyword_list <- .create_keyword_list()

  dois <- names(pdf_text_sentences) |>
    stringr::str_replace_all(stringr::fixed("+"), stringr::fixed("/")) |>
    stringr::str_remove(stringr::fixed(".txt"))

  data_journal_doi <- dplyr::tibble(
    is_data_journal = purrr::map_lgl(dois, stringr::str_detect, pattern = keyword_list[["data_journal_dois"]]))

  return(data_journal_doi)

}


#' assign the detected Open Data cases to certain categories depending
#' on the keyword category in which it was found
#' @noRd
.OD_category <- function(specific_repo,
                         general_repo,
                         suppl,
                         reuse,
                         request,
                         github,
                         unknown_source,
                         data_journal
                         )
{
  category = vector()

  # should give out several categories
  # if there are multipe matches
  if (specific_repo == TRUE) {
    category <- category |> c("field-specific repository")
  }
  if (general_repo == TRUE) {
    category <- category |> c("general-purpose repository")
  }
  if (suppl == TRUE) {
    category <- category |> c("supplement")
  }
  if (reuse == TRUE) {
    category <- category |> c("re-use")
  }
  if (request == TRUE) {
    category <- category |> c("upon request")
  }
  if (github == TRUE) {
    category <- category |> c("github")
  }
  if (unknown_source == TRUE) {
    category <- category |> c("unknown/misspecified source")
  }
  if (data_journal == TRUE) {
    category <- category |> c("data journal")
  }
  category <- category |> paste(collapse = ", ")

  return(category)
}

#' check if data availability statement (das) contains a url that is not a git url
#' @noRd
.has_url <- function(das){
  stringr::str_detect(das, "(((https?|ftp|smtp):\\/\\/(?! ?git)|(www\\.?))[a-z0-9]+\\.[a-z ]+(\\/[a-z0-9#]+\\/?)*)|(?<!git(hub|lab)?)\\.(com|org)")
}

#---------------------------------------------------------------------
# 5 - Combine search steps to obtain Open Data status &
#     detected sentences
#---------------------------------------------------------------------

#' @noRd
.open_data_detection <- function(pdf_text_sentences, keyword_results) {

  com_general_repo <- com_specific_repo <- is_data_journal <- com_code <-
    com_suppl_code <- com_code_reuse <- dataset <- com_file_formats <- com_reuse  <-
    com_code_reuse <- com_supplemental_data <- com_request <- com_github_data <-
    com_unknown_source <- article <- is_general_purpose <- is_supplement <-
    is_reuse <- is_open_data <- is_open_code <- open_data_category <- is_code_reuse <-
    is_code_supplement <- NULL

  #one part of the keyword search acts on the tokenized sentences while another part acts on the full text
  keyword_results_tokenized <- .keyword_search_tokenized(keyword_results)
  keyword_results_near_wd <- .keyword_search_near_wd(pdf_text_sentences)
  data_journal_doi <- .check_journal_doi(pdf_text_sentences)

  keyword_results_combined <- cbind(keyword_results_tokenized,
                                    keyword_results_near_wd,
                                    data_journal_doi) |>
    dplyr::as_tibble()

  #check if any of the combined columns was positive to determine if the publication has Open Data or Open Code
  open_data_publication <- keyword_results_combined |>
    dplyr::mutate(is_open_data = com_specific_repo | com_general_repo |
                    is_data_journal,
                  is_open_code = com_code,
                  # | com_suppl_code,
                  is_supplement = dataset | com_file_formats | com_supplemental_data,
                  is_general_purpose = com_general_repo,
                  is_reuse = com_reuse,
                  open_data_category = furrr::future_pmap_chr(list(com_specific_repo, is_general_purpose,
                                                                   is_supplement, is_reuse, com_request,
                                                                   com_github_data,
                                                                   com_unknown_source,
                                                                   is_data_journal), .OD_category),
                  is_code_supplement = com_suppl_code,
                  is_code_reuse = com_code_reuse) |>
    tibble::add_column(article = names(pdf_text_sentences)) |>
    dplyr::select(article, is_open_data, open_data_category, is_reuse, is_open_code, is_code_supplement, is_code_reuse)

  return(open_data_publication)
}


#' @noRd
.open_data_sentences <- function(pdf_text_sentences, das_sentences, sentences_with_das, cas_sentences, keyword_results) {

  com_specific_repo <- com_general_repo <- com_github_data <- dataset <- com_file_formats <-
    com_supplemental_data <- com_reuse <- com_unknown_source <- das <- cas <- com_code <-
    com_code_reuse <- com_suppl_code <- article <- open_data_statements <- open_code_statements <- NULL

  keyword_list <- .create_keyword_list()
  #add simple TRUE/FALSE for the categories where the whole text is searched for nearby words
  keyword_results_near_wd <- .keyword_search_near_wd(pdf_text_sentences, extract_text = TRUE)

  das_sentences <- das_sentences |>
    furrr::future_imap_chr(.f = \(x, idx) dplyr::case_when(
      # !any(.has_das(x, keyword_list)) ~ "",
      # length(sentences) > 30 ~ "multiple DAS mentions",
      !idx %in% names(sentences_with_das) ~ "",
      .default =  x |>
        paste(collapse = " ") |>
        unlist()))

  cas_sentences <- cas_sentences |>
    furrr::future_map_chr(\(x) paste(x, collapse = " ") |>  unlist())
  # das_sentences <- cas_sentences <- ""
# tib <- tibble::tibble(text = das_sentences)
  #identifies the text fragments in which the Open Data keywords were detected
  open_data_sentences <- furrr::future_map(keyword_results, .text_fragments)
  open_data_sentences <- do.call(rbind, open_data_sentences)
  open_data_sentences <- cbind(names(keyword_results), open_data_sentences,
                               keyword_results_near_wd, das_sentences, cas_sentences) |>
    dplyr::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  colnames(open_data_sentences) <- c("article", "com_specific_repo", "com_general_repo",
                                     "com_github_data", "dataset", "com_code", "com_suppl_code",
                                     "com_reuse",
                                     "com_code_reuse",
                                     "com_request",
                                     "com_unknown_source",
                                     "com_file_formats", "com_supplemental_data", "das", "cas")
  open_data_sentences[is.na(open_data_sentences)] = "" #unify empty fields

  #collapse the found statements into one column for Open Data and one for Open Code
  open_data_sentences <- open_data_sentences |>
    dplyr::mutate(
      open_data_statements =
        paste(com_specific_repo, com_general_repo,
              com_github_data, dataset, com_file_formats,
              com_supplemental_data, com_reuse, com_code_reuse, com_unknown_source, sep = " ") |>
        trimws()
                  ) |>
    # copy over das to cas if das is actually also a cas
    dplyr::mutate(
      cas = ifelse(stringr::str_detect(stringr::str_sub(das, 1, 30), "(?<!accession) code|software|materials"), das, cas),
      open_code_statements =
        paste(com_code, com_suppl_code, com_code_reuse, sep = " ") |>
        trimws()
      ) |>
    dplyr::select(article, das, open_data_statements, cas, open_code_statements)

  return(open_data_sentences)
}
# categories <- "re-use"
# Remove repeated categories listed in a string separated by commas
#' @noRd
.remove_repeats <- function(categories) {
  strsplit(categories, ", ") |>
    unlist() |>
    unique() |>
    paste0(collapse = ", ")
}
