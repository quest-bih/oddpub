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
  txt_filenames <- file.path(pdf_text_folder, txt_filenames)

  # produce version of the full texts where all sentences are separate vector elements
  PDF_text_sentences <- txt_filenames |>
    furrr::future_map(.tokenize_sections, .progress = TRUE)

  names(PDF_text_sentences) <- txt_filenames_short

  return(PDF_text_sentences)
}

#' search for sentences that were falsely split on abbreviations like accession nr.
#' and pastes them together again
#' @noRd
.correct_tokenization <- function(PDF_text)
{
  regex_to_correct <- c(
    "a?cc(ession)? nrs?\\.$",
    "a?cc(ession)? nos?\\.$",
    "fig\\.$",
    "doi\\.$",
    "drs?\\.",
    "zenodo\\.",
    "neurovault\\.",
    "et al\\.$",
    "ncbi\\.$",
    "github\\.$",
    "www\\.$",
    "no\\.",
    " st\\.",
    "https?:\\/\\/$",
    "^(<section> )?\\w\\.$",
    "^(<section> )?\\d\\.\\d\\.$"
  ) |>
    paste(collapse = "|")

  PDF_text_corrected <- PDF_text
  sentence_paste_idx <- PDF_text  |>
    stringr::str_sub(-14, -1) |>
    stringr::str_detect(regex_to_correct) |>
    which()

  #for all indices do a pairwise pasting
  if(length(sentence_paste_idx) > 0)
  {
    for(i in 1:length(sentence_paste_idx))
    {
      PDF_text_corrected <- .paste_idx(PDF_text_corrected, sentence_paste_idx[i]-(i-1))
    }
  }

  return(PDF_text_corrected)
}

#' paste together sentences where tokenization needs to be corrected by index
#' @noRd

.paste_idx <- function(PDF_text, idx)
{
  #create dummy sentences such that the indexing always works correctly,
  #even with only one element in PDF_text
  PDF_text_pasted <- c("x", PDF_text, "x")
  idx <- idx + 1 #shift idx due to dummy sentence

  PDF_text_pasted <- c(PDF_text_pasted[1:(idx-1)],
                       paste(PDF_text_pasted[idx], PDF_text_pasted[idx+1]),
                       PDF_text_pasted[(idx+2):length(PDF_text_pasted)])
  #remove dummy elemets
  PDF_text_pasted <- PDF_text_pasted[c(-1, -length(PDF_text_pasted))]

  return(PDF_text_pasted)
}
# tok <- tibble(text = tok)
#' format
#' @noRd
# textfile <- paste0(pdf_text_folder, txt_filenames)
# textfile <- txt_filenames
.tokenize_sections <- function(textfile)
{
  tokenized <- readr::read_lines(textfile) |>
    paste(collapse = " ") |>
    stringr::str_replace_all("\n", ".") |>
    stringr::str_squish() |>
    tokenizers::tokenize_sentences(simplify = TRUE, lowercase = TRUE) |>
    tokenizers::tokenize_regex(pattern = " (?=<(section|insert|iend)>)", simplify = TRUE)

  if (is.list(tokenized)) {
    tokenized <- tokenized |>
      purrr::list_c()
  } else {
    warning(paste0(textfile, ": Document could not be parsed into sections! Check if the txt file parsed correctly!"))
  }

  tokenized |>
    stringr::str_replace_all(pattern = ",", replacement = "") |>
    stringr::str_replace_all(pattern = "- ", replacement = "") |>
    .correct_tokenization() |>
    stats::na.omit()
}