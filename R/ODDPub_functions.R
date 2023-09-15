
# set of utility functions used in the Open Data Detection

#---------------------------------------------------------------------
# 1 - PDF conversion
#---------------------------------------------------------------------

#' @noRd
.create_output_filename <- function(PDF_filename, output_folder)
{
  output_filename <- PDF_filename |>
    stringr::str_replace(stringr::fixed(".pdf"),
                         stringr::fixed(".txt"))
  output_filename <- utils::tail(stringr::str_split(output_filename, stringr::fixed("/"))[[1]],1)
  output_filename <- paste0(output_folder, output_filename)

  return(output_filename)
}

#' convert PDF file to txt file and saves it to output_folder (can now deal with multiple column output)
#' @noRd


.pdf_to_text <- function(PDF_filename, output_folder, overwriteExistingFiles = FALSE) {
  success <- FALSE
  output_filename <- .create_output_filename(PDF_filename, output_folder)

  if(!file.exists(PDF_filename)) {
    print("PDF file does not exist!")
  } else if(file.exists(output_filename) && overwriteExistingFiles == FALSE) {
    print("Output file already exists!")
    success <- TRUE
  } else  {
    tryCatch({
      text <- pdftools::pdf_data(PDF_filename, font_info = TRUE) |>
        purrr::map_chr(\(x) .textbox_to_str(x, PDF_filename))
      cat(text, file = output_filename)
      success <- TRUE
    }, error = function(e) {
      print("Could not convert pdf to text.")
    })
  }

  names(success) <- PDF_filename
  return(success)
}


#' calculate the estimated number of columns, based on the mean return symbol per line
#'
#' @noRd
.est_col_n <- function(text_data) {

  ralc <- space <- text <- y <- x <- line_n <- NULL

  # for elsevier data and code availability the column estimation is thrown off by the key ressources table

  ralc_statement_present <- text_data |>
    dplyr::mutate(dac = dplyr::if_else(dplyr::lag(space) == FALSE & text == "REAGENT" &
                                         dplyr::lead(text) == "or" &
                                         dplyr::lead(text, n = 2) == "RESOURCE" &
                                         dplyr::lead(space, n = 2) == FALSE &
                                         dplyr::lead(text, n = 3) == "SOURCE" &
                                         dplyr::lead(space, n = 3) == FALSE &
                                         dplyr::lead(text, n = 4) == "IDENTIFIER" &
                                         dplyr::lead(space, n = 4) == FALSE,
                                       1, 0)) |>
    dplyr::summarise(dac = sum(dac)) |>
    dplyr::pull(dac) |>
    as.logical()
  # if ralc is detected, force single-column layout
  generated_statement_present <- text_data |>
    dplyr::mutate(dac = dplyr::if_else(dplyr::lag(space) == FALSE & text == "The" &
                                         dplyr::lead(text) == "following" &
                                         stringr::str_detect(dplyr::lead(text, n = 2), "datasets?") &
                                         stringr::str_detect(dplyr::lead(text, n = 3), "was|were") &
                                         dplyr::lead(text, n = 4) == "generated:" &
                                         dplyr::lead(space, n = 4) == FALSE,
                                       1, 0)) |>
    dplyr::summarise(dac = sum(dac)) |>
    dplyr::pull(dac) |>
    as.logical()



  if (ralc_statement_present == TRUE | generated_statement_present == TRUE) return (1)

  # determine y of reference section for later exclusion
  reference_y <- text_data |>
    dplyr::filter(stringr::str_detect(text, "References") & space == FALSE) |>
    dplyr::pull(y)

  funding_y <- text_data |>
    dplyr::filter(stringr::str_detect(text, "Funding:")) |>
    dplyr::pull(y)

  funding_y <- max(funding_y, 0)

  # if (length(funding_y) == 0) funding_y <- 0

  if (length(reference_y) != 1) reference_y <- max(text_data$y) + 1

  if (reference_y < funding_y) return(1)

  cols <- text_data |>
    dplyr::arrange(y, x) |>
    # exclude what could be references
    dplyr::filter(!stringr::str_detect(text, "^\\d{1,3}\\.?$"),
                  y < reference_y,
                  y > 21, # exclude upper margin
                  font_size > 4) |>
    .add_line_n()

  if (nrow(cols) == 0) return (1)

  midpage_gap <- .find_midpage_x(text_data)

  if (midpage_gap == 0) return(1)

  cols <- cols |>
    dplyr::group_by(line_n) |>
    dplyr::summarise(ret_per_line = sum(space == FALSE),
                     midpage_words = sum(dplyr::between(x, midpage_gap, midpage_gap + 20)),
                     max_x = max(x))

  midpage_words <- cols |>
    dplyr::filter(midpage_words > 0) |>
    nrow()

  if (midpage_words / nrow(cols) < 0.2) return (2) # if words cross midpage gap less than 0.2 of rows assume 2col layout

  rows_over_one <- cols |>
    dplyr::filter(ret_per_line > 1) |>
    nrow()

  if (rows_over_one / nrow(cols) >= 0.2) { # if over 20% on the page seems to be split in columns, assume multicol layout
    cols <- cols |>
      dplyr::filter(ret_per_line > 1)
  }

  stats::median(cols$ret_per_line)
}



#' Obtain the most frequent value in a vector
#' @noRd
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' for the enragingly awful layout in frontiers and royal publishing journals, remove references first
#' @noRd
.reformat_columns <- function(text_data) {

  y_ref <- text_data |>
    dplyr::filter(stringr::str_detect(text, "REFERENCES|References"), # TODO: put ref stuff to third column??
                  space == FALSE) |>
    dplyr::pull(y) |>
    min(743) # if no references found, set to y value of footer

  text_data |>
    dplyr::filter(y < y_ref) |>
    dplyr::arrange(y, x)
}

#' find y coordinate of header for exclusion
#' @noRd
.find_header_y <- function(text_data) {

  header_candidate <- suppressWarnings(text_data |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0)) |>
    # dplyr::arrange(y) |>
    dplyr::filter(y < 70, y != min(y)) |>
    dplyr::filter(jump_size == max(jump_size)) |>
    # dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0)) |>
    # dplyr::filter(line_n == 1) |>
    dplyr::pull(y))

  if (length(header_candidate) == 0) return(55)

  min(max(header_candidate) - 1, 55)
}


#' add line numbers, but check if rearranging needed first
#' @noRd
.add_line_n <- function(text_data) {

  text_data |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0),
                  line_n = abs(jump_size) > 4,
                  # line_n = dplyr::lag(space == FALSE, default = 0) & jump_size > 5,
                  line_n = cumsum(line_n))
}


#' find x coordinate of the gap in between two columns on a 2col layout
#' @noRd
.find_midpage_x <- function(text_data) {
  gaps <- suppressMessages(text_data |>
    dplyr::filter(between(x, 200, 350)) |>
    dplyr::mutate(bins = round(x, -1)) |>
    dplyr::count(bins) |>
    dplyr::right_join(tibble::tibble(bins = seq(200, 350, by = 10))) |>
    tidyr::replace_na(list(n = 0)) |>
    dplyr::filter(n < 2) |>
    dplyr::pull(bins))


  if (length(gaps) == 0){
    return(0)
  } else {
    return(min(gaps))
  }

}

#' convert the dataframe extracted by pdftools::pdf_data into a one-column string
#' to be saved as a txt for further processing
#' @noRd
.textbox_to_str <- function(text_data, PDF_filename) {

  if (nrow(text_data) == 0) return("")

  # remove hidden text tags in Elsevier Journals
  if (stringr::str_detect(PDF_filename, "10\\.1016\\+j\\.ecl")) {
    text_data <- text_data |>
      dplyr::filter(font_name != "DOHGPO+AdvP48722B",
                    font_size > 6.5) |>
      dplyr::mutate(text = stringr::str_remove(text, "Tag$"))

  } else if (stringr::str_detect(PDF_filename, "10\\.1038\\+s41531")) {
    text_data <- text_data |>
      dplyr::filter(font_name != "BBKNAK+AdvTT6780a46b")
  }

  # special case for the frontiers, royal society special 2col formatting
  if (stringr::str_detect(PDF_filename, "10\\.3389\\+f|10\\.1098\\+rspb")) {
    text_data <- .reformat_columns(text_data)
    cols <- 2
  } else {
    cols <- .est_col_n(text_data)
  }

  col2_x <- 700 # initial estimate is the maximum, works for single column layouts
  col3_x <- col2_x
  min_x <- dplyr::case_when(
    stringr::str_detect(PDF_filename, "10\\.1371") & cols == 2 ~ 199, # for plos journals
    cols == 2 ~ .find_midpage_x(text_data),
   # cols == 3 ~ 150,
    .default = 150
  )


  if (cols > 1 | is.na(cols)) {

    if (nrow(text_data) < 2) return("")

    # estimate the x coordinate of the second column, usually around 303 - 306
    col2_x_est <- text_data |>
      dplyr::filter(x > min_x & dplyr::lag(space) == FALSE) |>
      # dplyr::count(x) |>
      # dplyr::filter(n > 3) |>
      dplyr::pull(x) |>
      min()

    # if (cols == 3) {
      col3_x_est <- suppressWarnings(text_data |>
        dplyr::filter(x > col2_x_est + 100 & dplyr::lag(space) == FALSE) |>
        dplyr::count(x) |>
        dplyr::filter(n > 5) |>
        dplyr::pull(x) |>
        min())
    # } else {
    #   col3_x_est <- col3_x
    # }


    # if above fails, take the max estimate, reducing in effect to a single column
    if (is.na(col2_x_est)) col2_x_est <- col2_x
    # if above fails, take the max estimate, reducing in effect to a single column
    if (is.na(col3_x_est) | cols != 3) col3_x_est <- col3_x
    # if max is at least 100 to the right of estimate, then estimate is good
    if (col2_x - col2_x_est > 100) col2_x <- col2_x_est
    if (col3_x - col3_x_est > 100) col3_x <- col3_x_est
 }

  min_y <- .find_header_y(text_data) # remove header in most journals
  max_y <- 750 # remove footer in most journals
  max_x <- 563 # remove right margin in most journals

  text_data <- text_data |>
    # remove page numbers, textboxes with citation numbers, line numbers, etc.
    # as well as the very first line on top of the page
    dplyr::filter(!(stringr::str_detect(text, "^\\d{1,3}\\.*$") & space == FALSE),
                  y > min_y, # remove header
                  y < max_y, # remove footer
                  x < max_x) |> # remove margin text, e.g. 'downloaded from...'
    dplyr::mutate(x_jump_size = x - dplyr::lag(x, default = 0),
           column = dplyr::case_when(
             x >= col2_x & x < col3_x ~ 2,
             x >= col3_x ~ 3,
             .default = 1))


  # if (cols > 1)
  text_data <- text_data |>
      dplyr::arrange(column, y, x)

  page_width <- max(text_data$x) - min(text_data$x)
  col_width <- page_width/2 # quick approximation perhaps sufficient?


  text_data <- .add_line_n(text_data) |>
  # text_data2 <- .add_line_n(text_data) |>
    dplyr::arrange(column, line_n, x) |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0)) |>
    dplyr::group_by(line_n) |>
    dplyr::mutate(prop_blank = case_when( # calculate approx. prop. blank space on line
      cols == 1 ~ 1 - sum(width)/page_width,
      cols == 2 ~ 1 - sum(width)/col_width,
      .default = 1 - sum(width)/(col_width/3)
    )) |>
    # ),
    # line_ends_dot = x == max(x) & stringr::str_detect(text, "\\.$")) |>
    dplyr::ungroup()


  section_jump <- text_data$jump_size[text_data$jump_size > 0] |>
    Mode() * 1.3

  section_jump <- min(section_jump, 25) # some manuscripts are double-spaced
  # regular_font_size <- text_data |>
  #   filter(font_size > 7) |>
  #   pull(font_size) |>
  #   Mode()
  # str_detect("[B,b]old|\\.[B,b]|PSHN-H$|PA5D1$")
  heading_font_regex <- c("[B,b]old",
                          "\\.[B,b]",
                          "CharisSIL-Italic$",
                          "AdvTT47f7fe79.I$",
                          "AdvPSHN-H$",
                          "AdvPA5D1$",
                          "AdvOTd67905e7$",
                          "AdvTT99c4c969$",
                          "Bd(Cn)?$") |>
    paste(collapse = "|")

  res <- text_data |>
    dplyr::mutate(
      heading_font = dplyr::if_else(
        abs(font_size - dplyr::lag(font_size)) > 1.4 |
          !stringr::str_detect(text, "[[:lower:]]") | # only caps
        # font_size - regular_font_size > 1 |
          stringr::str_detect(font_name, heading_font_regex), TRUE, FALSE),
      newline_heading = line_n == 1 & is.na(heading_font) | # very first line
        line_n > dplyr::lag(line_n) &
        (stringr::str_detect(dplyr::lag(text), "\\.$|@|www|http") | # end of line can be full stop or some email or url
           (dplyr::lag(font_size < 6) & !stringr::str_detect(dplyr::lag(text), "[[:lower:]]"))) & # some lines end with citation superscripts
        (font_name != dplyr::lag(font_name)),
      paragraph_start = abs(jump_size) > section_jump,
      # for the three-column science layout DAS may start within the line
      science_section = (text == "Data" &
        dplyr::lead(text) == "and" &
        stringr::str_detect(dplyr::lead(text, 2), "mat")) |
        y > 711 & dplyr::lag(space) == FALSE & stringr::str_detect(text, "^[A-Z][a-z]") |
        text == "Submitted",
      # for some journals with das on first page (e.g. elsevier jclinepi, erj, etc.)
      plain_section =
        # should follow a new line should start with a capital, but not end on a capital (with or without fullstop) or a comma
        dplyr::lag(space) == FALSE & stringr::str_detect(text, "^[A-Z]") & !stringr::str_detect(dplyr::lag(text), "(,|[A-Z]\\.?)$") &
        (stringr::str_detect(text, ":$") | # e.g. Funding:
           stringr::str_detect(dplyr::lead(text), ":$") & line_n == dplyr::lead(line_n) | # e.g. Data sharing:
           stringr::str_detect(dplyr::lead(text, 2), ":$") & line_n == dplyr::lead(line_n, 2) | # e.g. Conflict of interest:
           stringr::str_detect(dplyr::lead(text, 3), ":$") & line_n == dplyr::lead(line_n, 3) | # e.g. Data and code availability:
           stringr::str_detect(dplyr::lead(text, 5), ":$") & line_n == dplyr::lead(line_n, 5)) | # e.g. Disclosure of potential conflict of interest:
        # (stringr::str_detect(text, "Data|Software") &
        # stringr::str_detect(dplyr::lead(text), "(A|a)vailability:")) |
        # dplyr::lead(text, 2) == "interest:" |
        # (text == "Data" & dplyr::lead(text) == "sharing" & dplyr::lead(text, 2) == "statement:") |
        # text == "Funding:" |
        text == "*" & dplyr::lag(space) == FALSE,
      section_start = (paragraph_start & (heading_font | prop_blank > 0.35)) |
        (heading_font & prop_blank > 0.8 & dplyr::lag(space == FALSE)) |
        (prop_blank > 0.6 & dplyr::lag(space == FALSE) & !stringr::str_detect(text, "\\.$")) |
        newline_heading | science_section | plain_section,
      text = dplyr::if_else(section_start == FALSE | is.na(section_start), text, paste("\n<section>", text))) |>
    dplyr::group_by(line_n) |>
    dplyr::summarise(text = paste(text, collapse = " ")) |>
    dplyr::summarise(text = paste(text, collapse = "\n ")) |>
    dplyr::pull(text)

  res
}
#--------------------------------------------------------------------------------------
# 2 - text data loading
#--------------------------------------------------------------------------------------

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
    "zenodo\\.",
    "et al\\.$",
    "ncbi\\.$",
    "www\\.$",
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

#' format
#' @noRd
# textfile <- paste0(pdf_text_folder, txt_filenames)
# textfile <- txt_filenames
.tokenize_sections <- function(textfile)
{
 readr::read_lines(textfile) |>
    paste(collapse = " ") |>
    stringr::str_replace_all("\n", ".") |>
    stringr::str_squish() |>
    tokenizers::tokenize_sentences(simplify = TRUE, lowercase = TRUE) |>
    tokenizers::tokenize_regex(pattern = " (?=<section>)", simplify = TRUE) |>
    purrr::list_c() |>
    stringr::str_replace_all(pattern = ",", replacement = "") |>
    stringr::str_replace_all(pattern = "- ", replacement = "") |>
    .correct_tokenization() |>
    stats::na.omit()
}


#--------------------------------------------------------------------------------------
# 3 - Open data identification keywords
#--------------------------------------------------------------------------------------

#' Search for several categories of similar keywords in a sentence.
#' Multiple categories must match to trigger a detection.
#' @noRd
.create_keyword_list <- function()
{
  keyword_list <- list()

  available <- c("(?<!which )included",
                 "deposited",
                 "archived",
                 "released",
                 "(is|are) provided",
                 "(is|are) hosted",
                 "contained in",
                 "contains",
                 "listed in",
                 "lodged with",
                 "(?<!the )available",
                 "doi of the data(set)? (is|are)",
                 "reproduce",
                 "accessible",
                 "(can|may) be (freely )?accessed",
                 "(can|may) be retrieved",
                 "submitted",
                 "(can|may) be downloaded",
                 "(can|may) be .*found",
                 "held in",
                 "reported in",
                 "uploaded",
                 "are public on",
                 "we (have )?added",
                 "(?<=data) at",
                 "(?<!\\d(-|\\))?(\\.|;) )doi: ") |> # exclude dois in references
    .format_keyword_vector()
  keyword_list[["available"]] <- available

  was_available <- c("(was|were) provided",
                     "(was|were)? previously published",
                     "(was|were|have been)? published previously",
                     "(was|were) contained in",
                     "(was|were|made) available",
                     "(was|were) accessible",
                     "(was|were) deposited by",
                     "(has (been )?)? previously (been )?deposited",
                     "(was|were) reproduced",
                     "were open source data",
                     "(previous|prior) study",
                     "were (used|analy[z,s]ed) in this study",
                     "were downloaded( and analy[z,s]ed)?",
                     "this study used .* publicly available",
                     "data ?set used previously",
                     "existing publicly available",
                     "data .*reanaly[z,s]ed (here|in (the present|this) study)",
                     "used data from a public",
                     "all data we used are public",
                     "covers public",
                     "available in .* previous",
                     "(we|was|were) obtained",
                     "checked .* on .* freely available") |>
    .format_keyword_vector()
  keyword_list[["was_available"]] <- was_available

  not_produced <- c("not developed",
                    "not produced",
                    "not generated") |>
    .format_keyword_vector()

  keyword_list[["not_produced"]] <- not_produced

  not_available <- c("not included",
                     "not deposited",
                     "not released",
                     "not provided",
                     "not contained in",
                     "not available",
                     "not accessible",
                     "not submitted",
                     "(few|no|not enough) \\w.* (is|are) available"
                     ) |>
    .format_keyword_vector()
  keyword_list[["not_available"]] <- not_available
  # str_detect(publ_sentences, accession_nr[1])field_specific_repo
 # stringr::str_detect("http://www.ncbi. nlm.nih.gov/ pubmed", "ncbi(?!\\. ?nlm\\. ?nih\\. ?gov/ ?pubmed)")
  # field_specific_repo |> sort() |> paste(collapse = ",\n")
  field_specific_repo <- c(
    "10xgenomics",
    "23andme",
    "4D Nucleome Consortium",
    "accession codes*",
    "accession numbers*",
    "ACDC",
    "Addgene",
    "ADHD *200",
    "AIBL",
    "Aphasia Resource",
    "aphasiatrials\\.org",
    "Array[ ,-]*Express",
    "BBMRI.nl",
    "bbmri.nl",
    "biocollections",
    "Biological General Repository for Interaction Datasets",
    "Biological Magnetic Resonance Data Bank",
    "BioProject",
    "BioSample",
    "BMRB",
    "Broad Institute",
    "Cambridge Crystallographic Data Centre",
    "caNanoLab",
    "Cancer Imaging Archive",
    "CATS",
    "CCDC",
    "CCMS",
    "Cell Image Library",
    "ChEMBL",
    # "clinicaltrials.gov", does not cover our data criteria
    # "ClinVar", does not cover our data criteria
    "Coherent X-ray Imaging Data Bank",
    "Computational Chemistry Datasets",
    "ConnectomeDB",
    "Conserved domains",
    "CPTAC",
    "Crystallography Open Database",
    "CXIDB",
    "Database of Interacting Proteins",
    "dbGaP",
    "dbSNP",
    "dbVar",
    "DDBJ",
    "DECIPHER",
    "DepMap Portal",
    "DNA Data Bank of Japan",
    "DZNE",
    "EBI",
    "EBRAINS",
    "Electron microscopy data bank",
    "EMBL-EBI",
    "emdb",
    "EMIDEC",
    "ega",
    "ENA",
    "ENCODE",
    "Environmental Data Initiative",
    "Eukaryotic Pathogen Database Resources",
    "EuPathDB",
    "European Genome[ ,-]*phenome Archive",
    "European Molecular Biology Laboratory",
    "European Nucleotide Archive",
    "European Variation Archive",
    "FlowRepository",
    "FASTGenomics",
    "G-Node",
    "GBIF",
    "GenBank",
    "Gene Expression Omnibus",
    "GenomeRNAi",
    "Genomic Expression Archive",
    "GEO",
    "GESIS",
    "GHDx",
    "GigaDB",
    "Gin",
    "Global Biodiversity Information Facility",
    "gmgc",
    "GPCRMD",
    "GWAS",
    "HIV Data Archive Program",
    "hPSCreg",
    "Image Data Resource",
    "ImmPort",
    "Influenza Research Database",
    "INDI",
    "IntAct",
    "Integrated Taxonomic Information System",
    "IoChem-BD",
    "ITIS",
    "Japanese Genotype-phenotype Archive",
    "KiMoSys",
    "Kinetic Models of Biological Systems",
    "Knowledge Network for Biocomplexity",
    "LCSB",
    "LTER Network Information System Data Portal",
    "MassIVE",
    "MetaboLights",
    "Metagenomics Rapid Annotation using Subsystem Technology",
    "Metabolomics Workbench",
    "MG-RAST",
    "Mgnify",
    "MiDAS",
    "Morphobank",
    "Mouse Genome Informatics",
    "NAHDAP",
    "National Center for Biotechnology Information",
    "National Database for Autism Research",
    "National Database for Clinical Trials related to Mental Illness",
    "ncbi(?!\\. ?nlm\\. ?nih\\. ?gov/ ?(pubmed|pmc|clinvar|books))", # exclude some repositories
    "NDAR",
    "NDCT",
    "NDEx",
    "Neuroimaging Informatics Tools and Resources Collaboratory",
    "NeuroMorpho",
    "NIMH Data Archive",
    "NIST",
    "NITRC",
    "OAR[-,\\.]PTB",
    "Open *Agrar",
    "OpenNeuro",
    "PCDDB",
    "PDB",
    "PeptideAtlas",
    "PGS Catalog",
    "PhysioNet",
    "PRIDE",
    "Protein Circular Dichroism Data Bank",
    "Protein Data *Bank",
    "proteindiffraction.org",
    "Proteome ?e?Xchange",
    "PubChem",
    "Rat Genome Database",
    "RdoCdb",
    "Research Domain Criteria Database",
    "SAMS",
    "Sequence Read Archive",
    "SICAS Medical Image Repository",
    "SRA",
    "STRENDA",
    "St. Jude Cloud",
    "stjude.cloud",
    "Structural Biology Data Grid",
    "Synapse",
    "TCGA",
    "The Network Data Exchange",
    "Treebase",
    "ucdr.be/data",
    "UK Data Service",
    "UniProt[KB]*",
    "VectorBase",
    "Virtual Trials Archive",
    "Worldwide Protein Data Bank",
    "wwPDB",
    "Xenbase",
    "Zebrafish Model Organism Database",
    "ZFIN"
  ) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["field_specific_repo"]] <- field_specific_repo

  accession_nr <- c("G(SE|SM|DS|PL)[[:digit:]]{2,}", #GEO
                    "PRJ(E|D|N|EB|DB|NB)[:digit:]+",
                    "SAM(E|D|N)[A-Z]?[:digit:]+",
                    "[A-Z]{1}[:digit:]{4}", #GenBank
                    "[A-Z]{2}[:digit:]{6}",
                    "[A-Z]{3}[:digit:]{5}",
                    "[A-Z]{4,6}[:digit:]{3,}",
                    "GCA_[:digit:]{9}\\.[:digit:]+",
                    "SR(P|R|X|S|Z)[[:digit:]]{3,}",
                    "(E|P)-[A-Z]{4}-[:digit:]{1,}",
                    "(?<!(\\.|<))[:digit:]{1}[a-z]{1}[[:alnum:]]{2}",
                    "MTBLS[[:digit:]]{2,}",
                    "10.17590",
                    "10.5073",
                    "10.25493",
                    "10.6073",
                    "10.15468",
                    "10.5063",
                    "10.12751", #GIN
                    "fcon_1000\\.projects\\.nitrc\\.org", # URL for INDI
                    "()[[:digit:]]{6}",
                    "[A-Z]{2,3}_[:digit:]{5,}",
                    "[A-Z]{2,3}-[:digit:]{4,}",
                    "[A-Z]{2}[:digit:]{5}-[A-Z]{1}",
                    "DIP:[:digit:]{3}",
                    "FR-FCM-[[:alnum:]]{4}",
                    "ICPSR [:digit:]{4}",
                    "SN [:digit:]{4}",
                    "key resources table",
                    "accession numbers") |>
    .format_keyword_vector()
  keyword_list[["accession_nr"]] <- accession_nr

  repositories <- c("available online *(\\(|\\[)+.*\\d{1,4}(\\)|\\])+",
                    "open data repository (\\(|\\[)\\d{1,4}",
                    "(is|are) available *(\\(|\\[)+.*\\d{1,4}(\\)|\\])+",
                    "10\\.17617/3.", # Edmond
                    "10\\.17632", # Mendeley Data
                    "10\\.18452", # edoc HU
                    "10\\.13130", # UNIMI Dataverse
                    "10\\.1038 */s41597-", # scientific data
                    "10\\.11588", # hei DATA
                    # "doi\\.org/[[:graph:]]* (?!received)",
                    "code ocean *(capsule)?",
                    "cyverse",
                    "dataverse(\\.harvard.\\edu)?",
                    "DataverseNL",
                    "dryad",
                    "datadryad",
                    "(fi|\\ufb01)gshare",
                    "GIGADB",
                    "GigaScience database",
                    "harvard dataverse",
                    "heidata",
                    "mendeley data",
                    "OpenNeuro",
                    "open science framework",
                    "osf",
                    "tu datalib",
                    "UNIMI",
                    "zivahub",
                    "zenodo"
                    ) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["repositories"]] <- repositories

  github <- c("github") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["github"]] <- github


  data <- c("data(?! (availability|accessibility))",
            "datasets?",
            # "databases?",
            "responses",
            "materials") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["data"]] <- data


  all_data <- c("all data",
                "all array data",
                "raw data",
                "source data",
                "full data set",
                "full dataset",
                "subject-level data") |>
    .format_keyword_vector()
  keyword_list[["all_data"]] <- all_data


  not_data <- c("not all data",
                "not all array data",
                "no raw data",
                "no full data set",
                "no full dataset") |>
    .format_keyword_vector()
  keyword_list[["not_data"]] <- not_data

  source_code <- c("source code",
                   "code files?",
                   "analysis script",
                   "data (and )?code",
                   "github",
                   "gitlab",
                   "bitbucket",
                   "code ocean",
                   "SAS script",
                   "SPSS script",
                   "r[-, ]+script",
                   "r[-, ]+code",
                   "r[-, ]+package",
                   "python script",
                   "python code",
                   # "software",
                   "matlab script",
                   "matlab code",
                   "macro") |>
    .format_keyword_vector()
  keyword_list[["source_code"]] <- source_code

# str_detect("my.email@haha.com", weblink)
  weblink <- "(((https?|ftp|smtp):\\/\\/)|(www\\.))[a-z0-9]+\\.[a-z ]+(\\/[a-zA-Z0-9#]+\\/?)*"
  citation <- "\\(.*\\d{4}\\)|\\[\\d{1,3}\\]"
  grant <- "grant"

  keyword_list[["grant"]] <- grant

  keyword_list[["weblink"]] <- weblink
# str_detect(publ_sentences, source_code)
  reuse <- .near_wd(was_available,
                    paste(
                      accession_nr,
                      field_specific_repo,
                      repositories,
                      weblink,
                      citation,
                      github,
                      "cited as reference",
                      sep = "|"),
                    dist = 30)
  keyword_list[["reuse"]] <- reuse

  supplement <- c("supporting information",
                  "supplement",
                  "supplementa(l|ry) data",
                  "supplementa(l|ry) material",
                  "supplementa(l|ry) information"
                  # "provided with this paper",
                  # "(with)?in th(is|e) article",
                  ) |>
    .format_keyword_vector()
  keyword_list[["supplement"]] <- supplement

  file_formats <- c("csv",
                    "zip",
                    "xls",
                    "xlsx",
                    "sav",
                    "cif",
                    "fasta") |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["file_formats"]] <- file_formats

upon_request <- c("(up)?on( reasonable)? request",
                    "without undue reservation",
                    "the corresponding author",
                    "the lead contact",
                    "the techincal contact",
                    "requests.* should be (directed|submitted) to",
                    "data requests?")|>
    .format_keyword_vector()
  keyword_list[["upon_request"]] <- upon_request

  data_availability <- c("Data and (Software|Resource) Availability",
                         "Software and Data Availability",
                         "Statistical Analysis and Data Availability",
                         "Data sharing statement",
                         "Data sharing",
                         "Data availability statement & Data deposition",
                         "D ?a ?t ?a A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y S ?t ?a ?t ?e ?m ?e ?n ?t",
                         "D ?a ?t ?a A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y P ?o ?l ?i ?c ?y",
                         "D ?a ?t ?a A ?v ?a ?i ?l ?a ?b ?i ?l ?i ?t ?y",
                         "Data and code availability",
                         "Data and ma-*te-*ri-*als a-*vai-*la-*bi-*li-*ty",
                         "Data, Materials, and Software Availability",
                         "Data deposition",
                         "Deposited Data",
                         "Data Archiving",
                         "(Accessi|availa)bility of data and materials",
                         "Availability of data",
                         "Data Accessibility",
                         "Data Access",
                         "Accessibility of data and code",
                         "Accessibility of data",
                         "Availability and implementation",
                         "Accession codes",
                         "Accession numbers sequence data",
                         "Open practices") |>
    .format_keyword_vector()
  keyword_list[["data_availability"]] <- data_availability


  code_availability <- c("Code sharing",
                         "Code availability statement",
                         "Code availability",
                         # "(Data and)? Software availability",
                         # "Data and code availability",
                         # "Data and materials availability",
                         # "Accessibility of data and code",
                         "Source code available from:") |>
    .format_keyword_vector()
  keyword_list[["code_availability"]] <- code_availability

  supplemental_table_name <- c("supplementa(l|ry) tables?",
                          "tables?",
                          "additional file",
                          "file", "files",
                          "article/supplementa(l|ry) material",
                          "supplementa(l|ry) information",
                          "supplementa(l|ry) file")
  supplemental_table_number <- c("S[[:digit:]]",
                                 "[[:digit:]]",
                                 "[A-Z]{2}[[:digit:]]")
  supplemental_table <- .outer_str(supplemental_table_name, supplemental_table_number) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["supplemental_table"]] <- supplemental_table

  supplemental_dataset <- c("supplementary data [[:digit:]]{1,2}",
                      "supplementary dataset [[:digit:]]{1,2}",
                      "supplementary data set [[:digit:]]{1,2}",
                      "supplemental data [[:digit:]]{1,2}",
                      "supplemental dataset [[:digit:]]{1,2}",
                      "supplemental data set [[:digit:]]{1,2}")
  dataset_name <- c("data", "dataset", "datasets", "data set", "data sets")
  dataset_number <- c("S[[:digit:]]{1,2}")
  dataset <- .outer_str(dataset_name, dataset_number)
  dataset <- c(dataset, supplemental_dataset) |>
    .format_keyword_vector(end_boundary = TRUE)
  keyword_list[["dataset"]] <- dataset

  protocol <- c("protocols?") |>
    .format_keyword_vector()
  keyword_list[["protocol"]] <- protocol


  data_journal_dois <- c(
    # "10.1038/s41597-019-", # Scientific Data (2019)
                         "10.3390/data", # Data
                         "10.1016/j.dib", # Data in Brief
                         "10.1038/s41597-", # Scientific Data (all years)
                         "10.3897/BDJ.", # BioDiversity Data Journal
                         "10.1016/j.cdc.", # Chemical Data Collection
                         "10.5194/essd-", # Earth System Science Data
                         "10.1002/gdj3.", # GeoScience Data Journal
                         "10.1016/j.gdata.", # Genomics Data
                         "10.5334/joad.", # Journal of Open Archaeology Data
                         "10.5334/johd." # Journal of Open Humanities Data
                         ) |>
    .format_keyword_vector()
  keyword_list[["data_journal_dois"]] <- data_journal_dois


  #special regex pattern that looks for word closeness instead of words being in the same sentence
  #effect: all_data & file_format words are at most 10 words apart from each other
  all_data_file_formats <- .near_wd_sym(all_data, file_formats, dist = 10)
  keyword_list[["all_data_file_formats"]] <- all_data_file_formats


  supp_table_data <- .near_wd_sym(supplemental_table,
                                 paste(file_formats, all_data, sep = "|"),
                                 dist = 10)
  keyword_list[["supp_table_data"]] <- supp_table_data

  # data_in_DAS <- .near_wd(data_availability,
  #                                         paste(
  #                                           accession_nr, repositories,
  #                                           weblink,
  #                                           sep = "|"),
  #                                         dist = 30)
  # keyword_list[["data_in_DAS"]] <- data_in_DAS


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
.near_wd_sym <- function(x, y, dist = 10)
{
  combined <- paste0("\\b(",
                     x,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     y,
                     ")\\b|\\b(",
                     y,
                     ")(?:\\W+\\w+){0,", dist, "}?\\W+(",
                     x,
                     ")\\b")

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
# publ_sentences <- PDF_text_sentences[[1]]

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
                                "reuse",
                                "grant",
                                "file_formats", "upon_request", "dataset", "protocol")

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
# keyword <- "data_availability_statement"
#' search for all keyword categories in the sentences
#' @noRd
.search_keyword_cat <- function(keyword, sentences, keyword_list)
{
  detection_col <- sentences |>
    furrr::future_map_lgl(stringr::str_detect, pattern = keyword_list[[keyword]])

  return(detection_col)
}
# sections_v <- PDF_text_sentences[DAS_start:DAS_end]
# sections_v <- DAS
#' search plos DAS when spread over two pages
#' @noRd
.splice_plos_twopager <- function(sections_v) {

  # if (any(stringr::str_detect(sections_v, "<section> plos"))) {
    sections <- stringr::str_detect(sections_v, "^<section> ")

    if (sum(sections) == 1) return(sections_v)

    splice_start_piece <- which(sections)[2] - 1
    splice_end_piece <- sections[length(sections):1] |> which.max() # invert vector to find last occurrence of section
    splice_end_piece <- length(sections) - splice_end_piece + 1 # last occurence of section

    c(sections_v[1:splice_start_piece], sections_v[splice_end_piece:length(sections_v)])
    # plos_line <- which.max(stringr::str_detect(sections_v, "<section> plos"))

    # if (plos_line == length(sections_v)) {
      # return(sections_v[-length(sections_v)])
      # if plos line at the end of DAS there is no page break
    # } else if (plos_line == which(sections)[2] & stringr::str_detect(sections_v[plos_line - 1], "\\.$")) {
      # return(sections_v[1:(plos_line - 1)])
    # } else {
      # splice_start_piece <- which(sections)[2] - 1
      # splice_end_piece <- sections[length(sections):1] |> which.max() # invert vector to find last occurrence of section
      # splice_end_piece <- length(sections) - splice_end_piece + 1 # last occurence of section
      # return(c(sections_v[1:splice_start_piece], sections_v[splice_end_piece:length(sections_v)]))
    # }

  # } else {
  #   return(sections_v)
  # }

}
# sentence <- PDF_text_sentences[927]
#' test if text contains data availability statement
#' @noRd
.has_DAS <- function(sentence, keyword_list) {

  data_availability <- keyword_list[["data_availability"]]

  data_availability <- paste0("(<section>)\\W+[\\d,\\W]*(", data_availability, ")\\b")
  stringr::str_detect(sentence, data_availability)
}

# sentence <- "<section> data availability"
# .has_DAS(sentence)

#' extract data availability statement
#' @noRd
.extract_DAS <- function(PDF_text_sentences) {

  keyword_list <- .create_keyword_list()

  data_availability <- keyword_list[["data_availability"]]


  DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
                      \(sentence) stringr::str_detect(sentence, data_availability))

  if (sum(DAS_detections) > 0) {
    DAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
                                            \(sentence) .has_DAS(sentence, keyword_list))
  }

  DAS_start <- which(DAS_detections)

  if (length(DAS_start) == 2) {
    statement_detections <- PDF_text_sentences[DAS_start] |>
      stringr::str_detect("statement")
    if (sum(statement_detections) == 1) {
      DAS_start <- DAS_start[statement_detections]
    } else {
      DAS_start <- min(DAS_start)
    }

    # if more than two detections of DAS were made, then return full document
  } else if (length(DAS_start) != 1) {
    return(PDF_text_sentences)
  }

  if (length(PDF_text_sentences) - DAS_start < 2) return(PDF_text_sentences[DAS_start:length(PDF_text_sentences)])

  str_DAS <- PDF_text_sentences[DAS_start] |>
    stringr::str_trim()
  str_DAS_sameline <- str_DAS |>
    stringr::str_remove(data_availability) |>
    stringr::str_remove("<section> ")

  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  DAS_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + 1):length(PDF_text_sentences)],
                                         \(sentence) stringr::str_detect(sentence, "section> (?!d )|^\\d\\.")) |>
    which() - 1
  # check if candidates are full sentences ending in full stop. This achieves splicing if section continues on next page
  completed_sentences <- furrr::future_map_lgl(PDF_text_sentences[DAS_start + DAS_end_candidates],
                                               \(sentence) stringr::str_detect(sentence, "\\.$"))

  if (stringr::str_length(str_DAS_sameline) < 5) {
    # first_sentence <- DAS_start + 1

    DAS_end <- DAS_end_candidates[-1][completed_sentences[-1]][1]#

  } else {
    DAS_end <- DAS_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section

    if (DAS_start / length(DAS_detections) < 0.1 & DAS_end != 0) { # for plos journals with DAS on first page

      DAS_second_part <- furrr::future_map_lgl(PDF_text_sentences[(DAS_start + DAS_end + 1):length(PDF_text_sentences)],
                                               \(sentence) stringr::str_detect(sentence, "<section> funding:"))

      if (sum(DAS_second_part) == 0) {
        DAS_end <- DAS_end
        } else {
          DAS_end <- DAS_end + which(DAS_second_part) - 1
        }
    }
  }

  if (is.na(DAS_end)) {
    DAS_end <- min(DAS_end_candidates)
  }

  if (DAS_start + DAS_end > length(PDF_text_sentences)) {
    DAS_end <- DAS_start
  }

  DAS_end <- DAS_start + DAS_end

  DAS <- PDF_text_sentences[DAS_start:DAS_end]

  if (DAS_start < 50 & any(stringr::str_detect(PDF_text_sentences[1:10], "plos"))) {
    DAS <- .splice_plos_twopager(DAS)
  }
   DAS |>
    paste(collapse = " ") |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim() |>
    # in some cases the references interpolate between DAS title and DAS text
    stringr::str_remove("<section> references.*") |> # remove references
    stringr::str_remove("<section> authors.*") |> # remove author contributions
    stringr::str_replace("(?<=repository)\\.", ":") |>  # for the weird cases when after repository a . and not : follows
    stringr::str_replace("(?<=were analy(z|s)ed in this study)\\.", ":") |>  # for the standard phrasing of data re-use
    tokenizers::tokenize_regex(pattern = "(?<=\\.) ", simplify = TRUE) |> # tokenize sentences
    .correct_tokenization()

}


#' extract code availability statement
#' @noRd
.extract_CAS <- function(PDF_text_sentences) {

  keyword_list <- .create_keyword_list()

  code_availability <- keyword_list[["code_availability"]]

  CAS_detections <- furrr::future_map_lgl(PDF_text_sentences,
                                          \(sentence) stringr::str_detect(sentence, code_availability))

  CAS_start <- which(CAS_detections)

  if (length(CAS_start) == 2) {
    CAS_start <- max(CAS_start)
  } else if (length(CAS_start) != 1 ) {
    return("")
  }

  str_CAS <- PDF_text_sentences[CAS_start] |>
    stringr::str_trim()
  str_CAS_sameline <- str_CAS |>
    stringr::str_remove(code_availability) |>
    stringr::str_remove("<section> ")


  # candidates are sentences after the first section but before the next
  # which begin with <section> or digit. (reference number at start of line)
  CAS_end_candidates <- furrr::future_map_lgl(PDF_text_sentences[(CAS_start + 1):length(PDF_text_sentences)],
                                              \(sentence) stringr::str_detect(sentence, "section> (?!d )|^\\d\\.")) |>
    which() - 1
  # check if candidates are full sentences ending in full stop. This achieves splicing if section contines on next page
  completed_sentences <- furrr::future_map_lgl(PDF_text_sentences[CAS_start + CAS_end_candidates],
                                               \(sentence) stringr::str_detect(sentence, "\\.$"))

  if (stringr::str_length(str_CAS_sameline) < 5) {
    # first_sentence <- DAS_start + 1

    CAS_end <- CAS_end_candidates[-1][completed_sentences[-1]][1]#

  } else {
    CAS_end <- CAS_end_candidates[completed_sentences][1] # the first complete sentence before the beginning of a section
  }

  if (is.na(CAS_end)) {
    CAS_end <- min(CAS_end_candidates)
  }

  if (CAS_start + CAS_end > length(PDF_text_sentences)) {
    CAS_end <- CAS_start
  }

  CAS_end <- CAS_start + CAS_end

  PDF_text_sentences[CAS_start:CAS_end] |>
    # .splice_plos_twopager() |>
    paste(collapse = " ") |>
    stringr::str_remove_all("\\u200b") |> # remove zerowidth spaces
    stringr::str_trim() |>
    # in some cases the references interpolate between CAS title and DAS text
    stringr::str_remove("<section> references.*") |> # remove references
    stringr::str_remove("funding.*") |> # remove funding
    tokenizers::tokenize_regex(pattern = "(?<=\\.) ", simplify = TRUE) |> # tokenize sentences
    .correct_tokenization()

}

#'
#' @noRd
.remove_references <- function(PDF_text_sentences)
{
  line_before_refs <- PDF_text_sentences |>
    stringr::str_detect("<section> r ?e ?f ?e ?r ?e ?n ?c ?e ?s(?! and notes)") |>
    which() - 1

  if (length(line_before_refs) > 1) line_before_refs <- line_before_refs[length(line_before_refs)]

  if (sum(line_before_refs) == 0) {

    return(PDF_text_sentences)

  } else {

    return(PDF_text_sentences[1:line_before_refs])

  }


}

#'
#' @noRd
.keyword_search_full <- function(PDF_text_sentences)
{
# odc <- open_data_categories[[1]]
  # PDF_text_sentences <- publ_sentences
  # search for open data keywords in the full texts
  open_data_categories <- furrr::future_map(PDF_text_sentences, .map_keywords, .progress = TRUE)

  # combine columns for the different open data keywords
  keyword_results_combined <- open_data_categories  |>
    purrr::map(dplyr::mutate, com_specific_repo =
                 field_specific_repo &
                 accession_nr & available & !not_available & !was_available & !protocol
               )|>
    purrr::map(dplyr::mutate, com_general_repo = repositories & available &
                 !not_available & !was_available & !protocol) |>
    purrr::map(dplyr::mutate, com_github_data = data & github & available &
                 !not_available & !was_available) |>
    purrr::map(dplyr::mutate, com_code = source_code & available &
                 !not_available & !was_available & !upon_request) |>
    purrr::map(dplyr::mutate, com_suppl_code = supplement & source_code) |>
    purrr::map(dplyr::mutate, com_reuse = reuse & !grant) |>
    purrr::map(dplyr::mutate, com_request = upon_request) |>
    purrr::map(dplyr::select, publ_sentences, com_specific_repo, com_general_repo,
                com_github_data, dataset, com_code, com_suppl_code, com_reuse, com_request)

  return(keyword_results_combined)
}

#' keyword search on the tokenized sentences
#' @noRd
.keyword_search_tokenized <- function(keyword_results_combined)
{
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
.keyword_search_near_wd <- function(PDF_text_sentences, extract_text = FALSE)
{
  keyword_list <- .create_keyword_list()

  # needs text input in two formats: split into sentences, and one long string for
  PDF_text_full <- PDF_text_sentences |>
    furrr::future_map_if(.p = \(sentence) length(sentence) > 1,
                         .f = \(sentence) paste(sentence, collapse = " "))

  # search for the last combination in the non-tokenized text and add it to the results table for the publications
  # either give out TRUE/FALSE or return detected string
  if(extract_text) {
    str_function <- stringr::str_extract
    map_function <- furrr::future_map_chr
  } else {
    str_function <- stringr::str_detect
    map_function <- furrr::future_map_lgl
  }
  keyword_results_near_wd <- dplyr::tibble(
    com_file_formats = map_function(PDF_text_full, str_function,
                                    pattern = keyword_list[["all_data_file_formats"]]),
    com_supplemental_data = map_function(PDF_text_full, str_function,
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
.check_journal_doi <- function(PDF_text_sentences)
{
  keyword_list <- .create_keyword_list()

  dois <- names(PDF_text_sentences) |>
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
                         # unknown,
                         data_journal)
{
  category = vector()

  # should give out several categories
  # if there are multipe matches
  if(specific_repo == TRUE) {
    category <- category |> c("field-specific repository")
  }
  if(general_repo == TRUE) {
    category <- category |> c("general-purpose repository")
  }
  if(suppl == TRUE) {
    category <- category |> c("supplement")
  }
  if(reuse == TRUE) {
    category <- category |> c("re-use")
  }
  if(request == TRUE) {
    category <- category |> c("upon request")
  }
  # if(unknown == TRUE) {
  #   category <- category |> c("unknown repository")
  # }
  if(data_journal == TRUE) {
    category <- category |> c("data journal")
  }
  category <- category |> paste(collapse = ", ")

  return(category)
}

#' check if data availability statement (das) contains a url that is not a git url
#' @noRd
.has_url <- function(das){
  stringr::str_detect(das, "(https?|ftp|smtp):\\/\\/(?! ?git)")
}


#---------------------------------------------------------------------
# 5 - Combine search steps to obtain Open Data status &
#     detected sentences
#---------------------------------------------------------------------


#' @noRd
.open_data_detection <- function(PDF_text_sentences, keyword_results)
{
  #one part of the keyword search acts on the tokenized sentences while another part acts on the full text
  keyword_results_tokenized <- .keyword_search_tokenized(keyword_results)
  keyword_results_near_wd <- .keyword_search_near_wd(PDF_text_sentences)
  data_journal_doi <- .check_journal_doi(PDF_text_sentences)

  keyword_results_combined <- cbind(keyword_results_tokenized,
                                    keyword_results_near_wd,
                                    data_journal_doi) |>
    dplyr::as_tibble()

  #check if any of the combined columns was positive to determine if the publication has Open Data or Open Code
  open_data_publication <- keyword_results_combined |>
    dplyr::mutate(is_open_data = com_specific_repo | com_general_repo |
                    is_data_journal,
                  is_open_code = com_code | com_suppl_code,
                  is_supplement = dataset | com_file_formats | com_supplemental_data,
                  is_general_purpose = com_general_repo | com_github_data,
                  is_reuse = com_reuse,
                  open_data_category = furrr::future_pmap_chr(list(com_specific_repo, is_general_purpose,
                                                                   is_supplement, is_reuse, com_request,
                                                                   is_data_journal), .OD_category)) |>
    tibble::add_column(article = names(PDF_text_sentences)) |>
    dplyr::select(article, is_open_data, open_data_category, is_reuse, is_open_code)

  return(open_data_publication)
}


#' @noRd
.open_data_sentences <- function(PDF_text_sentences, DAS_sentences, sentences_with_DAS, CAS_sentences, keyword_results)
{
  keyword_list <- .create_keyword_list()
  #add simple TRUE/FALSE for the categories where the whole text is searched for nearby words
  keyword_results_near_wd <- .keyword_search_near_wd(PDF_text_sentences, extract_text = TRUE)

  DAS_sentences <- DAS_sentences |>
    furrr::future_imap_chr(.f = \(x, idx) dplyr::case_when(
      # !any(.has_DAS(x, keyword_list)) ~ "",
      # length(sentences) > 30 ~ "multiple DAS mentions",
      !idx %in% names(sentences_with_DAS) ~ "",
      .default =  x |>
        paste(collapse = " ") |>
        unlist()))

  CAS_sentences <- CAS_sentences |>
    furrr::future_map_chr(\(x) paste(x, collapse = " ") |>  unlist())

# tib <- tibble::tibble(text = DAS_sentences)
  #identifies the text fragments in which the Open Data keywords were detected
  open_data_sentences <- furrr::future_map(keyword_results, .text_fragments)
  open_data_sentences <- do.call(rbind, open_data_sentences)
  open_data_sentences <- cbind(names(keyword_results), open_data_sentences,
                               keyword_results_near_wd, DAS_sentences, CAS_sentences) |>
    dplyr::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  colnames(open_data_sentences) <- c("article", "com_specific_repo", "com_general_repo",
                                     "com_github_data", "dataset", "com_code", "com_suppl_code",
                                     "com_reuse",
                                     "com_request",
                                     # "com_unknown",
                                     "com_file_formats", "com_supplemental_data", "das", "cas")
  open_data_sentences[is.na(open_data_sentences)] = "" #unify empty fields

  #collapse the found statements into one column for Open Data and one for Open Code
  open_data_sentences <- open_data_sentences |>
    dplyr::mutate(
      open_data_statements =
        paste(com_specific_repo, com_general_repo,
              com_github_data, dataset, com_file_formats,
              com_supplemental_data, com_reuse, sep = " ") |>
        trimws()
                  ) |>
    # copy over das to cas if das is actually also a cas
    dplyr::mutate(
      cas = ifelse(stringr::str_detect(stringr::str_sub(das, 1, 30), "code|software|materials"), das, cas),
      open_code_statements =
        paste(com_code, com_suppl_code, sep = " ") |>
        trimws()
      ) |>
    dplyr::select(article, das, open_data_statements, cas, open_code_statements)



  return(open_data_sentences)
}


