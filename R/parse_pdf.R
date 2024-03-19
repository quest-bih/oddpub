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
      suppressWarnings({
        text <- pdftools::pdf_data(PDF_filename, font_info = TRUE) |>
          purrr::map_chr(\(x) .textbox_to_str(x, PDF_filename))
      })
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
.est_col_n <- function(text_data, PDF_filename) {

  ralc <- space <- text <- y <- x <- line_n <- has_jama <- text_left_margin <- NULL

  if (stringr::str_detect(PDF_filename, "10\\.3390|e(L|l)ife")) return(1)


  text_data <- text_data |>
    dplyr::filter(insert == 0)

  if (nrow(text_data) == 0) return(1)

  # for PLoS articles
  if (stringr::str_detect(PDF_filename, "10\\.1371")) {
    text_left_margin <- text_data |>
      dplyr::filter(x < 200) |>
      nrow()
    if (text_left_margin > 0) {
      return(2)
    } else {
      return(1)
    }
  }

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


  cochrane_statement_present <- text_data |>
    dplyr::mutate(contrib = dplyr::if_else((dplyr::lag(space) == FALSE | is.na(dplyr::lag(space))) & text == "C" &
                                             dplyr::lead(text) == "O" &
                                             dplyr::lead(text, n = 2) == "N" &
                                             dplyr::lead(text, n = 3) == "T" &
                                             dplyr::lead(text, n = 4) == "R" &
                                             dplyr::lead(text, n = 5) == "I" &
                                             dplyr::lead(text, n = 6) == "B" &
                                             dplyr::lead(text, n = 7) == "U",
                                           1, 0)) |>
    dplyr::summarise(contrib = sum(contrib, na.rm = TRUE)) |>
    dplyr::pull(contrib) |>
    as.logical() & stringr::str_detect(PDF_filename, "14651858")
  # if ralc is detected, force single-column layout
  generated_statement_present <- text_data |>
    dplyr::mutate(dac = dplyr::if_else(dplyr::lag(space) == FALSE & text == "The" &
                                         dplyr::lead(text) == "following" &
                                         stringr::str_detect(dplyr::lead(text, n = 2), "datasets?") &
                                         stringr::str_detect(dplyr::lead(text, n = 3), "was|were") &
                                         dplyr::lead(text, n = 4) == "generated:" &
                                         dplyr::lead(space, n = 4) == FALSE,
                                       1, 0)) |>
    dplyr::summarise(dac = sum(dac, na.rm = TRUE)) |>
    dplyr::pull(dac) |>
    as.logical()


  if (ralc_statement_present == TRUE | generated_statement_present == TRUE |
      cochrane_statement_present == TRUE) return (1)

  # determine y of reference section for later exclusion
  reference_yx <- text_data |>
    dplyr::filter(stringr::str_detect(text, "References") & space == FALSE) |>
    dplyr::select(y, x)

  reference_y <- reference_yx$y
  reference_x <- reference_yx$x

  funding_y <- text_data |>
    dplyr::filter(stringr::str_detect(text, "Funding:")) |>
    dplyr::pull(y)

  funding_y <- max(funding_y, 0)

  ackn_y <- text_data |>
    dplyr::filter(stringr::str_detect(text, "Acknowledgements")) |>
    dplyr::pull(y)

  ackn_y <- max(ackn_y, 0)

  # if (length(funding_y) == 0) funding_y <- 0

  if (length(reference_y) != 1) reference_y <- max(text_data$y) + 1
  if (ackn_y > reference_y) reference_y <- max(text_data$y) + 1
  if (length(reference_x) != 1) reference_x <- max(text_data$x) + 1

  if (reference_y < max(text_data$y)/2 & reference_x > max(text_data$x)/2) return(2)

  if (reference_y < funding_y) return(1)


  cols <- text_data |>
    dplyr::arrange(y, x) |>
    # exclude what could be references
    dplyr::filter(!stringr::str_detect(text, "^\\d{1,3}\\.?$"),
                  y < reference_y,
                  y > 21, # exclude upper margin
                  rel_width > 0.3,
                  font_size > 4) |>
    .add_line_n()

  cols_x <- .find_cols_left_x(cols)



  if (nrow(cols_x) > 1 & nrow(cols_x) < 4) return(nrow(cols_x))
  #178 not but 50 and 306 # find out typical 3-col values!

  if (nrow(cols) == 0) return (1)

  midpage_gap <- .find_midpage_x(text_data)

  has_jama <- sum(stringr::str_detect(text_data$text, "10.1001/jama"))

  jama_article_info_y <- text_data |>
    dplyr::filter(stringr::str_detect(text, "ARTICLE") &
                    stringr::str_detect(dplyr::lead(text), "INFORMATION")) |>
    dplyr::pull(y)


  if (has_jama > 0 & !purrr::is_empty(jama_article_info_y)) {
    jama_cols <- text_data |>
      dplyr::filter(y == jama_article_info_y) |>
      dplyr::summarise(n_cols = sum(space == FALSE)) |>
      dplyr::pull(n_cols)

    if (jama_cols == 3) jama_cols <- 5
    return(jama_cols) # special case for JAMA articles
  }

  # if (midpage_gap == 0) return(1) # TODO: but it could also be three for nonJama?

  cols <- cols |>
    dplyr::group_by(line_n) |>
    dplyr::summarise(ret_per_line = sum(space == FALSE),
                     midpage_words = sum(dplyr::between(x, midpage_gap, midpage_gap + 15)),
                     max_x = max(x))

  midpage_words <- cols |>
    dplyr::filter(midpage_words > 0) |>
    nrow()

  if (midpage_words / nrow(cols) < 0.2 & midpage_gap > 220) return (2) # if words cross midpage gap less than 0.2 of rows assume 2col layout

  rows_over_one <- cols |>
    dplyr::filter(ret_per_line > 1) |>
    nrow()

  if (rows_over_one / nrow(cols) >= 0.2) { # if over 20% on the page seems to be split in columns, assume multicol layout
    cols <- cols |>
      dplyr::filter(ret_per_line > 1)
  }

  cols <- stats::median(cols$ret_per_line)

  if (cols > 3) return(1)

  cols

}



#' Obtain the most frequent value in a vector
#' @noRd
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' find y coordinate of header for exclusion
#' @noRd
.find_header_y <- function(text_data) {

  if (.str_has_insert(text_data$text[1])) return(0)

  page_width <- max(text_data$x) - min(text_data$x)
  # page_width / 2

  linejumps <- tibble::tibble(
    y = sort(unique(text_data$y)),
    y_jump = dplyr::lead(sort(unique(text_data$y))) - y) |>
    dplyr::mutate(y_jump = dplyr::if_else(is.na(y_jump), y, y_jump))

  # typical_jump <- linejumps$y_jump |>
  #   Mode()

  header_candidate <- suppressWarnings(
    # ht <-
      text_data |>
      dplyr::filter(y < min(y) + 18,
                    !stringr::str_detect(text, "\\.$")) |>
      dplyr::arrange(y, x) |>
      .add_line_n() |>
      dplyr::mutate(x_jump = x - dplyr::lag(x, default = 0) -
                      dplyr::lag(width, default = 0)) |>
      dplyr::group_by(line_n) |>
      dplyr::mutate(prop_width = sum(width)/page_width,
                    max_x_jump = max(x_jump),
                    has_coded_break = stringr::str_detect(text, "\\\b"),
                    potential_page_n = stringr::str_detect(text, "\\d{1,5}") &
                      space == FALSE & (x < 100 | x > 500),
                    section_phrase = stringr::str_detect(text, "^Table$|^Fig(u|\\.)|^Appendix(?!,)|^FIG(U|\\.)|^TABLE$|ORCID|[O,o]rcid|[C,c]ontribut|Acknow") |
                      stringr::str_detect(text, "^TA?") & stringr::str_detect(dplyr::lead(text), "^B$|^A$") |
                      stringr::str_detect(text, "^F$") & stringr::str_detect(dplyr::lead(text), "^I$"),
                    is_section = any(section_phrase, na.rm = TRUE)) |>
      dplyr::ungroup() |>
      dplyr::left_join(linejumps, by = "y") |>
      dplyr::mutate(section_on_line_one = any(is_section == TRUE & line_n == 1)) |>
      dplyr::filter((prop_width < 0.5 & font_size < 9) | max_x_jump > 170 |
                      prop_width < 0.2 |
        y_jump > 20 |
        potential_page_n |
        has_coded_break,
        section_on_line_one == FALSE,
      is_section == FALSE,
      !stringr::str_detect(font_name, "Bold")
      ) |>
      dplyr::filter((y_jump == max(y_jump) |
                       # stringr::str_detect(text, "[C,c]ontinued") |
                       stringr::str_detect(text, "20\\d{2}|\\u00a9"))
                    # & y_jump > 13
                    ) |>
      dplyr::pull(y)
      )

  if (length(header_candidate) == 0) return(0) # in case no header is detected

  max(header_candidate)
}


#' find y coordinate of footer for exclusion
#' @noRd
.find_footer_y <- function(text_data) {

  page_width <- max(text_data$x) - min(text_data$x)
  min_y <- min(max(text_data$y) - 50, 700)

  if (min_y < 400) return(max(text_data$y) + 3) # in case no footer was detected

  # linejumps <- tibble::tibble(
  #   y = sort(unique(text_data$y)),
  #   y_jump = dplyr::lead(sort(unique(text_data$y))) - y) |>
  #   dplyr::mutate(y_jump = dplyr::if_else(is.na(y_jump), 0, y_jump))

  months <- lubridate::month(1:12, label = TRUE, abbr = FALSE,
                             locale = "English_United States") |>
    paste(collapse = "|")

  footer_candidate <- suppressWarnings(
    # ft <-
    text_data |>
      dplyr::filter(y > min_y,
                    height < 20,
                    ) |>
      dplyr::arrange(y, x) |>
      dplyr::mutate(jump_size = y - dplyr::lag(y, default = min_y),
                    line_n = abs(jump_size) > 4,
                    line_n = cumsum(line_n)) |>
      dplyr::group_by(line_n) |>
      dplyr::arrange(line_n, x) |>
      dplyr::mutate(prop_width = sum(width)/page_width,
                    x_jump = x - dplyr::lag(x, default = 0) - dplyr::lag(width, default = 0),
                    max_x_jump = max(x_jump),
                    jump_size = max(jump_size),
                    potential_page_n = stringr::str_detect(text, "\\d{1,5}") &
                      space == FALSE & (x < 100 | x > 500),
                    has_insert = any(.str_has_insert(text)),
                    candidate = any(prop_width < 0.6 & round(font_size) < 9) |
                      any(max_x_jump > 170) |
                      potential_page_n |
                      (any(stringr::str_detect(text, paste0("(?<!orcid)\\.org|release|\\u00a9|Volume|Published|", months))) &
                         !any(stringr::str_detect(text, "(A|a)ccessed|((A|a)t)")))) |>
      # dplyr::left_join(linejumps, by = "y") |>
      dplyr::filter(candidate == TRUE & has_insert == FALSE,
                    !stringr::str_detect(text, "doi.*(g|t)0\\d{1,2}|[O,o]rcid|ORCID")) |>
      # dplyr::summarise(jump_criterion = )
      dplyr::ungroup() |>
      dplyr::filter(jump_size == max(jump_size) & jump_size > 15 |
                      (stringr::str_detect(text, paste0("(?<!orcid)\\.org|Journal|release|\\u00a9|Volume|", months)) &
                         !stringr::str_detect(dplyr::lag(text), "(A|a)t") &
                         max(line_n) - line_n < 3)
      #               # |
      #               #   potential_page_n == TRUE
                    ) |>
      dplyr::pull(y)
  )

  if (length(footer_candidate) == 0) return(max(text_data$y) + 3) # in case no footer is detected

  min(footer_candidate)
}

#' add line numbers, but check if rearranging needed first
#' @noRd
.add_line_n <- function(text_data) {

  text_data |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0),
                  line_n = abs(jump_size) > 4,
                  # line_n = dplyr::lag(space == FALSE, default = 0) & jump_size > 5,
                  line_n = cumsum(line_n)) |>
    dplyr::group_by(line_n) |>
    dplyr::mutate(ends_dot = stringr::str_detect(dplyr::last(text), "\\.$")) |>
    dplyr::ungroup()
}


#' find left x coordinates of the potential columns on a page
#' @noRd
.find_cols_left_x <- function(text_data) {

  text_data <- text_data |>
    dplyr::filter(insert == 0)

  if (nrow(text_data) == 0) return(tibble::tibble(x = 0))

  if (min(text_data$x) > 200) return(tibble::tibble(x = min(text_data$x)))

  text_data |>
    dplyr::filter(dplyr::lag(space) == FALSE,
                  # space == TRUE,
                  # font_size > 6,
                  stringr::str_detect(text, "[A-Za-z]|\\d{1,3}|\\["),
                  !stringr::str_detect(text, "\\(")

                  ) |>
    dplyr::count(x) |>
    dplyr::slice_max(order_by = n, n = 5) |>
    dplyr::filter(n > 2) |>
    dplyr::arrange(x) |>
    dplyr::mutate(x_jump = x - dplyr::lag(x, default = 0)) |>
    dplyr::filter(x_jump > 50 | x == x_jump)
}

#' find right x coordinates of the potential columns on a page
#' @noRd
.find_cols_right_x <- function(text_data, min_x = 250) {
  text_data <- text_data |>
    dplyr::filter(insert == 0)

  if (nrow(text_data) == 0) return(tibble::tibble(x = 0))

  if (min(text_data$x) > 200) return(tibble::tibble(x = max(text_data$x)))

  text_data |>
    dplyr::filter(x > min_x, space == FALSE,
                  stringr::str_detect(text, "^[a-z]|\\W$")) |>
    dplyr::mutate(x = x + width + 2) |>
    dplyr::count(x) |>
    dplyr::slice_max(order_by = n, n = 5) |>
    dplyr::filter(n > 2) |>
    dplyr::arrange(x) |>
    dplyr::mutate(x_jump = x - dplyr::lag(x, default = 0)) |>
    dplyr::filter(x_jump > 50 | x == x_jump)
}


#' find x coordinate of the gap in between two columns on a 2col layout
#' @noRd
.find_midpage_x <- function(text_data, min_x = 250) {

  # the right edge of the gap (the higher x value)
  gaps_r <- .find_cols_left_x(text_data) |>
    dplyr::filter(x >= min_x) |>
    dplyr::pull(x)

  if (length(gaps_r) == 0) gaps_r <- 304

  # the left edge of the gap (the lower x value)
  gaps_l <- .find_cols_right_x(text_data, min_x = min_x) |>
    dplyr::filter(x < min(gaps_r),
                  x != 0) |>
    dplyr::pull(x)

  if (length(gaps_l) == 0 | # for no consistent midgap
      length(gaps_r) > length(gaps_l)) { # or for tabular data?
    return(290)
  } else {
    return(mean(c(gaps_l, gaps_r)))
  }

}

#' find the y coordinate of the horizontal split in mixed layout page (first page) in Elsevier
#' @noRd
.get_elsevier_divider_y <- function(text_data) {

  title_page <- text_data |>
    dplyr::filter(stringr::str_detect(text, "ARTICLE") & font_size > 10) |>
    nrow()

  title_page <- title_page == 1

  if (title_page == TRUE) {
    divider_y <- text_data |>
      dplyr::filter(y > 400) |>
      dplyr::distinct(y) |>
      dplyr::arrange(y) |>
      dplyr::mutate(y = y + 1,
                    jump_next = dplyr::lead(y) - y) |>
      dplyr::slice_max(jump_next) |>
      dplyr::pull(y)
  } else {
    second_page <- text_data |>
      dplyr::filter(stringr::str_detect(text, "Published") &
                      stringr::str_detect(dplyr::lead(text), "by") &
                      stringr::str_detect(dplyr::lead(text, 2), "Elsevier"),
                    y < 600) |>
      nrow()

    second_page <- second_page == 1

    if (second_page == TRUE) {
      divider_y <- text_data |>
        dplyr::filter(stringr::str_detect(text, "Introduction|ABBREVIATIONS") & font_size > 9 & x < 100) |>
        dplyr::mutate(y = y - 10) |>
        dplyr::summarise(y = min(y)) |>
        dplyr::pull(y)

      if (rlang::is_empty(divider_y)) divider_y <- 800
    } else {
      divider_y <- 800
    }
  }
  divider_y
}

#' add a column called column that splits the layout in the appropriate number of columns
#' @noRd
.add_column_info <- function(text_data, cols, PDF_filename) {
# text_data <- flagged_text_data

  col2_x <- 800 # initial estimate is the maximum, works for single column layouts
  col3_x <- col_predevider_x_est <- col2_x

  has_insert <- max(text_data$insert) > 0

  min_x <- dplyr::case_when(
    cols == 1 ~ 150,
    stringr::str_detect(PDF_filename, "10\\.1371") & cols == 2 ~ 199, # for plos journals # check if necessary
    stringr::str_detect(PDF_filename, "10\\.3324") & cols == 2 ~ 370, # for haematology
    stringr::str_detect(PDF_filename, "10\\.3389\\+f") & cols == 2 ~ .find_midpage_x(text_data, 170), # for frontiers

    cols == 2 ~ .find_midpage_x(text_data),
    cols == 3 ~ .find_cols_left_x(text_data)$x[2],
    cols > 3 & has_insert == FALSE ~ .find_cols_left_x(text_data)$x[2],
    .default = 150
    # .default = 150
  )

  # is_elsevier_mixed_layout <- any(stringr::str_detect(text_data$text, "https://doi.org/10.1016/j.jclinepi."))

  layout_divider_y <- 800

  if (stringr::str_detect(PDF_filename, "j\\.(jclinepi|ejc)")) {
    layout_divider_y <- .get_elsevier_divider_y(text_data)
    min_x <- .find_midpage_x(text_data |>
                               dplyr::filter(y > layout_divider_y))

  } else if (stringr::str_detect(PDF_filename, "10\\.1001\\+jama")) {
    layout_divider_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "ARTICLE") &
                      stringr::str_detect(dplyr::lead(text), "INFORMATION")) |>
      dplyr::pull(y)
    if (purrr::is_empty(layout_divider_y)) layout_divider_y <- 800
  } else if (stringr::str_detect(PDF_filename, "10\\.1159|10\\.1098\\+rs(if|pb)|10\\.3389\\+f|10\\.3324|10\\.1200|10\\.1182|10\\.1128")) {
    layout_divider_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "References|REFERENCES") & space == FALSE & x < 350) |>
      dplyr::mutate(y = y - 10) |>
      dplyr::pull(y)
    if (purrr::is_empty(layout_divider_y) | length(layout_divider_y) > 1) layout_divider_y <- 800
    if (layout_divider_y == min(text_data$y)) layout_divider_y <- 800
  } else if (stringr::str_detect(PDF_filename, "10\\.1038\\+(s41|ncomms)")) {
    cc_tag_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "creativecommons.org")) |>
      # dplyr::mutate(y = y + 20) |>
      dplyr::pull(y)

    if (!rlang::is_empty(cc_tag_y)) {

      suppressWarnings(
        layout_divider_y <- text_data |>
        dplyr::filter(y > cc_tag_y - 10,
                      font_size > dplyr::first(font_size),
                      x_jump_size < -200,
                      !stringr::str_detect(text, "AUTHOR")) |>
        dplyr::summarise(divider = min(y) - 10) |>
        dplyr::pull(divider)

      )

      if (layout_divider_y == Inf) {
        layout_divider_y <- 800 # cc_tag_y + 20
      } else {
        min_x <- 800
      }
  } else {
      layout_divider_y <- 800
    }

  } else if (stringr::str_detect(PDF_filename, "jneurosci")) {
    layout_divider_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "Significance|Received|Introduction"))

    if (nrow(layout_divider_y) >= 2) {
      layout_divider_y <- min(layout_divider_y$y[-1])
    } else {
      layout_divider_y <- 800
    }
    min_x <- .find_midpage_x(text_data |>
                               dplyr::filter(y > layout_divider_y))
  } else if (stringr::str_detect(PDF_filename, "10\\.2196")) {
    layout_divider_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "Acknowledgments") & dplyr::lag(space) == FALSE) |>
      dplyr::mutate(y = y - 10) |>
      dplyr::pull(y)
    if (purrr::is_empty(layout_divider_y) | length(layout_divider_y) > 1) {
      layout_divider_y <- 800
    } else {
      min_x <- 800
    }
  } else if (stringr::str_detect(PDF_filename, "10\\.1037")) {

    affils <- text_data |>
      dplyr::filter(stringr::str_detect(text, "Author") &
                      stringr::str_detect(dplyr::lead(text), "liations\\:") )

    if (nrow(affils) > 0) {
      layout_divider_y <- text_data |>
        dplyr::filter(y > affils$y, font_size == affils$font_size) |>
        dplyr::pull(y) |>
        max()
      layout_divider_y <- layout_divider_y + 5
    } else {
      layout_divider_y <- 800
    }

  }

  has_mixed_layout <- layout_divider_y < 700 & cols > 1
  # predivider_text <- text_data


  if (cols > 1 | is.na(cols)) {

    if (has_mixed_layout) {
      cols <- .est_col_n(text_data |>
                           dplyr::filter(y < layout_divider_y), PDF_filename)

      if (cols > 1) {
        col_predevider_x_est <- .find_midpage_x(text_data |>
                                                  dplyr::filter(y < layout_divider_y))
      } else {
        col_predevider_x_est <- 800
      }



      if (is.na(col_predevider_x_est) | is.infinite(col_predevider_x_est) ) col_predevider_x_est <- col2_x
    }

    # estimate the x coordinate of the second column, usually around 303 - 306 for 2col layout
    col2_x_est <- min_x

    # if (cols == 3) {
    col3_x_est <- suppressWarnings(
      text_data |>
        dplyr::filter(x > col2_x_est + 100 & dplyr::lag(space) == FALSE) |>
        dplyr::count(x) |>
        dplyr::filter(n > 5) |>
        dplyr::pull(x) |>
        min()
    )
    # } else {
    #   col3_x_est <- col3_x
    # }


    # if above fails, take the max estimate, reducing in effect to a single column
    if (is.na(col2_x_est)) col2_x_est <- col2_x
    # if above fails, take the max estimate, reducing in effect to a single column
    if (is.na(col3_x_est) | cols < 3) col3_x_est <- col3_x
    # if max is at least 100 to the right of estimate, then estimate is good
    if (col2_x - col2_x_est > 100) col2_x <- col2_x_est
    if (col3_x - col3_x_est > 100) col3_x <- col3_x_est
  }


  cols_w_inserts <- text_data |>
    dplyr::arrange(insert) |>
    dplyr::mutate(column = dplyr::case_when(
      insert == 0 & x >= col_predevider_x_est & y < layout_divider_y & has_mixed_layout ~ 2,
      insert == 0 & x >= col2_x & x < col3_x & !has_mixed_layout ~ 2,
      insert == 0 & x >= col3_x & !has_mixed_layout ~ 3,
      insert == 0 & x < col2_x & y >= layout_divider_y & has_mixed_layout ~ 3,
      insert == 0 & x >= col2_x & x < col3_x & y >= layout_divider_y & has_mixed_layout ~ 4,
      insert == 0 & x >= col3_x & y >= layout_divider_y & has_mixed_layout ~ 5,
      insert > 0 ~ insert + cols,
      .default = 1),
      text = dplyr::case_when(
        insert > 0 & dplyr::lag(insert, default = 0) != insert ~ paste("\n<insert>", text),
        insert > 0 & dplyr::lead(insert, default = 0) != insert ~ paste(text, "\n<iend>\n"),
        .default = text)
      )

  main_text <- cols_w_inserts |>
    dplyr::filter(insert == 0) |>
    dplyr::arrange(column, y, x)
# cols_w_inserts |>
#   dplyr::arrange(insert, column, y, x)
  #
  # res <-
  cols_w_inserts |>
    dplyr::filter(insert > 0) |>
    dplyr::arrange(column) |>
    dplyr::bind_rows(main_text)

}

# insert_label <- "Appendix"
#' find instances of tables, figures, or other inserts are on the page
#' @noRd
.find_inserts <- function(text_data) {
  insert_candidates <- text_data |>
    # dplyr::arrange(y, x) |>
    dplyr::filter(!stringr::str_detect(text, "\\u25c2")) |>
    dplyr::mutate(
      x_jump_size = x - dplyr::lag(x, default = 0) - dplyr::lag(width, default = 0),
      y_jump = abs(y - dplyr::lag(y, default = 0)),
      leads = (stringr::str_detect(dplyr::lead(text), "[A-Z]") |
        stringr::str_detect(dplyr::lead(text, 2), "[A-Z]|continued"))
      &
        (!stringr::str_detect(dplyr::lag(text, default = ""), "^see$|^and$|^in$|,|Summary") |
           dplyr::lag(font_name) != font_name & dplyr::lag(font_size) != font_size) &
        !stringr::str_detect(dplyr::lead(text), "^S\\w+:$")
      ,
      # has_insert_text = .str_has_insert(text),
      vertical = width <= height & stringr::str_length(text) > 2 & width < 15,
      verticality = mean(vertical, na.rm = TRUE)
      ) |>
    dplyr::filter(
      # y_jump != 0,
                  .str_has_insert(text) &
                    (y_jump >= 14 |
                       (x > 300 & y < 100 & dplyr::lag(space) == FALSE) |
                      (x_jump_size < -260) & y_jump < 10
                     ) |
                    stringr::str_detect(text, "^REAGENT$") &
                    stringr::str_detect(dplyr::lead(text, 1), "^or$") &
                    stringr::str_detect(dplyr::lead(text, 2), "^RESOURCE$"),
                  stringr::str_length(text) > 2 |
                    (stringr::str_detect(text, "^TA?$|^F$") &
                       stringr::str_detect(dplyr::lead(text, 1), "^A$|^B$|^I$") &
                       stringr::str_detect(dplyr::lead(text, 2), "^B$|^L$|^G$")),
                  # is.na(dplyr::lag(space)) | dplyr::lag(space) == FALSE,
                  leads == TRUE
                  ) |>
    dplyr::select(-leads)

  insert_candidates <- insert_candidates |>
    dplyr::mutate(vertical = dplyr::if_else(verticality > 0.3 & vertical == TRUE,
                                            TRUE,
                                            FALSE))
  if (any(insert_candidates$vertical)) {

    max_y <- max(text_data$y)
    min_x <- min(text_data$x)

    insert_candidates <- insert_candidates |>
      dplyr::filter(y > max_y - 20 | x < min_x + 20)

  }
  insert_candidates
}

#' detect if a string has the regex for insert
#' @noRd
.str_has_insert <- function(string) {
  stringr::str_detect(string, "^TA?$|^F$|^I$|^Table$|^Fig(u|\\.)|^Appendix(?!,)|^FIG(U|\\.)|^TABLE$|^KEY$|^REAGENT$")
}


#' find the first insert leftmost and topmost on the page and return its metadata
#' @noRd
.find_first_insert <- function(text_data) {

  inserts <- text_data |>
    .find_inserts()
  if (nrow(inserts) == 0) {
    inserts <- text_data |>
      dplyr::arrange(y, x) |>
      .add_line_n() |>
      dplyr::arrange(line_n, x) |>
      .find_inserts()
  }

  first_insert <- inserts |>
    dplyr::filter(x == min(x)) |> # has to happen in two steps!!!
    dplyr::filter(y == min(y))

  first_insert
}



#' find a table or figure starting y coordinate
#' @noRd
.find_insert_min_y <- function(text_data) {

  first_insert <- .find_first_insert(text_data)

  if (first_insert$vertical == FALSE) {
    min_y <- first_insert |>
      dplyr::pull(y)

  } else { # if vertical table
    min_y <- text_data |>
      dplyr::filter(height > width) |>
      dplyr::filter(y == min(y)) |>
      dplyr::summarise(y = dplyr::first(y)) |>
      dplyr::pull(y)
  }

  min_y
}

#' find a table or figure starting x coordinate
#' @noRd
.find_insert_min_x <- function(text_data) {

  first_insert <- .find_first_insert(text_data)

  if (nrow(first_insert) == 0) return (800)
#
#   first_insert <- inserts |>
#     dplyr::filter(x == min(x)) |>
#     dplyr::filter(y == min(y))

  midpage_gap <- .find_midpage_x(text_data)
  cols <- .find_cols_left_x(text_data) |> nrow()
  # col_width <- .get_page_width(text_data) / 2
  min_x <- text_data$x |> min()

  if (!dplyr::between(first_insert$x, min_x, midpage_gap) |
      first_insert$vertical == TRUE |
      (cols == 3 & !stringr::str_detect(first_insert$text, "T|Appendix"))) {
    min_x <- min(midpage_gap + 1, first_insert$x)
  }

  min_x
}

#' find a table or figure ending x coordinate
#' @noRd
.find_insert_max_x <- function(text_data) {
  # text_data <- flagged_text_data |> filter(insert == 0)
  start_x <- .find_insert_min_x(text_data)
  if (start_x > 199) return(800)

  start_y <- .find_insert_min_y(text_data)

  col_width <- .get_page_width(text_data) / 2

  # midpage <- min(col_width + min(text_data$x), .find_midpage_x(text_data))
  midpage <- min(col_width + min(text_data$x), 290)

  inserts <- text_data |>
    .find_inserts()

  first_insert <- text_data |>
    .find_first_insert()

  if (first_insert$text == "REAGENT") return(800)

  if (nrow(inserts) == 0) inserts <- first_insert


  if (first_insert$vertical == TRUE) {

    mean_widths <- text_data |>
      dplyr::mutate(first_col = dplyr::if_else(x < midpage, "col1", "col2")) |>
      dplyr::group_by(first_col) |>
      dplyr::summarise(width = mean(width)) |>
      tidyr::pivot_wider(names_from = first_col, values_from = width)

    if (mean_widths$col2 > 10) { # if second column horizontal
      return(midpage)
    } else { # if full page vertical

      next_insert_x <- inserts |>
        dplyr::filter(x > start_x) |>
        dplyr::arrange(x) |>
        dplyr::slice(1) |>
        dplyr::pull(x)

      if (length(next_insert_x) > 0) {
        max_x <- next_insert_x - 2
      } else {
        max_x <- 800
      }
      return(max_x)
    }
  }

  x_vals <- unique(round(inserts$x, - 1))

  if (length(x_vals) > 1) {
    sidebyside <- tidyr::expand_grid(f = x_vals, s = x_vals) |>
      dplyr::mutate(diff = abs(f - s)) |>
      dplyr::summarise(twocol = any(diff > col_width)) |>
      dplyr::pull(twocol)

    sidebyside_same_line <- inserts |>
      dplyr::filter(abs(y - first_insert$y) < 10)

    # if two-col layout with inserts in each column
    if (sidebyside) {
      # next_insert_y <- inserts |>
      #   dplyr::arrange(y, x) |>
      #   dplyr::slice(2) |>
      #   dplyr::pull(y)
      # if (next_insert_y - first_insert$y < 20) {
      max_x <- inserts |>
        dplyr::filter(dplyr::between(x, midpage, midpage + 50)) |>
        dplyr::mutate(x = x - 6) |>
        dplyr::pull(x) |>
        max(na.rm = TRUE)
      return(max_x)
    } else if (nrow(sidebyside_same_line) > 1) {
      max_x <- sidebyside_same_line |>
        dplyr::filter(x > min(x)) |>
        dplyr::mutate(x = x - 5) |>
        dplyr::pull(x) |>
        min(na.rm = TRUE)

      return(max_x)
    }
  }

  # sidebyside_same_line <- inserts |>
  #   dplyr::filter(abs(y - first_insert$y) < 10)
  #
  # # if two-col layout with inserts in each column
  # if (sidebyside) {
  #   # next_insert_y <- inserts |>
  #   #   dplyr::arrange(y, x) |>
  #   #   dplyr::slice(2) |>
  #   #   dplyr::pull(y)
  #   # if (next_insert_y - first_insert$y < 20) {
  #   max_x <- inserts |>
  #     dplyr::filter(dplyr::between(x, midpage, midpage + 50)) |>
  #     dplyr::mutate(x = x - 5) |>
  #     dplyr::pull(x) |>
  #     max(na.rm = TRUE)
  #
  #   # }
  # }
  # # else if (nrow(sidebyside_same_line) > 1) {
  # #   max_x <- sidebyside_same_line |>
  # #     dplyr::filter(x > min(x)) |>
  # #     dplyr::mutate(x = x - 5) |>
  # #     dplyr::pull(x) |>
  # #     min(na.rm = TRUE)
  # # }
  # return(max_x)

  cols <- text_data |>
    dplyr::filter(y >= start_y - 20,
                  y < start_y + 50,
                  is_subpscript == FALSE,
                  dplyr::lead(is_subpscript, default = FALSE) == FALSE,
                  text != "="
                  ) |>
    dplyr::arrange(y, x) |>
    .add_line_n() |>
    dplyr::mutate(first_col = x < midpage) |>
    dplyr::arrange(line_n, x) |>
    dplyr::group_by(line_n) |>
    dplyr::mutate(next_space_width = dplyr::lead(x) - x - width,
                  prop_space = next_space_width /
                    mean(next_space_width, na.rm = TRUE),
                  widths_left = as.numeric(first_col) * width |>
                    dplyr::na_if(0),
                  widths_right = as.numeric(first_col == FALSE) * width |>
                    dplyr::na_if(0),
                  prop_widths_left = sum(widths_left, na.rm = TRUE) / col_width,
                  prop_widths_right = sum(widths_right, na.rm = TRUE) / col_width,
                  line_start_y = any(.str_has_insert(text)),
                  n_cols = sum(space == FALSE),
                  n_cols_left = sum(first_col == TRUE & space == FALSE) |>
                    dplyr::na_if(0),
                  n_cols_right = sum(first_col == FALSE & space == FALSE) |>
                    dplyr::na_if(0)
                  ) |>
    dplyr::ungroup() |>
    dplyr::mutate(has_fig_caption_right = any(stringr::str_detect(text, "^\\([a-zA-Z]\\)$") &
                                                                  first_col == FALSE),
                  mean_cols = mean(n_cols, na.rm = TRUE),
                  mean_cols_left = mean(n_cols_left, na.rm = TRUE),
                  mean_cols_right = mean(n_cols_right, na.rm = TRUE),
                  mean_prop_widths_right = mean(prop_widths_right |> dplyr::na_if(0),
                                                na.rm = TRUE)) |>
    dplyr::filter(line_start_y == TRUE) |>
    dplyr::filter(line_n == min(line_n))

  short_title_followed_by_2col <- cols |>
    dplyr::summarise(two_col_layout = max(x, na.rm = TRUE) < midpage &
                       dplyr::first(n_cols) < dplyr::first(mean_cols) &
                       dplyr::first(mean_cols) > 1 &
                       dplyr::first(mean_cols) < 2.5) |>
    dplyr::pull(two_col_layout)

  cols_left_est <- .find_cols_left_x(text_data)



  multicol_layout <- cols |> # e.g. when caption in first col, nature
    # or in two cols, science
    dplyr::mutate(mean_cols_left = tidyr::replace_na(mean_cols_left, 0),
                  last_col_x = x == max(cols_left_est)) |>
    dplyr::summarise(multicol = dplyr::first(n_cols) > 2 &
                       dplyr::first(mean_cols) > 2 &
                       dplyr::first(prop_widths_left) < 0.7,
                     twocol = dplyr::first(n_cols) == 2 &
                       (dplyr::first(mean_cols_left) > 1 |
                          dplyr::first(has_fig_caption_right) == TRUE
                       ),
                     twothirdscol = sum(last_col_x, na.rm = TRUE) > 0 &
                       # dplyr::first(mean_cols_left) == 0 &
                                            nrow(cols_left_est) > 2
                     )
#####
  if (multicol_layout$twothirdscol == TRUE) {
    max_x <- cols_left_est |>
      dplyr::filter(x > midpage) |>
      dplyr::pull(x)

    if (min(max_x) > 400) {
      max_x <- 800
    } else {
      max_x <- min(max_x) - 2
    }
    return(max_x)
  }

  # empty_second_col <- cols |>
  #     dplyr::summarise(second_col = dplyr::first(prop_widths_right) == 0 &
  #                        max(x) < midpage &
  #                        dplyr::first(mean_prop_widths_right) > 0.5,
  #                        n() > 1
  #                      ) |> # single word on line
  #     dplyr::pull(second_col)

    if (short_title_followed_by_2col == TRUE
        # |
        # empty_second_col == TRUE
        ) {
      cols <- 2
    } else if (multicol_layout$multicol == TRUE |
               multicol_layout$twocol == TRUE) {
      cols <- 0
    } else {
      cols <- cols |>
        dplyr::summarise(
          n_breaks = sum(space == FALSE &
                           (is.na(prop_space) | (prop_space > 1 & next_space_width > 5)),
                         na.rm = TRUE)
        ) |>
        dplyr::pull(n_breaks)
    }

  if (cols < 2) { # 1 col or 2-col captions
    return(800)
  } else { # if multiple inserts in same column or no inserts

    max_x <- midpage
  }
  max_x

}

#' find a horizontal gap on the page if there is a big one
#' @noRd
.find_y_gap <- function(text_data, crit_jump_min = 30, crit_jump_max = 100) {

  if (!any(stringr::str_detect(text_data$text, "Table|Appendix"))) {
    gap_candidates <- text_data |>
      dplyr::filter(rel_width > 0.05,
                    stringr::str_length(text) > 1,
                    !stringr::str_detect(text, "\\*"))
  } else {
    gap_candidates <- text_data
  }


  suppressWarnings(
    gap_candidates <- gap_candidates |>
    # text_data |>
      dplyr::arrange(y, x) |>
      .add_line_n() |>
      dplyr::group_by(line_n) |>
      dplyr::summarise(y = max(y) # TODO:perhaps mean here?
                       # ,
                       # rel_width = mean(rel_width)
                       ) |>
      dplyr::ungroup() |>
      dplyr::mutate(y_jump = dplyr::lead(y) - y) |>
      dplyr::filter(y_jump >= crit_jump_min,
                    y_jump < crit_jump_max)
  )

    # if (nrow(gap_candidates) > 3) {
    #   gap_candidates <- gap_candidates |>
    #     dplyr::filter(y_jump != y_jump)
    # }
  gap_candidates
}

#' find a table or figure ending x coordinate
#' @noRd
.find_insert_max_y <- function(text_data) {
  # text_data <- flagged_text_data |> dplyr::filter(insert == 0)

  first_insert <- .find_first_insert(text_data)

  if (first_insert$vertical == TRUE) return (800)

  min_x <- .find_insert_min_x(text_data) # consider including figure stuff here as well
  min_y <- .find_insert_min_y(text_data)
  max_x <- .find_insert_max_x(text_data)

  min_y_next_insert <- .find_inserts(text_data) |>
    dplyr::filter(x <= max_x,
                  y > min_y) |>
    dplyr::pull(y)

  is_last_insert <- rlang::is_empty(min_y_next_insert)
  # & !stringr::str_detect(first_insert$text, "^T")
  if (max_x == 800 & is_last_insert == TRUE) {
    suppressWarnings(y_gap <- .find_y_gap(text_data) |>
      dplyr::pull(y) |>
      min()
      )
  } else {
    y_gap <- 800
  }

  if (y_gap < 800 & y_gap > min_y) return(y_gap)

  # end_section <- text_data |>
  #   dplyr::filter(stringr::str_detect(text_data$text, "REFERENCES|Abbreviations"),
  #                 rel_width < 0.5)

  has_references <- any(stringr::str_detect(text_data$text, "REFERENCES"))
  # has_references <- any(stringr::str_detect(end_section$text, "REFERENCES"))
  # has_abbreviations <- any(stringr::str_detect(end_section$text, "Abbreviations"))

  min_y_next_insert <- min(max(text_data$y) + 2, min_y_next_insert)

  if (max(text_data$x) > 600) return(min_y_next_insert - 3)

  if (min_y_next_insert < max(text_data$y) + 2 | has_references) {
    critical_max_y_jump <- 30
  } else {
    critical_max_y_jump <- 80
  }

  last_row <- text_data |>
    dplyr::filter(y >= min_y - 2,
                  y < min_y_next_insert - 1,
                  x >= min_x,
                  x <= max_x,
                  is_subpscript == FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(y, x) |>
    dplyr::mutate(y_jump = dplyr::lead(y, default = 0) - y) |>
    dplyr::filter(abs(y_jump) >= round(2.5 * font_size) | # for 7.5 font it is 20, for 7 it is 18
                    y_jump > 19 |
                    (
                      y_jump > 15 & (round(dplyr::lead(font_size)) > round(font_size) |
                       stringr::str_detect(dplyr::lead(font_name), "(B|b)old"))
                     ) |
                    y == max(y)
                  ) |>
    dplyr::mutate(y_jump = dplyr::lead(y, default = 0) - y,
                  max_y_before_last = dplyr::if_else(dplyr::lead(y == max(y), default = 0), 1, 0) * y_jump,
                  is_fig_caption = stringr::str_detect(text, "^\\(?\\w\\)?$"),
                  y_exceeded = dplyr::if_else(
                    y_jump <= 0 &
                      max(max_y_before_last, na.rm = TRUE) > critical_max_y_jump, 1, 0),
                  # for inserts on a page with the references
                  y_exceeded = dplyr::if_else(
                    (stringr::str_detect(first_insert$text, "^F") &
                      is_fig_caption) |
                      (has_references & font_size < dplyr::first(font_size)),
                    1,
                    y_exceeded),
                  y_exceeded = cumsum(y_exceeded)) |>
    dplyr::filter(y_exceeded == 0)

  if (nrow(last_row) == 0 | (is_last_insert == FALSE & min_y_next_insert == max(last_row$y) + 2)) {
    return(min_y_next_insert - 3)
  }



  last_row <- last_row |>
    dplyr::filter(
      font_size <= dplyr::first(font_size) + 0.5
      # font_size <= ceiling(dplyr::first(font_size))
      )

  last_row <- last_row |>
      dplyr::pull(y) |>
      max()

  last_row
}
# text_data <- flagged_text_data |> dplyr::filter(insert == 0)
#

#' flag a single table or figure as insert
#' @param coord_vec coordinates given as c(min_x, max_x, min_y, max_y)
#' @noRd
.flag_as_insert <- function(text_data, coord_vec, insert_n) {
  #coord_vec <- c(min_x, max_x, min_y, max_y)

  text_data |>
    dplyr::mutate(insert = dplyr::case_when(
      x >= coord_vec[1] & x <= coord_vec[2] &
        y >= coord_vec[3] - 3 & y <= coord_vec[4] ~ insert_n,
      .default = insert
    ))
}

#' flag all inserts: figures, tables, appendix tables containing Authorship Info
#' @noRd
.flag_all_inserts <- function(text_data) {

  inserts <- .find_inserts(text_data)

  flagged_text_data <- text_data |>
    dplyr::mutate(insert = 0)

  if (nrow(inserts) == 0) return(flagged_text_data)

  for (i in 1:nrow(inserts)) {
    # i <- 1
    flagged_text_data <- flagged_text_data |>
      .flag_as_insert(c(.find_insert_min_x(flagged_text_data |>
                                             dplyr::filter(insert == 0)) - 2,
                        .find_insert_max_x(flagged_text_data |>
                                             dplyr::filter(insert == 0)),
                        .find_insert_min_y(flagged_text_data |>
                                             dplyr::filter(insert == 0)) - 2,
                        .find_insert_max_y(flagged_text_data |>
                                             dplyr::filter(insert == 0)) + 2),
                      insert_n = i)

  }

  flagged_text_data
}

#' detect margins and remove text from them, as well as from hidden text layers
#' @noRd
.clear_margins <- function(text_data, PDF_filename) {

  # remove hidden text tags and layers in various journals
  if (stringr::str_detect(PDF_filename, "10\\.1016\\+j\\.(ecl|pul)")) {
    text_data <- text_data |>
      dplyr::filter(font_name != "DOHGPO+AdvP48722B",
                    font_size > 6.5) |>
      dplyr::mutate(text = stringr::str_remove(text, "Tag$"))

  } else if (stringr::str_detect(PDF_filename, "10\\.1038\\+s41")) {
    text_data <- text_data |>
      dplyr::filter(font_name != "BBKNAK+AdvTT6780a46b")
  } else if (stringr::str_detect(PDF_filename, "10\\.1371")) {
    text_data <- text_data |>
      dplyr::filter(!stringr::str_detect(font_name, "LMNJSZ"))
  } else if (stringr::str_detect(PDF_filename, "10\\.3390")) {
    text_data <- text_data |>
      dplyr::filter(!stringr::str_detect(font_name, "Palatino|Font"))
  }

  end_x <- max(text_data$x)
  if (end_x > 600) {
    max_x <- end_x + 2
  } else {
    max_x <- 563 # right margin in most journals
  }
  max_height <- 300 # vertical text, e.g. column separator "......"

  text_data <- text_data |>
    dplyr::filter(x < max_x, # remove margin text, e.g. 'downloaded from...'
                  x > 18, # remove margin text, e.g. 'downloaded from...'
                  height < max_height)
  if (nrow(text_data) == 0) return(text_data)

  min_y <- .find_header_y(text_data) # detect header in most journals
  max_y <- .find_footer_y(text_data) # detect footer in most journals

  if (text_data |> dplyr::filter(x > max_x, height > 20) |> nrow() == 0) max_x <- max(text_data$x) + 1 # for articles without margins, e.g. haematology

  text_data |>
    # remove page numbers, textboxes with citation numbers, line numbers, etc.
    # as well as the header and footer
    dplyr::filter(
      !(stringr::str_detect(text, "^\\d{1,3}\\.*$") & space == FALSE &
                      !stringr::str_detect(dplyr::lag(text), "Fig(u|\\.)|Table|Supplement|Section|and|FIG(U|\\.)|TABLE")),
                  y > min_y + 2, # remove header, extra 2 to compensate fuzzy y values
                  y < max_y - 2, # remove footer, extra 2 to compensate fuzzy y values
                  width > 1 | stringr::str_detect(text, "I|J|i|l|1"),
                  !stringr::str_detect(text, "^\\.\\W?$")) |> # remove zero width spaces and single punctuation
    dplyr::mutate(x_jump_size = x - dplyr::lag(x, default = 0) - dplyr::lag(width, default = 0),
                  is_subpscript = abs(x_jump_size) < 5 & dplyr::lag(font_size, default = 100) > font_size &
                                                              dplyr::lead(font_size, default = 100) > font_size
    )

}

#' Estimate the width of the page
#' @noRd
.get_page_width <- function(text_data) {

  # max_x <- max(text_data$x)
  # max_x <- text_data |>
  #   dplyr::filter(x == max_x) |>
  #   dplyr::mutate(x = x + width) |>
  #   dplyr::pull(x)
  # max_x - min(text_data$x)
  max(text_data$x) - min(text_data$x)
}

#' Add proportional sum of widths for each y value
#' @noRd
.add_rel_width <- function(text_data) {
  page_width <- .get_page_width(text_data)

  text_data |>
    dplyr::group_by(y) |>
    dplyr::mutate(rel_width = sum(width)/page_width) |>
    dplyr::ungroup()
}

#' convert the dataframe extracted by pdftools::pdf_data into a one-column string
#' to be saved as a txt for further processing
#' @noRd
.textbox_to_str <- function(text_data, PDF_filename) {

  if (nrow(text_data) == 0) return("")

  text_data <- text_data |>
    .clear_margins(PDF_filename) |>
    dplyr::mutate(insert = 0)

  tryCatch({
    text_data <- text_data |>
      .add_rel_width() |>
      .flag_all_inserts()
  }, error = function(e) {
    print(paste("There were insert parsing issues with", PDF_filename))
  })

  cols <- .est_col_n(text_data, PDF_filename) |> floor()

  if (nrow(text_data) < 2) return("")

  text_data <- text_data |>
    .add_column_info(cols, PDF_filename)

  page_width <- .get_page_width(text_data)

  text_data <- .add_line_n(text_data) |>
    # text_data2 <- .add_line_n(text_data) |>
    dplyr::arrange(column, line_n, x) |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0)) |>
    dplyr::group_by(line_n) |>
    dplyr::mutate(prop_blank = dplyr::case_when( # calculate approx. prop. blank space on line
      cols == 1 ~ 1 - rel_width,
      cols == 2 ~ 1 - sum(width)/(page_width/2),
      .default = 1 - sum(width)/(page_width/3)
    )) |>
    dplyr::ungroup()

  section_jump <- text_data$jump_size[text_data$jump_size > 3] |>
    Mode() * 1.3

  section_jump <- min(section_jump, 25) # some manuscripts are double-spaced

  heading_font_regex <- c("[B,b]old",
                          "\\.[B,b]",
                          "CharisSIL-Italic$",
                          "AdvTT47f7fe79.I$",
                          "AdvPSHN-H$",
                          "AdvPA5D1$",
                          "AdvOTd67905e7$",
                          "AdvTT99c4c969$",
                          "NimbusRomNo9L-Medi",
                          "Bd(Cn)?$") |>
    paste(collapse = "|")

  res <- text_data |>
    dplyr::mutate(
      dot = cumsum(dplyr::case_when(
        is.na(dplyr::lag(text)) ~ 0,
        stringr::str_detect(dplyr::lag(text), "\\.$") ~ 1,
        .default = 0
      )),
      heading_font = dplyr::if_else(
         is_subpscript == FALSE &
          (abs(font_size - dplyr::lag(font_size)) > 1.4 |
          !stringr::str_detect(text, "[[:lower:]]|\\)?\\.$") | # only caps and not end of sentence
          # font_size - regular_font_size > 1 |
          stringr::str_detect(font_name, heading_font_regex)), TRUE, FALSE),
      newline_heading = line_n == 1 & is.na(heading_font) | # very first line
        line_n > dplyr::lag(line_n) &
        (stringr::str_detect(dplyr::lag(text), "\\.$|@|www|http") | # end of line can be full stop or some email or url
           (dplyr::lag(prop_blank > 0.8)) | # previous line is very short
           (dplyr::lag(font_size < 6) & !stringr::str_detect(dplyr::lag(text), "[[:lower:]]"))) & # some lines end with citation superscripts
        (font_name != dplyr::lag(font_name)),
      paragraph_start = abs(jump_size) > section_jump,
      sameline_title = line_n > dplyr::lag(line_n) & heading_font &
        (dplyr::lead(font_name) != font_name |
           dplyr::lead(font_name, 2) != font_name |
           dplyr::lead(font_name, 3) != font_name),
      # for the three-column science layout DAS may start within the line
      science_section =
        # (
        # text == "Data" &
        # dplyr::lead(text) == "and" &
        # stringr::str_detect(dplyr::lead(text, 2), "mat")) |
        # y > 711 & dplyr::lag(space) == FALSE & stringr::str_detect(text, "^[A-Z][a-z]") |
        stringr::str_detect(font_name, "(Bold|\\.B)$") & stringr::str_detect(text, "^[A-Z]") &
        dplyr::lag(!stringr::str_detect(font_name, "(Bold|\\.B)$"))
      # |
      # text == "Submitted"
      ,
      # for some journals with das on first page (e.g. elsevier jclinepi, erj, etc.)
      plain_section =
        # should follow a new line should start with a capital, but not end on a capital (with or without fullstop) or a comma
        dplyr::lag(space) == FALSE & stringr::str_detect(text, "^[A-Z]") & !stringr::str_detect(dplyr::lag(text), "(,|[A-Z]\\.?)$") &
        (stringr::str_detect(text, ":$") | # e.g. Funding:
           stringr::str_detect(dplyr::lead(text), ":$")  & dot == dplyr::lead(dot) | # e.g. Data sharing: & line_n == dplyr::lead(line_n)
           stringr::str_detect(dplyr::lead(text, 2), ":$") & dot == dplyr::lead(dot, 2) | # e.g. Conflict of interest: & line_n == dplyr::lead(line_n, 2)
           stringr::str_detect(dplyr::lead(text, 3), ":$") & dot == dplyr::lead(dot, 3) | # e.g. Data and code availability: & line_n == dplyr::lead(line_n, 3)
           stringr::str_detect(dplyr::lead(text, 4), ":$") & dot == dplyr::lead(dot, 4) | # e.g. Availability of data and materials: & line_n == dplyr::lead(line_n, 4)
           stringr::str_detect(dplyr::lead(text, 5), ":$") & dot == dplyr::lead(dot, 5)) | # e.g. Disclosure of potential conflict of interest: & line_n == dplyr::lead(line_n, 5)
        text == "*" & dplyr::lag(space) == FALSE,
      section_start = insert == 0 &
        ((paragraph_start & (heading_font | prop_blank > 0.35 | dplyr::lag(prop_blank) > 0.35)) |
        (heading_font & prop_blank > 0.8 & dplyr::lag(space == FALSE)) |
        sameline_title |
        (prop_blank > 0.6 & dplyr::lag(space == FALSE) & !ends_dot & stringr::str_length(text) > 1) |
        newline_heading | science_section | plain_section),
      text = dplyr::if_else(section_start == FALSE | is.na(section_start), text, paste("\n<section>", text))
      ) |>
    dplyr::group_by(line_n) |>
    dplyr::summarise(text = paste(text, collapse = " ")) |>
    dplyr::summarise(text = paste(text, collapse = "\n ")) |>
    dplyr::pull(text)

  res
}
