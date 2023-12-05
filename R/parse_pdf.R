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
.est_col_n <- function(text_data, PDF_filename) {

  ralc <- space <- text <- y <- x <- line_n <- has_jama <- NULL

  # for elsevier data and code availability the column estimation is thrown off by the key ressources table

  text_data <- text_data |>
    dplyr::filter(insert == 0)

  if (nrow(text_data) == 0) return(1)

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

  # if (length(funding_y) == 0) funding_y <- 0

  if (length(reference_y) != 1) reference_y <- max(text_data$y) + 1
  if (length(reference_x) != 1) reference_x <- max(text_data$x) + 1

  if (reference_y < max(text_data$y)/2 & reference_x > max(text_data$x)/2) return(2)

  if (reference_y < funding_y) return(1)

  cols <- text_data |>
    dplyr::arrange(y, x) |>
    # exclude what could be references
    dplyr::filter(!stringr::str_detect(text, "^\\d{1,3}\\.?$"),
                  y < reference_y,
                  y > 21, # exclude upper margin
                  font_size > 4) |>
    .add_line_n()

  cols_x <- .find_cols_x(cols)

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

  linejumps <- tibble::tibble(
    y = sort(unique(text_data$y)),
    y_jump = dplyr::lead(sort(unique(text_data$y))) - y) |>
    dplyr::mutate(y_jump = dplyr::if_else(is.na(y_jump), y, y_jump))

  # typical_jump <- linejumps$y_jump |>
  #   Mode()

  header_candidate <- suppressWarnings(
    text_data |>
      dplyr::filter(y < 70,
                    !stringr::str_detect(text, "\\.")) |>
      dplyr::arrange(y, x) |>
      dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0),
                    line_n = abs(jump_size) > 4,
                    line_n = cumsum(line_n),
                    x_jump = x - dplyr::lag(x, default = 0)) |>
      dplyr::group_by(line_n) |>
      dplyr::mutate(prop_width = sum(width)/page_width,
                    max_x_jump = max(x_jump),
                    has_coded_break = stringr::str_detect(text, "\\\b"),
                    has_original_investigation = text == "Investigation" & dplyr::lag(text) == "Original" &
                      dplyr::lead(x_jump) > 40,
                    n_breaks = sum(space == FALSE, na.rm = TRUE),
                    is_insert = any(.str_has_insert(text))) |>
      dplyr::left_join(linejumps, by = "y") |>
      dplyr::filter((prop_width < 0.4 & font_size < 9) | max_x_jump > 170 |
                      prop_width < 0.2 | has_original_investigation |
                      has_coded_break,
                    is_insert == FALSE,
                    !stringr::str_detect(font_name, "Bold")) |>
      dplyr::ungroup() |>
      dplyr::filter((y_jump == max(y_jump) | stringr::str_detect(text, "20\\d{2}|\\u00a9")) & y_jump > 13) |>
      dplyr::pull(y)
      )

  if (length(header_candidate) == 0) return(0) # in case no header is detected

  max(header_candidate)
}


#' find y coordinate of footer for exclusion
#' @noRd
.find_footer_y <- function(text_data) {

  page_width <- max(text_data$x) - min(text_data$x)
  min_y <- max(max(text_data$y) - 40, 720)

  # linejumps <- tibble::tibble(
  #   y = sort(unique(text_data$y)),
  #   y_jump = dplyr::lead(sort(unique(text_data$y))) - y) |>
  #   dplyr::mutate(y_jump = dplyr::if_else(is.na(y_jump), 0, y_jump))

  months <- lubridate::month(1:12, label = TRUE, abbr = FALSE) |>
    paste(collapse = "|")

  footer_candidate <- suppressWarnings(
    text_data |>
      dplyr::filter(y > min_y) |>
      dplyr::arrange(y, x) |>
      dplyr::mutate(jump_size = y - dplyr::lag(y, default = min_y),
                    line_n = abs(jump_size) > 4,
                    line_n = cumsum(line_n)) |>
      dplyr::group_by(line_n) |>
      dplyr::arrange(line_n, x) |>
      dplyr::mutate(prop_full = sum(width)/page_width,
                    x_jump = x - dplyr::lag(x, default = 0),
                    max_x_jump = max(x_jump),
                    jump_size = max(jump_size)) |>
      # dplyr::left_join(linejumps, by = "y") |>
      dplyr::filter((prop_full < 0.6 & round(font_size) < 9) | max_x_jump > 170 |
                      stringr::str_detect(text, paste0("(?<!orcid)\\.org|release|\\u00a9|Volume|", months))) |>
      dplyr::ungroup() |>
      dplyr::filter(jump_size == max(jump_size) & jump_size > 15 | stringr::str_detect(text, paste0("(?<!orcid)\\.org|release|\\u00a9|Volume|", months))) |>
      dplyr::pull(y)
  )

  if (length(footer_candidate) == 0) return(max(text_data$y) + 1) # in case no footer is detected

  min(footer_candidate) - 1
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


#' find x coordinates of the potential columns on a page
#' @noRd
.find_cols_x <- function(text_data) {
  # has_table <- which(stringr::str_detect(text_data$text, "table \\d"))
  # table_y <- text_data |>
  #   dplyr::filter(stringr::str_detect(text, "(T|t)able") |
  #            stringr::str_detect(text, "T") &
  #            stringr::str_detect(dplyr::lead(text), "A") &
  #            stringr::str_detect(dplyr::lead(text, 2), "B") &
  #            stringr::str_detect(dplyr::lead(text, 3), "L") &
  #            stringr::str_detect(dplyr::lead(text, 4), "E")
  #          ) |>
  #   dplyr::pull(y)
  #
  # if (!rlang::is_empty(table_y)) {
  #   table_max_x <- text_data |>
  #     dplyr::filter(y == table_y) |>
  #     dplyr::pull(x) |>
  #     max(na.rm = TRUE)
  # } else {
  #   table_max_x <- min(text_data$x, na.rm = TRUE) + 1
  # }
  text_data <- text_data |>
    dplyr::filter(insert == 0)

  if (nrow(text_data) == 0) return(tibble::tibble(x = 0))

  if (min(text_data$x) > 200) return(tibble::tibble(x = min(text_data$x)))

  text_data |>
    dplyr::filter(dplyr::lag(space) == FALSE,
                  stringr::str_detect(text, "[A-Za-z]|\\d{1,3}|\\["),
                  !stringr::str_detect(text, "\\(")) |>
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

  gaps <- .find_cols_x(text_data) |>
    dplyr::filter(x >= min_x) |>
    dplyr::pull(x)


  # if (any(stringr::str_detect(text_data$text, "https://doi.org/10.1016/j.jclinepi."))) return(300)

  # gaps <- suppressMessages(
  #   text_data |>
  #     dplyr::filter(dplyr::between(x, 200, 350),
  #                   font_size > 7) |>
  #     dplyr::mutate(bins = floor(x/5) * 5) |>
  #     dplyr::count(bins) |>
  #     dplyr::full_join(tibble::tibble(bins = seq(200, 345, by = 5))) |>
  #     tidyr::replace_na(list(n = 0)) |>
  #     dplyr::arrange(bins) |>
  #     # dplyr::mutate(dist_next_bin = dplyr::lead(bins) - bins) |>
  #     # dplyr::slice_max(dist_next_bin) |>
  #     dplyr::filter(n == min(n) & n < 2)
  #   )
  #
  # if (min(gaps$bins) >= 280) {
  #   gaps <- gaps$bins
  #
  # } else {
  #   # TODO: get gaps via mode x
  #   cols_x <- .find_cols_x(text_data)
  #
  #
  #  # gaps <- gaps |>
  #  #    dplyr::arrange(bins) |>
  #  #    dplyr::mutate(dist_next_bin = dplyr::lead(bins) - bins) |>
  #  #    dplyr::filter(dist_next_bin == 5, dplyr::lead(dist_next_bin) == 5) |>
  #  #    dplyr::pull(bins)
  # }

  if (length(gaps) == 0){
    return(290)
  } else {
    return(min(gaps) - 2)
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
    divider_y <- 800
  }
  divider_y
}

#' add a column called column that splits the layout in the appropriate number of columns
#' @noRd
.add_column_info <- function(text_data, cols, PDF_filename) {
# text_data <- flagged_text_data
  ### early return for one column only?

  col2_x <- 800 # initial estimate is the maximum, works for single column layouts
  col3_x <- col_predevider_x_est <- col2_x

  min_x <- dplyr::case_when(
    stringr::str_detect(PDF_filename, "10\\.1371") & cols == 2 ~ 199, # for plos journals # check if necessary
    stringr::str_detect(PDF_filename, "10\\.3324") & cols == 2 ~ 370, # for haematology
    stringr::str_detect(PDF_filename, "10\\.3389\\+f") & cols == 2 ~ .find_midpage_x(text_data, 170), # for frontiers
    cols == 2 ~ .find_midpage_x(text_data),
    cols == 3 ~ .find_cols_x(text_data)$x[2],
    .default = 150
  )

  # is_elsevier_mixed_layout <- any(stringr::str_detect(text_data$text, "https://doi.org/10.1016/j.jclinepi."))

  layout_divider_y <- 800

  if (stringr::str_detect(PDF_filename, "j\\.jclinepi")) {
    layout_divider_y <- .get_elsevier_divider_y(text_data)
    min_x <- .find_midpage_x(text_data |>
                               dplyr::filter(y > layout_divider_y))

  } else if (stringr::str_detect(PDF_filename, "10\\.1001\\+jama")) {
    layout_divider_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "ARTICLE") &
                      stringr::str_detect(dplyr::lead(text), "INFORMATION")) |>
      dplyr::pull(y)
    if (purrr::is_empty(layout_divider_y)) layout_divider_y <- 800
  } else if (stringr::str_detect(PDF_filename, "10\\.1159|10\\.1098\\+rspb|10\\.3389\\+f|10\\.3324")) {
    layout_divider_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "References|REFERENCES") & space == FALSE & x < 350) |>
      dplyr::pull(y)
    if (purrr::is_empty(layout_divider_y) | length(layout_divider_y) > 1) layout_divider_y <- 800
    if (layout_divider_y == min(text_data$y)) layout_divider_y <- 800
  } else if (stringr::str_detect(PDF_filename, "10\\.1038\\+s41")) {
    layout_divider_y <- text_data |>
      dplyr::filter(stringr::str_detect(text, "\\u00a9") &
                      stringr::str_detect(dplyr::lead(text), "The") &
                      stringr::str_detect(dplyr::lead(text, 2), "Author\\(s\\)")) |>
      dplyr::mutate(y = y + 20) |>
      dplyr::pull(y)

    if (!rlang::is_empty(layout_divider_y)) {
      min_x <- .find_midpage_x(text_data |>
                                 dplyr::filter(y > layout_divider_y))
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
  }

  has_mixed_layout <- layout_divider_y < 700 & cols > 1
  # predivider_text <- text_data


  if (cols > 1 | is.na(cols)) {

    if (has_mixed_layout) {

      col_predevider_x_est <- .find_midpage_x(text_data |>
                                                dplyr::filter(y < layout_divider_y))
      #   text_data |>
      #     dplyr::filter(y < layout_divider_y,
      #                   x > min_x & dplyr::lag(space) == FALSE & stringr::str_detect(dplyr::lag(text), "\\."),
      #                   # font_size > 5,
      #                   stringr::str_detect(text, "[[:upper:]]")) |>
      #     dplyr::pull(x) |>
      #     min()
      # )
      # predivider_text <- text_data |>
      #   dplyr::filter(y < layout_divider_y)

      if (is.na(col_predevider_x_est) | is.infinite(col_predevider_x_est) ) col_predevider_x_est <- col2_x
    }

    # estimate the x coordinate of the second column, usually around 303 - 306 for 2col layout
    col2_x_est <- min_x
    # text_data |>
    # dplyr::filter(x > min_x & dplyr::lag(space) == FALSE,
    #               font_size > 5,
    #               stringr::str_detect(text, "[[:upper:]]|\\[|\\d\\.$")) |>
    # dplyr::pull(x) |>
    # min()

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

  # n_inserts <- max(text_data$insert)

  cols_w_inserts <- text_data |>
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
        insert > 0 & dplyr::lag(insert, default = 0) == 0 ~ paste("\n<insert>", text),
        insert > 0 & dplyr::lead(insert, default = 0) == 0 ~ paste(text, "\n<iend>"),
        .default = text)
      )

  # main_text <- cols_w_inserts |>
  #   dplyr::filter(insert == 0) |>
  #   dplyr::arrange(column, y, x)
### TODO: order by insert as well??
cols_w_inserts |>
  dplyr::arrange(insert, column, y, x)
  #
  # cols_w_inserts |>
  #   dplyr::filter(insert > 0) |>
  #   dplyr::bind_rows(main_text)

}

# insert_label <- "Appendix"
#' find instances of tables, figures, or other inserts are on the page
#' @noRd
.find_inserts <- function(text_data) {
  text_data |>
    dplyr::mutate(y_jump = abs(y - dplyr::lag(y, default = 0)),
                  leads = stringr::str_detect(dplyr::lead(text, 1), "[A-Z]") |
                                stringr::str_detect(dplyr::lead(text, 2), "[A-Z]")) |>
    dplyr::filter(.str_has_insert(text) & (y_jump > 16 | (x > 300 & y < 100)),
                  stringr::str_length(text) > 2 | (stringr::str_detect(text, "^TA?$|^F$") &
                       stringr::str_detect(dplyr::lead(text, 1), "^A$|^B$|^I$") &
                       stringr::str_detect(dplyr::lead(text, 2), "^B$|^L$|^G$")),

                  is.na(dplyr::lag(space)) | dplyr::lag(space) == FALSE,
                  leads == TRUE,
                  !stringr::str_detect(dplyr::lead(text, 1), "\\."))
}

#' detect if a string has the regex for insert
#' @noRd
.str_has_insert <- function(string) {
  stringr::str_detect(string, "^TA?$|^F$|^I$|^Table|^Fig|^Appendix")
}

#' find a table or figure starting y coordinate
#' @noRd
.find_insert_min_y <- function(text_data) {
  first_insert <- text_data |>
    .find_inserts() |>
    dplyr::filter(x == min(x)) |>
    dplyr::filter(y == min(y))

  first_insert |>
    dplyr::pull(y)
}

#' find a table or figure starting x coordinate
#' @noRd
.find_insert_min_x <- function(text_data) {

  inserts <- text_data |>
    .find_inserts()

  if (nrow(inserts) == 0) return (800)

  inserts |>
    dplyr::filter(x == min(x)) |>
    dplyr::pull(x) |>
    min()
}

#' find a table or figure ending x coordinate
#' @noRd
.find_insert_max_x <- function(text_data) {
  # text_data <- flagged_text_data |> filter(insert == 0)
  start_x <- .find_insert_min_x(text_data)
  if (start_x > 200) return(800)

  start_y <- .find_insert_min_y(text_data)

  cols <- text_data |>
    dplyr::filter(y >= start_y - 10,
                  y < start_y + 50) |>
    dplyr::arrange(y, x) |>
    .add_line_n() |>
    dplyr::group_by(line_n) |>
    dplyr::mutate(line_start_y = start_y %in% y) |>
    dplyr::filter(line_start_y == TRUE) |>
    # dplyr::ungroup() |>
    dplyr::summarize(n_breaks = sum(space == FALSE, na.rm = TRUE)) |>
    dplyr::pull(n_breaks)

  if (cols == 1) {
    return(800)
  } else {
    inserts <- text_data |>
      .find_inserts()
    # if two-col layout with inserts in each column
    if (nrow(inserts) > 1 & length(unique(inserts$x)) > 1) {
      max_x <- inserts |>
        dplyr::mutate(x = x - 2) |>
        dplyr::pull(x) |>
        max(na.rm = TRUE)
    } else { # if multiple inserts in same column
      max_x <- text_data |>
        .find_midpage_x()
    }
  }
  max_x
}

#' find a table or figure ending x coordinate
#' @noRd
.find_insert_max_y <- function(text_data) {
  # text_data <- flagged_text_data |> dplyr::filter(insert == 0)
  y_min <- .find_insert_min_y(text_data)
  x_min <- .find_insert_min_x(text_data)
  x_max <- .find_insert_max_x(text_data)

  last_row <- text_data |>
    dplyr::filter(y > y_min,
                  x >= x_min,
                  x <= x_max) |>
    dplyr::arrange(y, x) |>
    dplyr::mutate(y_jump = dplyr::lead(y, default = 0) - y) |>
    dplyr::filter(y_jump >= 24)

  if (nrow(last_row) == 0) return(800)

  last_row <- last_row |>  #== max(y_jump)) |>
    dplyr::filter(font_name == dplyr::first(font_name)) |>
    dplyr::pull(y)

  if (text_data |>
      dplyr::filter(x == x_min, y == y_min) |>
      dplyr::pull(text) |>
      stringr::str_detect("Fig|^F")) {
    last_row <- min(last_row)

  } else {
    last_row <- max(last_row)
  }
  last_row
}
# text_data <- flagged_text_data |> filter(insert == 0)
#

#' flag a single table or figure as insert
#' @param coord_vec coordinates given as c(min_x, max_x, min_y, max_y)
#' @noRd
.flag_as_insert <- function(text_data, coord_vec, insert_n) {
  #coord_vec <- c(min_x, max_x, min_y, max_y)

  text_data |>
    dplyr::mutate(insert = dplyr::case_when(
      x >= coord_vec[1] & x <= coord_vec[2] &
        y >= coord_vec[3] & y <= coord_vec[4] ~ insert_n,
      .default = insert
    ))
}

.flag_all_inserts <- function(text_data) {
  # figures/tables/appendices

  # insert_label <- "^Figure|^Table|^Appendix"
  inserts <- .find_inserts(text_data)

  flagged_text_data <- text_data |>
    dplyr::mutate(insert = 0)

  if (nrow(inserts) == 0) return(flagged_text_data)

  for (i in 1:nrow(inserts)) {
    # i <- 1
    flagged_text_data <- flagged_text_data |>
      .flag_as_insert(c(.find_insert_min_x(flagged_text_data |>
                                             dplyr::filter(insert == 0)),
                        .find_insert_max_x(flagged_text_data |>
                                             dplyr::filter(insert == 0)),
                        .find_insert_min_y(flagged_text_data |>
                                             dplyr::filter(insert == 0)),
                        .find_insert_max_y(flagged_text_data |>
                                             dplyr::filter(insert == 0))),
                      insert_n = i)

    # flagged_text_data |>
    #   dplyr::select(x, y, insert, insert_i)
    # text_data |>
    #   dplyr::anti_join(flagged_text_data, by = c("x", "y"))

  }

  flagged_text_data


}

# t2 <- text_data |>
#   # dplyr::mutate(insert = FALSE) |>
#   .flag_as_insert(c(.find_insert_min_x(text_data, insert_label),
#                                         .find_insert_max_x(text_data, insert_label),
#                                         .find_insert_min_y(text_data, insert_label),
#                                         .find_insert_max_y(text_data, insert_label)))

# t2 |>


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

  } else if (stringr::str_detect(PDF_filename, "10\\.1038\\+s41")) {
    text_data <- text_data |>
      dplyr::filter(font_name != "BBKNAK+AdvTT6780a46b")
  }

  min_y <- .find_header_y(text_data) # remove header in most journals
  max_y <- .find_footer_y(text_data) # remove footer in most journals
  max_x <- 563 # remove right margin in most journals
  max_height <- 300 # remove vertical text, e.g. column separator "......"

  if (text_data |> dplyr::filter(x > max_x, height > 20) |> nrow() == 0) max_x <- max(text_data$x) + 1 # for articles without margins, e.g. haematology

  text_data <- text_data |>
    # remove page numbers, textboxes with citation numbers, line numbers, etc.
    # as well as the header and footer
    dplyr::filter(!(stringr::str_detect(text, "^\\d{1,3}\\.*$") & space == FALSE &
                      !stringr::str_detect(dplyr::lag(text), "Fig|Table|Supplement|Section|and")),
                  y > min_y, # remove header
                  y < max_y, # remove footer
                  x < max_x, # remove margin text, e.g. 'downloaded from...'
                  x > 18, # remove margin text, e.g. 'downloaded from...'
                  height < max_height,
                  width > 1 | stringr::str_detect(text, "I|J|i|l|1"),
                  text != "\\.") |> # remove zero width spaces and single dots
    dplyr::mutate(x_jump_size = x - dplyr::lag(x, default = 0))

  text_data <- text_data |>
    .flag_all_inserts()

  # if (stringr::str_detect(PDF_filename, "10\\.7554")) { # elife always 1 col, hardcoded to prevent confusion from tables
  #   cols <- 1
  # } else {
  cols <- .est_col_n(text_data, PDF_filename) |> floor()
  # }

  if (nrow(text_data) < 2) return("")

  text_data <- text_data |>
    .add_column_info(cols, PDF_filename)

  page_width <- max(text_data$x) - min(text_data$x)
  col_width <- page_width/2 # quick approximation perhaps sufficient?

  text_data <- .add_line_n(text_data) |>
    # text_data2 <- .add_line_n(text_data) |>
    dplyr::arrange(column, line_n, x) |>
    dplyr::mutate(jump_size = y - dplyr::lag(y, default = 0)) |>
    dplyr::group_by(line_n) |>
    dplyr::mutate(prop_blank = dplyr::case_when( # calculate approx. prop. blank space on line
      cols == 1 ~ 1 - sum(width)/page_width,
      cols == 2 ~ 1 - sum(width)/col_width,
      .default = 1 - sum(width)/(page_width/3)
    )) |>
    dplyr::ungroup()


  section_jump <- text_data$jump_size[text_data$jump_size > 3] |>
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
        abs(font_size - dplyr::lag(font_size)) > 1.4 |
          !stringr::str_detect(text, "[[:lower:]]") | # only caps
          # font_size - regular_font_size > 1 |
          stringr::str_detect(font_name, heading_font_regex), TRUE, FALSE),
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