plos_paper <- pdftools::pdf_data(test_path("10.1371+journal.pmed.1003873.pdf"),
                                 font_info = TRUE)
elife_paper <- pdftools::pdf_data(test_path("10.7554+elife.59907.pdf"),
                                  font_info = TRUE)
wiley_paper <- pdftools::pdf_data(test_path("10.1002+acm2.14114.pdf"),
                                  font_info = TRUE)
wkh_paper <- pdftools::pdf_data(test_path("10.1212+NXI.0000000000000763.pdf"),
                                font_info = TRUE)
rs_paper <- pdftools::pdf_data(test_path("10.1098+rsif.2022.0070.pdf"),
                               font_info = TRUE)
oxford_paper <- pdftools::pdf_data(test_path("10.1093+ndt+gfaa294.pdf"),
                                   font_info = TRUE)
frontiers_paper <- pdftools::pdf_data(test_path("10.3389+fimmu.2022.915001.pdf"),
                                      font_info = TRUE)
jama_paper <- pdftools::pdf_data(test_path("10.1001+jama.2020.7172.pdf"),
                                 font_info = TRUE)
tand_paper <- pdftools::pdf_data(test_path("10.1080+21678421.2022.2104649.pdf"),
                                 font_info = TRUE)
springer_paper <- pdftools::pdf_data(test_path("10.1007+s00424-021-02582-7.pdf"),
                                     font_info = TRUE)
science_paper <- pdftools::pdf_data(test_path("10.1126+science.abm8668.pdf"),
                                    font_info = TRUE)
nature_paper <- pdftools::pdf_data(test_path("10.1038+s41526-020-00129-1.pdf"),
                                   font_info = TRUE)
elsevier_paper <- pdftools::pdf_data(test_path("10.1016+j.ssmph.2022.101285.pdf"),
                                  font_info = TRUE)
cell_paper <- pdftools::pdf_data(test_path("10.1016+j.celrep.2022.110564.pdf"),
                                 font_info = TRUE)
r2_paper <- pdftools::pdf_data(test_path("10.21203+rs.3.rs-2838995+v1.pdf"),
                               font_info = TRUE)
asco_paper <- pdftools::pdf_data(test_path("10.1200+JCO.2017.74.7642.pdf"),
                                 font_info = TRUE)
mdpi_paper <- pdftools::pdf_data(test_path("10.3390+toxins8070200.pdf"),
                                 font_info = TRUE)
ios_paper <- pdftools::pdf_data(test_path("10.3233+TAD-190227.pdf"),
                                font_info = TRUE)
fsf_paper <- pdftools::pdf_data(test_path("10.3324+haematol.2017.168716.pdf"),
                                font_info = TRUE)
pnas_paper <- pdftools::pdf_data(test_path("10.1073+pnas.2123476119.pdf"),
                                 font_info = TRUE)
wkh2_paper <- pdftools::pdf_data(test_path("10.1097+as9.0000000000000095.pdf"),
                               font_info = TRUE)
embo_paper <- pdftools::pdf_data(test_path("10.15252+embr.202154000.pdf"),
                                 font_info = TRUE)
amegr_paper <- pdftools::pdf_data(test_path("10.21037+jgo-20-203.pdf"),
                             font_info = TRUE)
degr_paper <- pdftools::pdf_data(test_path("10.1515+jpm-2019-0153.pdf"),
                                 font_info = TRUE)
bmc_paper <- pdftools::pdf_data(test_path("10.1186+s42466-019-0022-4.pdf"),
                                font_info = TRUE)
karger_paper <- pdftools::pdf_data(test_path("10.1159+000521415.pdf"),
                                font_info = TRUE)


# text_data <- wp
.extract_insert_dim <- function(text_data, insert_num) {
  text_data |>
    dplyr::filter(insert == insert_num) |>
    dplyr::summarise(min_x = min(x),
                     max_x = max(x),
                     min_y = min(y),
                     max_y = max(y)) |>
    as.numeric()
}

.extract_col_dim <- function(text_data, col_num) {
  text_data |>
    dplyr::filter(column == col_num) |>
    dplyr::summarise(min_x = min(x),
                     max_x = max(x),
                     min_y = min(y),
                     max_y = max(y)) |>
    as.numeric()
}

.extract_gap_coords <- function(text_data) {
  text_data |>
    dplyr::group_by(column) |>
    dplyr::summarise(min_x = min(x),
                     max_x = max(x)) |>
    dplyr::summarise(gap_l = mean(if_else(column == 1, max_x, NA), na.rm = TRUE),
                     gap_r = mean(if_else(column == 2, min_x, NA), na.rm = TRUE)) |>
    as.numeric()
}

context("header and footer detection")

test_that("headers", {
  expect_equal(.find_header_y(r2_paper[[6]]), 0) # first text 37
  expect_equal(.find_header_y(wkh_paper[[3]]), 0) # no header first text 34
  expect_equal(.find_header_y(oxford_paper[[4]]), 0) # no header first text 41 (page 3)
  expect_equal(.find_header_y(rs_paper[[10]]), 44) # first text 44 (page number also on 44)
  expect_equal(.find_header_y(pnas_paper[[5]]), 0) # first text 49
  expect_equal(.find_header_y(fsf_paper[[4]]), 20) # first text 71
  expect_equal(.find_header_y(science_paper[[7]]), 22) # first text 49
  expect_equal(.find_header_y(wiley_paper[[5]]), 23) # first text 48 (page 3)
  expect_equal(.find_header_y(tand_paper[[4]]), 27) # first text 51
  expect_equal(.find_header_y(tand_paper[[7]]), 27) # first text 51
  expect_equal(.find_header_y(wkh2_paper[[4]]), 27) # first text 51
  expect_equal(.find_header_y(asco_paper[[5]]), 28) # first text 56
  expect_equal(.find_header_y(karger_paper[[4]]), 0) # first text 119
  expect_equal(.find_header_y(degr_paper[[5]]), 31) # first text 64
  expect_equal(.find_header_y(jama_paper[[3]]), 32) # first text 64
  expect_equal(.find_header_y(elsevier_paper[[4]]), 33) # first text 51
  expect_equal(.find_header_y(embo_paper[[2]]), 33) # first text 81
  expect_equal(.find_header_y(bmc_paper[[3]]), 33) # first text 81
  expect_equal(.find_header_y(springer_paper[[3]]), 34) # first text 55
  expect_equal(.find_header_y(elife_paper[[5]]), 36) # first text 53 (page 2)
  expect_equal(.find_header_y(plos_paper[[5]]), 40) # first text 77
  expect_equal(.find_header_y(frontiers_paper[[8]]), 43) # first text 91 (page 3)
  expect_equal(.find_header_y(nature_paper[[5]]), 45) # first text 59
  expect_equal(.find_header_y(mdpi_paper[[6]]), 57) # first text 90
  expect_equal(.find_header_y(amegr_paper[[9]]), 58) # first text 83
  expect_equal(.find_header_y(cell_paper[[8]]), 69) # first text 101
  expect_equal(.find_header_y(ios_paper[[4]]), 95) # first text 119

})

# text_data <- jama_paper[[3]]

test_that("footers", {
  expect_equal(.find_footer_y(wiley_paper[[10]]), 706) # no footer
  expect_equal(.find_footer_y(degr_paper[[2]]), 715) # no footer
  expect_equal(.find_footer_y(ios_paper[[6]]), 723) # no footer
  expect_equal(.find_footer_y(bmc_paper[[3]]), 729) # no footer
  expect_equal(.find_footer_y(science_paper[[7]]), 731) # last text 705
  expect_equal(.find_footer_y(asco_paper[[2]]), 735) # last text 709
  expect_equal(.find_footer_y(jama_paper[[3]]), 743) # last text 718
  expect_equal(.find_footer_y(fsf_paper[[4]]), 773) # no footer
  expect_equal(.find_footer_y(springer_paper[[12]]), 736) # no visible footer
  expect_equal(.find_footer_y(amegr_paper[[10]]), 736) # last text 695
  expect_equal(.find_footer_y(tand_paper[[6]]), 745) # no footer
  expect_equal(.find_footer_y(elife_paper[[5]]), 748) # last text 713
  expect_equal(.find_footer_y(plos_paper[[5]]), 750) # last text 701
  expect_equal(.find_footer_y(nature_paper[[5]]), 753) # last text 727
  expect_equal(.find_footer_y(elsevier_paper[[4]]), 754) # last text 731
  expect_equal(.find_footer_y(pnas_paper[[5]]), 754) # last text 731
  expect_equal(.find_footer_y(wkh2_paper[[3]]), 760) # last text 738
  expect_equal(.find_footer_y(cell_paper[[8]]), 756) # last text 727
  expect_equal(.find_footer_y(embo_paper[[2]]), 756) # last text 728
  expect_equal(.find_footer_y(wkh_paper[[3]]), 756) # last text 731
  expect_equal(.find_footer_y(mdpi_paper[[14]]), 759) # no footer
  expect_equal(.find_footer_y(tand_paper[[4]]), 760) # no footer
  expect_equal(.find_footer_y(oxford_paper[[7]]), 767) # last text 720
  expect_equal(.find_footer_y(oxford_paper[[2]]), 764) # last text 725
  expect_equal(.find_footer_y(tand_paper[[7]]), 764) # no footer
  expect_equal(.find_footer_y(r2_paper[[6]]), 767) # last text 741
  expect_equal(.find_footer_y(karger_paper[[4]]), 780) # last text 738
  expect_equal(.find_footer_y(rs_paper[[6]]), 789) # no footer
  expect_equal(.find_footer_y(frontiers_paper[[8]]), 795) # last text 742
})

# text_data <- jama_paper[[5]]
# text_data <- asco_paper[[2]] |> .clear_margins("")
context("insert flagging")

test_that("figures", {
  wiley_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(45, 259, 244, 294))

  wiley_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
     expect_equal(c(306, 534, 244, 294))

  wiley_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(45, 261, 247, 267))

  wiley_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(45, 270, 492, 521))

  wiley_paper[[7]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(3) |>
    expect_equal(c(45, 282, 642, 652))

  wiley_paper[[8]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(45, 261, 627, 637))

  wiley_paper[[10]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(45, 527, 323, 335))

  springer_paper[[4]] |>
    .clear_margins(PDF_filename = "10.1007") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(51, 521, 55, 449))

  # springer_paper[[5]] |>
  #   .clear_margins() |>
  #   .flag_all_inserts() |>
  #   .extract_insert_dim(1)

  cell_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1016+j.celrep") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(384, 537, 101, 319))

  nature_paper[[3]] |>
    .clear_margins(PDF_filename = "10.1038") |>
    .flag_all_inserts() |>
    .extract_insert_dim(3) |>
    expect_equal(c(301, 542, 290, 363))

  jama_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1001+jama") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(72, 524, 70, 606))

  tand_paper[[9]] |>
    .clear_margins(PDF_filename = "10.1080") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(65, 534, 231, 261))

  oxford_paper[[2]] |>
    .clear_margins(PDF_filename = "10.1093") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(54, 516, 714, 725))

  rs_paper[[9]] |>
    .clear_margins(PDF_filename = "10.1098+rs") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(42, 541, 477, 525))

  frontiers_paper[[7]] |>
    .clear_margins(PDF_filename = "10.3389+f") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(67, 504, 452, 487))

  elife_paper[[6]] |>
    .clear_margins(PDF_filename = "10.7554+elife") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(37, 555, 228, 329))

  asco_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1200") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(37, 535, 478, 699))

  mdpi_paper[[17]] |>
    .clear_margins(PDF_filename = "10.3390+toxins") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(97, 489, 622, 712))

  ios_paper[[6]] |>
    .clear_margins(PDF_filename = "10.3233") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(310, 504, 246, 246))

  fsf_paper[[4]] |>
    .clear_margins(PDF_filename = "10.3324") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(315, 542, 669, 733))

  science_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1126") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(36, 547, 227, 290))

  science_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1126") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(36, 537, 507, 538))

  science_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1126") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(36, 545, 292, 375))

  wkh2_paper[[4]] |>
    .clear_margins(PDF_filename = "10.1097") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(296, 523, 302, 743))

  # sp <- science_paper[[6]] |>
  #   .clear_margins(PDF_filename = "10.1126") |>
  #   .flag_all_inserts() |>
  #   .extract_insert_dim(2) |>
  #   expect_equal(c(36, 340, 405, 708))

  pnas_paper[[2]] |>
    .clear_margins(PDF_filename = "10.1073") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(35, 270, 621, 731))

  pnas_paper[[8]] |>
    .clear_margins(PDF_filename = "10.1073") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(35, 541, 600, 685))

  embo_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1073") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(225, 512, 343, 364))

  karger_paper[[4]] |>
    .clear_margins(PDF_filename = "10.1159") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(42, 178, 717, 738))

})

test_that("regular tables", {

  wiley_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .extract_insert_dim(3) |>
    expect_equal(c(306, 537, 46, 188))

  wkh_paper[[2]] |>
    .clear_margins(PDF_filename = "10.1212") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(47, 536, 46, 674))

  springer_paper[[4]] |>
    .clear_margins(PDF_filename = "10.1007") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(51, 516, 624, 700))

  jama_paper[[7]] |>
   .clear_margins(PDF_filename = "10.1001+jama") |>
   .flag_all_inserts() |>
   .extract_insert_dim(2) |>
   expect_equal(c(72, 528, 233, 429))

  cell_paper[[14]] |>
   .clear_margins(PDF_filename = "10.1016+j.celrep") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(53, 406, 118, 167))

  elsevier_paper[[5]] |>
   .clear_margins(PDF_filename = "10.1016+j.") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(306, 545, 52, 728))

  nature_paper[[3]] |>
   .clear_margins(PDF_filename = "10.1038") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(38, 266, 62, 274))

  nature_paper[[3]] |>
   .clear_margins(PDF_filename = "10.1038") |>
   .flag_all_inserts() |>
   .extract_insert_dim(2) |>
   expect_equal(c(38, 267, 311, 523))

  tand_paper[[6]] |>
   .clear_margins(PDF_filename = "10.1080") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(65, 531, 366, 742))

  oxford_paper[[4]] |>
   .clear_margins(PDF_filename = "10.1093") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(54, 535, 45, 662))

  rs_paper[[4]] |>
   .clear_margins(PDF_filename = "10.1098+rs") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(42, 522, 42, 625))

  plos_paper[[8]] |>
   .clear_margins(PDF_filename = "10.1371") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(36, 545, 490, 703))

  plos_paper[[8]] |>
   .clear_margins(PDF_filename = "10.1371") |>
   .flag_all_inserts() |>
   .extract_insert_dim(2) |>
   expect_equal(c(200, 555, 77, 394))

  frontiers_paper[[5]] |>
   .clear_margins(PDF_filename = "10.3389+f") |>
   .flag_all_inserts() |>
   .extract_insert_dim(2) |>
   expect_equal(c(56, 519, 357, 502))

  frontiers_paper[[7]] |>
   .clear_margins(PDF_filename = "10.3389+f") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(56, 533, 591, 744))

  r2_paper[[5]] |>
   .clear_margins(PDF_filename = "10.21203+rs.3.rs") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(37, 545, 37, 623))

  mdpi_paper[[14]] |>
   .clear_margins(PDF_filename = "10.3390+toxins") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(87, 489, 359, 615))

  ios_paper[[4]] |>
   .clear_margins(PDF_filename = "10.3233") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(74, 495, 116, 404))

  wkh2_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1097") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(50, 280, 433, 743))

  embo_paper[[2]] |>
    .clear_margins(PDF_filename = "10.15252") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(46, 539, 509, 728))

  bmc_paper[[3]] |>
    .clear_margins(PDF_filename = "10.15252") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(56, 287, 88, 379))

  bmc_paper[[3]] |>
    .clear_margins(PDF_filename = "10.15252") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(304, 528, 258, 726))

  karger_paper[[3]] |>
    .clear_margins(PDF_filename = "10.1159") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(42, 528, 234, 487))

})

test_that("horizontal full page tables", {

  text_data <- mdpi_paper[[4]] |>
    .clear_margins(PDF_filename = "10.3390+toxins") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(92, 731, 90, 466))
})

# test_that("vertical figures", {
#
#
# })

test_that("vertical tables", {

 tand_paper[[7]] |>
  .clear_margins(PDF_filename = "10.1080") |>
  .flag_all_inserts() |>
  .extract_insert_dim(1) |>
  expect_equal(c(315, 491, 51, 761))

 asco_paper[[3]] |>
    .clear_margins(PDF_filename = "10.1200") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(186, 388, 65, 699))

 asco_paper[[4]] |>
    .clear_margins(PDF_filename = "10.1200") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(52, 526, 59, 702))

 amegr_paper[[10]] |>
   .clear_margins(PDF_filename = "10.21037") |>
   .flag_all_inserts() |>
   .extract_insert_dim(1) |>
   expect_equal(c(44, 526, 86, 698))

})

test_that("appendix table with contributions", {
  wkh_paper[[4]] |>
    .clear_margins(PDF_filename = "10.1212") |>
    .flag_all_inserts() |>
    .extract_insert_dim(1) |>
    expect_equal(c(47, 273, 46, 629))

  wkh_paper[[4]] |>
    .clear_margins(PDF_filename = "10.1212") |>
    .flag_all_inserts() |>
    .extract_insert_dim(2) |>
    expect_equal(c(301, 517, 46, 433))

})

context("page layout estimation")

test_that("two-column layouts", {
  cell_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1016+j.celrep") |>
    .flag_all_inserts() |>
    .est_col_n(PDF_filename = "10.1016+j.celrep") |>
    floor() |>
    expect_equal(2)

  pnas_paper[[10]] |>
    .clear_margins(PDF_filename = "10.1037") |>
    .flag_all_inserts() |>
    .est_col_n(PDF_filename = "10.1037") |>
    floor() |>
    expect_equal(2)

})

test_that("three column layouts", {

  embo_paper[[2]] |>
    .clear_margins(PDF_filename = "10.15252") |>
    .flag_all_inserts() |>
    .est_col_n(PDF_filename = "10.15252") |>
    floor() |>
    expect_equal(3)

  embo_paper[[6]] |>
    .clear_margins(PDF_filename = "10.15252") |>
    .flag_all_inserts() |>
    .est_col_n(PDF_filename = "10.15252") |>
    floor() |>
    expect_equal(3)

  science_paper[[7]] |>
    .clear_margins(PDF_filename = "10.1126") |>
    .flag_all_inserts() |>
    .est_col_n(PDF_filename = "10.1126") |>
    floor() |>
    expect_equal(3)

})

test_that("mixed layouts", {

  jama_paper[[8]] |>
    .clear_margins(PDF_filename = "10.1001+jama") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 3, PDF_filename = "10.1001+jama") |>
    .extract_col_dim(2) |>
    expect_equal(c(311, 531, 64, 316))

  jama_paper[[8]] |>
    .clear_margins(PDF_filename = "10.1001+jama") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 3, PDF_filename = "10.1001+jama") |>
    .extract_col_dim(3) |>
    expect_equal(c(72, 206, 345, 716))

  pnas_paper[[10]] |>
    .clear_margins(PDF_filename = "10.1037") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1037") |>
    .extract_col_dim(2) |>
    expect_equal(c(300, 544, 38, 293))

  asco_paper[[9]] |>
    .clear_margins(PDF_filename = "10.1200") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1200") |>
    .extract_col_dim(2) |>
    expect_equal(c(299, 542, 56, 343))

  frontiers_paper[[8]] |>
    .clear_margins(PDF_filename = "10.3389+f") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.3389+f") |>
    .extract_col_dim(2) |>
    expect_equal(c(309, 531, 267, 728))

  rs_paper[[10]] |>
    .clear_margins(PDF_filename = "10.1098+rsif") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1098+rsif") |>
    .extract_col_dim(2) |>
    expect_equal(c(311, 544, 42, 402))

  fsf_paper[[8]] |>
    .clear_margins(PDF_filename = "10.3324") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.3324") |>
    .extract_col_dim(2) |>
    expect_equal(c(314, 542, 71, 609))

  karger_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1159") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1159") |>
    .extract_col_dim(2) |>
    expect_equal(c(304, 536, 62, 104))
})

test_that("column widths", {

  wiley_paper[[10]] |>
    .clear_margins(PDF_filename = "10.1002") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1002") |>
    .extract_gap_coords() |>
    expect_equal(c(282, 306))

  nature_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1038") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1038") |>
    .extract_gap_coords() |>
    expect_equal(c(280, 301))

  springer_paper[[12]] |>
    .clear_margins(PDF_filename = "10.1007") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1007") |>
    .extract_gap_coords() |>
    expect_equal(c(284, 306))

  elsevier_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1016+j.") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1016+j.") |>
    .extract_gap_coords() |>
    expect_equal(c(284, 306))

  pnas_paper[[10]] |>
    .clear_margins(PDF_filename = "10.1073") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1073") |>
    .extract_gap_coords() |>
    expect_equal(c(276, 300))

  tand_paper[[11]] |>
    .clear_margins(PDF_filename = "10.1080") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1080") |>
    .extract_gap_coords() |>
    expect_equal(c(287, 332))

  oxford_paper[[8]] |>
    .clear_margins(PDF_filename = "10.1093") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1093") |>
    .extract_gap_coords() |>
    expect_equal(c(308, 318))

  bmc_paper[[5]] |>
    .clear_margins(PDF_filename = "10.1186") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1186") |>
    .extract_gap_coords() |>
    expect_equal(c(283, 304))

  # asco_paper[[9]] |>
  #   .clear_margins(PDF_filename = "10.1200") |>
  #   .flag_all_inserts() |>
  #   .add_column_info(cols = 2, PDF_filename = "10.1200") |>
  #   .extract_gap_coords() |>
  #   expect_equal(c(283, 304))

  wkh_paper[[3]] |>
    .clear_margins(PDF_filename = "10.1212") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1212") |>
    .extract_gap_coords() |>
    expect_equal(c(285, 307))

  degr_paper[[6]] |>
    .clear_margins(PDF_filename = "10.1515") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.1515") |>
    .extract_gap_coords() |>
    expect_equal(c(279, 297))

  fsf_paper[[8]] |>
    .clear_margins(PDF_filename = "10.3324") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.3324") |>
    .extract_gap_coords() |>
    expect_equal(c(298, 314))


  amegr_paper[[16]] |>
    .clear_margins(PDF_filename = "10.21037") |>
    .flag_all_inserts() |>
    .add_column_info(cols = 2, PDF_filename = "10.21037") |>
    .extract_gap_coords() |>
    expect_equal(c(278, 308))

})

