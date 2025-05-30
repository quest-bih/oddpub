
.keyword_search_tokenized_2 <- function(pdf_text_sentences)
{
  return(pdf_text_sentences |>
           .keyword_search_full() |>
           .keyword_search_tokenized())
}

test_that("field_specific_databases",
          {
            expect_true(.keyword_search_tokenized_2("deposited in geo with accession number gse77534" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("The crystallographic data and structure were deposited in the Protein Data Bank under ID: 5AHK." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("CIF file for the Na12[Co5POM] is deposited with the Cambridge Crystallographic Data Centre (CCDC no. 1558372)" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("data described herein is available at european nucleotide archive under the project identifiers prjeb402 prjeb6610 and prjeb7988 pangaea48–50" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("Microarray-based methylome data are available at Array-Express (E-MTAB-5797)." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("genome and chromatin immunoprecipitation sequencing data have been deposited in the ncbi sequence read archive and gene expression omnibus databases (bioproject 320056 and data set gse81160 respectively)." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("the rna-seq data for human cd4 þ central memory t cells referenced in this study are available in the ‘european nucleotide archive’ with the accession codes erp004883 (ref." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("the new 18s genotypes reported in this paper are available in the genbank under the accession number(s): kj170100.1 to kj170108.1  5." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("the data reported in this article have been deposited in the proteomexchange (accession number pxd004606)." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("Metabolome data have been submitted to the Metabolights database75 (http://www.ebi.ac.uk/metabolights/) with the accession number: MTBLS168." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("are available in the OpenAgrar repository: https://doi.org/10.17590/20171025-153520 and https://doi.org/10.17590/20171025-154025" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("Microarray data were uploaded in Gene Expression Omnibus (GEO) repository, accession number: GSE94381." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("were deposited in the Electron Microscopy Data Bank (European Molecular Biology Laboratory-European Bioinformatics Institute, Cambridge, UK) with the accession numbers EMD-3221 (classical), EMD-3223" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("Raw sequencing data are available via the European Genome-phenome Archive (accession EGAS00001002213)." |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in the European Variation Archive with ID GCA_000002285.2" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in dbVar with ID nstd102" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in Mgnify with ID MGYA00088411" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in NeuroMorpho with ID NMO_00001" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in GenomeRNAi with ID GR00175-A" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in the Database of Interacting Proteins with ID DIP:310N" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in the Global Biodiversity Information Facility with doi 10.15468/aomfnb" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in the FlowRepository with ID FR-FCM-ZY68" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in NAHDAP with ID ICPSR 33581" |> tolower())[["com_specific_repo"]])
            expect_true(.keyword_search_tokenized_2("deposited in the UK Data Service with ID SN 854233" |> tolower())[["com_specific_repo"]])
          })



test_that("general_purpose_databases",
          {
            expect_true(.keyword_search_tokenized_2("openly available via http://dx.doi. org/10.5061/dryad.p4s57.")[["com_general_repo"]])
            expect_true(.keyword_search_tokenized_2("data availability statement: all the relevant data for this study is available from the http://figshare.com database (figshare.com/s/ 205e6d4a552511e4856c06ec4bbcf141 http://dx.doi. org/10.6084/m9.figshare.1206313.")[["com_general_repo"]])
            expect_true(.keyword_search_tokenized_2("data was uploaded as online supporting information (file s2) and deposited in zenodo (doi 10.5281/zenodo.17098).")[["com_general_repo"]])
            expect_true(.keyword_search_tokenized_2("All files are available from the Open Science Framework database" |> tolower())[["com_general_repo"]])
            expect_true(.keyword_search_tokenized_2("the raw quantification data files have been deposited in the Mendeley Data repositroy (http://dx.doi.org/10.17632/vgpmnzdz55.1)" |> tolower())[["com_general_repo"]])
            expect_true(.keyword_search_tokenized_2("Data Availability Statement: All relevant data are available from Dryad (doi:10.5061/dryad.g8143).")[["com_general_repo"]])
          })

test_that("data_on_github",
          {
            expect_true(.keyword_search_tokenized_2("The datasets generated and analysed during the current study are available in the github repository https://github.com/finlaycampbell/BMC_outbreaker2." |> tolower())[["com_github_data"]])
            expect_true(.keyword_search_tokenized_2("Data are available from https://github.com/ralphkenna/CGG." |> tolower())[["com_github_data"]])
            expect_true(.keyword_search_tokenized_2("All analysis R code and summarized datasets are publicly accessible via Github (github.com/wanderswest/IRRIG)." |> tolower())[["com_github_data"]])
            expect_true(.keyword_search_tokenized_2("All code and data necessary to reproduce the results of this paper is freely available on GitHub" |> tolower())[["com_github_data"]])
            expect_true(.keyword_search_tokenized_2("All the benchmark datasets and source code can be downloaded from GitHub (https://github.com/QUST-BSBRC/PsePSSM-DCCA-LFDA/)." |> tolower())[["com_github_data"]])
            expect_false(.keyword_search_tokenized_2("The code of Labclock Web can be downloaded from its public repository: https://github.com/txipi/Labclock-Web." |> tolower())[["com_github_data"]])
            expect_false(.keyword_search_tokenized_2("The scripts needed to run our models is available from https://github.com/CINPLA." |> tolower())[["com_github_data"]])
          })



example_text <- list(a = c("sentence",
                             "deposited in geo with accession number gse77534",
                             "sentence"),
                     b = c("sentence",
                             "sentence",
                             "sentence"))

test_that("open_data_search",
          {
            expect_equivalent(open_data_search(example_text)$is_open_data, c(TRUE, FALSE))
            expect_equivalent(open_data_search(example_text)$is_open_code, c(FALSE, FALSE))
            expect_equivalent(open_data_search(example_text)$is_open_data, c(TRUE, FALSE))
            expect_equivalent(open_data_search(example_text)$is_open_code, c(FALSE, FALSE))
          })

test_that("open_data_sentences",
          {
            expect_equivalent(open_data_search(example_text)$open_data_statements,
                              c("deposited in geo with accession number gse77534", ""))
          })

