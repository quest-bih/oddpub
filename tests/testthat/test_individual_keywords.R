keywords <- .create_keyword_list()



test_that("available",
          {
            expect_true(.detect_keywords("The data were available in ", "available"))
            expect_true(.detect_keywords("The data were deposited in ", "available"))
            expect_true(.detect_keywords("The data were submitted to ", "available"))
            expect_false(.detect_keywords("The data are unavailable ", "available"))
          })

test_that("was_available",
          {
            expect_true(.detect_keywords("The data were available in ", "was_available"))
            expect_true(.detect_keywords("The data were deposited by ", "was_available"))
            expect_true(.detect_keywords("The data was provided by ", "was_available"))
            expect_false(.detect_keywords("The data were deposited in", "was_available"))
            expect_false(.detect_keywords("The data are available from", "was_available"))
          })

test_that("not_available",
          {
            expect_true(.detect_keywords("The data were not included in this ", "not_available"))
            expect_true(.detect_keywords("The data were not available by ", "not_available"))
            expect_true(.detect_keywords("The data were not accessible from the database ", "not_available"))
            expect_false(.detect_keywords("The data were available", "not_available"))
            expect_false(.detect_keywords("The data are provided", "not_available"))
          })

test_that("field_specific_repo",
          {
            expect_true(.detect_keywords("data were deposited in the geo repository ", "field_specific_repo"))
            expect_true(.detect_keywords("data were deposited in the sequence read archive", "field_specific_repo"))
            expect_true(.detect_keywords("data were deposited in the dbGaP database ", "field_specific_repo"))
            expect_true(.detect_keywords("data were deposited in the treebase database ", "field_specific_repo"))
            expect_false(.detect_keywords("data were deposited in the database ", "field_specific_repo"))
          })

test_that("accession_nr",
          {
            expect_true(.detect_keywords("accession nr GSE77534", "accession_nr"))
            expect_true(.detect_keywords("accession nr GSM77534", "accession_nr"))
            expect_true(.detect_keywords("accession nr GDS77534", "accession_nr"))
            expect_true(.detect_keywords("accession nr GPL77534", "accession_nr"))
            expect_true(.detect_keywords("accession nr ERP105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr DRP105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr SRP105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr SRZ105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr SRS105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr MSV1014893", "accession_nr"))
            expect_true(.detect_keywords("accession nr PDX105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr PRJNA251432", "accession_nr"))
            expect_true(.detect_keywords("accession nr PRJN251432", "accession_nr"))
            expect_true(.detect_keywords("accession nr PRJD251432", "accession_nr"))
            expect_true(.detect_keywords("accession nr PRJNB251432", "accession_nr"))
            expect_true(.detect_keywords("accession nr PRJDB251432", "accession_nr"))
            expect_true(.detect_keywords("accession nr ERS1058934", "accession_nr"))
            expect_true(.detect_keywords("accession nr SAMD1058934", "accession_nr"))
            expect_true(.detect_keywords("accession nr ERX105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr SRX105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr DRR105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr ERR105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr PDX105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr PRD105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr DRA105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr ERA105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr DRP105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr ERP105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr DRX105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr DRS105893", "accession_nr"))
            expect_true(.detect_keywords("accession nr GCA_123456789.1", "accession_nr"))
            expect_true(.detect_keywords("accession nr MF150710", "accession_nr"))
            expect_true(.detect_keywords("accession nr PRJEB11743", "accession_nr"))
            expect_true(.detect_keywords("accession nr SRX668701", "accession_nr"))
            expect_true(.detect_keywords("accession nr SRP080911", "accession_nr"))
            expect_true(.detect_keywords("accession nr E-MTAB-3870", "accession_nr"))
            expect_true(.detect_keywords("accession nr KP796148", "accession_nr"))
            expect_true(.detect_keywords("accession nr MG100841", "accession_nr"))
            expect_true(.detect_keywords("accession nr KU170128-40", "accession_nr"))
            expect_true(.detect_keywords("accession nr NSHJ01000000", "accession_nr"))
            expect_true(.detect_keywords("accession nr KX808392", "accession_nr"))
            expect_true(.detect_keywords("accession nr 6C4I", "accession_nr"))
            expect_true(.detect_keywords("accession nr DRA006579", "accession_nr"))
            expect_true(.detect_keywords("accession nr MTBLS168", "accession_nr"))
            expect_true(.detect_keywords("accession nr 10.17590", "accession_nr"))
            expect_true(.detect_keywords("accession nr 10.5073", "accession_nr"))
            expect_true(.detect_keywords("accession nr EMD-3221", "accession_nr"))
            expect_true(.detect_keywords("accession nr 1558372", "accession_nr"))
            expect_true(.detect_keywords("accession nr S24351", "accession_nr"))
            expect_true(.detect_keywords("accession nr NC_027838", "accession_nr"))
            expect_true(.detect_keywords("accession nr ss48400959", "accession_nr"))
            expect_true(.detect_keywords("accession nr phs000166.v2.p1", "accession_nr"))
            expect_true(.detect_keywords("accession nr IM-9475", "accession_nr"))
            expect_true(.detect_keywords("accession nr AB981664", "accession_nr"))
            expect_true(.detect_keywords("accession nr SCV000677102,", "accession_nr"))
            expect_true(.detect_keywords("accession nr MKYY00000000", "accession_nr"))
            expect_true(.detect_keywords("accession nr GCA_000002285.2", "accession_nr"))
            expect_true(.detect_keywords("accession nr GCJV01000000", "accession_nr"))
            expect_true(.detect_keywords("accession nr KY286086", "accession_nr"))
            expect_true(.detect_keywords("accession nr nstd102", "accession_nr"))
            expect_true(.detect_keywords("accession nr SRS2651772", "accession_nr"))
            expect_true(.detect_keywords("accession nr MGYA00088411", "accession_nr"))
            expect_true(.detect_keywords("accession nr ASM1483986v1", "accession_nr"))
            expect_true(.detect_keywords("accession nr CD0004002012", "accession_nr"))
            expect_true(.detect_keywords("accession nr NMO_00001", "accession_nr"))
            expect_true(.detect_keywords("accession nr 10.25493/A2KP-FKD", "accession_nr"))
            expect_true(.detect_keywords("accession nr GR00339-A-1", "accession_nr"))
            expect_true(.detect_keywords("accession nr DIP:19766N", "accession_nr"))
            expect_true(.detect_keywords("accession nr IM-11377-7", "accession_nr"))
            expect_true(.detect_keywords("accession nr JGAD00000000192", "accession_nr"))
            expect_true(.detect_keywords("accession nr 146156434", "accession_nr"))
            expect_true(.detect_keywords("accession nr P-GEAD-10", "accession_nr"))
            expect_true(.detect_keywords("accession nr 10.6073/pasta/c174404b0bb5d9a65bc8eccb40db825c", "accession_nr"))
            expect_true(.detect_keywords("accession nr 10.15468/aomfnb", "accession_nr"))
            expect_true(.detect_keywords("accession nr 10.5063/F1J964SR", "accession_nr"))
            expect_true(.detect_keywords("accession nr FR-FCM-ZY68", "accession_nr"))
            expect_true(.detect_keywords("accession nr P1110", "accession_nr"))
            expect_true(.detect_keywords("accession nr AJ457961", "accession_nr"))
            expect_true(.detect_keywords("accession nr 708451", "accession_nr"))
            expect_true(.detect_keywords("accession nr ICPSR 3049", "accession_nr"))
            expect_true(.detect_keywords("accession nr SN 8680", "accession_nr"))
            expect_true(.detect_keywords("accession nr CHEMBL3301451", "accession_nr"))
          })

test_that("repositories",
          {
            expect_true(.detect_keywords("data were uploaded to figshare ", "repositories"))
            expect_true(.detect_keywords("data were uploaded to osf ", "repositories"))
            expect_true(.detect_keywords("data were uploaded to zenodo", "repositories"))
            expect_true(.detect_keywords("data were uploaded to DataverseNL", "repositories"))
            expect_true(.detect_keywords("data were uploaded to the GigaScience database", "repositories"))
            expect_false(.detect_keywords("data were uploaded to github ", "repositories"))
          })

test_that("data",
          {
            expect_true(.detect_keywords("all data were available ", "data"))
            expect_true(.detect_keywords("the full dataset was available", "data"))
            expect_true(.detect_keywords("the datasets were available ", "data"))
            expect_false(.detect_keywords("the database included ", "data"))
          })

test_that("all_data",
          {
            expect_true(.detect_keywords("all data are in supplementary data S1", "all_data"))
            expect_true(.detect_keywords("the full data set is available as online data", "all_data"))
            expect_false(.detect_keywords("all data were available ", "all_data"))
            expect_false(.detect_keywords("the data were available ", "all_data"))
          })

test_that("not_data",
          {
            expect_true(.detect_keywords("not all data were available ", "not_data"))
            expect_true(.detect_keywords("no full data set was available", "not_data"))
            expect_false(.detect_keywords("all data were available ", "not_data"))
          })

test_that("source_code",
          {
            expect_true(.detect_keywords("the source code was available ", "source_code"))
            expect_true(.detect_keywords("all data and code were uploaded ", "source_code"))
            expect_true(.detect_keywords("all scripts related to the analysis ", "source_code"))
            expect_true(.detect_keywords("code and data generated for ", "source_code"))
            expect_false(.detect_keywords("available from github ", "source_code"))
            expect_false(.detect_keywords("bar code on the tube ", "source_code"))
          })

test_that("supplement",
          {
            expect_true(.detect_keywords("in the supplement ", "supplement"))
            expect_true(.detect_keywords("in the supporting information", "supplement"))
            expect_false(.detect_keywords("with support from ", "supplement"))
          })

test_that("file_formats",
          {
            expect_true(.detect_keywords("Table 1 (XLSX)", "file_formats"))
            expect_true(.detect_keywords("Table 1 (csv)", "file_formats"))
            expect_true(.detect_keywords("Table 1 (fasta)", "file_formats"))
            expect_false(.detect_keywords("are displayed in Table 1", "file_formats"))
            expect_false(.detect_keywords("unzip the file", "file_formats"))
          })

test_that("upon_request",
          {
            expect_true(.detect_keywords("available upon request", "upon_request"))
            expect_true(.detect_keywords("available on request", "upon_request"))
            expect_false(.detect_keywords("have been requested", "upon_request"))
          })

test_that("data_availability",
          {
            expect_true(.detect_keywords("data availability statement", "data_availability"))
            expect_true(.detect_keywords("data deposition", "data_availability"))
            expect_false(.detect_keywords("data were available from", "data_availability"))
          })

test_that("supplemental_table",
          {
            expect_true(.detect_keywords("supplementary table S3", "supplemental_table"))
            expect_true(.detect_keywords("supplemental tables 1-4", "supplemental_table"))
            expect_true(.detect_keywords("S2 Table", "supplemental_table"))
            expect_true(.detect_keywords("additional file 8", "supplemental_table"))
            expect_true(.detect_keywords("additional files 2-4", "supplemental_table"))
            expect_true(.detect_keywords("s2 file", "supplemental_table"))
          })

test_that("dataset",
          {
            expect_true(.detect_keywords("data S3", "dataset"))
            expect_true(.detect_keywords("data set S3", "dataset"))
            expect_true(.detect_keywords("S2 data", "dataset"))
            expect_true(.detect_keywords("supplementary data 1", "dataset"))
            expect_true(.detect_keywords("supplemental data set 18", "dataset"))
            expect_true(.detect_keywords("supplemental data set S8", "dataset"))
            expect_true(.detect_keywords("S1 dataset", "dataset"))
            expect_false(.detect_keywords("datasets 1-4", "dataset"))
            expect_false(.detect_keywords("data set S30456", "dataset"))
            expect_false(.detect_keywords("2 supplementary data", "dataset"))
            expect_false(.detect_keywords("supplemental data 2014", "dataset"))
            expect_false(.detect_keywords("data available from", "dataset"))
            expect_false(.detect_keywords("phase 3 data", "dataset"))
            expect_false(.detect_keywords("datasets 80 women", "dataset"))
            expect_false(.detect_keywords("8 databases", "dataset"))
            expect_false(.detect_keywords("transcriptome data 10 cdpk genes", "dataset"))
            expect_false(.detect_keywords("no data 98.7", "dataset"))
            expect_false(.detect_keywords("using cd4 data to estimate", "dataset"))
            expect_false(.detect_keywords("multimedia appendix 2 data tables", "dataset"))
            expect_false(.detect_keywords("in the dataset 4 gold standard", "dataset"))
          })

test_that("data_journal_dois",
          {
            expect_true(.detect_keywords("10.1038/s41597-019-6854651", "data_journal_dois"))
            expect_true(.detect_keywords("10.3390/data.3456fg", "data_journal_dois"))
            expect_true(.detect_keywords("10.1016/j.dib.354656", "data_journal_dois"))
          })


context("search of nearby keywords")

test_that("all_data_file_formats",
          {
            expect_true(.detect_keywords("all data word word word word word xlsx", "all_data_file_formats"))
            expect_true(.detect_keywords("xlsx word word word word word all data", "all_data_file_formats"))
            expect_true(.detect_keywords("raw data word word word word word csv", "all_data_file_formats"))
            expect_true(.detect_keywords("raw data word word. word word word csv", "all_data_file_formats"))
            # expect_false(.detect_keywords("all data word word word word word word word word word word word xlsx", "all_data_file_formats"))
          })

test_that("supp_table_data",
          {
            expect_true(.detect_keywords("supplementary table S3 word word word word word xlsx", "supp_table_data"))
            expect_true(.detect_keywords("supplementary table S3 word word word word word all data", "supp_table_data"))
            expect_true(.detect_keywords("csv word word word word word all S2 Table", "supp_table_data"))
            expect_true(.detect_keywords("S2 Table word word word word word raw data", "supp_table_data"))
            expect_true(.detect_keywords("raw data word word. word word word additional file 8", "supp_table_data"))
            expect_true(.detect_keywords("Proteomics data for all unique proteins detected (Table S1) (XLSX)", "supp_table_data"))
            # expect_false(.detect_keywords("supplementary table S3 word word word word word word word word word word word xlsx", "supp_table_data"))
          })

test_that("data_availability_statement",
          {
            expect_true(.has_das("<section> data availability word word word word word doi", keywords))
            expect_true(.has_das("<section> data sharing word word word word word GSE77534", keywords))
            expect_true(.has_das("<section> availability of data and materials word word word word word figshare", keywords))
            expect_true(.has_das("<section> Deposited Data Original data This paper https://data.mendeley.com/datasets/ https://doi.org/10.17632/rypw3jv8mj.1" |> tolower(), keywords))
            expect_true(.has_das("<section> data availability word word word word word word word word word word word word word word word word word word word word word word doi", keywords))
            expect_true(.has_das("<section> Data Accessibility DNA sequence data and genotypes to assess recombination in the Wave family were archived in NCBI SRA (PRJNA493979)" |> tolower(), keywords))
            expect_true(.has_das("<section> Deposited Data Original data This paper https://data.mendeley.com/datasets/ https://doi.org/10.17632/rypw3jv8mj.1" |> tolower(), keywords))
            expect_false(.has_das("doi word word word word word data availability", keywords))
            expect_false(.has_das("data availability word word word word word word word word word word word word word word word word word word word word word word word word word word word word word word word word doi", keywords))
          })