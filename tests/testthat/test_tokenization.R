test_that("paste_accession_nr",
          {
            expect_equal(.correct_tokenization(c("accession nr.", "123")), "accession nr. 123")
            expect_equal(.correct_tokenization(c("accession no.", "123")), "accession no. 123")
            expect_equal(.correct_tokenization(c("accession nrs.", "123")), "accession nrs. 123")
            expect_equal(.correct_tokenization(c("accession nos.", "123")), "accession nos. 123")
            expect_equal(.correct_tokenization(c("acc no.", "123")), "acc no. 123")
            expect_equal(.correct_tokenization(c("fig.", "S1")), "fig. S1")
            expect_equal(.correct_tokenization(c("zenodo.", "org/sometext")), "zenodo. org/sometext")
            expect_equal(.correct_tokenization(c("doi.", "org/sometext")), "doi. org/sometext")
            expect_equal(.correct_tokenization(c("et al.", "for their data")), "et al. for their data")
          })