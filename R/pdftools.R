#' set up reticulate, venv and other python backend for pdf to Markdown processing
#' @noRd
.initialize_qpdft <- function() {

  ## 1. find out where in the user the oddpub library and its python folder gets installed
  venv_path <- system.file(package = "oddpub")
  ## 2. set the RETICULATE_VIRTUALENV_ROOT to this folder
  Sys.setenv(RETICULATE_VIRTUALENV_ROOT = venv_path)
  # Sys.getenv("RETICULATE_VIRTUALENV_ROOT")

  if (!reticulate::virtualenv_exists("qpdft")) {

    message("Installing necessary libraries for pdf conversion:")

    reticulate::virtualenv_create(envname = "qpdft",
                                  python_version = "3.13",
                                  packages = c("doclayout_yolo==0.0.3",
                                               "huggingface-hub==0.28.1",
                                               "PyMuPDF==1.24.13",
                                               "numpy==2.2.2"),
                                  # force = TRUE
    )

  }
  reticulate::use_virtualenv("qpdft")

  class_initialized <- FALSE
  if (exists("PDFProcessor")) {
    if (inherits(PDFProcessor, "python.builtin.type") &
        !reticulate::py_is_null_xptr(PDFProcessor)) {
      class_initialized <- TRUE
    }
  }

  if (class_initialized == FALSE) {
    # true installation path:
    # reticulate::source_python(file.path(venv_path, "python", "pdf_processor.py"))
    # temporary development path:
    reticulate::source_python(here::here("inst", "python", "pdf_processor.py"))
  }

}

# Sys.getenv("RETICULATE_VIRTUALENV_ROOT")
### for now try out with my usual path
# usethis::edit_r_environ()


# path <- system.file("python", "doc_layout.py", package = "oddpub")

# pdf_filename <- "C:/Datenablage/ODDPub_DEV/pdf_test/10.7554+elife.59907.pdf"


#' convert PDF file to md file and save to output_folder (pdftools in Python)
#' @noRd
.pdf_to_rmd <- function(pdf_filename, output_folder, overwrite_existing_files = FALSE) {
  success <- FALSE

  tryCatch({
    # Initialize and run processor
    processor <- PDFProcessor(pdf_filename, output_folder)
    # processor$extract_markdown()
    processor$extract_text()
    success <- TRUE
  }, error = function(e) {
    print("Could not convert PDF to Markdown.")
  })

  names(success) <- pdf_filename
  return(success)
}


