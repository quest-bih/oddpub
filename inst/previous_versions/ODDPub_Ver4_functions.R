
# set of utility functions used in the Open Data Detection

#---------------------------------------------------------------------
# 1 - PDF conversion
#---------------------------------------------------------------------

#converts PDF file to txt file and saves it to output_folder
#requires the pdftotext program that is run via a terminal command
PDF_to_text <- function(PDF_filename, output_folder, overwriteExistingFiles = FALSE)
{
  success <- FALSE
  output_filename <- create_output_filename(PDF_filename, output_folder)

  if(!file.exists(PDF_filename)) {
    print("PDF file does not exist!")
  } else if(file.exists(output_filename) && overwriteExistingFiles == FALSE) {
    print("Output file already exists!")
    success <- TRUE
  } else  {
    tryCatch({
      com <- paste0('pdftotext ', '\"', PDF_filename, '\" ',
                    '\"', output_filename, '\"')
      system(com, wait = T)
      success <- TRUE
    }, error=function(e){
      print("Could not convert pdf to text.")
    })
  }

  names(success) <- PDF_filename
  return(success)
}


create_output_filename <- function(PDF_filename, output_folder)
{
  output_filename <- sub('.pdf', '.txt', PDF_filename)
  output_filename <- tail(strsplit(output_filename, "/")[[1]],1)
  output_filename <- paste0(output_folder, output_filename)

  return(output_filename)
}


#---------------------------------------------------------------------
# 4 - Open data identification
#---------------------------------------------------------------------

#identifies the text fragments in which the Open Data keywords were detected
text_fragments <- function(open_data_tibble) {

  sentence_idx <- apply(open_data_tibble[2:5], 2, which)

  if(length(sentence_idx) == 0) {
    return(c("", "", "", ""))
  }

  text_frag <- vector()
  for(idx in sentence_idx)
  {
    if(length(idx) == 0) {
      text_frag <- c(text_frag, "")
    } else {
      current_txt <- open_data_tibble$value[idx] %>% paste(collapse = ";     ")
      text_frag <- c(text_frag, current_txt)
    }
  }

  return(text_frag)
}
