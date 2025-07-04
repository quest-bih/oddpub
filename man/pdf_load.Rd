% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_txt.R
\name{pdf_load}
\alias{pdf_load}
\title{Load converted PDFs.}
\usage{
pdf_load(pdf_text_folder, lowercase = TRUE, remove_regex = ",")
}
\arguments{
\item{pdf_text_folder}{String of the folder name from which the converted files will be loaded.}

\item{lowercase}{Boolean, whether the text is set to lowercase upon loading. Defaults to TRUE.}

\item{remove_regex}{String, the regular expression for symbols to remove, by default commas.}
}
\value{
List with one element per document.
Each document is split into its sentences and saved as a vector of strings.
}
\description{
Loads the text files into a list of string vectors that can subsequently be searched by the text-mining algorithm.
The returned object has a list structure with one list element per document.
Each list element is a vector of strings containing the sentences of the document.
First use the function pdf_convert to create the converted files, if you have them in PDF format.
}
\examples{
\dontrun{
pdf_load("examples/", lowercase = FALSE)
}
}
