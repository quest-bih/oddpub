# Open Data Detection in Publications (ODDPub)

[![codecov](https://codecov.io/gh/quest-bih/oddpub/branch/master/graph/badge.svg)](https://codecov.io/gh/quest-bih/oddpub)
[![License: AGPL v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17602245.svg)](https://doi.org/10.5281/zenodo.17602245)

ODDPub is a text mining algorithm that parses a set of publications and detects
which publications disseminated Open Data or Open Code together with the
publication. It is tailored towards biomedical literature.

## Authors

Nico Riedel, Vladislav Nachev (vladislav.nachev@charite.de), Miriam Kip, Evgeny Bobrov (evgeny.bobrov@bih-charite.de) - QUEST Center for Transforming Biomedical Research, Berlin Institute of Health

## Publication

More information on the development and validation of the algorithm can be found in the publication https://doi.org/10.5334/dsj-2020-042. The related training and validation datasets can be found under https://osf.io/yv5rx/.

## Installation

The latest version of the algorithm is structured as an R package and can easily be installed with the following command:

``` r
# install.packages("pak") # if pak currently not installed
pak::pak("quest-bih/oddpub")
```

## Description

The algorithm searches for several categories of similar keywords in each 
sentence. Multiple categories have to match for a single sentence to trigger a 
detection. Among keyword categories are categories for specific biomedical
repositories as well as their corresponding accession numbers (as regular
expressions), general-purpose repositories or different file formats typically
used to distribute raw data in the supplement. (If the algorithm is applied to
fields other than biomedicine, classification performance is expected to be
lower, due to unfamiliar discipline-specific repository names.)

Additionally, Open Code dissemination is detected using keywords categories
for source code or typical code repositories.

## Usage

The package exposes three main functions. For faster computation these functions
can be started in several parallel processes with the use of the packages
`future` and `furrr`. Progress visualization is done via `progressr`.
This allows for the following workflow:

``` r
library(oddpub)
library(furrr) # for parallel processing and considerably faster screening
library(progressr) # for visualizing computational progress

plan(multisession) # general setting for parallel processing
handlers(global = TRUE) # general setting for progress visualization

pdf_folder <- "/path/to/input/pdf/files" # make sure slashes are right-leaning
txt_folder <- "/path/to/output/txt/files" # make sure slashes are right-leaning

conversion_success <- pdf_convert(pdf_folder, txt_folder)

# examine if any files failed to convert:
list.files(pdf_folder)[!conversion_success] 
```
Converts PDFs contained in one folder to txt-files and saves them into the output
folder. This conversion does not aim to parse the whole pdf cleanly, but to
detect the column layout and to (liberally) add section tags at the beginning of
article sections, including data and code availability statements, to enable
their detection and extraction. This is a crucial step in the workflow and
conversion via other methods will result in poorer performance.
A convention that will further enhance the PDF to txt conversion is to name the
PDF files as the DOI of the publication, with slashes "/" replaced by plus signs
"+", e.g. 10.1371+journal.pone.0302787.pdf. The txt file names are the same as
the input PDF, except for the file extension.
The DOI information in the file name improves the detection of the column layout
of the input PDF file and therefore the quality of the txt output and the
remaining ODDPub detection algorithms.

``` r
pdf_text_sentences <- oddpub::pdf_load(pdf_text_folder)
```
Loads all text files from given folder.

``` r
open_data_results <- oddpub::open_data_search(pdf_text_sentences, screen_das = "extra")
```
Actual Open Data detection. Returns for each file if Open Data or
Open Code is detected.
Additionally returns the identified Open Data/Code categories,
as well as the detected sentences, which can be deactivated using the
additional parameter ```detected_sentences = FALSE```.

The output data frame can be exported to csv, with the help of the `readr`
package.

```r
write_csv(open_data_results, "path/to/output/folder/open_data_results.csv") 
  
```

## Validation

To validate the algorithm, we manually screened a sample of 792 publications
that were randomly selected from PubMed. On this validation dataset,
our algorithm detects Open Data publications with a sensitivity of 0.73
and specificity of 0.97.

The algorithm has been updated since the original publication and validation.
A new validation is planned for Q4 2026.

## Detailed description of the keywords

In the following, we give an overview over the different parts of the keywords
used in the algorithm, in pseudo-code with function and keyword definitions.

We begin with high-level keyword categories that are searched in the full text.
During screening these categories are applied as search filters on the sentence level and are therefore
not mutually exclusive on the level of the publication (`open_data_category`
would list both values detected). Thus, if keywords in any sentence are detected
to match any of the following two categories below, the publication is flagged
as Open Data (`is_open_data` is set to TRUE).
For definitions of the individual lower-level keywords, see below.

| Compound Keyword Category | Keywords | Flag in `open_data_category` | Sentences extracted? |
|---------------------------|----------|------------------------------|----------------------|
| Field-specific repository | FIELD_SPECIFIC_REPO NEAR (ACCESSION_NR OR WEBLINK) NEAR (AVAILABLE NOT (NOT_AVAILABLE OR RECOMMENDATION)) NEAR (OWNERSHIP_CLAIM OR NOT (WAS_AVAILABLE OR REUSE)) NEAR (DATA OR NOT (MISC_NOT_DATA OR PROTOCOL OR SUPPLEMENT OR SOURCE_CODE OR GRANT)) | field-specific repository | Yes |
| General-purpose repository| REPOSITORIES NEAR (AVAILABLE NOT (NOT_AVAILABLE OR RECOMMENDATION)) NEAR (OWNERSHIP_CLAIM OR NOT (WAS_AVAILABLE OR REUSE)) NEAR (DATA OR NOT (MISC_NOT_DATA OR PROTOCOL OR SUPPLEMENT OR SOURCE_CODE)) | general-purpose repository | Yes |

In addition, if the DOI of the journal publication belongs to one of known
so-called (open access) data journals, the publication is also flagged as
open data (`is_open_data` is set to TRUE).

| Keyword Category          | Keywords | Flag in `open_data_category` | Sentences extracted? |
|---------------------------|----------|------------------------------|----------------------|
| Data journal              | DATA_JOURNAL [applied at DOI] | data journal | No |

Further copound categories of interest are listed below. They may refer to data
that do not satisfy our open data criteria, (`is_open_data` is not set to TRUE)
but are still detected and reported.

| Compound Keyword Category | Keywords | Flag in `open_data_category` | Sentences extracted? |
|---------------------------|----------|------------------------------|----------------------|
| Supplemental data sharing | DATASET OR ALL_DATA_FILE_FORMATS OR SUPPLEMENTAL_DATA | supplement | Yes |
| Dataset on Github |	DATA NEAR GITHUB NEAR AVAILABLE NOT(NOT_AVAILABLE OR WAS_AVAILABLE) | github | Yes |
| Re-use | REUSE_STATEMENTS OR (WAS_AVAILABLE NEAR_WD(30) (FIELD_SPECIFIC_REPO OR REPOSITORIES OR GITHUB)) NEAR (DATA OR NOT(MISC_NOT_DATA OR PROTOCOL OR SUPPLEMENT OR SOURCE_CODE OR GRANT))| re-use | Yes |
| Recommendation | RECOMMENDATION NEAR DATA NEAR AVAILABLE NEAR (REPOSITORIES OR FIELD_SPECIFIC_REPO) NEAR NOT(NOT_AVAILABLE OR WAS_AVAILABLE OR Re-use OR Code re-use) | recommendation | Yes |
| Unknown/misspecified source | DATA NEAR AVAILABLE NEAR WEBLINK NOT(NOT_AVAILABLE OR ACCESSION_NR OR SUPPLEMENT OR Field-specific repository OR General-purpose repository OR Re-use OR Dataset on Github OR MISC_NOT_DATA OR SUPPLEMENT) | unknown/misspecified source | Yes |
| Upon request | UPON_REQUEST | upon request | No |

The category *unknown/misspecified* catches cases that are potentially open data,
but missing a specific mention of e.g. accession number, or are linked to a
personal website, or a repository not listed in 
FIELD_SPECIFIC_REPO or REPOSITORIES. 
When multiple weblinks are detected in
the same statement then the conditions
`Dataset Github OR MISC_NOT_DATA OR SUPPLEMENT` are dropped to avoid detecting
links back to the original publication, GitHub or other non-data related sources.
GitHub is not a suitable data sharing repository, because it lacks persistence.
The categories *re-use*, *recommendation*, and *unknown/misspecified source* are
being continuously developed and their final definitions will be updated after
validation.

The detection of Open Code statements follows a similar logic, but only three
categories are flagged in the output as separate Boolean variables:
 `is_open_code`, `is_code_supplement`, and `is_code_reuse`:

| Keyword Category          | Keywords | Output column | Sentences extracted? |
|---------------------------|----------|---------------|----------------------|
| Open code availability    | SOURCE_CODE NEAR NOT(NOT_AVAILABLE) NEAR (OWNERSHIP_CLAIM OR NOT(WAS_AVAILABLE OR REUSE OR SOFTWARE_USE OR RECOMMENDATION)) NEAR (GIT_OR_URL OR (AVAILABLE NEAR NOT(UPON_REQUEST OR SUPPLEMENT OR DATASET))) | is_open_code | Yes |
| Code in supplement        | SOURCE_CODE NEAR (SUPPLEMENT OR DATASET) | is_code_supplement | Yes |
| Code re-use               | SOURCE_CODE NEAR (REUSE OR SOFTWARE_USE) NEAR NOT(NOT_AVAILABLE) NEAR (AVAILABLE OR GIT_OR_URL) | is_code_reuse | Yes | 
| No custom software        | NO_NEW_CODE | - | No |

If a match for NO_NEW_CODE (for example "This paper does not report original
code.") is detected anywhere in the publication, then any detections for
*Open code availability* get converted to *Code re-use* instead.

Further compound categories that are not extracted,
but used for the detection of higher-level categories are defined below: 

| Keyword Category          | Keywords |
|---------------------------|----------|
| DATASET                   | (DATASET_NAME OUTER DATASET_NUMBER) |
| ALL_DATA_FILE_FORMATS     | (ALL_DATA NOT(NOT_DATA)) NEAR_WD(12) FILE_FORMATS |
| SUPPLEMENTAL_DATA         | SUPPLEMENTAL_TABLE NEAR_WD(12) (FILE_FORMATS OR ALL_DATA) |
| GIT_OR_URL                | GUTHUB OR NEAR ("www" OR "http") |

Individual keyword and function defintions are further explained below
(for corresponding regular expressions, check the `keywords_patterns.yml` file): 

| Definitions    | Description  |
|----------------|--------------|
| x NEAR y       | The two keywords (or groups of keywords) x and y are in the same sentence |
| x NEAR_WD(n) y | Word distance version of NEAR that counts how many words are between the two keywords. If the number lies at or below a cutoff value (e.g. 10 words), the two input words are considered "near". This implementation is needed for cases like "S2 Table. Raw data. https://doi.org/10.1371/journal.pone.0158039.s002 (XLS)" |
| NOT(y)         | The negation of near, equivalent to y is not included in the same sentence as x |
| x OUTER y      | Matches when x is followed by y or vice versa |
| AVAILABLE      | Expressions that indicate that the data are or have been made available in some way |
| WAS_AVAILABLE  | Expressions that indicate that data are not made available in the paper, or suggest that data from a different source were used |
| REUSE_STATEMENTS| Expressions that indicate that data from previously existing sources were used |
| SOFTWARE_USE   | Expressions that indicate that software or code was used |
| OWNERSHIP CLAIM| Expressions that indicate that authors have ownership (e.g. over data or code) |
| NOT_AVAILABLE  | Expressions of negated availability |
| NO_NEW_CODE    | Expressions for (boilerplate) negation of producing new or custom code |
| RECOMMENDATION | Expressions that indicate that something is recommended, to help flag sentences where sharing of data or code is recommended but not applied |
| FIELD_SPECIFIC_REPO | Expressions for names, DOIs, and abbreviations of field-specific repositories |
| ACCESSION_NR   | Expressions that represent the accession number formats of different (biomedicine-related) repositories |
| REPOSITORIES   | Expressions for names of general-purpose repositories |
| GITHUB | Simply the word "github".	Used for compound keywords for data detection, to ensure data and not only code was shared on Github |
| DATA | Expressions for explicit data mention |
| ALL_DATA       | Expressions associated with all data or raw data |
| NOT_DATA       | Expressions of negated data |
| SOURCE_CODE    | Expressions for mentions of (source) code |
| WEBLINK        | Expression for weblinks |
| GRANT          | Expressions associated with funding statements. Useful to exclude grant numbers that may get falsely detected as accession numbers |
| SUPPLEMENT     | Expressions for supplement of an article |
| FILE_FORMATS   | Expressions for common file formats |
| UPON_REQUEST   | Expressions that indicate that data are only available upon request |
| DATA_AVAILABILITY	| Expressions for section headings for data availability statements |
| CODE AVAILABILITY | Expressions for section headings for code availability statements |
| SUPPLEMENTAL_TABLE| Expressions for mentions of supplemental tables or files |
| SUPPLEMENTAL_TABLE_NUMBER |	Expressions for numbers of supplemental tables or files |
| SUPPLEMENTAL_DATASET |	Expressions for supplemental datasets |
| DATASET_NAME	| Expressions for names for datasets |
| DATASET_NUMBER | Expressions for numbers for datasets |
| PROTOCOL | Expressions for protocols, assays, sharing of open methods |
| MISC_NON_DATA | Expressions with context that should invalidate open data detection. For example protein names, gene sequences, etc. could be falsely detected as accession numbers. |
| DATA_JOURNAL_DOIS |	Expressions for Open Data Journal DOIs for which the publication DOI is checked (from filename, not part of actual keyword search) |
| SECTION_STOPWORDS | Expressions with section titles usually following data or code availability statements, to help detect where these statements end |

## License

ODDPub is available under the APL-v3 license. See the [LICENSE](https://github.com/quest-bih/oddpub/blob/master/LICENSE) file for more info.
