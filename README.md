# Open Data Detection in Publications (ODDPub)

ODDPub is a text mining algorithm that parses a set of publications and detects
which publications disseminated Open Data or Open Code together with the
publication. It is tailored towards biomedical literature.

The algorithm searches for several categories of similar keywords in each 
sentence. Multiple categories have to match for a single sentence to trigger a 
detection. Among keyword categories are categories for specific biomedical
databases as well as their corresponding accession numbers (as regular
expressions), general-purpose repositories or different file formats typically
used to distribute raw data in the supplement.

Additionally, Open Code dissemination is detected using keywords categories
for source code or code repositories.

The workflow of the script is as follows:
 - publication PDFs are taken from a specified input folder and converted to 
 text files using the pdftotext program (which needs to be installed and 
 accessible via the command line)
 - the converted PDFs are loaded and split into individual sentences
 - all sentences are paresed for combinations of the keyword categories to
 determine the Open Data & Open Code status
 - as an additional output the sentences in which the Open Data & Open Code 
 statements are detected are given

The algorithm was applied to an institutional publication record (N=8654) and 
achieved a sensitivity of 93.9% and a specificity of 96.2% on a sample 
consisting of 248 publications detected as Open Data by the algorithm + a random
sample of 500 negative cases (however, parts of the sample was used to develop
the algorithm. An independent validation sample is still work in progress.)

In the following, we give an overview over the different parts of the keywords used in the algoritm

| Definitions    | Description  |  Keywords  |
|----------------|--------------|------------|
| x NEAR y       | Are the two keywords (or groups of keywords) x and y in the same sentence? | |
| x NEAR_WD(n) y | Second definition of NEAR. This time counts how many words are between the two keywords. If the number lies below a cutoff value (e.g. 10 words), the two input words are considered "near". This additional definition is needed for cases like "S2 Table. Raw data. https://doi.org/10.1371/journal.pone.0158039.s002 (XLS)" | |
| NOT y          | y is not included in the same sentence | |
| AVAILABLE      | Abbreviation for a set of words that frequently occur to denote that the data have been made available in some way | ("included" OR "deposited" OR "released" OR "is provided" "are provided" OR "contained in" OR "available" OR "reproduce" OR "accessible" OR "can be accessed" OR "submitted" OR "can be downloaded" OR “reported in” OR “uploaded to”) |
| WAS_AVAILABLE  | Set of words that indicate that data is not made available in the paper, but instead that data from a different source was used | ("was provided" OR "were provided" OR "was contained in" OR "were contained in" OR "was available" OR "were available" OR "was accessible" OR "were accessible" OR "available from" OR "deposited by" OR "were reproduced") |
| NOT_AVAILABLE  | Set of negated availability phrases | ("not included" OR "not deposited" OR "not released" OR "not provided" OR "not contained in" OR "not available" OR "not accessible" OR "not submitted")|
| UPON_REQUEST   | Phrase describing that data are only available upon request | ("upon request" OR "on request" OR "upon reasonable request") |
| ALL_DATA       | Set of words describing all data or raw data | ("all data" OR  "all array data" OR "raw data" OR "full data set" OR "full dataset") |
| NOT_DATA       | Set of negations of the data phrases | ("not all data" OR "not all array data" OR "no raw data" OR "no full data set" OR "no full dataset") |
| GENE_DB        | Set of names and abbreviations of field specific (gene-related) databases | ("GEO" OR "Gene Expression Omnibus" OR "European Nucleotide Archive" OR "National Center for Biotechnology Information" OR "NCBI" OR "European Molecular Biology Laboratory" OR "EMBL-EBI" OR "BioProject" OR "Sequence Read Archive" OR "SRA" OR "ENA" OR "MassIVE" OR "ProteomeXchange" OR "Proteome Exchange" OR "ProteomeExchange" OR "MetaboLights" OR "Array-Express" OR "ArrayExpress" OR "PRIDE" OR "DNA Data Bank of Japan" OR "DDBJ" OR "Genbank" OR "Protein Databank" OR "PDB" OR "Metagenomics Rapid Annotation using Subsystem Technology" OR "MG-RAST" OR "accession number" OR "accession code" OR "accession numbers" OR "accession codes") |
| ACCESSION_NR   | Set of regular expressions that represent the accession number formats of different (biomedicine-related) databases | ("GSE[[:digit:]]{2,8}" OR "GSM[[:digit:]]{2,8}" OR "GDS[[:digit:]]{2,8}" OR "GPL[[:digit:]]{2,8}" OR "(E\|D\|S)RP[:digit:]{6,}" OR "PRJ(E\|D\|N\|EB\|DB\|NB)[:digit:]+" OR "ERS[:digit:]{6,}" OR "SAM(E\|D\|N)[A-Z]?[:digit:]+" OR "[A-Z]{1}[:digit:]{5}\\.[:digit:]+" OR "[A-Z]{2}[:digit:]{6}\\.[:digit:]+" OR "[A-Z]{4}S?[:digit:]{8,9}\\.[:digit:]+" OR "[A-Z]{3}[:digit:]{5}\\.[:digit:]+" OR "[A-Z]{5}[:digit:]{7}\\.[:digit:]+" OR "[A-Z]{1}[:digit:]{5}" OR "[A-Z]{2}[:digit:]{6}" OR "[A-Z]{3}[:digit:]{5}" OR "[A-Z]{4}[:digit:]{8}" OR "[A-Z]{5}[:digit:]{7}" OR "[A-Z]{6}[:digit:]{9}" OR "(E\|D\|S)RX[:digit:]{6,}" OR "(E\|D\|S)RR[:digit:]{6,}" OR "GCA_[:digit:]{9}\\.[:digit:]+" OR "PRJNA[[:digit:]]{3,8}" OR "SRP[[:digit:]]{3,8}" OR "SRR[[:digit:]]{3,8}" OR "SRX[[:digit:]]{3,8}" OR "SRS[[:digit:]]{3,8}" OR "SRZ[[:digit:]]{3,8}" OR "MSV[[:digit:]]{7,9}" OR "PDX[[:digit:]]{5,6}" OR "E-[A-Z]{4}-[:digit:]{1,6}" OR "PXD[[:digit:]]{5,6}" OR "PRD[[:digit:]]{5,6}" OR "DRA[[:digit:]]{6}" OR "ERA[[:digit:]]{6}" OR "DRP[[:digit:]]{6}" OR "ERP[[:digit:]]{6}" OR "DRX[[:digit:]]{6}" OR "DRS[[:digit:]]{6}" OR "DRR[[:digit:]]{6}" OR "[:digit:]{1}[A-Z]{1}[[:alnum:]]{2}") |
| REPOSITORIES   | Set of names of general-purpose repositories | ("figshare" OR "dryad" OR "zenodo" OR “dataverse” OR “osf”) |
| FILE_FORMATS   | Set of file formats | ("csv" OR "zip" OR "xls" OR "xlsx") |
| SOURCE_CODE    | Set of expressions describing source code | ("source code" OR "analysis script" OR "github" OR "SAS script" OR "SPSS script" OR "R script" OR "R code" OR "python script" OR "python code" OR "matlab script" OR "matlab code") |
| SUPPLEMENT     | Set of expression describing the supplement of an article | ("supporting information" OR "supplement" OR "supplementary data") |


| Combined Keyword Category | Keywords |
|---------------------------|----------|
| Specific databases        | GENE_DB NEAR ACCESSION_NR NEAR (AVAILABLE NOT (NOT_AVAILABLE OR WAS_AVAILABLE)) |
| General-purpose databases | REPOSITORIES NEAR (AVAILABLE NOT (NOT_AVAILABLE OR WAS_AVAILABLE)) |
| Supplementary raw/full data with specific file format | (ALL_DATA NOT NOT_DATA) NEAR_WD FILE_FORMATS |
| All Open Data keywords combined | (((GENE_DB NEAR ACCESSION_NR) OR REPOSITORIES NEAR (AVAILABLE NOT (NOT_AVAILABLE OR WAS_AVAILABLE)) OR ((ALL_DATA NOT NOT_DATA) NEAR_WD FILE_FORMATS) |

Additionally, the detection of Open Code statements is done with the following keywords:

| Combined Keyword Category | Keywords |
|---------------------------|----------|
| Source-code availability  | SOURCE_CODE NEAR AVAILABLE NOT (NOT_AVAILABLE OR WAS_AVAILABLE OR UPON_REQUEST) |
| Supplementary Source-code | SOURCE_CODE NEAR SUPPLEMENT |
| All Open Code keywords combined | SOURCE_CODE NEAR (SUPPLEMENT OR (AVAILABLE NOT (NOT_AVAILABLE OR WAS_AVAILABLE OR UPON_REQUEST)) |


