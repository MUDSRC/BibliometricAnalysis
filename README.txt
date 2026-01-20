R scripts supporting the bibliometric manuscript “A bathymetric and bibliometric analysis of glass sponge (Porifera: Hexactinellida) science”

CONTENTS
- Keyword cleaning and normalization
- Per-year keyword frequencies and high-frequency keyword extraction
- Keyword frequency plots
- Bathymetric study-effort visualizations
- General bibliometry plots
- WoRMS taxonomic traversal and prolific taxonomists extraction

REQUIREMENTS

R version >= 4.1 recommended.

Required packages:
tidyverse, bibliometrix, tidytext, readxl, worrms, rstudioapi,
tibble, rlang, forcats.

QUICKSTART

A) KEYWORD PIPELINE

1) BibliometryCleaningKW.R
Input:
- data/bibliometric_metadata/remaining_papers.csv
Required columns:
- Author Keywords
- Index Keywords
- KW_merged

Output:
- cleaned_keywords.csv

2) BibliometryGroupKeyWords.R
Input:
- cleaned_keywords.csv

Parameters:
- dedupe_within_doc (TRUE/FALSE)
- normalize_case (TRUE/FALSE)

Outputs:
- keyword_frequencies.csv
- unique_keywords.csv

3) KeywordFrequencyPlots.R
Inputs:
- unique_keywords.csv
- keyword_frequencies.csv

Outputs:
- Keyword__Barplot.pdf
- Keyword_TimeSeries.pdf

Known issues:
- Year vs PY column name mismatch between scopus and WoS formats

B) BATHYMETRIC STUDY EFFORT

BathymetricStudyEffort.R
Input:
- Bathymetric range.csv
Expected:
- PY column
- Shallow, Mesophotic, Bathyal, Abyssal, Hadal columns marked with 'x'

Outputs:
- Publication per year per depth zone.pdf
- Pie chart depth zones.pdf

C) WORMS TO TAXONOMISTS

RetrieveChildrenTaxa.R

Filters:
- marine_only = TRUE
- accepted, valid, temporary names only
- excludes infraspecific ranks

MostProlificTaxonomists.R
Input:
- CSV exported from RetrieveChildrenTaxa.R

Required columns:
- aphiaid, scientificname, authority, rank, status

Outputs:
- clean_species.csv
- first_authors.csv
- authors_long.csv
- most_prolific_taxonomists.csv

D) GENERAL BIBLIOMETRY PLOTS

BibliometryPlots.R
Inputs:
- Tables.xlsx (ArticlesPerYear, Sources sheets)
- clean_species.csv

Outputs:
- Publication over the Years.pdf
- Major Journals.pdf
- Taxonomic effort.pdf
- Major Authors.pdf (intended)

CONTACT

Alfredo Marchiò
Minderoo–UWA Deep-Sea Research Centre
The University of Western Australia
alfredo.marchio@research.uwa.edu.au
