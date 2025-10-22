# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-07-01
# ~ Version:        1.0
#
# ~ Script Name:    MostProlificTaxonomists
#
# ~ Script Description:
# Read species list with authorship strings.
# Parse authorship into individual author names.
# Count author occurrences across all species records.
# Print summary table and export to CSV.
#
# Copyright 2025 - Alfredo Marchio'
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(stringr)
library(dplyr)
library(readxl)
library(readr)
library(tibble)

# ---- 1) Author parser helper ----------------------------------------------
# Extract a character vector of author names from a single raw authorship string.
# - Removes trailing ", YYYY"
# - Normalizes "&" to ","
# - Splits on commas with optional spaces
extract_authors <- function(raw_authorship) {
  raw_authorship <- str_remove(raw_authorship, ", \\d{4}$")
  raw_authorship <- str_replace(raw_authorship, " & ", ", ")
  authors <- str_split(raw_authorship, ",\\s*")[[1]]
  return(authors)
}

# ---- 2) Input --------------------------------------------------------------
# Load the worksheet that includes an 'Authorship' column
df <- read_excel(file.choose())
authorships <- df$Authorship

# ---- 3) Apply parser and summarize ----------------------------------------
# Flatten all author vectors, tabulate counts, sort, and rename columns
all_authors <- unlist(lapply(authorships, extract_authors))
author_counts <- as_tibble(table(all_authors)) %>%
  arrange(desc(n)) %>%
  rename(Author = all_authors, Count = n)

print(author_counts)

# ---- 4) Save outputs -------------------------------------------------------
# Write the ranked author frequency table
write_csv(author_counts, "most_prolific_taxonomists.csv")
