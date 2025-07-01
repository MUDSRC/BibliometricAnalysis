# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-07-01
# ~ Version:        0.1
#
# ~ Script Name:    MostProlificTaxonomists
#
# ~ Script Description:
#
#
#
# Copyright 2025 - Alfredo Marchio'
#
# ----------------------------------------------------

## Packages
library(stringr)
library(dplyr)
library(readxl)
library(readr)
library(tibble)

## Get Authors Function
extract_authors <- function(raw_authorship) {
  raw_authorship <- str_remove(raw_authorship, ", \\d{4}$")
  raw_authorship <- str_replace(raw_authorship, " & ", ", ")
  authors <- str_split(raw_authorship, ",\\s*")[[1]]
  return(authors)
}

## Input
df <- read_excel("SpeciesListWithYear.xlsx")
authorships <- df$Authorship

## Apply and print
all_authors <- unlist(lapply(authorships, extract_authors))
author_counts <- as_tibble(table(all_authors)) %>%
  arrange(desc(n)) %>%
  rename(Author = all_authors, Count = n)

print(author_counts)

## Save table
write_csv(author_counts, "most_prolific_taxonomists.csv")
