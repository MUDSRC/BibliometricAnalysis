# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-02-03
# ~ Version:        1.1
#
# ~ Script Name:    BibliometryCleaningKW
#
# ~ Script Description:
#
#
#
# Copyright 2025 - Alfredo Marchio'
#
# ----------------------------------------------------

## Libraries
library(bibliometrix)
library(dplyr)
library(stringr)
library(readr)
library(tidytext)

## Input
biblioMetadata <- read_csv("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/remaining_papers.csv")
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/")

## Removing irrelevant keywords
removedKeywords <- c("review",
                     "reviews",
                     "NA",
                     "na",
                     "paper",
                     "papers",
                     "article",
                     "articles",
                     "nonhuman", 
                     "study", 
                     "research",
                     "priority journal",
                     "controlled study",
                     "abundance",
                     "world",
                     "quantitative analysis")

## Synonym dictionary
synonymDictionary <- c(
  "\\bglass sponges\\b" = "hexactinellida",
  "\\bglass porifera\\b" = "hexactinellida",
  "\\bhexactinellid\\b" = "hexactinellida",
  "\\bhexactinellids\\b" = "hexactinellida",
  "\\bhexactinellida porifera\\b" = "hexactinellida",
  
  "\\bsponge\\b" = "porifera",
  "\\bsponge \\(porifera\\)\\b" = "porifera",
  "\\bporifera\\(porifera\\)\\b" = "porifera", 
  "\\bsponges\\b" = "porifera",
  
  "\\banimal\\b" = "metazoa",
  "\\banimals\\b" = "metazoa",
  "\\banimalia\\b" = "metazoa",
  "\\bmetazoan\\b" = "metazoa",
  "\\bmetazoaia\\b" = "metazoa",
  "\\bmetazoon\\b" = "metazoa",
  
  "\\bdeepsea\\b" = "deep-sea",
  "\\bdeep sea\\b" = "deep-sea",
  
  "\\bbacteria (microorgansim)\\b" = "bacteria",
  "\\bcrustaceans\\b" = "crustacea",
  "\\bmicroscopy, electron, scanning\\b" = "scanning electron microscopy",
  "\\becosystems\\b" = "paleoecology",
  "\\bpalaeoecology\\b" = "ecosystem",
  "\\binvertebrata\\b" = "invertebrate"
  
)

## Refining the matrix
biblioMetadata <- biblioMetadata %>%
  mutate(across(c(`Author Keywords`, `Index Keywords`, `KW_merged`), ~ str_to_lower(.))) %>%  # Convert to lowercase
  mutate(across(c(`Author Keywords`, `Index Keywords`, `KW_merged`), ~ str_replace_all(., synonymDictionary))) %>%  # Standardize synonyms
  mutate(across(c(`Author Keywords`, `Index Keywords`, `KW_merged`), ~ str_remove_all(., paste(removedKeywords, collapse = "|"))))  # Remove irrelevant terms

## Save
write_csv(biblioMetadata, "cleaned_keywords.csv")
