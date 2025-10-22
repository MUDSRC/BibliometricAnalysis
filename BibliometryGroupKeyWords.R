# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-10-15
# ~ Version:        1.1
#
# ~ Script Name:    BibliometryGroupKeyWords
#
# ~ Script Description:
#
#
#
# Copyright 2025 - Alfredo Marchio'
#
# ----------------------------------------------------

## Libraries
library(tidyverse)
library(dplyr)

## Input 
# USe cleaned_keywords.csv derived from BibliometryCleaningKW.R
keywords <- read.csv("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/cleaned_keywords.csv")
keywords_reduced <- keywords %>% select(PY, KW_merged)


##Parameters
# If a keyword appears multiple times within the same row/document, count it once (TRUE) or as many times as listed (FALSE).
dedupe_within_doc <- TRUE

# Convert to lower-case? (set to TRUE to normalize case)
normalize_case <- FALSE


## Cleaning & explode keywords
cleaned <- keywords_reduced %>%
  mutate(
    PY = as.integer(PY),
    KW_merged = na_if(KW_merged, ""),
    KW_merged = if_else(str_trim(KW_merged) == "", NA_character_, KW_merged),
    .doc_id = row_number()  # synthetic document id for optional de-duplication
  ) %>%
  drop_na(KW_merged) %>%
  separate_rows(KW_merged, sep = ";") %>%
  mutate(
    Keyword = str_trim(KW_merged),
    Keyword = if (normalize_case) str_to_lower(Keyword) else Keyword
  ) %>%
  filter(Keyword != "")


## De-duplicate repeated keywords within the same document row
if (dedupe_within_doc) {
  cleaned <- cleaned %>%
    distinct(.doc_id, PY, Keyword)
}

## Count frequencies
yearly <- cleaned %>%
  group_by(PY, Keyword) %>%
  summarise(Frequency = n(), .groups = "drop")

totals <- yearly %>%
  group_by(Keyword) %>%
  summarise(Total_Frequency = sum(Frequency), .groups = "drop")

result <- yearly %>%
  left_join(totals, by = "Keyword") %>%
  arrange(PY, Keyword)

## Retrieve keywords with frequency at least
unique_kw <- keywords %>%
  select(KW_merged) %>%
  filter(!is.na(KW_merged), KW_merged != "") %>%
  separate_rows(KW_merged, sep = ";") %>%
  transmute(Keyword = str_trim(KW_merged)) %>%
  filter(Keyword != "") %>%
  mutate(Keyword = if (normalize_case) str_to_lower(Keyword) else Keyword) %>%
  count(Keyword, name = "Total_Frequency") %>%
  filter(Total_Frequency > 9) %>%
  arrange(desc(Total_Frequency))


## Output to CSV
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/")
write.csv(result, "keyword_frequencies.csv", row.names = FALSE)
write.csv(unique_kw, "unique_keywords.csv", row.names = FALSE)
