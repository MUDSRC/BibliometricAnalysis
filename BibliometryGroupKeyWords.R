# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-10-15
# ~ Version:        1.1
#
# ~ Script Name:    BibliometryGroupKeyWords
#
# ~ Script Description:
# 1) Load cleaned bibliometric keywords.
# 2) Normalize and explode merged keywords into single records.
# 3) Optionally de-duplicate repeated keywords within each document.
# 4) Compute per-year frequencies and global totals.
# 5) Extract high-frequency keywords (threshold > 9).
# 6) Export frequency tables to CSV.
#
# Copyright 2025 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(tidyverse)
library(dplyr)

# ---- 1) Input --------------------------------------------------------------
# Use cleaned_keywords.csv produced by BibliometryCleaningKW.R
keywords <- read.csv("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/cleaned_keywords.csv")
keywords_reduced <- keywords %>% select(PY, KW_merged)

# ---- 2) Parameters ---------------------------------------------------------
# Count within-document duplicates once (TRUE) or as many times as listed (FALSE)
dedupe_within_doc <- TRUE

# Convert all keywords to lower case? (normalization toggle)
normalize_case <- FALSE

# ---- 3) Cleaning & explode keywords ---------------------------------------
# Trim empties, create synthetic document id, split on ';', and optional case normalization
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

# ---- 4) De-duplicate within document (optional) ---------------------------
if (dedupe_within_doc) {
  cleaned <- cleaned %>%
    distinct(.doc_id, PY, Keyword)
}

# ---- 5) Count frequencies --------------------------------------------------
# Per-year counts and global totals; then join and order
yearly <- cleaned %>%
  group_by(PY, Keyword) %>%
  summarise(Frequency = n(), .groups = "drop")

totals <- yearly %>%
  group_by(Keyword) %>%
  summarise(Total_Frequency = sum(Frequency), .groups = "drop")

result <- yearly %>%
  left_join(totals, by = "Keyword") %>%
  arrange(PY, Keyword)

# ---- 6) High-frequency keywords (> 9) -------------------------------------
# Build list of unique keywords across all docs and count overall frequency
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

# ---- 7) Output to CSV -----------------------------------------------------
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/")
write.csv(result,    "keyword_frequencies.csv", row.names = FALSE)
write.csv(unique_kw, "unique_keywords.csv",     row.names = FALSE)
