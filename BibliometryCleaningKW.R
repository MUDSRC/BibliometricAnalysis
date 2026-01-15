# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-02-03
# ~ Version:        1.1
#
# ~ Script Name:    BibliometryCleaningKW
#
# ~ Script Description:
# Load raw bibliometric metadata (CSV).
# Define removal list and synonym/normalization dictionary.
# Normalize keyword strings and drop irrelevant terms.
# Produce a cleaned keywords file for downstream analyses.
#
# Copyright 2025 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(bibliometrix)
library(dplyr)
library(stringr)
library(readr)
library(tidytext)

# ---- 1) Input --------------------------------------------------------------
# Read metadata and set working directory for outputs
biblioMetadata <- read_csv("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/remaining_papers.csv")
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/")

# ---- 2) Removal list and synonym dictionary -------------------------------
# Irrelevant/generic keywords to discard (lowercase)
removedKeywords <- c(
  "review", "reviews", "paper", "papers", "revision", "insight",
  "article", "articles", "nonhuman", "study", "research",
  "priority journal", "controlled study", "abundance", "world",
  "quantitative analysis", "na"
)

# Regex-based normalization dictionary (keys = patterns, values = replacements)
synonymDictionary <- c(
  # 1) PORIFERA (general terms and dataset variants)
  "\\bporifera \\(porifera\\)\\b"                  = "porifera",
  "^\\(porifera\\)$"                               = "porifera",
  "\\bsponges?\\b"                                 = "porifera",
  "\\bmarine[ -]porifera\\b"                       = "porifera",
  "\\bdeep[- ]sea porifera\\b"                     = "porifera",
  "\\bdeep[- ]sea porifera aggregations\\b"        = "porifera bed",
  "\\bfresh[- ]water porifera\\b"                  = "porifera",
  "\\bfreshwater porifera\\b"                      = "porifera",

  "\\bsiliceous porifera\\b"                       = "porifera",
  "\\bcalcareous porifera\\b"                      = "porifera",
  
  "\\bporifera aggregations\\b"                    = "porifera bed",
  "\\bporifera beds?\\b"                           = "porifera bed",
  "\\bsea porifera grounds\\b"                     = "porifera grounds",
  
  # 2) HEXACTINELLIDA (glass sponges)
  "[^;]*hexactinellida[^;]*"                       = "hexactinellida",
  "[^;]*hexactinellid[a]?[^;]*"                    = "hexactinellida",
  "glass\\s+porifera"                              = "hexactinellida",
  "\\bhexactinellida\\b"                           = "hexactinellida",
  
  # 3) DEMOSPONGIAE
  "\\bdemosponge(s)?\\b"                           = "demospongiae",
  
  # 4) SPECIES NORMALIZATION
  "\\bdemosponge suberites-domuncula\\b"           = "suberites-domuncula",
  "\\bsuberites domuncula\\b"                      = "suberites-domuncula",
  "\\brhabdocalyptus-dawsoni lambe\\b"             = "rhabdocalyptus-dawsoni",
  "\\bcoral lophelia-pertusa\\b"                   = "lophelia-pertusa",
  
  # 5) METAZOA
  "\\banimalia\\b"                                 = "metazoa",
  "\\banimals?\\b"                                 = "metazoa",
  "\\bmetazoan\\b"                                 = "metazoa",
  "\\bmetazoon\\b"                                 = "metazoa",
  "\\bmetazoaia\\b"                                = "metazoa",
  
  # 6) DEEP SEA
  "\\bdeepsea\\b"                                  = "deep-sea",
  "\\bdeep sea\\b"                                 = "deep-sea",
  
  # 7) COMMUNITY
  "\\bcommunities\\b"                              = "community",
  
  # 8) ATLANTIC
  "\\bnorth[ -]?atlantic\\b"                       = "atlantic",
  "\\bnorth[ -]?east(?:ern)?[ -]atlantic\\b"       = "atlantic",
  "\\bne[ -]atlantic\\b"                           = "atlantic",
  "\\bnorth[ -]?west(?:ern)?[ -]atlantic\\b"       = "atlantic",
  "\\bsouth[ -]?west(?:ern)?[ -]atlantic\\b"       = "atlantic",
  "\\bmid[ -]atlantic[ -]ridge\\b"                 = "atlantic",
  "\\batlantic-ocean\\b"                           = "atlantic",
  
  # 9) BIOSILICA
  "\\bbiological silica\\b"                        = "biosilica",
  "\\bbiogenic silica\\b"                          = "biosilica",
  "\\bbio-silica\\b"                               = "biosilica",
  "\\bbiosilica-glass\\b"                          = "biosilica",
  
  # 10) OTHER NORMALIZATIONS
  "\\bbacteria \\(microorganism\\)\\b"             = "bacteria",
  "\\bcrustaceans\\b"                              = "crustacea",
  "\\bmicroscopy, electron, scanning\\b"           = "scanning electron microscopy",
  "\\bpalaeoecology\\b"                            = "paleoecology",
  "\\becosystems\\b"                               = "ecosystem",
  "\\binvertebrata\\b"                             = "invertebrate",
  "\\bbenthic\\b"                                  = "benthos",
  "\\bbritish-columbia\\b"                         = "british columbia",
  "\\bassemblages\\b"                              = "assemblage",
  "\\bspicules\\b"                                 = "spicule",
  "\\bhabitats\\b"                                 = "habitat",
  "\\bcorals\\b"                                   = "coral",
  "\\bimpact\\b"                                   = "impacts",
  "\\breefs?\\b"                                   = "reef",
  "\\bseamounts?\\b"                               = "seamount",
  "\\bsediment(ation)?s?\\b"                       = "sediments",
  "\\bsequence(?: alignment)?s?\\b"                = "sequence",
  "\\bskeletons?\\b"                               = "skeleton",
  "\\bsouth china(?: sea)?\\b"                     = "south china sea",
  "\\bspecies[ -]?(?:diversity)\\b"                = "diversity",
  "\\b(?:sp\\.\\s*nov|new species)\\b"             = "new species",
  "\\bspecies distribution model(?:ling|ing|s)?\\b"= "species distribution modelling",
  "\\bwaters?\\b"                                  = "water",
)


# ---- 3) Helpers -----------------------------------------------------------
# NOTE: These functions are part of the original script. No changes to logic.
normalize_terms <- function(x, dict) {
  pats <- names(dict)[order(nchar(names(dict)), decreasing = TRUE)]  # longest-first
  for (p in pats) x <- stringr::str_replace_all(x, p, dict[[p]])
  x
}

clean_kw_col <- function(col, dict, removed) {
  col %>%
    # Normalize synonyms first (operate on full string)
    normalize_terms(dict) %>%
    # Standardize separators and spacing
    stringr::str_replace_all("\\s*;\\s*", ";") %>%
    stringr::str_squish() %>%
    # Split to tokens
    stringr::str_split(";") %>%
    purrr::map(~{
      toks <- stringr::str_squish(unlist(.x))
      toks <- toks[toks != ""]
      # Drop irrelevant tokens by exact match (lowercased upstream)
      toks <- toks[!(toks %in% removed)]
      # Rejoin
      paste(toks, collapse = "; ")
    }) %>%
    unlist()
}

# ---- 4) Refine keyword fields --------------------------------------------
# Lowercase, then clean Author/Index keywords and merged column using the dictionary
biblioMetadata <- biblioMetadata %>%
  dplyr::mutate(across(c(`Author Keywords`, `Index Keywords`, `KW_merged`), ~ stringr::str_to_lower(.))) %>%
  dplyr::mutate(
    `Author Keywords` = clean_kw_col(`Author Keywords`, synonymDictionary, removedKeywords),
    `Index Keywords`  = clean_kw_col(`Index Keywords`,  synonymDictionary, removedKeywords),
    KW_merged         = clean_kw_col(KW_merged,         synonymDictionary, removedKeywords)
  )

# ---- 5) Save --------------------------------------------------------------
# Export normalized/cleaned metadata for downstream grouping/plots
write_csv(biblioMetadata, "cleaned_keywords.csv")
