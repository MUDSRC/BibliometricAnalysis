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
                     "paper",
                     "papers",
                     "revision",
                     "insight",
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

synonymDictionary <- c(

  # Porifera (general)
  "\\b(sponge \\(porifera\\)|porifera\\(porifera\\)|porifera porifera)\\b" = "porifera",
  "\\b(sponges|sponge)\\b" = "porifera",
  "\\bmarine[- ]porifera\\b" = "porifera",
  "\\bporifera[- ]grounds?\\b" = "porifera grounds",
  
  # Hexactinellida
  "\\b(hexactinellida|hexactinellida porifera|hexactinellid(?:s)?|hexactinellid[- ]porifera|porifera[- ]hexactinellida)\\b" = "hexactinellida",
  "\\b(?:glass porifera(?: reef| porifera)?|glass)\\b" = "hexactinellida",
  "\\b(?:hexactinellida\\W+porifera|porifera\\W+hexactinellida)\\b" = "hexactinellida",
  
  # Demospongiae
  "\\b(demosponge|demosponges)\\b" = "demospongiae",
  
  # Species normalization
  "\\b(demosponge suberites-domuncula|suberites domuncula)\\b" = "suberites-domuncula",
  "\\b(rhabdocalyptus-dawsoni lambe)\\b" = "rhabdocalyptus-dawsoni",
  "\\bcoral lophelia-pertusa\\b" = "lophelia-pertusa",
  
  # Metazoa
  "\\b(animalia|animals|animal|metazoan|metazoon|metazoaia)\\b" = "metazoa",
  
  # Deep sea
  "\\b(deepsea|deep sea)\\b" = "deep-sea",
  
  # Community
  "\\bcommunity structure\\b" = "community",
  "\\bcommunities\\b"= "community",
  
  # Atlantic
  "\\bnorth[- ]?atlantic\\b" = "atlantic", 
  "\\bnorth[- ]?east(?:ern)?[- ]atlantic\\b" = "atlantic",
  "\\bne[- ]atlantic\\b" = "atlantic", 
  "\\bnorth[- ]?west(?:ern)?[- ]atlantic\\b" = "atlantic",  
  "\\bsouth[- ]?west(?:ern)?[- ]atlantic\\b" = "atlantic", 
  "\\bmid[- ]atlantic[- ]ridge\\b" = "atlantic",
  "\\batlantic-ocean\\b" = "atlantic",
  
  # Biosilica
  "\\b(biological silica|biogenic silica|bio-silica|biosilica-glass)\\b" = "biosilica",
  
  # Misc. normalizations / spelling / singularization
  "\\bbacteria \\(microorganism\\)\\b" = "bacteria",
  "\\bcrustaceans\\b" = "crustacea",
  "\\bmicroscopy, electron, scanning\\b" = "scanning electron microscopy",
  "\\bpalaeoecology\\b" = "paleoecology",
  "\\becosystems\\b" = "ecosystem",
  "\\binvertebrata\\b" = "invertebrate",
  "\\bbenthic\\b" = "benthos",
  "\\bbritish-columbia\\b" = "british columbia",
  "\\bassemblages\\b" = "assemblage",
  "\\bspicules\\b" = "spicule",
  "\\bhabitats\\b" = "habitat",
  "\\bcorals\\b" = "coral",
  "\\bimpact\\b" = "impacts",
  "\\breefs?\\b" = "reef",
  "\\bseamounts?\\b" = "seamount",
  "\\bsediment(ation)?s?\\b" = "sediments",
  "\\bsequence(?: alignment)?s?\\b" = "sequence",
  "\\bskeletons?\\b" = "skeleton",
  "\\bsouth china(?: sea)?\\b" = "south china sea",
  "\\bspecies[- ]?(?:diversity|richness)\\b" = "species diversity",
  "\\b(?:sp\\.\\s*nov|new species)\\b" = "new species",
  "\\bspecies distribution model(?:ling|ing|s)?\\b" = "species distribution modelling",
  "\\bwaters?\\b" = "water"
)

## Helpers
normalize_terms <- function(x, dict) {
  pats <- names(dict)[order(nchar(names(dict)), decreasing = TRUE)]  # longest-first
  for (p in pats) x <- stringr::str_replace_all(x, p, dict[[p]])
  x
}

clean_kw_col <- function(col, dict, removed) {
  col %>%
    # 1) normalize synonyms first (on the full string)
    normalize_terms(dict) %>%
    # 2) standardize separators and spacing
    stringr::str_replace_all("\\s*;\\s*", ";") %>%
    stringr::str_squish() %>%
    # 3) split to tokens
    stringr::str_split(";") %>%
    purrr::map(~{
      toks <- stringr::str_squish(unlist(.x))
      toks <- toks[toks != ""]
      # 4) drop irrelevant tokens by exact match (already lowercased upstream)
      toks <- toks[!(toks %in% removed)]
      # 5) rejoin
      paste(toks, collapse = "; ")
    }) %>%
    unlist()
}

## Refining the matrix
biblioMetadata <- biblioMetadata %>%
  dplyr::mutate(across(c(`Author Keywords`, `Index Keywords`, `KW_merged`), ~ stringr::str_to_lower(.))) %>%
  dplyr::mutate(
    `Author Keywords` = clean_kw_col(`Author Keywords`, synonymDictionary, removedKeywords),
    `Index Keywords`  = clean_kw_col(`Index Keywords`,  synonymDictionary, removedKeywords),
    KW_merged         = clean_kw_col(KW_merged,         synonymDictionary, removedKeywords)
  )

## Save
write_csv(biblioMetadata, "cleaned_keywords.csv")
