# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2024-11-25
# ~ Version:        2.0
#
# ~ Script Name:    bibliometricAnalysis
#
# ~ Script Description:
#
#
#
# Copyright 2024 - Alfredo Marchio'
#
# ----------------------------------------------------

library(bibliometrix)
library(bibliometrixData)
library(textstem)
library(hunspell)

# Setting working directory
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science")

# Import
biblioData <- convert2df(file.choose(), dbsource = "scopus", format = "csv")

# Duplicate removal
duplicateTitles <- duplicated(biblioData$TI)
dataCleaned <- biblioData[!duplicateTitles, ]

# Keywords plurals
keywordsPlurals <- c("hexactinellids", "poriferas", "spicules", "glass sponges", "ecosystems", "animals", "articles", "reviews")
keywordsSingulars <- gsub("s$", "", keywordsPlurals)

# Synonyms mapping
synonyms <- list(
  "hexactinellids" = "hexactinellida",
  "hexactinellid" = "hexactinellida",
  "glass sponge" = "hexactinellida",
  "glass sponges" = "hexactinellida",
  "glass" = "hexactinellida",
  "hexactinellid sponge" = "hexactinellida",                                      
  "hexactinellid sponges" = "hexactinellida",
  "hexactinellid sponge spicules" = "spicules",
  "animalia" = "metazoa",
  "animals" = "metazoa",
  "animal" = "metazoa",
  "sponge" = "porifera",
  "sponges" = "porifera",
  "porifera (sponges)" = "porifera",
  "sponge (porifera)" = "porifera",
  "deep-sea sponges" = "porifera",
  "deep-sea" = "deep sea",
  "deepsea" = "deep sea",
  "deep ocean" = "deep sea",
  "vulnerable ecological region" = "vulnerable marine ecosystems",
  "vulnerable ecosystems"  = "vulnerable marine ecosystems",
  "vulnerable marine ecosystem"  = "vulnerable marine ecosystems",
  "vulnerable habitats" = "vulnerable marine ecosystems"
)

keywordsStandardized <- sapply(keywordsSingulars, function(x) {
  if (tolower(x) %in% names(synonyms)) synonyms[[tolower(x)]] else tolower(x)
})

# Stopwords
stopwords <- c("article", 
               "review", 
               "research", 
               "study", 
               "studies", 
               "analysis", 
               "human", 
               "nonhuman", 
               "priority journal", 
               "controlled study"
               )

# Function to clean keywords
clean_keywords <- function(keywords_column) {
  sapply(strsplit(keywords_column, "; "), function(kw) { 
    kw <- trimws(kw)
    cleaned <- sapply(kw, function(x) {
      x <- ifelse(tolower(x) %in% names(synonyms), synonyms[[tolower(x)]], tolower(x)) 
      if (tolower(x) %in% stopwords) NA else x  
    })
    cleaned <- cleaned[!is.na(cleaned)]
    paste(cleaned, collapse = "; ")  
  })
}

# Apply cleaning to DE and ID columns
dataCleaned$DE <- clean_keywords(dataCleaned$DE)
dataCleaned$ID <- clean_keywords(dataCleaned$ID)

#uniqueKeywords <- unlist(strsplit(dataCleaned$ID, ";"))
#uniqueKeywords <- trimws(uniqueKeywords)
#uniqueKeywords <- unique(uniqueKeywords)
#uniqueKeywords <- sort(uniqueKeywords)

colnames(dataCleaned)[which(names(dataCleaned) == "DE")] <- "Author Keywords"
colnames(dataCleaned)[which(names(dataCleaned) == "ID")] <- "Index Keywords"

# Save
write.csv(dataCleaned, "cleaned_data.csv", row.names = FALSE)
