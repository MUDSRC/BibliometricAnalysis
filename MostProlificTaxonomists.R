# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-07-01
# ~ Version:        2.0
#
# ~ Script Name:    MostProlificTaxonomists
#
# ~ Script Description:
# Read species list with authorship strings.
# Clean brackets/parentheses, normalize accents.
# Parse authorship into individual author names and extract year.
# Produce cleaned species table and author statistics.
# Export: clean_species.csv, authors_long.csv, most_prolific_taxonomists.csv
#
# Copyright 2025 - Alfredo Marchio'
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(rlang)

# ---- Helpers --------------------------------------------------------------

# Normalize text: trim, collapse whitespace, repair common mojibake (latin1→UTF-8)
normalize_text <- function(x) {
  x <- str_replace_all(x, "\\s+", " ")
  x <- str_trim(x)
  x
}

# Parse the authority string
parse_authority <- function(auth) {
  auth <- normalize_text(auth)
  auth <- str_replace_all(auth, "[\\[\\]]", "")           # drop bracket chars
  auth <- str_replace(auth, "^\\((.*)\\)$", "\\1")        # unwrap (...) if whole string
  auth <- str_squish(auth)
  
  yr   <- str_extract(auth, "\\d{4}$")
  base <- str_trim(str_remove(auth, ",\\s*\\d{4}$"))
  
  list(
    authority    = ifelse(is.na(yr) | yr == "", base, paste(base, yr, sep = ", ")),
    year         = yr %||% "",
    authors_only = base
  )
}

# Split authors by comma or "&" (after removing the year)
split_authors <- function(authors_only) {
  unified <- str_replace_all(authors_only, "\\s*&\\s*", ", ")
  parts   <- str_split(unified, "\\s*,\\s*", simplify = FALSE)
  map(parts, ~ .x[.x != ""])
}


# ---- 1) Input -------------------------------------------------------------
# Use output of RetrieveChildrenTaxa
df_raw <- read.csv(file.choose(), check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Make column names lower for resilient access
names(df_raw) <- tolower(names(df_raw))

# Check required columns exist
required <- c("aphiaid", "scientificname", "authority", "rank")
missing  <- setdiff(required, names(df_raw))
if (length(missing)) stop(sprintf("Missing required column(s): %s", paste(missing, collapse = ", ")))

# Keep only rows where rank == species (case-insensitive)
df <- df_raw %>%
  mutate(
    rank          = normalize_text(rank),
    scientificname = normalize_text(scientificname),
    authority     = normalize_text(authority)
  ) %>%
  filter(tolower(rank) == "species", tolower(status) == "accepted")

# ---- 2) Clean authority + extract year/authors ----------------------------
df_parsed <- df %>%
  mutate(
    parsed         = map(authority, parse_authority),
    Authority_clean = map_chr(parsed, "authority"),
    Year            = map_chr(parsed, "year"),
    Authors_only    = map_chr(parsed, "authors_only"),
    Authors_list    = split_authors(Authors_only)
  )

# ---- 3) Build outputs -----------------------------------------------------
# Clean species table
clean_species <- df_parsed %>%
  transmute(
    Species  = scientificname,
    AphiaID  = aphiaid,
    Authority = Authority_clean,
    Year     = Year
  )

# Long author list: one row per couple species x author
authors_long <- df_parsed %>%
  select(Species = scientificname, AphiaID = aphiaid, Year, Authority = Authority_clean, Authors_list) %>%
  unnest_longer(Authors_list, values_to = "Author") %>%
  mutate(Author = str_squish(Author)) %>%
  filter(Author != "")

# Counts
author_counts <- authors_long %>%
  count(Author, sort = TRUE, name = "Count")

# ---- 4) Print summary and export -----------------------------------------
print(author_counts)

# Save into /species_list, create it into the wd if missing
if (basename(normalizePath(getwd())) != "species_list") {
  target <- file.path(getwd(), "species_list")
  if (!dir.exists(target)) dir.create(target, recursive = TRUE)
  setwd(target)
}

write_excel_csv(clean_species,   "clean_species.csv")
write_excel_csv(authors_long,    "authors_long.csv")
write_excel_csv(author_counts,   "most_prolific_taxonomists.csv")

message("Done. Files written: clean_species.csv, authors_long.csv, most_prolific_taxonomists.csv")
