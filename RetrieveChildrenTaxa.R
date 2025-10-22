# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-10-22
# ~ Version:        1.0
#
# ~ Script Name:    RetrieveChildrenTaxa
#
# ~ Script Description:
# Prompts for a parent AphiaID.
# Validates it against WoRMS and shows name/rank/status.
# Traverses ALL descendants down to species (excludes infraspecific ranks).
# Prompts for a CSV path and saves results.
#
# Copyright 2025 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(worrms)
library(rstudioapi)
library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)
library(rlang)

# ---- Configs ---------------------------------------------------------------
marine_only        <- TRUE    # restrict to marine-only records
polite_delay       <- 0.05    # seconds between API calls
allowed_statuses <- c("accepted",
                      "valid", 
                      "temporary name")

# ---- Small helper kept local --------------------------------------------
# Pull all children pages for a single node 
children_all <- function(id) {
  out <- list(); offset <- 1L; page_size <- 50L
  repeat {
    chunk <- tryCatch(
      wm_children(id = id, marine_only = marine_only, offset = offset),
      error = function(e) NULL
    )
    if (is.null(chunk) || nrow(chunk) == 0) break
    out[[length(out) + 1]] <- chunk
    offset <- offset + page_size    # advance by fixed page size
    if (polite_delay > 0) Sys.sleep(polite_delay/10)  # tiny pause between pages
  }
  if (length(out) == 0) tibble() else bind_rows(out)
}

# ---- 1) Ask for AphiaID -----------------------------------------------------
aphia_input <- showPrompt(
  title   = "WoRMS: Enter parent AphiaID",
  message = "Provide the AphiaID of the parent taxon:",
  default = ""
)
if (is.null(aphia_input) || !nzchar(trimws(aphia_input))) {
  showDialog("Cancelled", "No AphiaID provided.")
  return(invisible(NULL))
}
aphia_id <- suppressWarnings(as.integer(trimws(aphia_input)))
if (is.na(aphia_id)) {
  showDialog("Error", "AphiaID must be an integer.")
  return(invisible(NULL))
}

# ---- 2) Validate parent record ----------------------------------------------
parent <- tryCatch(wm_record(aphia_id), error = function(e) NULL)
if (is.null(parent)) {
  showDialog("Not found", sprintf("No record found for AphiaID %s in WoRMS.", aphia_id))
  return(invisible(NULL))
}
parent_name   <- if (is.null(parent$scientificname) || !nzchar(parent$scientificname)) "<unknown>" else parent$scientificname
parent_rank   <- str_to_title(parent$rank %||% "<unknown>")
parent_status <- parent$status %||% "<unknown>"

showDialog(
  title = "Parent taxon found",
  message = sprintf(
    "Name: %s \r\nRank: %s\r\nStatus: %s\\r\nnFetching descendants to species (accepted, valid, temporary names; marine-only).",
    parent_name, parent_rank, parent_status
  )
)

# ---- 3) Breadth-First Search traversal to species -------------------------------
infraspecific <- c("Subspecies","Variety","Form","Subvariety","Forma","Subform")
norm_rank <- function(x) str_to_title(as.character(x))

# queue: AphiaIDs waiting to be processed; start from the parent
queue   <- list(aphia_id)
# visited: keep track of AphiaIDs already expanded, prevents cycles/repeats
visited <- integer(0)
# stash: a list of data frames; append each "children" page here and bind later
stash   <- list()

while (length(queue) > 0) {
  # Pop the first AphiaID (FIFO order => breadth-first)
  current <- queue[[1]]
  queue <- queue[-1]
  
  # Skip if we've already expanded this node
  if (current %in% visited) next
  visited <- c(visited, current)
  
  # Be polite to the API: tiny delay between node requests (set to 0 to disable)
  if (polite_delay > 0) Sys.sleep(polite_delay)
  
  # Query WoRMS for the children of the current node (handles pagination)
  children_taxon <- children_all(current)
  if (nrow(children_taxon) == 0) next  # no children: move on
  
  # Ensure consistent types/casing for downstream filters/joins
  children_taxon <- children_taxon %>%
    mutate(
      rank            = norm_rank(rank),              # e.g., "genus" -> "Genus"
      status          = as.character(status),         # ensure character
      scientificname  = as.character(scientificname)  # ensure character
    )
  
  # Keep only statuses you care about (e.g., accepted/valid/temporary name)
  # NOTE: assumes you defined `allowed_statuses <- c("accepted","valid","temporary name")`
  children_taxon <- children_taxon %>%
    mutate(status = tolower(status)) %>%
    filter(status %in% allowed_statuses)
  
  if (nrow(children_taxon) == 0) next  # everything was filtered out
  
  # Accumulate this batch; we'll bind all batches at the end for efficiency
  stash[[length(stash) + 1]] <- children_taxon
  
  # Determine which child nodes should be expanded further:
  #  - expand everything ABOVE species (Kingdom..Genus, incl. subranks if you allow them)
  #  - DO NOT expand species or anything below (infraspecific ranks)
  expand_ids <- children_taxon %>%
    filter(rank != "Species", !(rank %in% infraspecific)) %>%
    pull(AphiaID) %>%
    unique()
  
  # Enqueue those for later processing
  if (length(expand_ids)) queue <- c(queue, as.list(expand_ids))
}

# If we never found any acceptable children anywhere in the tree
if (length(stash) == 0) {
  showDialog("No descendants", "No children were found down to species under these filters.")
  return(invisible(tibble()))
}

# Bind all collected child pages and do a final tidy/filter
descendants <- bind_rows(stash) %>%
  # Drop anything below species (safety check; already filtered during expansion)
  filter(!(rank %in% infraspecific)) %>%
  # Keep only the ranks you want in the output table.
  # To include Subfamily / Subgenus, add "Subfamily","Subgenus" here.
  filter(rank %in% c("Kingdom","Phylum","Class","Order","Family","Genus","Species")) %>%
  # Select the canonical WoRMS columns you need
  select(
    AphiaID, scientificname, authority, rank, status,
    valid_name, kingdom, phylum, `class`, `order`, family, genus
  ) %>%
  distinct()

# Species-only table
species_only <- bind_rows(stash) %>%
  filter(!(rank %in% infraspecific)) %>%
  filter(rank == "Species") %>%
  select(AphiaID, scientificname, authority) %>%
  distinct()

# ---- 4) Save CSVs ------------------------------------------------------------
# User pick a base file name, then derive two files:
#   <chosen>_full.csv and <chosen>_species.csv
csv_base <- selectFile(
  caption  = "Choose base name for CSVs",
  label    = "Save",
  path     = getwd(),
  existing = FALSE
)
if (is.null(csv_base) || !nzchar(csv_base)) {
  showDialog("Cancelled", "Export cancelled. No files saved.")
  return(invisible(descendants))
}

# Remove eventual .csv the user might have typed, then append suffixes
csv_base_nocsv <- sub("\\.csv$", "", csv_base, ignore.case = TRUE)
full_path     <- paste0(csv_base_nocsv, "_full.csv")
species_path  <- paste0(csv_base_nocsv, "_species.csv")

# Write both files
write_csv(descendants, file = full_path)
write_csv(species_only, file = species_path)

# One concise confirmation dialog covering both outputs
showDialog(
  "Export complete",
  sprintf(
    "Exported %d total records (accepted, valid, temporary names; marine-only).\nFull list: %s\nSpecies only (%d unique species): %s",
    nrow(descendants), full_path, nrow(species_only), species_path
  )
)