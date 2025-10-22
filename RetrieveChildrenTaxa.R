# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-10-22
# ~ Version:        1.0
#
# ~ Script Name:    RetrieveChildrenTaxa
#
# ~ Script Description:
# 1) Prompts for a parent AphiaID.
# 2) Validates it against WoRMS and shows name/rank/status.
# 3) Traverses ALL descendants down to species (excludes infraspecific ranks).
# 4) Prompts for a CSV path and saves results.
#
# Copyright 2025 - Alfredo Marchio'
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
include_unaccepted <- FALSE   # keep only accepted/valid names
marine_only        <- TRUE    # restrict to marine-only records
polite_delay       <- 0.10    # seconds between API calls

# ---- Small helper kept local --------------------------------------------
# Pull all children pages for a node 
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
    "Name: %s\nRank: %s\nStatus: %s\n\nFetching descendants to species (accepted-only, marine-only)…",
    parent_name, parent_rank, parent_status
  )
)

# ---- 3) BFS traversal to species (no subspecies etc.) -----------------------
infraspecific <- c("Subspecies","Variety","Form","Subvariety","Forma","Subform")
norm_rank <- function(x) str_to_title(as.character(x))

queue   <- list(aphia_id)
visited <- integer(0)
stash   <- list()

while (length(queue) > 0) {
  current <- queue[[1]]; queue <- queue[-1]
  if (current %in% visited) next
  visited <- c(visited, current)
  
  if (polite_delay > 0) Sys.sleep(polite_delay)
  
  kids <- children_all(current)
  if (nrow(kids) == 0) next
  
  kids <- kids %>%
    mutate(
      rank = norm_rank(rank),
      status = as.character(status),
      scientificname = as.character(scientificname)
    )
  
  if (!include_unaccepted && "status" %in% names(kids)) {
    kids <- filter(kids, tolower(status) %in% c("accepted","valid"))
  }
  if (nrow(kids) == 0) next
  
  stash[[length(stash) + 1]] <- kids
  
  expand_ids <- kids %>%
    filter(rank != "Species", !(rank %in% infraspecific)) %>%
    pull(AphiaID) %>% unique()
  
  if (length(expand_ids)) queue <- c(queue, as.list(expand_ids))
}

if (length(stash) == 0) {
  showDialog("No descendants", "No children were found down to species under these filters.")
  return(invisible(tibble()))
}

descendants <- bind_rows(stash) %>%
  filter(!(rank %in% infraspecific)) %>%
  filter(rank %in% c("Kingdom","Phylum","Class","Order","Family","Genus","Species")) %>%
  select(
    AphiaID, scientificname, authority, rank, status,
    valid_name, kingdom, phylum, `class`, `order`, family, genus
  ) %>%
  distinct()

# ---- 4) Save CSV -------------------------------------------------------------
csv_path <- selectFile(
  caption  = "Save descendants as CSV",
  label    = "Save",
  path     = getwd(),
  existing = FALSE
)
if (is.null(csv_path) || !nzchar(csv_path)) {
  showDialog("Cancelled", "Export cancelled. Data not saved.")
  return(invisible(descendants))
}
if (!grepl("\\.csv$", csv_path, ignore.case = TRUE)) csv_path <- paste0(csv_path, ".csv")

write_csv(descendants, file = csv_path)

showDialog("Export complete", sprintf("Exported %d records (accepted-only, marine-only; to species).\n\nFile:\n%s",
          nrow(descendants), csv_path))

