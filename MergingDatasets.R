library(bibliometrix)

wos_files <- c("wos_part1.bib", "wos_part2.bib", "wos_part3.bib", "wos_part4.bib")

# Read each WoS .bib, convert to bibliometrix dataframe
wos_list <- lapply(wos_files, function(f)
  convert2df(file = f, dbsource = "wos", format = "bibtex")
)

# Merge WoS parts and remove internal duplicates
wos_all <- mergeDbSources(wos_list, remove.duplicated = TRUE)

scopus <- convert2df(file = "Scopus Export.bib", dbsource = "scopus", format = "bibtex")

M <- mergeDbSources(list(wos_all, scopus), remove.duplicated = TRUE)

# Normalize DOI field
if (!"DI" %in% names(M)) M$DI <- NA_character_
M$DI <- tolower(trimws(gsub("^https?://(dx\\.)?doi\\.org/", "", M$DI)))

# Drop exact DOI duplicates (where DOI exists)
keep <- !(nzchar(M$DI) & duplicated(M$DI))
M <- M[keep, ]

# Fallback: de-dup by normalized Title + Year for records without DOI
norm_title <- tolower(gsub("[^[:alnum:]]+", "", M$TI))
key <- paste(norm_title, M$PY)
M <- M[!duplicated(key), ]

save(M, file = "merged_wos_scopus.RData")