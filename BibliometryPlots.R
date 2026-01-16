# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2024-11-27
# ~ Version:        1.2
#
# ~ Script Name:    BibliometryPlots
#
# ~ Script Description:
# Load bibliometric tables (per-year counts, sources, authors, species list).
# Plot publications over time.
# Plot rank–frequency for journals and for authors.
# Compute yearly species description counts and cumulative totals; plot both.
# Export all figures as PDFs.
#
# Copyright 2024 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(ggplot2)
library(readxl)
library(dplyr)

# Set project root
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science")

# ---- 1) Import main tables -----------------------------------------------
# Excel sheets:
#  - ArticlesPerYear: columns Year, Articles
#  - Sources:         columns Journal (or Source), Articles
#  - Authorship:      columns Author, Articles
#  - SpeciesListWithYear: species with AphiaID and Year of description
yearTable    <- read_excel("Tables.xlsx", sheet = "ArticlesPerYear")
sourcesTable <- read_excel("Tables.xlsx", sheet = "Sources")
speciesList  <- read_csv("clean_species.csv")

# ---- 2) Publications over time -------------------------------------------
# Line + points + smooth; vertical reference line at 2020
paperPlot <- ggplot(yearTable, aes(x = Year, y = Articles)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  geom_vline(xintercept = 2020) +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10)) +
  labs(
    title = "Publications over the years",
    x = "Year",
    y = "Number of Publications"
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title  = element_text(hjust = 0.5, size = 14)
  )

# ---- 3) Rank–frequency: journals -----------------------------------------
# Collapse minor sources (<5 articles) separately; rank by descending Articles
otherSources    <- subset(sourcesTable, Articles < 5)
otherSourcesSum <- sum(otherSources$Articles)

majorSources <- subset(sourcesTable, Articles >= 5)
majorSources <- majorSources[order(-majorSources$Articles), ]
majorSources$Rank <- 1:nrow(majorSources)

journalPlot <- ggplot(majorSources, aes(x = Rank, y = Articles)) +
  geom_point(color = "steelblue") +
  geom_line() +
  labs(
    title = "Rank-Frequency Distribution of Journals Publications",
    x = "Rank of Journal",
    y = "Number of Publications"
  ) +
  theme_minimal()


# ---- 4) Taxonomic effort" ---------------------------------------------------
# Yearly species counts and cumulative total
species_count <- aggregate(AphiaID ~ Year, data = speciesList, FUN = length)
species_count$Year <- as.numeric(as.character(species_count$Year))
colnames(species_count) <- c("Year", "SpeciesCount")
species_count$CumulativeCount <- cumsum(species_count$SpeciesCount)

# Bars = yearly descriptions; red line = cumulative (scaled to primary axis)
descriptionPlot <- ggplot(species_count, aes(x = Year)) +
  geom_bar(aes(y = SpeciesCount), stat = "identity", fill = "steelblue") +
  geom_line(aes(y = CumulativeCount / max(CumulativeCount) * max(SpeciesCount)),
            color = "red", linewidth = 1.2) +
  scale_y_continuous(
    name = "Number of Species Described",
    sec.axis = sec_axis(
      transform = ~ . * max(species_count$CumulativeCount) / max(species_count$SpeciesCount),
      name = "Cumulative Number of Species"
    )
  ) +
  scale_x_continuous(breaks = seq(min(species_count$Year), max(species_count$Year), by = 10)) +
  labs(title = "Species Descriptions and Cumulative Totals Over Time", x = "Year") +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, size = 14),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.background = element_rect(fill = "white", color = NA)
  )

# ---- 6) Save outputs ------------------------------------------------------
# Write PDFs to the Plots folder with fixed sizes and dpi
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Raw Plots")

ggsave("Publication over the Years.pdf", paperPlot,  width = 20, height = 16, units = "cm", dpi = 300)
ggsave("Major Journals.pdf", journalPlot, width = 20, height = 16, units = "cm", dpi = 300)  
<<<<<<< HEAD
=======
=======
>>>>>>> d791231b52457bdc764bba98323001ea11fbac01
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Plots")

ggsave("Publication over the Years.pdf", paperPlot,  width = 20, height = 16, units = "cm", dpi = 300)
ggsave("Major Journals.pdf", journalPlot, width = 20, height = 16, units = "cm", dpi = 300)  
ggsave("Major Authors.pdf", authorsPlot, width = 20, height = 16, units = "cm", dpi = 300)
ggsave("Taxonomic effort.pdf", descriptionPlot, width = 20, height = 16, units = "cm", dpi = 300)
