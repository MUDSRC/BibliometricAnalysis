# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-11-14
# ~ Version:        1.0
#
# ~ Script Name:    KeywordFrequencyPlots.R
#
# ~ Script Description:
# Load unique keyword list and yearly keyword frequencies.
# Create:
#   1) Bar chart of total frequency for unique keywords (min freq >= 25).
#   2) Horizontal violin plots showing yearly frequency distributions
#      for the same set of keywords.
#
# Copyright 2025 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr) 
library(forcats)

# ---- 1) Input ------------------------------------------------------------
# unique_keywords.csv:   Keyword; Total_Frequency
# keyword_frequencies.csv: Year; Keyword; Frequency; Total_Frequency

# Use read_csv2 for semicolon-separated files
unique_keywords <- read_csv(
  file = "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/unique_keywords.csv",
  col_types = cols(
    Keyword        = col_character(),
    Total_Frequency = col_double()
  )
)

keyword_frequencies <- read_csv(
  file = "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/keyword_frequencies.csv",
  col_types = cols(
    Year            = col_double(),
    Keyword         = col_character(),
    Frequency       = col_double(),
    Total_Frequency = col_double()
  )
)

# ---- 2) Wrangling --------------------------------------------------------
# Keep only keywords that are in unique_keywords
kw_list <- unique_keywords$Keyword

kw_freq_use <- keyword_frequencies %>%
  filter(Keyword %in% kw_list)

# Order keywords by total frequency (ascending for nicer coord_flip)
kw_order <- unique_keywords %>%
  arrange(Total_Frequency) %>%
  pull(Keyword)

unique_keywords <- unique_keywords %>%
  mutate(Keyword = factor(Keyword, levels = kw_order))

kw_freq_use <- kw_freq_use %>%
  mutate(Keyword = factor(Keyword, levels = kw_order))

# ---- 3) Bar chart of unique keywords -----------------------------
bar_keywords <- ggplot(unique_keywords,
                       aes(x = Keyword, y = Total_Frequency)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Keyword",
    y = "Total frequency",
  ) +
  theme_bw() +
  theme(
    plot.title   = element_text(hjust = 0.5),
    axis.text.y  = element_text(size = 8),
    axis.text.x  = element_text(size = 8)
  )

print(bar_keywords)

# ---- 4) Horizontal violin plots per keyword ----------------------
kw_violin <- keyword_frequencies %>%
  filter(Keyword %in% kw_list,
         Frequency > 0) %>% 
  mutate(
    Keyword = factor(Keyword, levels = kw_order)
  ) %>%
  uncount(weights = Frequency)         

min_year <- 1960
max_year <- 2025

violin_keywords <- ggplot(kw_violin, aes(x = PY, y = Keyword)) +
  geom_violin(trim = TRUE, scale = "width", fill = "grey80", colour = "black") +
  labs(
    x = "Year",
    y = "Keyword",
  ) +
  scale_x_continuous(breaks = seq(min_year, max_year, by = 10)) +
  theme_bw() +
  theme(
    plot.title  = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1)
  )
print(violin_keywords)

# ---- 5) Save outputs ------------------------------------------------------
# Write PDFs to the Plots folder with fixed sizes and dpi
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Raw Plots")

ggsave("Keyword__Barplot.pdf", bar_keywords, width = 8, height = 10, unit = "mm", dpi = 300)
ggsave("Keyword_TimeSeries.pdf", violin_keywords, width = 210, height = 297, unit = "mm", dpi = 300)
