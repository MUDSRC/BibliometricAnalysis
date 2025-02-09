# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-02-08
# ~ Version:        0.1
#
# ~ Script Name:    TrendAnalysis
#
# ~ Script Description:
#
#
#
# Copyright 2025 - Alfredo Marchio'
#
# ----------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

## Setting working directory
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science")

## Import the main tables
df <- read_excel("AllKeyWords.xlsx")

## Create long format df
dfLong <- df %>%
  pivot_longer(cols = -Year, names_to = "Keyword_Type", values_to = "Keyword") %>%
  drop_na() %>%
  select(-Keyword_Type)

## Frequencies
keywordTrends <- dfLong %>%
  group_by(Year, Keyword) %>%
  summarise(Frequency = n(), .groups = 'drop')

## Cut less than 10 occurences
keywordTrends <- keywordTrends %>%
  group_by(Keyword) %>%
  mutate(Total_Frequency = sum(Frequency)) %>%
  filter(Total_Frequency >= 10) %>%
  ungroup()

#write.csv(keywordTrends, "KeyWord Trends.csv")

## General plot
trendsPlot <- ggplot(keywordTrends, aes(x = Year, y = Keyword, size = Frequency, color = Frequency)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(1, 10)) +
  scale_color_viridis_c() +
  theme_bw() +  # White background
  labs(title = "Trending Terms",
       x = "Year",
       y = "Keyword",
       size = "Frequency",
       color = "Frequency") +
  theme(axis.text.y = element_text(size = 10))

## Filtered subtopic plots
topicSheets <- c("Taxonomy", "Paleontology", "Invertebrate Zoology", "General Marine Biology", "Geographical Areas", "Genetic and Molecular Biology")
title = topicSheets

plot_bubble <- function(topicSheet, title) {
  topic_keywords <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/KeyWord Trends.xlsx", sheet = topicSheet)
  
  # Ensure column is correctly named
  colnames(topic_keywords) <- c("Keyword")
  
  # Filter main data to include only keywords from the topic sheet
  filtered_data <- keywordTrends %>%
    filter(Keyword %in% topic_keywords$Keyword)
  
  # Create bubble plot
  p <- ggplot(filtered_data, aes(x = Year, y = Keyword, size = Frequency, color = Frequency)) +
    geom_point(alpha = 0.7) +
    scale_size(range = c(2, 10)) +  # Adjust bubble size
    scale_color_viridis_c() +       # Use a color gradient
    theme_minimal() +
    theme(panel.background = element_rect(fill = "white")) + # White background
    labs(title = title, x = "Year", y = "Keyword", size = "Frequency", color = "Frequency") +
    guides(size = guide_legend(order = 1), color = guide_colorbar(order = 1))
  
  # Save plot
  setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/Plots/Trends")
  ggsave(paste0("bubbleplot_", topicSheet, ".pdf"), plot = p, width = 20, height = 26, units = "cm", dpi = 300)
  ggsave(paste0("bubbleplot_", topicSheet, ".jpg"), plot = p, width = 20, height = 26, units = "cm", dpi = 300)
}

# List of sheet names for topics
topicSheets <- c("Taxonomy", "Paleontology", "Invertebrate Zoology", "General Marine Biology", "Geographical Areas", "Genetic and Molecular Biology")

# Generate bubble plots for each topic
for (i in 1:length(topicSheets)) {
  plot_bubble(topicSheets[i], topicSheets[i])  # Pass sheet name as title
}

