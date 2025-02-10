# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-02-08
# ~ Version:        1.8
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

library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)
library(viridisLite)


## List of sheet names for topics
topicSheets <- c("Glass Sponge Science", "Paleontology", "Invertebrate Zoology", 
                 "Marine Biology and Ecology", "Geographical Areas", "Genetic and Molecular Biology")

## Create an empty list to store the plots
plot_list <- list()

## Read the full keyword trend data
keywordTrends <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/KeyWord Trends.xlsx", 
                            sheet = 1)

## Viridis-gradient step color for legend
legend_breaks <- c(5, 15, 30, 45, 60, 75) 

## Read the data and generate the plots
for (topicSheet in topicSheets) {
  topic_keywords <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/KeyWord Trends.xlsx", 
                               sheet = topicSheet)
  
  colnames(topic_keywords) <- c("Keyword")
  
  filtered_data <- keywordTrends %>%
    filter(Keyword %in% topic_keywords$Keyword)
  
  # Ordering bubbleplots by increasing median year
  median_data <- filtered_data %>%
    group_by(Keyword) %>%
    summarize(median_year = median(Year, na.rm = TRUE))
  ordered_keywords <- median_data %>%
    arrange(median_year) %>%  
    pull(Keyword)  
  
  #Apply the ordering to the Y-axis
  filtered_data <- filtered_data %>%
    mutate(Keyword = factor(Keyword, levels = ordered_keywords))
  
  # Merge median data for plotting red X
  median_data <- median_data %>%
    mutate(Keyword = factor(Keyword, levels = ordered_keywords))
  
  # Create bubble plot with red "X" for median year
  p <- ggplot(filtered_data, aes(x = Year, y = Keyword, size = Frequency, color = Frequency)) +
    geom_point(alpha = 0.7) +  ## Bubbles
    geom_point(data = median_data, aes(x = median_year, y = Keyword), 
               shape = 4, color = "red", size = 4, stroke = 1.5) + 
    scale_x_continuous(limits = c(1960, 2025), breaks = seq(1960, 2025, by = 10)) +
    scale_size(range = c(2, 10), limits = c(1, 75), breaks = legend_breaks) +  
    scale_color_viridis_c(limits = c(1, 75), breaks = legend_breaks, option = "viridis") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    ) +
    labs(title = topicSheet, x = "Year", y = "Keyword", size = "Frequency", color = "Frequency") +
    guides(size = "none", color = "none")
  
  plot_list[[topicSheet]] <- p
}

## Combine all plots into a 2x3 grid
combined_plot <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]]) /
  (plot_list[[4]] | plot_list[[5]] | plot_list[[6]])

## Save the final combined plot
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/Plots/Trends")
ggsave("combined_bubbleplot_medianX.pdf", plot = combined_plot, width = 18, height = 12, dpi = 300)
