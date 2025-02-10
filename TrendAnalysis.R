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

library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)
library(viridisLite)

## List of sheet names for topics
topicSheets <- c("Taxonomy", "Paleontology", "Invertebrate Zoology", 
                 "General Marine Biology", "Geographical Areas", "Genetic and Molecular Biology")

## Create an empty list to store the plots
plot_list <- list()

keywordTrends <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/KeyWord Trends.xlsx", 
                            sheet = 1)

## Viridis-gradient step color for legend
legend_breaks <- c(5, 15, 30, 45, 60, 75) 
legend_sizes <- scales::rescale(legend_breaks, to = c(2, 10)) 
legend_colors <- viridis(length(legend_breaks)) 
y_positions <- seq(10, 100, length.out = length(legend_breaks))

## Experimental teardrop legend
teardrop_data <- data.frame(
  x = rep(1, 100),
  y = seq(1, 100, length.out = 100),  
  size = seq(2, 10, length.out = 100),  
  color = viridis(100)  
)

labels_data <- data.frame(
  y = y_positions,
  label = as.character(legend_breaks),
  color = viridis(length(legend_breaks))
)

## Teardrop legend plot
teardrop_legend <- ggplot(teardrop_data, aes(x, y, size = size, color = color)) +
  geom_point(shape = 16, alpha = 0.8) +
  scale_size_identity() + 
  scale_color_identity() +
  geom_text(data = labels_data, aes(x = 1, nugde_x = 10, y = y, label = label, color = "black"), size = 4, hjust = 0) +  # Frequency labels
  theme_void() +
  theme(legend.position = "none")

## Read the data and generate the plots
for (topicSheet in topicSheets) {

  # Read the specific sheet
  topic_keywords <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/KeyWord Trends.xlsx", 
                               sheet = topicSheet)
  
  # Ensure column is correctly named
  colnames(topic_keywords) <- c("Keyword")
  
  # Filter main data to include only keywords from the topic sheet
  filtered_data <- keywordTrends %>%
    filter(Keyword %in% topic_keywords$Keyword)
  
  # Create bubble plot
  p <- ggplot(filtered_data, aes(x = Year, y = Keyword, size = Frequency, color = Frequency)) +
    geom_point(alpha = 0.7) +
    scale_x_continuous(limits = c(1960, 2025), breaks = seq(1960, 2025, by = 10)) +
    theme(panel.background = element_rect(fill = "white"),
          plot.margin = margin(15, 10, 15, 10)) +
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
    
    p <- p + teardrop_legend + plot_layout(ncol = 2, widths = 1)
  
  plot_list[[topicSheet]] <- p
}

## Combine all plots into a 2x3 grid
combined_plot <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]]) /
  (plot_list[[4]] | plot_list[[5]] | plot_list[[6]])

## Save the final combined plot
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science/Plots/Trends")
ggsave("combined_bubbleplot.pdf", plot = combined_plot, width = 18, height = 12, dpi = 300)
