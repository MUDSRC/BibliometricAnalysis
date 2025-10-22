# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
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

## Libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)
library(viridisLite)
library(tidyr)

### Temporal trends

## List of sheet names for topics
topicSheets <- c("Glass Sponge Science", "Paleontology", "Invertebrate Zoology", 
                 "Marine Biology and Ecology", "Geographical Areas", "Genetic and Molecular Biology")

## Create an empty list to store the plots
plot_list <- list()

## Read the full keyword trend data
keywordTrends <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Misc tables/KeyWord Trends.xlsx", 
                            sheet = 1)

## Viridis-gradient step color for legend
legend_breaks <- c(5, 15, 30, 45, 60, 75) 
legend_sizes <- scales::rescale(legend_breaks, to = c(2, 10)) 
legend_colors <- viridis(length(legend_breaks)) 
y_positions <- seq(10, 100, length.out = length(legend_breaks))

## Teardrop legend 
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

teardrop_legend <- ggplot(teardrop_data, aes(x, y, size = size, color = color)) +
  geom_point(shape = 16, alpha = 0.8) +
  scale_size_identity() + 
  scale_color_identity() +
  geom_text(data = labels_data, aes(x = 1, y = y, label = label, color = "black"), size = 4, hjust = 0) +
  theme_void() +
  theme(legend.position = "none")


## Read the data and generate the plots
for (topicSheet in topicSheets) {
  topic_keywords <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Misc tables/KeyWord Trends.xlsx", 
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

  # Create bubble plot with red "X" for median year
  p <- ggplot(filtered_data, aes(x = Year, y = Keyword, size = Frequency, color = Frequency)) +
    geom_point(alpha = 0.7) +  ## Bubbles
    scale_x_continuous(limits = c(1969, 2025), breaks = seq(1970, 2025, by = 10)) +
    scale_size(range = c(2, 10), limits = c(1, 75), breaks = legend_breaks) +
    scale_y_discrete(expand = expansion(mult = 0.035)) +
    scale_color_viridis_c(limits = c(1, 75), breaks = legend_breaks, option = "viridis") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.title = element_text(face = "bold")
      ) +
    labs(title = topicSheet, x = "Year", size = "Frequency", color = "Frequency", y = NULL) +
    guides(size = "none", color = "none")
  
  plot_list[[topicSheet]] <- p
}

## Combine all plots into a 2x3 grid
combined_plot <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]]) /
  (plot_list[[4]] | plot_list[[5]] | plot_list[[6]])
 

## Save the final combined plot
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Plots/Trends")
ggsave("combined_bubbleplot_medianX.pdf", plot = combined_plot, width = 20, height = 16, dpi = 300)

### Topic comparison

## Create an empty dataframe to store all topics
all_data <- data.frame()

## Read data and merge all topics
for (topicSheet in topicSheets) {
  topic_keywords <- read_excel("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Misc tables/KeyWord Trends.xlsx", 
                               sheet = topicSheet)
  
  colnames(topic_keywords) <- c("Keyword")
  
  filtered_data <- keywordTrends %>%
    filter(Keyword %in% topic_keywords$Keyword) %>%
    mutate(Topic = topicSheet)  # Add topic as a new column
  
  all_data <- bind_rows(all_data, filtered_data)
}

# Aggregate frequencies by Topic and Year
freq_summary <- all_data %>%
  group_by(Topic, Year) %>%
  summarise(Frequency = sum(Frequency), .groups = "drop")

#  Uncount to reflect frequency in density estimation
expanded_data <- freq_summary %>%
  uncount(weights = Frequency)

# Compute mode (~peak of density) and round to integer year
mode_segments <- expanded_data %>%
  group_by(Topic) %>%
  nest() %>%
  transmute(
    Topic,
    Year = map_dbl(data, ~ {
      dens <- density(.x$Year)
      dens$x[which.max(dens$y)]
    }),
    RoundedYear = round(Year)
  )

# Plot violin and add segment at mode year (rounded)
p <- ggplot(expanded_data, aes(x = Topic, y = Year)) +
  geom_violin(fill = "lightgrey", color = "black", trim = TRUE, scale = "width") +
  geom_segment(data = mode_segments,
               aes(x = as.numeric(factor(Topic)) - 0.3,
                   xend = as.numeric(factor(Topic)) + 0.3,
                   y = RoundedYear, yend = RoundedYear),
               color = "red", linewidth = 1.2, inherit.aes = FALSE) +
  scale_x_discrete() +
  theme_minimal() +
  labs(x = "Topic", y = "Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"))

## Save the plot
ggsave("Figure 5 - Temporal trend topic.pdf", plot = p, width = 12, height = 8, dpi = 300)
