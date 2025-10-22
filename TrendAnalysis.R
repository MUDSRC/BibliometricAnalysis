# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-02-08
# ~ Version:        1.8
#
# ~ Script Name:    TrendAnalysis
#
# ~ Script Description:
# Load full keyword trend data and per-topic keyword lists from Excel.
# Build per-topic temporal bubble plots (Frequency ~ Year), ordered by median year.
# Arrange plots in a 2×3 grid and export to PDF.
# Compare topics with a violin plot of Year and a mode (rounded) segment; export to PDF.
#
# Copyright 2025 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(ggplot2)
library(readxl)
library(dplyr)
library(patchwork)
library(viridisLite)
library(tidyr)

# ---- Configs ---------------------------------------------------------------
topicSheets <- c(
  "Glass Sponge Science",
  "Paleontology",
  "Invertebrate Zoology", 
  "Marine Biology and Ecology",
  "Geographical Areas",
  "Genetic and Molecular Biology"
)

# ---- 1) Temporal trends ----------------------------------------------------
# Containers and source table
plot_list <- list()   # holds per-topic ggplots

# Full keyword trend table (sheet 1) with cols: Keyword, Year, Frequency
keywordTrends <- read_excel(
  "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Misc tables/KeyWord Trends.xlsx", 
  sheet = 1
)

# Legend scaffolding for bubble size/color (Viridis gradient)
legend_breaks  <- c(5, 15, 30, 45, 60, 75)
legend_sizes   <- scales::rescale(legend_breaks, to = c(2, 10))
legend_colors  <- viridis(length(legend_breaks))
y_positions    <- seq(10, 100, length.out = length(legend_breaks))

# Optional teardrop legend
teardrop_data <- data.frame(
  x = rep(1, 100),
  y = seq(1, 100, length.out = 100),
  size = seq(2, 10, length.out = 100),
  color = viridis(100)
)

labels_data <- data.frame(
  y     = y_positions,
  label = as.character(legend_breaks),
  color = viridis(length(legend_breaks))
)

teardrop_legend <- ggplot(teardrop_data, aes(x, y, size = size, color = color)) +
  geom_point(shape = 16, alpha = 0.8) +
  scale_size_identity() +
  scale_color_identity() +
  geom_text(
    data = labels_data,
    aes(x = 1, y = y, label = label, color = "black"),
    size = 4, hjust = 0
  ) +
  theme_void() +
  theme(legend.position = "none")

# Build per-topic bubble plots (ordered by median year)
for (topicSheet in topicSheets) {
  # Read one-column keyword list for the topic
  topic_keywords <- read_excel(
    "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Misc tables/KeyWord Trends.xlsx", 
    sheet = topicSheet
  )
  colnames(topic_keywords) <- c("Keyword")
  
  # Filter global table for the topic’s keywords
  filtered_data <- keywordTrends %>%
    filter(Keyword %in% topic_keywords$Keyword)
  
  # Compute and use median Year to order keywords on the y-axis
  median_data <- filtered_data %>%
    group_by(Keyword) %>%
    summarize(median_year = median(Year, na.rm = TRUE))
  ordered_keywords <- median_data %>%
    arrange(median_year) %>%
    pull(Keyword)
  
  filtered_data <- filtered_data %>%
    mutate(Keyword = factor(Keyword, levels = ordered_keywords))
  
  # Bubble plot: Frequency drives size and color; Year on x; Keyword on y
  p <- ggplot(filtered_data, aes(x = Year, y = Keyword, size = Frequency, color = Frequency)) +
    geom_point(alpha = 0.7) +
    scale_x_continuous(limits = c(1969, 2025), breaks = seq(1970, 2025, by = 10)) +
    scale_size(range = c(2, 10), limits = c(1, 75), breaks = legend_breaks) +
    scale_y_discrete(expand = expansion(mult = 0.035)) +
    scale_color_viridis_c(limits = c(1, 75), breaks = legend_breaks, option = "viridis") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.position  = "right",
      legend.title     = element_text(face = "bold")
    ) +
    labs(
      title = topicSheet,
      x     = "Year",
      y     = NULL,
      size  = "Frequency",
      color = "Frequency"
    ) +
    guides(size = "none", color = "none")
  
  plot_list[[topicSheet]] <- p
}

# Assemble a 2×3 panel (Patchwork) --------------------------------------
combined_plot <- (plot_list[[1]] | plot_list[[2]] | plot_list[[3]]) /
  (plot_list[[4]] | plot_list[[5]] | plot_list[[6]])

# Export combined panel
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Plots/Trends")
ggsave("combined_bubbleplot_medianX.pdf", plot = combined_plot, width = 20, height = 16, dpi = 300)

# ---- 2) Topic comparison ---------------------------------------------------
# Build a tall table (Topic, Year, Frequency) by merging all topics
all_data <- data.frame()

for (topicSheet in topicSheets) {
  topic_keywords <- read_excel(
    "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Misc tables/KeyWord Trends.xlsx", 
    sheet = topicSheet
  )
  colnames(topic_keywords) <- c("Keyword")
  
  filtered_data <- keywordTrends %>%
    filter(Keyword %in% topic_keywords$Keyword) %>%
    mutate(Topic = topicSheet)
  
  all_data <- bind_rows(all_data, filtered_data)
}

# Sum frequency per Topic-Year, then expand rows for density estimation
freq_summary <- all_data %>%
  group_by(Topic, Year) %>%
  summarise(Frequency = sum(Frequency), .groups = "drop")

expanded_data <- freq_summary %>%
  uncount(weights = Frequency)

# Mode year (~peak of kernel density), rounded for display
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

# Violin plot with red segment at rounded modal year
p <- ggplot(expanded_data, aes(x = Topic, y = Year)) +
  geom_violin(fill = "lightgrey", color = "black", trim = TRUE, scale = "width") +
  geom_segment(
    data = mode_segments,
    aes(
      x    = as.numeric(factor(Topic)) - 0.3,
      xend = as.numeric(factor(Topic)) + 0.3,
      y    = RoundedYear,
      yend = RoundedYear
    ),
    color = "red",
    linewidth = 1.2,
    inherit.aes = FALSE
  ) +
  scale_x_discrete() +
  theme_minimal() +
  labs(x = "Topic", y = "Year") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title  = element_text(size = 14, face = "bold"),
    plot.title  = element_text(size = 16, face = "bold")
  )

# Export topic comparison
ggsave("Figure 5 - Temporal trend topic.pdf", plot = p, width = 12, height = 8, dpi = 300)
