# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2026-06-19
# ~ Version:        2.0
#
# ~ Script Name:    ViolinPlots.R
#
# ~ Script Description:
# Load geographical area, topic, and bathymetric zone tags per paper.
# Use X marks to create violin plots of publication-year distribution.
#
# Not assigned / NA papers are excluded from violin plots.
#
# Copyright 2026 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
library(patchwork)

# ---- Parameters -----------------------------------------------------------
# Input and output paths
input_file <- "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/area_topic_bathymetry.csv"
output_folder <- "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Raw Plots"

# Thresold year
year_threshold <- 1960

# Output file names
area_violin_file <- "Publication per year per geographical area.pdf"
topic_violin_file <- "Publication per year per topic.pdf"
bathymetry_violin_file <- "Publication per year per bathymetric zone.pdf"
combined_violin_file <- "Combined violin plots area topic bathymetry.pdf"

# Plot dimensions
single_plot_width <- 20
single_plot_height <- 16

combined_plot_width <- 28
combined_plot_height <- 32

plot_units <- "cm"
plot_dpi <- 300

# Classification groups
valid_areas <- c(
  "Pacific Ocean",
  "Atlantic Ocean",
  "Indian Ocean",
  "Southern Ocean",
  "Mediterranean Sea",
  "Others"
)

valid_topics <- c(
  "Taxonomy&Biology",
  "Biotechnology",
  "Ecology",
  "Paleontology"
)

valid_bathymetry <- c(
  "Shallow",
  "Mesophotic",
  "Bathyal",
  "Abyssal",
  "Hadal"
)

# Cleaner display labels
category_label_lookup <- c(
  "Pacific Ocean" = "Pacific Ocean",
  "Atlantic Ocean" = "Atlantic Ocean",
  "Indian Ocean" = "Indian Ocean",
  "Southern Ocean" = "Southern Ocean",
  "Mediterranean Sea" = "Mediterranean Sea",
  "Others" = "Others",
  "Taxonomy&Biology" = "Taxonomy & biology",
  "Biotechnology" = "Biotechnology",
  "Ecology" = "Ecology",
  "Paleontology" = "Paleontology",
  "Shallow" = "Shallow",
  "Mesophotic" = "Mesophotic",
  "Bathyal" = "Bathyal",
  "Abyssal" = "Abyssal",
  "Hadal" = "Hadal"
)

# Manual colour palettes
area_palette <- c(
  "Pacific Ocean" = "#1B9E77",
  "Atlantic Ocean" = "#D95F02",
  "Indian Ocean" = "#7570B3",
  "Southern Ocean" = "#E7298A",
  "Mediterranean Sea" = "#66A61E",
  "Others" = "#E6AB02"
)

topic_palette <- c(
  "Taxonomy & biology" = "#F0E442",
  "Biotechnology" = "#0072B2",
  "Ecology" = "#D55E00",
  "Paleontology" = "#009E73"
)

bathymetry_palette <- c(
  "Shallow" = "#D6EAF8",
  "Mesophotic" = "#85C1E9",
  "Bathyal" = "#3498DB",
  "Abyssal" = "#21618C",
  "Hadal" = "#0B1F4D"
)

# Violin settings
violin_width <- 1.7
violin_alpha <- 1
base_font_size <- 12
x_axis_text_angle <- 45

# ---- Functions ------------------------------------------------------------
prepare_violin_data <- function(data,
                                valid_categories,
                                category_label,
                                category_label_lookup) {
  
  # Keep only columns that actually exist in the dataset
  category_cols <- intersect(names(data), valid_categories)
  
  if (length(category_cols) == 0) {
    stop(paste("No matching columns found for", category_label))
  }
  
  # Convert X/x to 1/0
  data[category_cols] <- lapply(data[category_cols], function(col) {
    as.integer(tolower(trimws(as.character(col))) == "x")
  })
  
  # Count number of assignments per paper
  data$NumberAssignments <- rowSums(data[category_cols], na.rm = TRUE)
  
  # Summary counts
  n_total_papers <- nrow(data)
  n_not_assigned <- sum(data$NumberAssignments == 0, na.rm = TRUE)
  n_assigned_once <- sum(data$NumberAssignments == 1, na.rm = TRUE)
  n_multiple <- sum(data$NumberAssignments > 1, na.rm = TRUE)
  n_tagged <- sum(data$NumberAssignments > 0, na.rm = TRUE)
  
  cat(
    "\n", category_label, "\n",
    "Total papers: ", n_total_papers, "\n",
    "Not assigned: ", n_not_assigned,
    sprintf(" (%.1f%%)\n", 100 * n_not_assigned / n_total_papers),
    "Assigned once: ", n_assigned_once,
    sprintf(" (%.1f%%)\n", 100 * n_assigned_once / n_total_papers),
    "Multiple assignments: ", n_multiple,
    sprintf(" (%.1f%%)\n", 100 * n_multiple / n_total_papers),
    "Assigned at least once: ", n_tagged,
    sprintf(" (%.1f%%)\n", 100 * n_tagged / n_total_papers),
    sep = ""
  )
  
  # Long format for assigned categories only
  # Not assigned / NA papers are excluded from violin plots.
  long_assigned <- data %>%
    filter(NumberAssignments > 0) %>%
    pivot_longer(
      cols = all_of(category_cols),
      names_to = "Category",
      values_to = "Count"
    ) %>%
    filter(Count > 0) %>%
    mutate(
      Category = factor(Category, levels = valid_categories),
      CategoryLabel = recode(as.character(Category), !!!category_label_lookup),
      CategoryLabel = factor(
        CategoryLabel,
        levels = recode(valid_categories, !!!category_label_lookup)
      ),
      PY = as.integer(PY)
    )
  
  # Category assignment totals
  category_totals <- long_assigned %>%
    count(CategoryLabel, name = "AssignmentCount") %>%
    complete(
      CategoryLabel = factor(
        recode(valid_categories, !!!category_label_lookup),
        levels = recode(valid_categories, !!!category_label_lookup)
      ),
      fill = list(AssignmentCount = 0)
    )
  
  cat("\nCategory assignment totals for ", category_label, ":\n", sep = "")
  print(category_totals)
  
  return(
    list(
      violin_data = long_assigned,
      category_totals = category_totals
    )
  )
}


make_violin_plot <- function(violin_data,
                             category_label,
                             x_axis_label,
                             fill_palette,
                             violin_width,
                             violin_alpha,
                             base_font_size,
                             x_axis_text_angle) {
  
  if (nrow(violin_data) == 0) {
    stop(paste("No assigned records available for", category_label))
  }
  
  ymin <- min(violin_data$PY, na.rm = TRUE)
  ymax <- max(violin_data$PY, na.rm = TRUE)
  
  ggplot(
    violin_data,
    aes(
      x = CategoryLabel,
      y = PY,
      fill = CategoryLabel
    )
  ) +
    geom_violin(
      scale = "count",
      trim = TRUE,
      alpha = violin_alpha,
      width = violin_width
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_continuous(
      limits = c(ymin, ymax),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_fill_manual(
      values = fill_palette,
      drop = FALSE
    ) +
    labs(
      title = category_label,
      x = x_axis_label,
      y = "Publication year",
      fill = NULL
    ) +
    theme_minimal(base_size = base_font_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none",
      axis.text.x = element_text(
        angle = x_axis_text_angle,
        hjust = 1
      ),
      panel.grid.minor = element_blank()
    )
}


save_pdf_plot <- function(plot_object,
                          filename,
                          output_folder,
                          width,
                          height,
                          units,
                          dpi) {
  
  ggsave(
    filename = file.path(output_folder, filename),
    plot = plot_object,
    width = width,
    height = height,
    units = units,
    dpi = dpi
  )
}

# ---- 1) Input -------------------------------------------------------------
raw_matrix <- read.csv(
  input_file,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("")
)

# ---- 2)  Headers data cleaning  -------------------------------------------
names(raw_matrix) <- trimws(names(raw_matrix))
names(raw_matrix)[names(raw_matrix) %in% c("ï..PY", "﻿PY")] <- "PY"

raw_matrix$PY <- as.integer(raw_matrix$PY)

# Remove sources published before certain year
raw_matrix <- raw_matrix %>%
  filter(!is.na(PY), PY >= year_threshold)

n_total_papers <- nrow(raw_matrix)

cat("\nTotal papers in dataset: ", n_total_papers, "\n", sep = "")

# ---- 3) Prepare violin data ----------------------------------------------
area_results <- prepare_violin_data(
  data = raw_matrix,
  valid_categories = valid_areas,
  category_label = "Geographical area",
  category_label_lookup = category_label_lookup
)

topic_results <- prepare_violin_data(
  data = raw_matrix,
  valid_categories = valid_topics,
  category_label = "Topic",
  category_label_lookup = category_label_lookup
)

bathymetry_results <- prepare_violin_data(
  data = raw_matrix,
  valid_categories = valid_bathymetry,
  category_label = "Bathymetric zone",
  category_label_lookup = category_label_lookup
)

# ---- 4) Make violin plots -------------------------------------------------
area_violin_plot <- make_violin_plot(
  violin_data = area_results$violin_data,
  category_label = "Geographical area",
  x_axis_label = "Geographical area",
  fill_palette = area_palette,
  violin_width = violin_width - 0.4,
  violin_alpha = violin_alpha,
  base_font_size = base_font_size,
  x_axis_text_angle = x_axis_text_angle
)

topic_violin_plot <- make_violin_plot(
  violin_data = topic_results$violin_data,
  category_label = "Topic",
  x_axis_label = "Topic",
  fill_palette = topic_palette,
  violin_width = violin_width,
  violin_alpha = violin_alpha,
  base_font_size = base_font_size,
  x_axis_text_angle = x_axis_text_angle
)

bathymetry_violin_plot <- make_violin_plot(
  violin_data = bathymetry_results$violin_data,
  category_label = "Bathymetric zone",
  x_axis_label = "Bathymetric zone",
  fill_palette = bathymetry_palette,
  violin_width = violin_width,
  violin_alpha = violin_alpha,
  base_font_size = base_font_size,
  x_axis_text_angle = x_axis_text_angle
)

# ---- 5) Combined violin plot ---------------------------------------------
combined_violin_plot <- 
  area_violin_plot / 
  topic_violin_plot / 
  bathymetry_violin_plot +
  plot_annotation(
    tag_levels = "A",
    title = "Publication-year distribution by assigned category",
    subtitle = "Not assigned publications are excluded from violin plots."
  )

# ---- 6) Save outputs ------------------------------------------------------
# Gegraphical areas
save_pdf_plot(
  plot_object = area_violin_plot,
  filename = area_violin_file,
  output_folder = output_folder,
  width = single_plot_width,
  height = single_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# Topic
save_pdf_plot(
  plot_object = topic_violin_plot,
  filename = topic_violin_file,
  output_folder = output_folder,
  width = single_plot_width,
  height = single_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# Bathymetry
save_pdf_plot(
  plot_object = bathymetry_violin_plot,
  filename = bathymetry_violin_file,
  output_folder = output_folder,
  width = single_plot_width,
  height = single_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# Combined
save_pdf_plot(
  plot_object = combined_violin_plot,
  filename = combined_violin_file,
  output_folder = output_folder,
  width = combined_plot_width,
  height = combined_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# ---- 7) Print final summaries --------------------------------------------
cat("\n\nFinal geographical area assignment totals:\n")
print(area_results$category_totals)

cat("\n\nFinal topic assignment totals:\n")
print(topic_results$category_totals)

cat("\n\nFinal bathymetric zone assignment totals:\n")
print(bathymetry_results$category_totals)