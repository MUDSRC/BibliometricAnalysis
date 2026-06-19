# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2026-06-19
# ~ Version:        1.2
#
# ~ Script Name:    StudyEffortStackedBars.R
#
# ~ Script Description:
# Create separate stacked horizontal bar plots showing the distribution of:
#   1) Geographical area
#   2) Topic
#   3) Bathymetric zone
#
# Also creates a separate plot showing assignment status:
#   Not assigned / Assigned once / Multiple assignments
#
# Multiply assigned publications are fractionally weighted so that each
# publication contributes a total weight of one within each classification group.
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(dplyr)
library(tidyr)
library(patchwork)

# ---- Parameters -----------------------------------------------------------

# Input and output paths
input_file <- "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/Bathymetric range.csv"
output_folder <- "C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Raw Plots"

# Output file names
area_plot_file <- "Stacked bar geographical area.pdf"
topic_plot_file <- "Stacked bar topic.pdf"
bathymetry_plot_file <- "Stacked bar bathymetric zone.pdf"
status_plot_file <- "Stacked bar assignment completeness.pdf"
combined_category_plot_file <- "Combined separate category stacked bars.pdf"

# Plot dimensions
single_plot_width <- 24
single_plot_height <- 8

status_plot_width <- 28
status_plot_height <- 12

combined_category_plot_width <- 28
combined_category_plot_height <- 24

plot_units <- "cm"
plot_dpi <- 300

# Label threshold
label_threshold <- 1

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

# Classification order
classification_order <- c(
  "Geographical area",
  "Topic",
  "Bathymetric zone"
)

# Assignment-status order
assignment_status_order <- c(
  "Not assigned",
  "Assigned once",
  "Multiple assignments"
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
  "Hadal" = "Hadal",
  "Not assigned" = "Not assigned"
)

# Manual colour palettes
area_palette <- c(
  "Pacific Ocean" = "#1B9E77",
  "Atlantic Ocean" = "#D95F02",
  "Indian Ocean" = "#7570B3",
  "Southern Ocean" = "#E7298A",
  "Mediterranean Sea" = "#66A61E",
  "Others" = "#E6AB02",
  "Not assigned" = "#BDBDBD"
)
topic_palette <- c(
  "Taxonomy & biology" = "#F0E442",  # yellow
  "Biotechnology" = "#0072B2",       # blue
  "Ecology" = "#D55E00",             # red/orange
  "Paleontology" = "#009E73",        # green
  "Not assigned" = "#BDBDBD"
)

bathymetry_palette <- c(
  "Shallow" = "#D6EAF8",
  "Mesophotic" = "#85C1E9",
  "Bathyal" = "#3498DB",
  "Abyssal" = "#21618C",
  "Hadal" = "#0B1F4D",
  "Not assigned" = "#BDBDBD"
)

status_palette <- c(
  "Not assigned" = "#BDBDBD",
  "Assigned once" = "#4DAF4A",
  "Multiple assignments" = "#377EB8"
)

# ---- Functions ------------------------------------------------------------
summarise_classification <- function(data,
                                     valid_categories,
                                     classification_name,
                                     assignment_status_order,
                                     include_not_assigned = TRUE) {
  
  category_cols <- intersect(names(data), valid_categories)
  
  if (length(category_cols) == 0) {
    stop(paste("No matching columns found for", classification_name))
  }
  
  # Convert X/x to 1/0
  data[category_cols] <- lapply(data[category_cols], function(col) {
    as.integer(tolower(trimws(as.character(col))) == "x")
  })
  
  # Number of assignments per paper
  data$NumberAssignments <- rowSums(data[category_cols], na.rm = TRUE)
  
  # Assignment status
  data$AssignmentStatus <- case_when(
    data$NumberAssignments == 0 ~ "Not assigned",
    data$NumberAssignments == 1 ~ "Assigned once",
    data$NumberAssignments > 1  ~ "Multiple assignments"
  )
  
  data$AssignmentStatus <- factor(
    data$AssignmentStatus,
    levels = assignment_status_order
  )
  
  # Assigned papers, fractionally weighted
  assigned_long <- data %>%
    filter(NumberAssignments > 0) %>%
    pivot_longer(
      cols = all_of(category_cols),
      names_to = "Category",
      values_to = "Presence"
    ) %>%
    filter(Presence > 0) %>%
    mutate(
      Classification = classification_name,
      Weight = 1 / NumberAssignments
    )
  
  # Category summary
  category_summary <- assigned_long %>%
    group_by(Classification, Category) %>%
    summarise(
      WeightedN = sum(Weight, na.rm = TRUE),
      RawAssignmentN = n(),
      .groups = "drop"
    )
  
  # Add not assigned as its own segment
  if (include_not_assigned) {
    
    not_assigned_n <- sum(data$NumberAssignments == 0, na.rm = TRUE)
    
    not_assigned_summary <- tibble(
      Classification = classification_name,
      Category = "Not assigned",
      WeightedN = not_assigned_n,
      RawAssignmentN = not_assigned_n
    )
    
    category_summary <- bind_rows(
      category_summary,
      not_assigned_summary
    )
  }
  
  # Assignment-status summary
  status_summary <- data %>%
    count(AssignmentStatus, name = "NumberPapers") %>%
    complete(
      AssignmentStatus = factor(
        assignment_status_order,
        levels = assignment_status_order
      ),
      fill = list(NumberPapers = 0)
    ) %>%
    mutate(
      Classification = classification_name
    )
  
  return(
    list(
      category_summary = category_summary,
      status_summary = status_summary
    )
  )
}


prepare_category_summary <- function(category_summary,
                                     category_label_lookup,
                                     n_total_papers,
                                     label_threshold,
                                     category_order) {
  
  category_summary %>%
    mutate(
      CategoryLabel = recode(Category, !!!category_label_lookup),
      CategoryLabel = factor(
        CategoryLabel,
        levels = recode(category_order, !!!category_label_lookup)
      ),
      Percentage = 100 * WeightedN / n_total_papers,
      Label = ifelse(
        WeightedN >= label_threshold,
        paste0(round(WeightedN, 0), "\n", round(Percentage, 1), "%"),
        ""
      )
    )
}


prepare_status_summary <- function(combined_status,
                                   classification_order,
                                   n_total_papers,
                                   label_threshold) {
  
  combined_status %>%
    mutate(
      Percentage = 100 * NumberPapers / n_total_papers,
      Label = ifelse(
        NumberPapers >= label_threshold,
        paste0(NumberPapers, "\n", round(Percentage, 1), "%"),
        ""
      ),
      Classification = factor(
        Classification,
        levels = classification_order
      )
    )
}


make_single_category_plot <- function(category_data,
                                      classification_name,
                                      fill_palette,
                                      n_total_papers) {
  
  ggplot(
    category_data,
    aes(
      x = classification_name,
      y = WeightedN,
      fill = CategoryLabel
    )
  ) +
    geom_col(
      width = 0.55,
      color = "white"
    ) +
    geom_text(
      aes(label = Label),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    coord_flip() +
    scale_fill_manual(
      values = fill_palette,
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(0, n_total_papers),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = classification_name,
      x = NULL,
      y = "Number of publications",
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}


make_status_plot <- function(combined_status,
                             status_palette,
                             n_total_papers) {
  
  ggplot(
    combined_status,
    aes(
      x = Classification,
      y = NumberPapers,
      fill = AssignmentStatus
    )
  ) +
    geom_col(
      width = 0.7,
      color = "white"
    ) +
    geom_text(
      aes(label = Label),
      position = position_stack(vjust = 0.5),
      size = 3
    ) +
    coord_flip() +
    scale_fill_manual(
      values = status_palette,
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(0, n_total_papers),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = "Assignment completeness by classification group",
      subtitle = paste0("Total publications = ", n_total_papers),
      x = NULL,
      y = "Number of publications",
      fill = "Assignment status"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major.y = element_blank(),
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

# ---- 2) Clean headers -----------------------------------------------------
names(raw_matrix) <- trimws(names(raw_matrix))
names(raw_matrix)[names(raw_matrix) %in% c("ï..PY", "﻿PY")] <- "PY"

raw_matrix$PY <- as.integer(raw_matrix$PY)

n_total_papers <- nrow(raw_matrix)

# ---- 3) Summarise classification groups ----------------------------------
area_summary <- summarise_classification(
  data = raw_matrix,
  valid_categories = valid_areas,
  classification_name = "Geographical area",
  assignment_status_order = assignment_status_order,
  include_not_assigned = TRUE
)

topic_summary <- summarise_classification(
  data = raw_matrix,
  valid_categories = valid_topics,
  classification_name = "Topic",
  assignment_status_order = assignment_status_order,
  include_not_assigned = TRUE
)

bathymetry_summary <- summarise_classification(
  data = raw_matrix,
  valid_categories = valid_bathymetry,
  classification_name = "Bathymetric zone",
  assignment_status_order = assignment_status_order,
  include_not_assigned = TRUE
)

# ---- 4) Prepare category data --------------------------------------------
area_categories <- prepare_category_summary(
  category_summary = area_summary$category_summary,
  category_label_lookup = category_label_lookup,
  n_total_papers = n_total_papers,
  label_threshold = label_threshold,
  category_order = c(valid_areas, "Not assigned")
)

topic_categories <- prepare_category_summary(
  category_summary = topic_summary$category_summary,
  category_label_lookup = category_label_lookup,
  n_total_papers = n_total_papers,
  label_threshold = label_threshold,
  category_order = c(valid_topics, "Not assigned")
)

bathymetry_categories <- prepare_category_summary(
  category_summary = bathymetry_summary$category_summary,
  category_label_lookup = category_label_lookup,
  n_total_papers = n_total_papers,
  label_threshold = label_threshold,
  category_order = c(valid_bathymetry, "Not assigned")
)

# ---- 5) Prepare assignment-status data -----------------------------------
combined_status <- bind_rows(
  area_summary$status_summary,
  topic_summary$status_summary,
  bathymetry_summary$status_summary
)

combined_status <- prepare_status_summary(
  combined_status = combined_status,
  classification_order = classification_order,
  n_total_papers = n_total_papers,
  label_threshold = label_threshold
)

# ---- 6) Make separate category plots -------------------------------------
area_plot <- make_single_category_plot(
  category_data = area_categories,
  classification_name = "Geographical area",
  fill_palette = area_palette,
  n_total_papers = n_total_papers
)

topic_plot <- make_single_category_plot(
  category_data = topic_categories,
  classification_name = "Topic",
  fill_palette = topic_palette,
  n_total_papers = n_total_papers
)

bathymetry_plot <- make_single_category_plot(
  category_data = bathymetry_categories,
  classification_name = "Bathymetric zone",
  fill_palette = bathymetry_palette,
  n_total_papers = n_total_papers
)

status_plot <- make_status_plot(
  combined_status = combined_status,
  status_palette = status_palette,
  n_total_papers = n_total_papers
)

# ---- 7) Optional combined category figure --------------------------------
# Legends are NOT collected, so each barplot keeps its own legend
# Note for myself: dont waste another saturday fixing it
combined_category_plot <- area_plot / topic_plot / bathymetry_plot +
  plot_annotation(
    title = "Distribution of study-effort assignments",
    subtitle = paste0(
      "Total publications = ", n_total_papers,
      ". Multiply assigned papers are fractionally weighted within each classification group."
    ),
    tag_levels = "A"
  )

# ---- 8) Save outputs ------------------------------------------------------
# Geographical region
save_pdf_plot(
  plot_object = area_plot,
  filename = area_plot_file,
  output_folder = output_folder,
  width = single_plot_width,
  height = single_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# Topic
save_pdf_plot(
  plot_object = topic_plot,
  filename = topic_plot_file,
  output_folder = output_folder,
  width = single_plot_width,
  height = single_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# Bathymetry
save_pdf_plot(
  plot_object = bathymetry_plot,
  filename = bathymetry_plot_file,
  output_folder = output_folder,
  width = single_plot_width,
  height = single_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# Status
save_pdf_plot(
  plot_object = status_plot,
  filename = status_plot_file,
  output_folder = output_folder,
  width = status_plot_width,
  height = status_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# Combined
save_pdf_plot(
  plot_object = combined_category_plot,
  filename = combined_category_plot_file,
  output_folder = output_folder,
  width = combined_category_plot_width,
  height = combined_category_plot_height,
  units = plot_units,
  dpi = plot_dpi
)

# ---- 9) Print summaries ---------------------------------------------------
cat("\nGeographical area category summary:\n")
print(area_categories)

cat("\nTopic category summary:\n")
print(topic_categories)

cat("\nBathymetric zone category summary:\n")
print(bathymetry_categories)

cat("\nAssignment status summary:\n")
print(combined_status)