# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchiò
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-11-12
# ~ Version:        1.0
#
# ~ Script Name:    BathymetricStudyEffort.R
#
# ~ Script Description:
# Load bathymetric zone per paper.
# Use count to create violin plot of the study effort.
#
# Copyright 2025 - Alfredo Marchiò
#
# ----------------------------------------------------

# ---- Libraries ------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)

# ---- 1) Input ------------------------------------------------------------
raw_matrix <- read.csv("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Bibliometric Metadata/Bathymetric range.csv",
                   check.names = FALSE,   
                   stringsAsFactors = FALSE,
                   na.strings = c("", "NA"))

# ---- 2) Data clenaning headers & define zones -----------------------------------------
# Keep column names
names(raw_matrix) <- trimws(names(raw_matrix))
names(raw_matrix)[names(raw_matrix) %in% c("ï..PY","﻿PY")] <- "PY"

valid_zones <- c("Shallow","Mesophotic","Bathyal","Abyssal","Hadal")
n_total_papers <- nrow(raw_matrix)
zone_cols   <- intersect(names(raw_matrix), valid_zones)  # only keep legit zone columns

# Convert presence marks to 0/1
raw_matrix[zone_cols] <- lapply(raw_matrix[zone_cols], function(col) {
  as.integer(tolower(trimws(as.character(col))) == "x")
})


# Number of tagged papers (at least one depth zone)
n_tagged_papers <- nrow(raw_matrix)

# Print summary
cat(
  "Tagged papers (with at least one depth zone):",
  n_tagged_papers, "out of", n_total_papers,
  sprintf(" (%.1f%%)\n", 100 * n_tagged_papers / n_total_papers)
)


# Remove papers with no bathymetry
raw_matrix <- raw_matrix[rowSums(raw_matrix[zone_cols], na.rm = TRUE) > 0, , drop = FALSE]

# Calculate totals
raw_matrix$CountZonesThisYear <- rowSums(raw_matrix[zone_cols], na.rm = TRUE)

# Ensure all five zones appear in totals
zone_counts <- setNames(integer(length(valid_zones)), valid_zones)
if (length(zone_cols)) zone_counts[zone_cols] <- colSums(raw_matrix[zone_cols], na.rm = TRUE)

totals <- tibble::tibble(
  DepthZone = factor(valid_zones, levels = valid_zones),
  Total     = as.integer(unname(zone_counts[valid_zones]))
)

long <- raw_matrix %>%
  pivot_longer(all_of(zone_cols), names_to = "DepthZone", values_to = "Count") %>%
  mutate(
    DepthZone = factor(DepthZone, levels = valid_zones),
    PY = as.integer(PY)
  )

# Papers with at least one depth zone marked 
has_any_tag <- rowSums(raw_matrix[zone_cols], na.rm = TRUE) > 0
n_tagged_papers <- sum(has_any_tag)

# Print summary (NAs in all zones NOT counted in the total)
cat(
  "Tagged papers (with at least one depth zone):",
  n_tagged_papers, "out of", n_total_papers,
  sprintf(" (%.1f%%)\n", 100 * n_tagged_papers / n_total_papers)
)


# ---- 3) Pie chart: total number of publications by zone --------------------
totals_out <- totals %>%
  arrange(desc(DepthZone)) %>%
  mutate(
    ymax  = cumsum(Total),
    ymin  = lag(ymax, default = 0),
    ymid  = (ymax + ymin) / 2,
    Label = as.character(Total)
  )

pie_plot <- ggplot(totals_out, aes(y = Total, x = 1, fill = DepthZone)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(y = ymid, x = 1.1, label = Label), size = 4, fontface = "bold") +
  xlim(0.5, 1.6) + 
  theme_void(base_size = 12)

# ---- 4) Violin plot ------------------------------------------------------
pub_years <- long %>%
  filter(Count > 0) %>%           # keep only years with at least one pub
  uncount(weights = Count) %>%    # replicate rows Count-times
  mutate(PY = as.integer(PY))

# For nice axis limits
ymin <- min(pub_years$PY, na.rm = TRUE)
ymax <- max(pub_years$PY, na.rm = TRUE)

# Violin: each category is a separate violin in the same plot
violin_plot <- ggplot(pub_years, aes(x = DepthZone, y = PY, fill = DepthZone)) +
  geom_violin(scale = "count", trim = TRUE, alpha = 1, width = 1.7) +
  scale_y_continuous(limits = c(ymin, ymax)) +
  labs(
    x = "Depth zone",
    y = "Publication year"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

# ---- 5) Save outputs ------------------------------------------------------
# Write PDFs to the Plots folder with fixed sizes and dpi
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 1 - Trend and actuality in glass sponge science/Raw Plots")

ggsave("Publication per year per depth zone.pdf", violin_plot,  width = 20, height = 16, units = "cm", dpi = 300)
ggsave("Pie chart depth zones.pdf", pie_plot, width = 20, height = 16, units = "cm", dpi = 300)  

