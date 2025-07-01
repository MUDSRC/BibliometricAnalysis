# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:   Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2025-05-21
# ~ Version:        0.1
#
# ~ Script Name:    SamplingEffort
#
# ~ Script Description:
#
#
#
# Copyright 2025 - Alfredo Marchio'
#
# ----------------------------------------------------

#### Libraries ####
library(ggplot2)
library(readxl)
library(dplyr)

#### Data input and cleaning #### 
obisDataset <- read.csv("ObisDataset.csv")

#### Spatial distribution analysis #### 


##### Distribution along depth gradient #### 
## Histogram
depthHistPlot <- ggplot(obisDataset, aes(x = depth)) +
  geom_histogram(binwidth = 200, fill = "skyblue", color = "black") +
  scale_x_reverse() +  
  coord_flip() +       
  labs(title = "Depth Distribution of Hexactinellid Reports",
       x = "Depth (m)",
       y = "Number of Records") +
  theme_minimal()

## Density Plot
depthDensPlot <- ggplot(obisDataset, aes(x = depth)) +
  geom_density(fill = "lightgreen", alpha = 0.5) +
  scale_x_reverse() +
  coord_flip() +
  labs(title = "Density of Depth Occurrences",
       x = "Depth (m)",
       y = "Density") +
  theme_minimal()


#### Taxonomic identification effort #### 



#### Sampling effort in time #### 
## Sampling effort by depth over time
depthYearBins <- obisDataset %>%
  mutate(YearRange = cut(date_year, breaks = c(0, 1950, 1980, 2000, 2010, 2025),
                         labels = c("Before 1950", "1950–1980", "1980–2000", "2000–2010", "2010–2025")))

depthByYear <- ggplot(depthYearBins, aes(x = YearRange, y = depth)) +
  geom_violin() +
  scale_y_reverse() +
  labs(title = "Depth Distribution Over Time",
       x = "Year Range",
       y = "Depth (m)") +
  theme_minimal()


#### Comparing hotspots with keywords: publications and sampling effort are correlated? #### 