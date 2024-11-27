# ~ HEADER --------------------------------------------
#
# ~ Author:         Alfredo Marchio'
# ~ Email:          alfredo.marchio@research.uwa.edu.au
# ~ Organization:    Minderoo-UWA Deep-Sea Research Centre
# 
# ~ Date:           2024-11-27
# ~ Version:        0.1
#
# ~ Script Name:    BibliometryPlots
#
# ~ Script Description:
#
#
#
# Copyright 2024 - Alfredo Marchio'
#
# ----------------------------------------------------

library(ggplot2)
library(readxl)

# Setting working directory
setwd("C:/Users/24207596/OneDrive - UWA/Alfredo PhD/Chapter 0 - Trend and actuality in glass sponge science")

# Import the main tables
yearTable <- read_excel("Tables.xlsx", sheet = "ArticlesPerYear")
sourcesTable <- read_excel("Tables.xlsx", sheet = "Sources")
authorsTable <- read_excel("Tables.xlsx", sheet = "Authorship")
trendsTable <- read_excel("Tables.xlsx", sheet = "Trends")

# Article per Year plot
ggplot(data = yearTable, mapping = aes(x = Year, y = Articles)) +
  ggtitle(label = "Documents per Year") +
  geom_line() +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(breaks = seq(1960, 2020, by = 10))

# Most frequent sources
      
