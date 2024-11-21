library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyBS)

tracks <- read.csv("csv/track_info.csv")
sections <- read.csv("csv/sections.csv")

track_sections <- merge(
  tracks[, c("track_name", "track_id", "popularity")], 
  sections[, c("track_id", "start", "duration", "loudness", "key")],
  by = "track_id"
)

track_sections_heatmap <- track_sections %>%
  select(track_name, start, duration, loudness, key) %>%
  mutate(end = start + duration) %>%
  mutate(key = factor(key))