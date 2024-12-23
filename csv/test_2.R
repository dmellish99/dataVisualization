library(shiny)
library(ggplot2)
library(GGally)

# Load data
features_hist <- read.csv('audiofeatures.csv')
trackinfo_hist <- read.csv('track_info.csv')
artists_hist <- read.csv('artists.csv')

# Merge and process data
artists_hist$track_id <- artists_hist$song_id
artists_hist_track <- merge(x = artists_hist, y = features_hist, by = "track_id", all.x = TRUE)
artists_hist_track$genre <- as.factor(artists_hist_track$genre)
dataset_hist <- artists_hist_track

