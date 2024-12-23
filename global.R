library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyBS)
library(GGally)

tracks <- read.csv("csv/track_info.csv")
sections <- read.csv("csv/sections.csv")

track_sections <- merge(
  tracks[, c("track_name", "track_id", "popularity")], 
  sections[, c("track_id", "start", "duration", "loudness", "key")],
  by = "track_id"
)

track_sections_heatmap <- track_sections %>%
  dplyr::select(track_name, start, duration, loudness, key) %>%
  dplyr::mutate(end = start + duration) %>%
  dplyr::mutate(key = factor(key))


features_hist <- read.csv('csv/audiofeatures.csv')
trackinfo_hist <- read.csv('csv/track_info.csv')
artists_hist <- read.csv('csv/artists.csv')

# Merge and process data
artists_hist$track_id <- artists_hist$song_id
artists_hist_track <- merge(x = artists_hist, y = features_hist, by = "track_id", all.x = TRUE)
artists_hist_track$genre <- as.factor(artists_hist_track$genre)
dataset_hist <- artists_hist_track



# Load data
features <- read.csv('csv/audiofeatures.csv')
artists <- read.csv('csv/artists.csv')
trackinfo<-read.csv('csv/track_info.csv')
# Merge and process 
artists$track_id<-artists$song_id

trackinfo<-merge(x = trackinfo, y = artists, by = "track_id", all.x = TRUE)

trackinfo<-merge(x = trackinfo, y = features, by = "track_id", all.x = TRUE)


trackinfo$artist_song=paste(trackinfo$artist,'-',trackinfo$track_name)

cols_to_keep<-c('artist_song','genre','happiness','acousticness','danceability','energy','speechiness','instrumentalness','liveness')
dataset<-trackinfo[,(names(trackinfo) %in% cols_to_keep)]

dataset$genre<-as.factor(dataset$genre)
