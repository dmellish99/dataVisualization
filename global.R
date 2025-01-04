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


#Idiom 4

bubble_data <- read.csv('csv/audiofeatures.csv')
bubble_trackinfo <- read.csv('csv/track_info.csv')
bubble_artists <- read.csv('csv/artists.csv')



bubble_data_track <- merge(x = bubble_data, y = bubble_trackinfo, by = "track_id", all.x = TRUE) %>%
  select(-popularity.y) %>%
  rename(popularity = popularity.x)

bubble_artists$track_id <- bubble_artists$song_id
bubble_data_final <- merge(x = bubble_artists, y = bubble_data_track, by = "track_id", all.x = TRUE)

bubble_data_final$genre <- as.factor(bubble_data_final$genre)

bubble_data <- bubble_data_final


bubble_data <- bubble_data %>%
  mutate(
    key_type = case_when(
      grepl("Major", key, ignore.case = TRUE) ~ "Major",
      grepl("Minor", key, ignore.case = TRUE) ~ "Minor",
      TRUE ~ "Unknown"
    ),
    key_type = factor(key_type, levels = c("Major", "Minor", "Unknown")),
    duration_minutes = sapply(strsplit(duration, ":"), function(x) {
      as.numeric(x[1]) + as.numeric(x[2]) / 60 + as.numeric(x[3]) / 3600
    }),
    duration_category = case_when(
      duration_minutes >= 1 & duration_minutes < 2 ~ "1-2",
      duration_minutes >= 2 & duration_minutes < 3 ~ "2-3",
      duration_minutes >= 3 & duration_minutes < 4 ~ "3-4",
      duration_minutes >= 4 ~ "4+"
    ),
    duration_category = factor(duration_category, levels = c("1-2", "2-3", "3-4", "4+"))
  )
