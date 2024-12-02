library(hrbrthemes)
library(GGally)
library(viridis)


data=read.csv('audiofeatures.csv')


trackinfo=read.csv('track_info.csv')

artists=read.csv('artists.csv')

artists$track_id=artists$song_id

artists_track=merge(x = artists, y = data, by = "track_id", all.x = TRUE)


artists_track$genre=as.factor(artists_track$genre)

# Plot
ggparcoord(artists_track,
           columns =c(7:12,14:15), groupColumn = 6, order = "anyClass",
           #scale="center",
           showPoints = TRUE, 
           title = "Standardize and center variables",
           alphaLines = 0.3
) + 
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    legend.position="none",
    plot.title = element_text(size=13)
  ) +
  xlab("")


library(ggplot2)


# Create multiple histograms using facets
ggplot(artists_track, aes(x = BPM,fill=genre)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Multiple Histograms", x = "Value", y = "Frequency") +
  theme_minimal()
