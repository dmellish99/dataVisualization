library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyBS)

server <- function(input, output, session) {
  
  output$track_selection <- renderUI({
    if (input$compare_tracks) {
      selectizeInput("multi_tracks", "Select Tracks for Comparison:", 
                     choices = unique(track_sections$track_name), 
                     selected = unique(track_sections$track_name)[1:2], 
                     multiple = TRUE,
                     options = list(plugins = list('remove_button')))
    }
  })
  
  selected_tracks <- reactive({
    if (input$compare_tracks) {
      track_sections_heatmap %>%
        filter(track_name %in% input$multi_tracks) %>%
        filter(loudness >= input$loudnessRange[1], loudness <= input$loudnessRange[2])
    } else {
      track_sections_heatmap %>%
        filter(track_name == input$track) %>%
        filter(loudness >= input$loudnessRange[1], loudness <= input$loudnessRange[2])
    }
  })
  
  output$heatmap <- renderPlot({
    req(selected_tracks())
    
    ggplot(selected_tracks(), aes(x = start, y = key, fill = loudness)) +
      geom_tile() +
      labs(title = ifelse(input$compare_tracks, 
                          paste("Heatmap for Tracks:", paste(input$multi_tracks, collapse = ", ")), 
                          paste("Heatmap for", input$track)), 
           x = "Start Time (s)", y = "Key") +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal()
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      if (input$compare_tracks) {
        paste("heatmap_comparison_", Sys.Date(), ".png", sep = "")
      } else {
        paste("heatmap_", input$track, ".png", sep = "")
      }
    },
    content = function(file) {
      g <- ggplot(selected_tracks(), aes(x = start, y = key, fill = loudness)) +
        geom_tile() +
        labs(title = ifelse(input$compare_tracks, 
                            paste("Heatmap for Tracks:", paste(input$multi_tracks, collapse = ", ")), 
                            paste("Heatmap for", input$track)), 
             x = "Start Time (s)", y = "Key") +
        scale_fill_gradient(low = "blue", high = "red") +
        theme_minimal()
      ggsave(file, plot = g, device = "png")
    }
  )
  
  # Idiom 2
  data <- read.csv("csv/all_genres_popularity_over_time.csv")
  years <- sort(unique(data$release_year))
  observe({
    updateSelectizeInput(
      session,
      inputId = "selected_genres",
      choices = unique(data$genre),
      selected = NULL,
      server = TRUE 
    )
    
    updateSelectInput(
      session = session,
      inputId = "start_year",
      choices = years,
      selected = min(years)  
    )
    
    updateSelectInput(
      session = session,
      inputId = "end_year",
      choices = years,
      selected = max(years)  
    )
  })
  
  observeEvent(input$end_year, {
    req(input$end_year) 
    
    end_year <- as.numeric(input$end_year)
    valid_start_years <- years[years <= end_year]
    
    updateSelectInput(
      session = session,
      inputId = "start_year",
      choices = valid_start_years,
      selected = ifelse(as.numeric(input$start_year) <= end_year, input$start_year, min(valid_start_years))
    )
  })
  
  filtered_data <- reactive({
    req(input$selected_genres, input$start_year, input$end_year)
    
    data %>%
      filter(
        genre %in% input$selected_genres,
        release_year >= as.numeric(input$start_year),
        release_year <= as.numeric(input$end_year)
      ) %>%
      group_by(release_year, genre) %>%
      summarize(avg_popularity = mean(popularity, na.rm = TRUE)) %>%
      arrange(release_year)
  })
  
  output$streamgraph_plot <- renderPlot({
    req(filtered_data())
    
    ggplot(filtered_data(), aes(x = release_year, y = avg_popularity, fill = genre)) +
      geom_area(alpha = 0.7, position = "stack") +
      labs(
        title = "Streamgraph of Genre Popularity",
        x = "Year",
        y = "Popularity",
        fill = "Genre"
      ) +
      scale_x_continuous(
        breaks = seq(
          from = floor(min(filtered_data()$release_year)), 
          to = ceiling(max(filtered_data()$release_year)), 
          by = 1
        ),
        labels = as.integer
      ) +
      theme_minimal() +
      scale_fill_brewer(palette = "Set2") +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#333333"),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom"
      )
  })
  
  # Idiom 3 Placeholder
  
  # Load data
  features <- read.csv('csv/audiofeatures.csv')
  trackinfo <- read.csv('csv/track_info.csv')
  artists <- read.csv('csv/artists.csv')
  
  # Merge and process data
  artists$track_id <- artists$song_id
  artists_track <- merge(x = artists, y = data, by = "track_id", all.x = TRUE)
  artists_track$genre <- as.factor(artists_track$genre)
  
  # Unique genres for dropdown menu
  cols <- colnames(artists_track)[7:12]
  
  col_group_by=colnames(artists_track)[6]
  
  
  dataset <- artists_track[,c(6:12,14,15)]
  
  
  output$plot_idiom_3 <- renderPlot({
    ggparcoord(dataset,
               columns =2:9, groupColumn = 1, order = "anyClass",
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
      xlab("Metrics")    
  })
  
  # Load data
  features_hist <- read.csv('csv/audiofeatures.csv')
  trackinfo_hist <- read.csv('csv/track_info.csv')
  artists_hist <- read.csv('csv/artists.csv')
  
  # Merge and process data
  artists_hist$track_id <- artists$song_id
  artists_hist_track <- merge(x = artists, y = data, by = "track_id", all.x = TRUE)
  artists_hist_track$genre <- as.factor(artists_track$genre)
  dataset_hist <- artists_hist_track
  
  # Idiom 4 Placeholder (Cambiar por el código del idiom)
  # Filter dataset by selected genres
  req(input$genres)  # Ensure input$genres is not NULL
  
  dataset_hist <- dataset_hist[dataset_hist$genre %in% input$genres, ]
  
  # Load data
  data <- read.csv('audiofeatures.csv')
  trackinfo <- read.csv('track_info.csv')
  artists <- read.csv('artists.csv')
  
  # Merge and process data
  artists$track_id <- artists$song_id
  artists_track <- merge(x = artists, y = data, by = "track_id", all.x = TRUE)
  artists_track$genre <- as.factor(artists_track$genre)
  data <- artists_track
  
  
  
  output$plot_idiom_4 <- renderPlot({

    
    # Create histogram with colors for each genre
    ggplot(dataset_hist, aes(x = BPM, fill = genre)) +
      geom_histogram(bins = 20, position = "identity", alpha = 0.6) +
      labs(title = "Histogram of BPM by Genre",
           x = "BPM",
           y = "Frequency") +
      scale_fill_brewer(palette = "Set3") +  # Use a color palette for genres
      theme_minimal()
  })
  
  
}