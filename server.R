library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyBS)
library (ggstream)
library(GGally)



server <- function(input, output, session) {
  
  output$track_selection <- renderUI({
    if (input$compare_tracks) {
      selectizeInput("multi_tracks", "Select Tracks for Displayment:", 
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
      geom_stream(type = "ridge", alpha = 0.7) +
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
  
  # # Load data
  # features <- read.csv('csv/audiofeatures.csv')
  # artists <- read.csv('csv/artists.csv')
  # trackinfo<-read.csv('csv/track_info.csv')
  # # Merge and process 
  # artists$track_id<-artists$song_id
  
  # trackinfo<-merge(x = trackinfo, y = artists, by = "track_id", all.x = TRUE)
  
  # trackinfo<-merge(x = trackinfo, y = features, by = "track_id", all.x = TRUE)
  
  
  # trackinfo$artist_song=paste(trackinfo$artist,'-',trackinfo$track_name)
  
  # cols_to_keep<-c('artist_song','genre','happiness','acousticness','danceability','energy','speechiness','instrumentalness','liveness')
  # dataset<-trackinfo[,(names(trackinfo) %in% cols_to_keep)]
  
  # dataset$genre<-as.factor(dataset$genre)
  

  
  # Reactive dataset filtered by genre and artist
  # Reactive dataset based on filter type

    # Reactive expression to filter data (same as before)
    par_coords_data <- reactive({
      if (input$filter_type == "genre") {
        req(input$genres)
        dataset %>%
          dplyr::select(genre, happiness, acousticness, danceability, energy, speechiness, instrumentalness, liveness) %>%
          dplyr::filter(genre %in% input$genres)
      } else if (input$filter_type == "artist_song") {
        req(input$artist_songs)
        dataset %>%
          dplyr::select(artist_song, happiness, acousticness, danceability, energy, speechiness, instrumentalness, liveness) %>%
          dplyr::filter(artist_song %in% input$artist_songs)
      }
    })
    
    # Render the plot
    output$plot_idiom_3 <- renderPlot({
      req(par_coords_data())
      req(input$column_order)

      # Validate column order
      valid_columns <- names(par_coords_data())
      user_columns <- input$column_order
      if (is.null(user_columns) || length(user_columns) == 0) {
        user_columns <- valid_columns[-1]  # Default to all columns except grouping column
      }
      
      if (!all(user_columns %in% valid_columns)) {
        stop("Invalid columns in input$column_order")
      }
      
      # Dynamically match columns to user-defined order
      column_indices <- match(user_columns, valid_columns)
      
      ggparcoord(
        data = par_coords_data(),
        columns = column_indices,
        groupColumn = 1,  # First column for grouping
        showPoints = TRUE,
        scale = "globalminmax",
        title = "Song Metrics by Genre or Song"
      ) +
        scale_color_viridis_d() +
        theme_minimal() +
        labs(x = "Metrics", y = "Values", color = "Grouping")
    })
    
    # Reset button functionality
    observeEvent(input$reset_button, {
      updateRadioButtons(session, "filter_type", selected = "genre")  # Reset filter type to genre
      
      # Reset genre or song input depending on filter type
      updateSelectInput(session, "genres", selected = unique(dataset$genre)[1])  # Reset genre to the first value
      updateSelectInput(session, "artist_songs", selected = unique(dataset$artist_song)[1])  # Reset song to the first value
      
      # Reset column order input
      updateSelectizeInput(
        session,
        inputId = "column_order",
        selected = c("happiness", "acousticness", "danceability", "energy", "speechiness", "instrumentalness", "liveness")
      )
    })
  
  
  
  
  # Idiom 4 Placeholder (Cambiar por el código del idiom)
  # Filter dataset by selected genres
  # Load data
  # features_hist <- read.csv('csv/audiofeatures.csv')
  # trackinfo_hist <- read.csv('csv/track_info.csv')
  # artists_hist <- read.csv('csv/artists.csv')

  # # Merge and process data
  # artists_hist$track_id <- artists_hist$song_id
  # artists_hist_track <- merge(x = artists_hist, y = features_hist, by = "track_id", all.x = TRUE)
  # artists_hist_track$genre <- as.factor(artists_hist_track$genre)
  # dataset_hist <- artists_hist_track


  
  
  hist_data <- reactive({
    req(input$genres_)  # Ensure input$genres_ is not NULL
    dataset_hist[dataset_hist$genre %in% input$genres_, ]
  })
  
  output$plot_idiom_4 <- renderPlot({
  # Filter dataset by selected genres

    
    # Create histogram with colors for each genre
    ggplot(hist_data(), aes(x = BPM, fill = genre)) +
      geom_histogram(bins = 20, position = "identity", alpha = 0.6) +
      labs(title = "Histogram of BPM by Genre",
           x = "BPM",
           y = "Frequency") +
      scale_fill_brewer(palette = "Set3") +  # Use a color palette for genres
      theme_minimal()
  })
  
  
}