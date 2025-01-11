library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyBS)
library(GGally)
library(viridis)


server <- function(input, output, session) {
  
  # Idiom 1
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
  
  # Load and prepare data for streamgraph
  data <- read.csv("csv/all_genres_popularity_over_time.csv")
  years <- sort(unique(data$release_year))
  
  # Update UI inputs for selecting genres and years
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
  
  # Ensure "start_year" stays valid when "end_year" changes
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
  
  # Reactive function to filter data for streamgraph
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
  
  # Render streamgraph plot
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
  
  # Idiom 3
    # Reactive expression to filter data
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
  
  
  
  
  # Idiom 4 Bubble Plot (BPM vs Energy vs Popularity)
    bubble_filtered_data <- reactive({
      req(bubble_data)
      bubble_data %>%
        filter(
          BPM >= input$bpm_range[1] & BPM <= input$bpm_range[2],
          energy >= input$energy_range[1] & energy <= input$energy_range[2],
          popularity >= input$popularity_range[1] & popularity <= input$popularity_range[2]
        )
    })
    
    output$bubblePlot <- renderPlot({
      ggplot(bubble_filtered_data(), aes(
        x = BPM,
        y = energy,
        size = popularity,
        color = .data[[input$color_by]] 
      )) +
        geom_point(alpha = 0.6) +
        scale_size_continuous(name = "Popularity", range = c(2, 15)) +
        scale_color_viridis_d(name = input$color_by) + 
        labs(
          title = "Bubble Chart: Energy vs BPM vs Popularity",
          x = "Beats Per Minute (BPM)",
          y = "Energy"
        ) +
        theme_minimal() +
        guides(
          color = guide_legend(override.aes = list(size = 5)) 
          
        )
    })
}
