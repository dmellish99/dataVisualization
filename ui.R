library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(bslib)
library(shinyBS)

ui <- fluidPage(
  theme = bs_theme(
    bg = "#f8f9fa",
    fg = "#333333",
    primary = "#007bff"
  ),
  img(src='logo.png', align = "left", height = "40px"),
  titlePanel("Idioms of Song Analysis"),
  
  tabsetPanel(
    #Heatmap
    tabPanel(titlePanel("Heatmap"), 
             sidebarLayout(
               sidebarPanel(
                 selectInput("track", "Select a Track:", choices = unique(track_sections$track_name)),
                 checkboxInput("compare_tracks", "Display Multiple Tracks Loudness", value = FALSE),
                 uiOutput("track_selection"),  # Dynamic UI for multiple track selection
                 
                 sliderInput("loudnessRange", "Select Loudness Range:",
                             min = min(track_sections$loudness, na.rm = TRUE),
                             max = max(track_sections$loudness, na.rm = TRUE),
                             value = c(min(track_sections$loudness, na.rm = TRUE), max(track_sections$loudness, na.rm = TRUE))
                 ),
                 
                 downloadButton("downloadPlot", "Download Heatmap")
               ),
               
               mainPanel(
                 plotOutput("heatmap")
               )
             )
    ),
    
    # Streamgraph
    tabPanel(titlePanel("Streamgraph"), 
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h4("Controls"),
                   selectizeInput(
                     inputId = "selected_genres",
                     label = "Select Genres to Display:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     options = list(
                       create = FALSE,
                       plugins = list('remove_button') 
                     )
                   ),
                 selectInput(
                   inputId = "start_year",
                   label = "Select Start Year:",
                   choices = NULL,
                   selected = NULL
                 ),
                 selectInput(
                   inputId = "end_year",
                   label = "Select End Year:",
                   choices = NULL,  
                   selected = NULL
                 )
               )
               ),
               mainPanel(
                 fluidRow(
                   column(12, align = "center", plotOutput("streamgraph_plot", height = "500px"))
                 )
               )
             )
    ),
    
    # Idiom 3 (Cambiar por el código del idiom)
    tabPanel(
      titlePanel("Song Metrics"),
      sidebarLayout(
        sidebarPanel(
          # Radio buttons to select filter type
          radioButtons(
            inputId = "filter_type",
            label = "Filter By:",
            choices = list("Genre" = "genre", "Individual Song" = "artist_song"),
            selected = "genre"
          ),
          
          # Conditional panel for genre selection
          conditionalPanel(
            condition = "input.filter_type == 'genre'",
            selectInput(
              inputId = "genres",
              label = "Select Genre(s):",
              choices = unique(dataset$genre),  # Replace with your genre column
              selected = unique(dataset$genre)[1],  # Preselect the first genre
              multiple = TRUE
            )
          ),
          
          # Conditional panel for song selection
          conditionalPanel(
            condition = "input.filter_type == 'artist_song'",
            selectInput(
              inputId = "artist_songs",
              label = "Select Song(s):",
              choices = unique(dataset$artist_song),  # Replace with your song column
              selected = unique(dataset$artist_song)[1],  # Preselect the first song
              multiple = TRUE
            )
          ),
          
          # Add selectizeInput to reorder columns
          selectizeInput(
            inputId = "column_order",
            label = "Order Columns:",
            choices = c("happiness", "acousticness", "danceability", "energy", "speechiness", "instrumentalness", "liveness"),
            selected = c("happiness", "acousticness", "danceability", "energy", "speechiness", "instrumentalness", "liveness"),
            multiple = TRUE,
            options = list(plugins = list("drag_drop"))
          ),
          # Reset Button
          actionButton(inputId = "reset_button", label = "Reset to Default")
        ),
        
        mainPanel(
          plotOutput("plot_idiom_3")
        )
      )
    ),
    
    # Idiom 4 (Cambiar por el código del idiom)
    tabPanel(titlePanel("Bubble Chart"),
             sidebarLayout(
               sidebarPanel(
                 sliderInput(
                   "bpm_range",
                   "Filter by BPM:",
                   min = min(data$BPM, na.rm = TRUE),
                   max = max(data$BPM, na.rm = TRUE),
                   value = c(min(data$BPM, na.rm = TRUE), max(data$BPM, na.rm = TRUE))
                 ),
                 sliderInput(
                   "energy_range",
                   "Filter by Energy:",
                   min = min(data$energy, na.rm = TRUE),
                   max = max(data$energy, na.rm = TRUE),
                   value = c(min(data$energy, na.rm = TRUE), max(data$energy, na.rm = TRUE))
                 ),
                 sliderInput("popularity_range", 
                             "Select Popularity Range", min = min(data$popularity),
                             max = max(data$popularity), value = c(min(data$popularity),
                                                                   max(data$popularity))
                 ),
                 selectInput(
                   "color_by",
                   "Color bubbles by:",
                   choices = c(
                     "Key Type" = "key_type",
                     "Duration Category" = "duration_category",
                     "Genre" = "genre",
                     "Explicit" = "is_explicit"
                   ),
                   selected = "key_type"
                 )
               ),
               mainPanel(
                 plotOutput("bubblePlot")
               )
             )
    )
  )
)
