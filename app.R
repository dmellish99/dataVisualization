library(shiny)
library(ggplot2)
library(dplyr)

# Load datasets
tracks <- read.csv("csv/track_info.csv")
sections <- read.csv("csv/sections.csv")

# Merge tracks and sections datasets, ensuring loudness is included from sections
track_sections <- merge(
  tracks[, c("track_name", "track_id", "popularity")], 
  sections[, c("track_id", "start", "duration", "loudness", "key")],  # Including loudness from sections
  by = "track_id"
)

# Prepare data for heatmap
track_sections_heatmap <- track_sections %>%
  select(track_name, start, duration, loudness, key) %>%
  mutate(end = start + duration) %>%
  mutate(key = factor(key))

# Define UI
ui <- fluidPage(
  titlePanel("Heatmap of Song Sections"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("track", "Select a Track:", choices = unique(track_sections$track_name)),
      checkboxInput("compare_tracks", "Compare Multiple Tracks", value = FALSE),
      uiOutput("track_selection"),  # Dynamic UI for multiple track selection
      
      # Loudness range slider
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
)

# Define server logic
server <- function(input, output) {
  
  # Dynamically create track selection input when comparing multiple tracks
  output$track_selection <- renderUI({
    if (input$compare_tracks) {
      # Use `selectizeInput` for multi-selection with easy deselection
      selectizeInput("multi_tracks", "Select Tracks for Comparison:", 
                     choices = unique(track_sections$track_name), 
                     selected = unique(track_sections$track_name)[1:2], 
                     multiple = TRUE,
                     options = list(plugins = list('remove_button')))
    }
  })
  
  # Filter the selected track(s) based on loudness range
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
  
  # Render the heatmap
  output$heatmap <- renderPlot({
    req(selected_tracks())  # Ensure there's data to plot
    
    ggplot(selected_tracks(), aes(x = start, y = key, fill = loudness)) +
      geom_tile() +
      labs(title = ifelse(input$compare_tracks, 
                          paste("Heatmap for Tracks:", paste(input$multi_tracks, collapse = ", ")), 
                          paste("Heatmap for", input$track)), 
           x = "Start Time (s)", y = "Key") +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_minimal()
  })
  
  # Download plot as PNG
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
}

# Run the application 
shinyApp(ui = ui, server = server)
