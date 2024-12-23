library(shiny)
library(ggplot2)

# Load data
data <- read.csv('audiofeatures.csv')
trackinfo <- read.csv('track_info.csv')
artists <- read.csv('artists.csv')

# Merge and process data
artists$track_id <- artists$song_id
artists_track <- merge(x = artists, y = data, by = "track_id", all.x = TRUE)
artists_track$genre <- as.factor(artists_track$genre)
data <- artists_track

# Unique genres for dropdown menu
genres <- unique(artists_track$genre) 



print(all_genres)
# Define UI
ui <- fluidPage(
  titlePanel("Histogram of BPM by Genre"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("genres", 
                  "Choose Genre(s):", 
                  choices = genres,
                  selected = genres[1],  # Default to "All Genres"
                  multiple = TRUE),          # Enable multi-selection
    
      actionButton("reset", "Reset All", class = "btn-primary")
      
      ),
    
    mainPanel(
      plotOutput("histPlot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  output$histPlot <- renderPlot({
    # Filter dataset by selected genres
    req(input$genres)  # Ensure input$genres is not NULL
  
      dataset <- data[data$genre %in% input$genres, ]
    
    
    # Create histogram with colors for each genre
    ggplot(dataset, aes(x = BPM, fill = genre)) +
      geom_histogram(bins = 20, position = "identity", alpha = 0.6) +
      labs(title = "Histogram of BPM by Genre",
           x = "BPM",
           y = "Frequency") +
      scale_fill_brewer(palette = "Set3") +  # Use a color palette for genres
      theme_minimal()
  })
}

# Run the App
shinyApp(ui = ui, server = server)
