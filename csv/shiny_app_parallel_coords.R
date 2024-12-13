library(shiny)
library(ggplot2)

library(hrbrthemes)
library(GGally)
library(viridis)

# Load data
features <- read.csv('audiofeatures.csv')
trackinfo <- read.csv('track_info.csv')
artists <- read.csv('artists.csv')

# Merge and process data
artists$track_id <- artists$song_id
artists_track <- merge(x = artists, y = data, by = "track_id", all.x = TRUE)
artists_track$genre <- as.factor(artists_track$genre)

# Unique genres for dropdown menu
cols <- colnames(artists_track)[7:12]

col_group_by=colnames(artists_track)[6]


dataset <- artists_track[,c(6:12,14,15)]



# Unique genres for dropdown menu
genres <- unique(dataset$genre) 



print(genres)
# Define UI
ui <- fluidPage(
  titlePanel("Song Metrics by Genre (Parallel Coordinates"),
  
   sidebarLayout(
    sidebarPanel(
      # selectInput("genres",
      #             "Choose Genre(s):",
      #             choices = genres,
      #             selected = genres[1],  # Default to "All Genres"
      #             multiple = TRUE),          # Enable multi-selection
      # 
      # actionButton("reset", "Reset All", class = "btn-primary")

    ),
  #   
    mainPanel(
      plotOutput("parallelPlot")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  output$parallelPlot <- renderPlot({
    # Filter dataset by selected genres
    # req(input$genres)  # Ensure input$genres is not NULL
    
    # dataset <- data[data$genre %in% input$genres, ]
    
    dataset<-data
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
      xlab("")    
    
  })
}

# Run the App
shinyApp(ui = ui, server = server)
