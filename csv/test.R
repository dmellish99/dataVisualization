library(shiny)
library(ggplot2)
# Load data
features <- read.csv('audiofeatures.csv')
artists <- read.csv('artists.csv')
trackinfo<-read.csv('track_info.csv')
# Merge and process 
artists$track_id<-artists$song_id

trackinfo<-merge(x = trackinfo, y = artists, by = "track_id", all.x = TRUE)

trackinfo<-merge(x = trackinfo, y = features, by = "track_id", all.x = TRUE)


trackinfo$artist_song=paste(trackinfo$artist,'_',trackinfo$track_name)

dataset <- artists_track[,c(6:12,14,15,20)]


# Define UI
ui <- fluidPage(
  titlePanel("Parallel Coordinates of Song Metrics by Track"),
  
  sidebarLayout(
    sidebarPanel(),
    
    mainPanel(
      plotOutput("plot_idiom_3")
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  output$plot_idiom_3 <- renderPlot({
    # Filter dataset by selected genres
    req(input$genres)  # Ensure input$genres is not NULL
    
    #  dataset_display <- data[data$genre %in% input$genres, ]
    dataset_display<-dataset
    ggparcoord(dataset_display,
               columns =2:8, groupColumn = 1, 
               # order = "anyClass",
               # #scale="center",
                showPoints = TRUE, 
               # title = "Standardize and center variables",
               # alphaLines = 0.3
    )}) 
  
}
    # + 
      # # scale_color_viridis(discrete=TRUE) +
      # # theme_ipsum()+
      # theme(
      #   legend.position="none",
      #   plot.title = element_text(size=13)
      # ) +
      # xlab("Metrics")    
      # 
  # })


# Run the App
shinyApp(ui = ui, server = server)
