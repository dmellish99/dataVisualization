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
    tabPanel("Heatmap", 
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
    tabPanel("Streamgraph", 
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
    tabPanel("Idiom 3",
             sidebarLayout(
               sidebarPanel(
                 helpText("Agregar controles para el tercer idiom aquí.")
               ),
               mainPanel(
                 plotOutput("plot_idiom_3") 
               )
             )
    ),
    
    # Idiom 4 (Cambiar por el código del idiom)
    tabPanel("Idiom 4",
             sidebarLayout(
               sidebarPanel(
                 helpText("Agregar controles para el cuarto idiom aquí.")
               ),
               mainPanel(
                 plotOutput("plot_idiom_4")  
               )
             )
    )
  )
)