library(shiny)
library(ggplot2)
library(dplyr)
library(viridis)  # Para usar una paleta de colores más bonita (opcional)

data <- read.csv('audiofeatures.csv')
trackinfo <- read.csv('track_info.csv')
artists <- read.csv('artists.csv')

data_track <- merge(x = data, y = trackinfo, by = "track_id", all.x = TRUE) %>%
  select(-popularity.y) %>%
  rename(popularity = popularity.x)

artists$track_id <- artists$song_id
data_final <- merge(x = artists, y = data_track, by = "track_id", all.x = TRUE)

data_final$genre <- as.factor(data_final$genre)

data <- data_final


data <- data %>%
  mutate(
    key_type = case_when(
      grepl("Major", key, ignore.case = TRUE) ~ "Major",
      grepl("Minor", key, ignore.case = TRUE) ~ "Minor",
      TRUE ~ "Unknown"
    ),
    key_type = factor(key_type, levels = c("Major", "Minor", "Unknown")),
    duration_minutes = sapply(strsplit(duration, ":"), function(x) {
      as.numeric(x[1]) + as.numeric(x[2]) / 60 + as.numeric(x[3]) / 3600
    }),
    duration_category = case_when(
      duration_minutes >= 1 & duration_minutes < 2 ~ "1-2",
      duration_minutes >= 2 & duration_minutes < 3 ~ "2-3",
      duration_minutes >= 3 & duration_minutes < 4 ~ "3-4",
      duration_minutes >= 4 ~ "4+"
    ),
    duration_category = factor(duration_category, levels = c("1-2", "2-3", "3-4", "4+"))
  )

# UI
ui <- fluidPage(
  titlePanel("Bubble Chart: Energy vs BPM"),
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

server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(
        BPM >= input$bpm_range[1] & BPM <= input$bpm_range[2],
        energy >= input$energy_range[1] & energy <= input$energy_range[2],
        popularity >= input$popularity_range[1] & popularity <= input$popularity_range[2]
      )
  })
  
  output$bubblePlot <- renderPlot({
    ggplot(filtered_data(), aes(
      x = BPM,
      y = energy,
      size = popularity,
      color = .data[[input$color_by]] # Dinámicamente selecciona la columna para el color
    )) +
      geom_point(alpha = 0.6) +
      scale_size_continuous(name = "Popularity", range = c(2, 15)) +
      scale_color_viridis_d(name = input$color_by) + # Cambia el nombre dinámicamente en la leyenda
      labs(
        title = "Bubble Chart: Energy vs BPM",
        x = "Beats Per Minute (BPM)",
        y = "Energy"
      ) +
      theme_minimal() +
      guides(
        color = guide_legend(override.aes = list(size = 5)) # Aumenta el tamaño de los puntos en la leyenda
          # Aumenta el tamaño de los puntos de tamaño
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
