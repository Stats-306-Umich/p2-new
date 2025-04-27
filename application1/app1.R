#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# app.R
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Placeholder IMDb-like dataset
set.seed(123)
genres <- c("Action", "Comedy", "Drama", "Horror", "Romance", "Sci-Fi")
fake_imdb <- data.frame(
  title = paste("Movie", 1:500),
  year = sample(1980:2022, 500, replace = TRUE),
  genre = sample(genres, 500, replace = TRUE),
  rating = round(runif(500, 4, 10), 1),
  votes = sample(100:100000, 500, replace = TRUE)
)

# UI
ui <- fluidPage(
  titlePanel("IMDb Movie Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Select Release Year Range:",
                  min = 1980, max = 2022, value = c(2000, 2022)),
      selectInput("genre", "Choose Genre:", choices = c("All", genres)),
      numericInput("minVotes", "Minimum Votes:", value = 1000, min = 0)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Scatter Plot", plotlyOutput("scatterPlot")),
        tabPanel("Average Rating by Genre", plotlyOutput("barPlot"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  filteredData <- reactive({
    data <- fake_imdb %>%
      filter(year >= input$yearRange[1], year <= input$yearRange[2],
             votes >= input$minVotes)
    if (input$genre != "All") {
      data <- data %>% filter(genre == input$genre)
    }
    data
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = votes, y = rating, color = genre, text = title)) +
      geom_point(alpha = 0.6) +
      scale_x_log10() +
      labs(title = "Rating vs. Number of Votes", x = "Votes (log scale)", y = "IMDb Rating")
    ggplotly(p, tooltip = "text")
  })
  
  output$barPlot <- renderPlotly({
    data <- filteredData() %>%
      group_by(genre) %>%
      summarise(avg_rating = mean(rating), .groups = 'drop')
    
    p <- ggplot(data, aes(x = genre, y = avg_rating, fill = genre)) +
      geom_col() +
      labs(title = "Average Rating by Genre", x = "Genre", y = "Average Rating") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

