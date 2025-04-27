# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(shinyjs)

# Clean up and preprocess the dataset once
title_basics <- title_basics %>%
  filter(!is.na(startYear), !is.na(genres), !is.na(titleType))  # Remove rows with missing data

# Convert columns to proper data types
title_basics$titleType <- as.factor(title_basics$titleType)
title_basics$genres <- as.character(title_basics$genres)

# UI of the Shiny app
ui <- fluidPage(
  titlePanel("IMDb Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      # Widget 1: Slider Input for Start Year
      sliderInput("yearInput", "Select Year Range:",
                  min = min(title_basics$startYear, na.rm = TRUE),
                  max = max(title_basics$startYear, na.rm = TRUE),
                  value = c(min(title_basics$startYear), max(title_basics$startYear)),
                  step = 1),
      
      # Widget 2: Checkbox for Title Type (e.g., Movie or TV show)
      checkboxGroupInput("titleTypeInput", "Select Title Type:",
                         choices = levels(title_basics$titleType),  # Use levels to show the factor levels
                         selected = levels(title_basics$titleType)),
      
      # Widget 3: Select Input for Genre
      selectInput("genreInput", "Select Genre:",
                  choices = c("All", unique(title_basics$genres)),
                  selected = "All")
    ),
    
    mainPanel(
      # Graph 1: Interactive Bar Plot for Genre Distribution
      plotlyOutput("genrePlot"),
      
      # Graph 2: Interactive Scatter Plot for Runtime vs. Start Year
      plotlyOutput("runtimePlot"),
      
      # Graph 3: Interactive Line Plot for Count of Titles Over Time
      plotlyOutput("titlesOverTimePlot")
    )
  )
)

# Server Logic for Shiny app
server <- function(input, output) {
  
  # Reactive expression to filter data based on user inputs
  filtered_data <- reactive({
    req(input$yearInput, input$titleTypeInput)  # Ensure inputs are available
    
    data <- title_basics %>%
      filter(startYear >= input$yearInput[1], startYear <= input$yearInput[2]) %>%
      filter(titleType %in% input$titleTypeInput)
    
    # Filter by genre if not "All"
    if (input$genreInput != "All") {
      data <- data %>%
        filter(str_detect(genres, input$genreInput))
    }
    
    return(data)
  })
  
  # Graph 1: Genre Distribution (Interactive Bar Plot)
  output$genrePlot <- renderPlotly({
    genre_counts <- filtered_data() %>%
      count(genres) %>%
      arrange(desc(n)) %>%
      head(20)  # Limit to top 20 genres for faster rendering
    
    plot <- ggplot(genre_counts, aes(x = reorder(genres, -n), y = n)) +
      geom_bar(stat = "identity") +
      labs(x = "Genre", y = "Count", title = "Genre Distribution") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(plot)
  })
  
  # Graph 2: Runtime vs. Start Year (Interactive Scatter Plot)
  output$runtimePlot <- renderPlotly({
    # Reduce data size for scatter plot (sample 1000 rows)
    plot_data <- filtered_data() %>%
      sample_n(1000)  # Sample 1000 rows for faster plotting
    
    plot <- ggplot(plot_data, aes(x = startYear, y = runtimeMinutes)) +
      geom_point(alpha = 0.5) +
      labs(x = "Start Year", y = "Runtime (Minutes)", title = "Runtime vs. Start Year") +
      theme_minimal()
    
    ggplotly(plot)
  })
  
  # Graph 3: Count of Titles Over Time (Interactive Line Plot)
  output$titlesOverTimePlot <- renderPlotly({
    titles_over_time <- filtered_data() %>%
      group_by(startYear) %>%
      count() %>%
      arrange(startYear)
    
    plot <- ggplot(titles_over_time, aes(x = startYear, y = n)) +
      geom_line(color = "blue") +
      labs(x = "Year", y = "Number of Titles", title = "Number of Titles Over Time") +
      theme_minimal()
    
    ggplotly(plot)
  })
  
}

# Run the Shiny App
shinyApp(ui = ui, server = server)