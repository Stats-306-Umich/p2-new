# Load necessary libraries
library(shiny)
library(dplyr)
library(DT)



# UI
ui <- fluidPage(
  titlePanel("IMDb Titles by Category and Job"),
  
  sidebarLayout(
    sidebarPanel(
      # Widget 1: Select Category
      selectInput("categoryInput", "Select a Category:",
                  choices = unique(title_principals$category),
                  selected = NULL),
      
      # Widget 2: Select Job (updated dynamically)
      selectInput("jobInput", "Select a Job:",
                  choices = NULL,  # will be filled based on category
                  selected = NULL)
    ),
    
    mainPanel(
      DTOutput("resultsTable")  # Output: Table showing results
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: Filter title_principals by selected category
  filtered_by_category <- reactive({
    req(input$categoryInput)
    title_principals %>% 
      filter(category == input$categoryInput)
  })
  
  # Update jobInput choices when categoryInput changes
  observe({
    jobs <- filtered_by_category() %>%
      filter(!is.na(job)) %>%
      distinct(job) %>%
      arrange(job) %>%
      pull(job)
    
    updateSelectInput(session, "jobInput", choices = jobs)
  })
  
  # Reactive: Filter title_principals by selected job
  filtered_by_job <- reactive({
    req(input$categoryInput, input$jobInput)
    title_principals %>%
      filter(category == input$categoryInput, job == input$jobInput)
  })
  
  # Reactive: Join with title_basics
  joined_data <- reactive({
    filtered_by_job() %>%
      inner_join(title_basics, by = "tconst")  # joining by tconst
  })
  
  # Render table
  output$resultsTable <- renderDT({
    datatable(joined_data())
  })
}

# Run the Shiny app
shinyApp(ui, server)

