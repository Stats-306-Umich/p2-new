library(shiny)
library(data.table)

#–– Load your preprocessed data first
# name_basics      <- readRDS("data/name_basics.rda")
# title_basics     <- readRDS("data/title_basics.rda")
# title_principals <- readRDS("data/title_principals.rda")

# (Assuming those objects already exist in your session:)
setDT(name_basics)
setDT(title_basics)
setDT(title_principals)

# Kevin Bacon’s nconst
kevin_bacon_id <- "nm0000102"

ui <- fluidPage(
  titlePanel("Six Degrees of Kevin Bacon"),
  sidebarLayout(
    sidebarPanel(
      textInput("movie", "Enter a movie/TV show title:"),
      actionButton("start", "Start Game"),
      uiOutput("choices_ui"),
      verbatimTextOutput("status")
    ),
    mainPanel(
      h4("Path So Far:"),
      verbatimTextOutput("path")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    path = character(0),
    depth = 0,
    current_ids = character(0)
  )
  
  get_cast_by_title <- function(title_name) {
    tcs <- title_basics[primaryTitle == title_name, tconst]
    if (length(tcs) == 0) return(character(0))
    unique(title_principals[tconst %in% tcs, nconst])
  }
  get_costars <- function(nm) {
    tcs <- title_principals[nconst == nm, tconst]
    unique(title_principals[tconst %in% tcs & nconst != nm, nconst])
  }
  
  # Start button sets up the first list of IDs
  observeEvent(input$start, {
    rv$depth       <- 1
    rv$path        <- input$movie
    rv$current_ids <- get_cast_by_title(input$movie)
  })
  
  # Render the selectInput with a dummy placeholder
  output$choices_ui <- renderUI({
    req(rv$depth > 0)
    names <- name_basics[nconst %in% rv$current_ids, primaryName]
    selectInput(
      "choice",
      label    = paste0("Step ", rv$depth, ": choose a person"),
      choices  = c("Select a person" = "", setNames(names, names)),
      selected = ""
    )
  })
  
  # Only fire when the user picks someone (not the empty placeholder)
  observeEvent(input$choice, {
    req(input$choice != "")         # ignore the placeholder
    sel_name <- input$choice
    sel_id   <- name_basics[primaryName == sel_name, nconst][1]
    rv$path  <- c(rv$path, sel_name)
    
    if (sel_id == kevin_bacon_id) {
      showNotification("You reached Kevin Bacon! You win!", type = "message")
      rv$depth <- 0
    } else if (rv$depth >= 6) {
      showNotification("Max steps reached—You lose!", type = "error")
      rv$depth <- 0
    } else {
      rv$depth       <- rv$depth + 1
      rv$current_ids <- get_costars(sel_id)
    }
  }, ignoreInit = TRUE)
  
  output$path <- renderText({ paste(rv$path, collapse = " → ") })
  output$status <- renderText({
    if (rv$depth == 0) {
      "Game over. Enter a new title to play again."
    } else {
      paste("Step", rv$depth, "of 6")
    }
  })
}

shinyApp(ui, server)
