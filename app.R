library(shiny)
library(httr)
library(jsonlite)
library(reticulate)

# Configure Python environment
python_path <- "C:/Users/31885/AppData/Local/Programs/Python/Python312/python.exe"
use_python(python_path, required = TRUE)

# Print Python configuration for debugging
py_config <- reticulate::py_config()
message("Using Python configuration:")
message(paste("Python path:", py_config$python))

# Source the Python script (save the MovieQueryEngine class in 'movie_query_engine.py')
source_python("movie_query_engine.py")

# Initialize the query engine
query_engine <- NULL  # Will be initialized in server

# Define UI (keeping your existing UI code)
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "body { background-color: #f4f4f9; font-family: Arial, sans-serif; }
       .title-panel { text-align: center; color: #2c3e50; margin-bottom: 20px; }
       .sidebar { background-color: #ecf0f1; padding: 15px; border-radius: 8px; }
       .btn-primary { background-color: #3498db; border: none; }
       .btn-primary:hover { background-color: #2980b9; }
       table { border-collapse: collapse; width: 100%; margin-top: 20px; table-layout: fixed; word-wrap: break-word; }
       th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
       th { background-color: #3498db; color: white; }
       .table-container { overflow-x: auto; }
       .alternative-suggestions { margin-top: 15px; padding: 10px; background-color: #fff; border-radius: 4px; }
      "
    ))
  ),
  
  titlePanel(div(class = "title-panel", "AI-Powered Movie Search from OMDB")),
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar",
          textAreaInput("search_query", "Describe the movie you're looking for:", 
                        value = "", height = "100px",
                        placeholder = "E.g., 'That sci-fi movie from the 90s with aliens and Will Smith'"),
          actionButton("search_button", "Search", class = "btn-primary"),
          br(), br(),
          actionButton("show_posters", "Show Poster", class = "btn-primary"),
          uiOutput("poster_output"),
          br(), br(),
          verbatimTextOutput("constructed_query"),
          div(class = "alternative-suggestions",
              uiOutput("alternative_suggestions"))
      )
    ),
    mainPanel(
      div(class = "table-container", tableOutput("movie_table"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize query engine with your OpenAI API key
  query_engine <<- MovieQueryEngine("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")  # Replace with your API key
  
  # Reactive values
  movies_data <- reactiveVal(NULL)
  posters_data <- reactiveVal(NULL)
  constructed_query <- reactiveVal("")
  alternative_titles <- reactiveVal(NULL)
  
  observeEvent(input$search_button, {
    req(input$search_query)
    
    # Construct query using Python class
    query <- query_engine$construct_query(input$search_query)
    constructed_query(query)
    
    # Get alternative suggestions
    alternatives <- query_engine$suggest_alternatives(input$search_query)
    alternative_titles(alternatives)
    
    # OMDB API endpoint and key
    api_key <- "f3ba2f8b"  # Replace with your OMDB API key
    base_url <- "http://www.omdbapi.com/?"
    
    # Make API request with constructed query
    response <- GET(base_url, query = list(apikey = api_key, t = query))
    
    if (response$status_code == 200) {
      data <- fromJSON(content(response, "text"), flatten = TRUE)
      
      if (!is.null(data$Title)) {
        movies_data(data.frame(
          Title = data$Title,
          Year = data$Year,
          Rated = data$Rated,
          Released = data$Released,
          Runtime = data$Runtime,
          Genre = data$Genre,
          Director = data$Director,
          Writer = data$Writer,
          Actors = data$Actors,
          Plot = data$Plot,
          Language = data$Language,
          Country = data$Country,
          Awards = data$Awards,
          imdbRating = data$imdbRating,
          imdbVotes = data$imdbVotes,
          Type = data$Type,
          BoxOffice = data$BoxOffice,
          Website = data$Website,
          stringsAsFactors = FALSE
        ))
        posters_data(if (data$Poster != "N/A") paste0("<img src=\"", data$Poster, "\" height=\"300\">") else "No Poster Available")
      } else {
        showNotification("No movies found.", type = "error")
        movies_data(NULL)
        posters_data(NULL)
      }
    } else {
      showNotification("Error fetching data from OMDB.", type = "error")
    }
  })
  
  # Display constructed query
  output$constructed_query <- renderText({
    if (constructed_query() != "") {
      paste("Constructed search:", constructed_query())
    }
  })
  
  # Display alternative suggestions
  output$alternative_suggestions <- renderUI({
    alternatives <- alternative_titles()
    if (!is.null(alternatives) && length(alternatives) > 0) {
      tagList(
        h4("Alternative Suggestions:"),
        tags$ul(
          lapply(alternatives, function(title) {
            tags$li(
              actionLink(
                inputId = paste0("alt_", gsub("[^[:alnum:]]", "", title)),
                label = title,
                onclick = sprintf("Shiny.setInputValue('search_query', '%s');", title)
              )
            )
          })
        )
      )
    }
  })
  
  # Render movie table
  output$movie_table <- renderTable({
    movies_data()
  }, rownames = FALSE)
  
  # Render posters
  observeEvent(input$show_posters, {
    output$poster_output <- renderUI({
      if (!is.null(posters_data())) {
        HTML(posters_data())
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)