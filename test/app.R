library(shiny)
library(DT)

# Sample data
data <- data.frame(
  Name = c("John", "Jane", "Paul", "Anna", "Mike", "George", "Emma", "Alice", "Bob", "Zoe"),
  Age = c(23, 29, 35, 42, 19, 25, 30, 37, 22, 27),
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Reduce font size and padding for pagination */
      .dataTables_paginate {
        font-size: 10px;  /* Smaller font size */
        padding: 2px;     /* Less padding */
      }
      .dataTables_paginate .paginate_button {
        padding: 4px 8px; /* Smaller padding for page buttons */
        font-size: 10px;   /* Smaller font size for page buttons */
      }
      .dataTables_paginate .paginate_button.disabled {
        background-color: #f5f5f5;  /* Disabled button background */
      }
      .dataTables_info {
        font-size: 10px;  /* Smaller font size for information text */
        padding: 5px;     /* Less padding */
      }
    "))
  ),
  
  titlePanel("Shiny DataTable with Compact Pagination"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      # Add some filters in the sidebar
      selectInput("city_filter", "City", 
                  choices = c("All", unique(data$City)), 
                  selected = "All"),
      sliderInput("age_filter", "Age Range", 
                  min = min(data$Age), max = max(data$Age), 
                  value = c(min(data$Age), max(data$Age)))
    ),
    
    mainPanel(
      DTOutput("table")  # DataTable in the main panel
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  filtered_data <- reactive({
    data %>%
      filter(if (input$city_filter != "All") City == input$city_filter else TRUE) %>%
      filter(Age >= input$age_filter[1] & Age <= input$age_filter[2])
  })
  
  # Render DataTable with compact pagination
  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 5,               # Limit the number of rows per page
        autoWidth = TRUE,             # Automatically adjust column width
        dom = 'frtip',                # Show filter (f), table (t), information (i), and pagination (p)
        pagingType = "simple"         # Use "simple" pagination (less space used)
      ),
      class = "compact",              # Apply compact class for reduced padding
      rownames = FALSE                # Hide row names
    )
  })
}

# Run the application
shinyApp(ui, server)
