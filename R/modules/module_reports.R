# ======================================================
# Report Generation Module with RMarkdown
# ======================================================

# ---- UI ----
ui_reports <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 4,
      box(
        title = "Report Settings",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        selectInput(ns("data_source"), "Select Data Source:",
                    choices = c("Player Stats", "Team Stats")),
        uiOutput(ns("field_selector")),
        uiOutput(ns("filters_ui")),
        selectInput(ns("sort_field"), "Sort By:", choices = NULL),
        radioButtons(ns("sort_order"), "Order:",
                     choices = c("Ascending" = "asc", "Descending" = "desc"),
                     inline = TRUE),
        numericInput(ns("num_records"), "Number of Records:", value = 20, min = 5, max = 500, step = 5),
        checkboxInput(ns("include_chart"), "Include Chart", TRUE),
        downloadButton(ns("download_pdf"), "Download PDF", class = "btn-success")
      )
    ),
    column(
      width = 8,
      box(
        title = "Report Preview",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        reactableOutput(ns("preview_table")),
        plotOutput(ns("chart_preview"))
      )
    )
  )
}

# ---- SERVER ----
server_reports <- function(id, player_data, team_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive dataset
    dataset <- reactive({
      if (input$data_source == "Player Stats") player_data else team_data
    })
    
    # Fields based on dataset
    output$field_selector <- renderUI({
      req(dataset())
      selectInput(ns("fields"), "Select Fields:", 
                  choices = names(dataset()), 
                  selected = names(dataset())[1:5], 
                  multiple = TRUE)
    })
    
    # Update sort field dynamically
    observe({
      req(dataset())
      updateSelectInput(session, "sort_field",
                        choices = names(dataset()),
                        selected = names(dataset())[1])
    })
    
    # Filters
    output$filters_ui <- renderUI({
      req(dataset())
      cols <- names(dataset())
      years <- if ("Year" %in% cols) sort(unique(dataset()$Year)) else NULL
      teams <- if ("Team" %in% cols) sort(unique(dataset()$Team)) else NULL
      players <- if ("Player" %in% cols) sort(unique(dataset()$Player)) else NULL
      
      tagList(
        if (!is.null(years)) selectInput(ns("filter_year"), "Filter by Year:", choices = c("All", years)),
        if (!is.null(teams)) selectInput(ns("filter_team"), "Filter by Team:", choices = c("All", teams)),
        if (!is.null(players)) selectInput(ns("filter_player"), "Filter by Player:", choices = c("All", players))
      )
    })
    
    # Filtered + sorted data
    filtered_data <- reactive({
      df <- dataset()
      req(df, input$fields)
      
      if (!is.null(input$filter_year) && input$filter_year != "All") {
        df <- df %>% filter(Year == input$filter_year)
      }
      if (!is.null(input$filter_team) && input$filter_team != "All") {
        df <- df %>% filter(Team == input$filter_team)
      }
      if (!is.null(input$filter_player) && input$filter_player != "All") {
        df <- df %>% filter(Player == input$filter_player)
      }
      
      # Select chosen fields
      df <- df[, input$fields, drop = FALSE]
      
      # Sorting
      if (!is.null(input$sort_field) && input$sort_field %in% colnames(df)) {
        if (input$sort_order == "asc") {
          df <- df %>% arrange(.data[[input$sort_field]])
        } else {
          df <- df %>% arrange(desc(.data[[input$sort_field]]))
        }
      }
      
      # Limit number of records
      df <- head(df, input$num_records)
      df
    })
    
    # Preview table
    output$preview_table <- renderReactable({
      df <- filtered_data()
      reactable(df,
        sortable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        compact = TRUE,
        height = 760,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        resizable = TRUE
      )
    })
    
    # Preview chart
    output$chart_preview <- renderPlot({
      req(input$include_chart)
      df <- filtered_data()
      num_cols <- df %>% select(where(is.numeric))
      req(ncol(num_cols) >= 2)
      
      ggplot(df, aes(x = num_cols[[1]], y = num_cols[[2]])) +
        geom_point(color = "darkred", size = 3) +
        theme_minimal(base_size = 14) +
        labs(title = "Report Visualization",
             subtitle = paste("Sorted by", input$sort_field),
             x = names(num_cols)[1], y = names(num_cols)[2])
    })
    
    # PDF download via RMarkdown
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("custom_report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "report_template.Rmd")
        
        # copy Rmd template
        file.copy("R/templates/report_template.Rmd", tempReport, overwrite = TRUE)
        
    params <- list(
      df = filtered_data(),
      include_chart = input$include_chart,
      watermark = paste("LMB Analytics | Axel Mora |", Sys.Date()),
      source = input$data_source,
      fields = input$fields,
      year = if (!is.null(input$filter_year) && input$filter_year != "All") input$filter_year else NULL,
      team = if (!is.null(input$filter_team) && input$filter_team != "All") input$filter_team else NULL,
      player = if (!is.null(input$filter_player) && input$filter_player != "All") input$filter_player else NULL,
      sort_field = input$sort_field,
      sort_order = input$sort_order
    )
        
        rmarkdown::render(
          tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}