# ======================================================
# Report Generation Module with Quarto
# ======================================================

# ---- UI ----
ui_reports <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      tabsetPanel(
        id = "tabsetpanel4",
        
        tabPanel(
          "Player Stats",
          fluidRow(   # <-- wrap in a fluidRow so columns align
            column(
              width = 4,   # wider so controls fit nicely
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
                numericInput(ns("num_records"), "Number of Records:",
                             value = 20, min = 5, max = 500, step = 5),
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
        ),
        
        tabPanel(
          "Hitting vs Pitching",
          fluidRow(
            column(
              width = 4,
              box(
                title = "Chart Settings",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                selectInput(ns("x_var"), "X Axis:", choices = NULL),
                selectInput(ns("y_var"), "Y Axis:", choices = NULL),
                selectInput(ns("color_var"), "Color By:", 
                            choices = c("Winning % (wPct)" = "wPct")),
                selectInput(ns("year_filter"), "Select Year:",
                            choices = 2024:2025, selected = 2025),
                uiOutput(ns("team_filter")),
                actionButton(ns("generate_chart"), "Generate Chart", class = "btn-primary"),
                br(), br(),
                downloadButton(ns("download_chart"), "Download Chart", class = "btn-success")
              )
            ),
            column(
              width = 8,
              box(
                title = "Dynamic Chart",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                plotOutput(ns("dynamic_chart"), height = "600px")
              )
            )
          )
        ),
        tabPanel("")
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
        df <- if (input$sort_order == "asc") {
          df %>% arrange(.data[[input$sort_field]])
        } else {
          df %>% arrange(desc(.data[[input$sort_field]]))
        }
      }
      
      # Limit number of records
      head(df, input$num_records)
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
    
    output$download_pdf <- downloadHandler(
        filename = function() {
          paste0("custom_report_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          df_to_render <- filtered_data()

          # Temporary working directory
          temp_dir <- file.path(tempdir(), paste0("quarto_render_", format(Sys.time(), "%Y%m%d_%H%M%S")))
          dir.create(temp_dir, showWarnings = FALSE)

          # Copy Quarto template
          temp_report <- file.path(temp_dir, "report_template.qmd")
          file.copy("R/templates/report_template.qmd", temp_report, overwrite = TRUE)

          # Chart path (only if user selected chart)
          chart_path <- NULL
          if (isTRUE(input$include_chart) && !is.null(df_to_render)) {
            chart_path <- file.path(temp_dir, "chart.png")

            # Example chart – replace `x_col` and `y_col` with real field names
            num_cols <- df_to_render %>% dplyr::select(where(is.numeric))
            if (ncol(num_cols) >= 2) {
              p <- ggplot(df_to_render, aes(x = num_cols[[1]], y = num_cols[[2]])) +
                geom_point(color = "#BA0C2F", size = 3) +
                theme_minimal(base_size = 14) +
                labs(
                  title = "Data Visualization",
                  x = names(num_cols)[1],
                  y = names(num_cols)[2],
                  caption = paste("Source:", input$data_source)
                )
              ggsave(chart_path, plot = p, width = 6, height = 4, dpi = 300)
            }
          }

          # Render to PDF
          temp_pdf_name <- paste0("report_", Sys.Date(), ".pdf")
          quarto::quarto_render(
            input = temp_report,
            output_file = temp_pdf_name,
            output_format = "pdf",
            execute_params = list(
              df = df_to_render,
              include_chart = input$include_chart,
              chart_path = chart_path,
              watermark = paste("LMB Analytics | Axel Mora |", Sys.Date()),
              source = input$data_source,
              fields = input$fields,
              year = if (!is.null(input$filter_year) && input$filter_year != "All") input$filter_year else NULL,
              team = if (!is.null(input$filter_team) && input$filter_team != "All") input$filter_team else NULL,
              player = if (!is.null(input$filter_player) && input$filter_player != "All") input$filter_player else NULL,
              sort_field = input$sort_field,
              sort_order = input$sort_order
            ),
            quiet = FALSE
          )

          # Move PDF to Shiny’s download location
          generated_pdf <- file.path(temp_dir, temp_pdf_name)
          file.copy(generated_pdf, file, overwrite = TRUE)

          # Clean up
          unlink(temp_dir, recursive = TRUE)
        }
      )  
    
    # Load cached data
    team_hitting_25 <- readRDS("data/cache/team_hitting_2025.rds")
    team_pitching_25 <- readRDS("data/cache/team_pitching_2025.rds")
    teams <- readRDS("data/cache/teams.rds")
    stan_lmb <- readRDS("data/cache/stan_lmb.rds")
    
    # Filter by selected year
    team_data <- reactive({req(input$year_filter, input$x_var, input$y_var)
      
      hitting <- team_hitting_25 %>% filter(Year == input$year_filter)
      pitching <- team_pitching_25 %>% filter(Year == input$year_filter)

      req(input$x_var %in% names(hitting), input$y_var %in% names(pitching))
      
      team_analyze <- hitting[,c("Team", input$x_var)] %>% 
        inner_join(pitching[,c("Team", input$y_var)], by="Team") %>% 
        left_join(stan_lmb[,c("Team","wPct")], by=c("Team"="Team")) %>% 
        mutate(wPct = as.numeric(wPct)) %>% 
        left_join(teams[,c("team_abbreviation","team_full_name")], by=c("Team"="team_full_name")) %>%
        select(c(5,4,3,2)) %>%
        rename("Team" = "team_abbreviation")
      
      if (!is.null(input$team_filter) && length(input$team_filter) > 0) {
        team_analyze <- team_analyze %>% filter(Team %in% input$team_filter)
      }
      
      team_analyze

      print(team_analyze)
    })

    # Populate variable selectors
    observe({
      hitting <- team_hitting_25 
      pitching <- team_pitching_25 
      vars_hit <- setdiff(names(hitting), c("Year", "Team"))
      vars_pit <- setdiff(names(pitching), c("Year", "Team"))
      updateSelectInput(session, "x_var", choices = vars_hit, selected = "OPS")
      updateSelectInput(session, "y_var", choices = vars_pit, selected = "ERA")
    })
      
    # Generate chart on demand
    chart_obj <- eventReactive(input$generate_chart, {
      df <- team_data()
      req(input$x_var, input$y_var)
      
      mean_x <- mean(df[[input$x_var]], na.rm=TRUE)
      mean_y <- mean(df[[input$y_var]], na.rm=TRUE)
      
      ggplot(df, aes_string(x=input$x_var, y=input$y_var, label="Team", color=input$color_var)) +
        geom_point(size=4) +
        geom_text(nudge_y = 0.002, check_overlap = TRUE) +
        geom_vline(xintercept = mean_x, linetype="dashed", color="gray50") +
        geom_hline(yintercept = mean_y, linetype="dashed", color="gray50") +
        scale_color_gradient(low="red", high="darkgreen") +
        labs(
          title = paste("LMB Teams:", input$y_var, "vs", input$x_var),
          subtitle = "Dashed lines = league averages",
          x = input$x_var,
          y = input$y_var,
          color = input$color_var
        ) +
        theme_minimal()
    })
    
    output$dynamic_chart <- renderPlot({
      chart_obj()
    })
    
    # Download chart as PNG
    output$download_chart <- downloadHandler(
      filename = function() {
        paste0("LMB_chart_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(file, plot = chart_obj(), width = 10, height = 7, dpi = 300)
      }
    )
    
    
    
    
  })
}
