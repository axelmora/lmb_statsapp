# ======================================================
# Player Stats - Server
# ======================================================
server_player_stats <- function(id, datasets, season_filter) {
  moduleServer(id, function(input, output, session) {

    # --- Generic filter helper ---
    filter_by_season <- function(df, sf) {
      if (!is.null(sf) && length(sf) == 1 && sf != "All") {
        if (is.numeric(df$Year)) {
          df <- df[df$Year == as.numeric(sf), ]
        } else {
          df <- df[df$Year == as.character(sf), ]
        }
      }
      df
    }

    # --- Generic reactable helper ---
    render_stats_table <- function(data) {
      reactable(
        data,
        sortable = TRUE,
        highlight = TRUE,
        striped = TRUE,
        compact = TRUE,
        height = 760,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        resizable = TRUE,
        defaultColDef = colDef(
          footer = function(values, name) {
            htmltools::div(name, style = list(fontWeight = 600))
          }
        ),
        columns = list(
          Name = colDef(sticky = "left",
                        style = list(borderRight = "1px solid #eee"),
                        headerStyle = list(borderRight = "1px solid #eee"),
                        width = 250),
          Year = colDef(sticky = "left",
                        style = list(borderRight = "1px solid #eee"),
                        headerStyle = list(borderRight = "1px solid #eee"))
        ),
        theme = reactableTheme(
          headerStyle = list(background = "#f8f9fa")
        )
      )
    }
    
    # --- Loop through datasets and build outputs dynamically ---
    lapply(names(datasets), function(name) {
      output_id <- paste0("player_", name, "_table")
      
      filtered_data <- reactive({
        filter_by_season(datasets[[name]], season_filter())
      })
      
      output[[output_id]] <- renderReactable({
        render_stats_table(filtered_data())
      })
    })
    
    
  })
}

