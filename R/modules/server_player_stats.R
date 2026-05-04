# ======================================================
# Player Stats - Server
# ======================================================
server_player_stats <- function(id, datasets, filters) {
  moduleServer(id, function(input, output, session) {
    
    # --- Generic filter helper ---
    filter_data <- function(df, sf, tf, qf) {
      # Season filter
      if (!is.null(sf) && length(sf) == 1 && sf != "All") {
        if (is.numeric(df$Year)) {
          df <- df[df$Year == as.numeric(sf), ]
        } else {
          df <- df[df$Year == as.character(sf), ]
        }
      }
      # Team filter
      if (!is.null(tf) && length(tf) >= 1 && !("All" %in% tf)) {
        df <- df[df$Team %in% tf, ]
      }
      # Qualified filter
      if (!is.null(qf) && length(qf) == 1 && qf != "All") {
        if ("Qualified" %in% names(df)) {
          df <- df[df$Qualified == qf, ]
        }
      }
      df
    }
    
    # --- Generic reactable helper ---
    render_stats_table <- function(data, sort_col = NULL) {
      # Build defaultSorted if the column exists
      # reactable expects a named list: list(colName = "desc") or a character vector
      default_sorted <- if (!is.null(sort_col) && sort_col %in% names(data)) {
        setNames(list("desc"), sort_col)
      } else {
        list()
      }
      
      reactable(
        data,
        sortable       = TRUE,
        highlight      = TRUE,
        striped        = TRUE,
        compact        = TRUE,
        height         = 760,
        defaultPageSize = 25,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(25, 50, 100, 200, 500),
        resizable      = TRUE,
        defaultSorted  = default_sorted,
        defaultColDef  = colDef(
          minWidth = 60,
          footer = function(values, name) {
            htmltools::div(name, style = list(fontWeight = 600))
          }
        ),
        columns = list(
          Name = colDef(sticky = "left",
                        style = list(borderRight = "1px solid #eee"),
                        headerStyle = list(borderRight = "1px solid #eee"),
                        width = 200),
          Team = colDef(sticky = "left",
                        style = list(borderRight = "1px solid #eee"),
                        headerStyle = list(borderRight = "1px solid #eee")),
          PlayerId = colDef(show = FALSE),
          dRPW = colDef(show = FALSE),
          rpL = colDef(show = FALSE),
          Qualified = colDef(show = FALSE), 
          cFIP = colDef(show = FALSE),
          PosAdj = colDef(show = FALSE),
          Replacement = colDef(show = FALSE),
          DRS_proxy = colDef(show = FALSE)
        )
      )
    }
    
    # --- Loop through datasets and build outputs dynamically ---
    lapply(names(datasets), function(name) {
      output_id <- paste0("player_", name, "_table")
      
      filtered_data <- reactive({
        filter_data(
          df = datasets[[name]],
          sf = filters$season(),
          tf = filters$team_player(),
          qf = filters$qualified_filter()
        )
      })
      
      # Use OPS as default sort column for hitting; NULL for others
      sort_col <- if (name == "hit") "OPS" else NULL
      
      output[[output_id]] <- renderReactable({
        render_stats_table(filtered_data(), sort_col = sort_col)
      })
    })
    
  })
}