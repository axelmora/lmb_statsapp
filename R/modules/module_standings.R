ui_zone_std <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      id = ns("standings"),
      box(
        width = 12,
        "Zona Norte",
        reactableOutput(ns("standing_norte"))
      ),
      box(
        width = 12,
        "Zona Sur",
        reactableOutput(ns("standing_sur"))
      )
    )
  )
}

ui_lg_std <- function(id) {
  ns <- NS(id)
  fluidRow(column(12, reactableOutput(ns("standing_lmb"))))
}

ui_h2h <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      id = ns("tabsetpanel3"),
      box(
        width = 12,
        "Zona Norte",
        gt_output(ns("h2h_nte_matrix"))
      ),
      box(
        width = 12,
        "Zona Sur",
        gt_output(ns("h2h_sur_matrix"))
      )
    )
  )
}


ui_std_evol <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      width = 12,
      tabsetPanel(
        id = ns("tabsetpanel8"),
        tabPanel(
          "Zona Norte",
          plotlyOutput(ns("rank_plot_nte"), height = "500px")
        ),
        tabPanel(
          "Zona Sur",
          plotlyOutput(ns("rank_plot_sur"), height = "500px")
        )
      )
    )
  )
}

server_h2h <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {

    render_matrix <- function(data) {
      data %>%
        gt(rowname_col = "Team") %>%
        tab_header(
          title = "Win-Loss Matrix",
          subtitle = "Head-to-head records by team"
        ) %>%
        opt_align_table_header(align = "left") %>%
        opt_table_font(font = list(gt::google_font("IBM Plex Sans Condensed"))) %>%
        cols_align(align = "center", columns = everything()) %>%
        tab_style(
          style = cell_fill(color = "#f9f9f9"),
          locations = cells_body()
        )
    }
    output$h2h_nte_matrix <- render_gt({
      render_matrix(datasets$nte)
    })
    output$h2h_sur_matrix <- render_gt({
      render_matrix(datasets$sur)
    })
  })
}

server_std <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {

    render_standing <- function(data) {
      reactable(
        data,
        searchable = FALSE,
        sortable = TRUE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        defaultPageSize = 20,
        defaultColDef = reactable_lmb_coldef(minWidth = 80, maxWidth = 140),
        columns = reactable_numeric_columns(data, digits = 3),
        theme = reactable_lmb_theme()
      )
    }
    output$standing_norte <- renderReactable({
      render_standing(datasets$nte)
    })
    output$standing_sur <- renderReactable({
      render_standing(datasets$sur)
    })
    output$standing_lmb <- renderReactable({
      render_standing(datasets$lmb)
    })
  })
}

server_std_evol <- function(id, datasets) {
  moduleServer(id, function(input, output, session) {

    output$rank_plot_nte <- renderPlotly({
      p <- ggplot(datasets$nte, aes(x = date, y = rank, color = team)) +
        geom_line(size = 0.8) +
        scale_y_reverse(breaks = 1:6) +
        labs(
          title = "LMB Norte Standings Over Time",
          x = "Date",
          y = "Rank",
          color = "Team"
        ) +
        theme_minimal() +
        scale_color_manual(values = datasets$colors_nte)

      ggplotly(p, tooltip = c("x", "y", "color"))
    })

    output$rank_plot_sur <- renderPlotly({
      p <- ggplot(datasets$sur, aes(x = date, y = rank, color = team)) +
        geom_line(size = 0.8) +
        scale_y_reverse(breaks = 1:6) +
        labs(
          title = "LMB Sur Standings Over Time",
          x = "Date",
          y = "Rank",
          color = "Team"
        ) +
        theme_minimal() +
        scale_color_manual(values = datasets$colors_sur)

      ggplotly(p, tooltip = c("x", "y", "color"))
    })

  })
}
