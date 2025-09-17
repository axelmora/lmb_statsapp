ui_zone_std <- function(id) {
  ns <- NS(id)
    fluidRow(
      column(
        width = 12,
          id = ns("standings"),
            box(
              width = 12,
              "Zona Norte",
              DTOutput(ns("standing_norte"))
            ),
            box(
              width = 12,
              "Zona Sur",
              DTOutput(ns("standing_sur"))
            )
      )
    )
}

ui_lg_std <- function(id) {
  ns <- NS(id)
  fluidRow(column(12, DTOutput(ns("standing_lmb"))))
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
  fluidRow(column(12, DTOutput(ns("game_logs_table"))))
}

server_game_logs <- function(id, gl_data) {
  moduleServer(id, function(input, output, session) {
    output$game_logs_table <- renderDT({
    datatable(
      gl_data
      ,escape = FALSE
      ,rownames = FALSE
      ,options = list(
        dom = 'tip'
        ,pageLength = 30
        ,scrollX = FALSE
        ,columnDefs = list(list(targets = 0, width = '100x')
                           ,list(targets = c(2,4), width = '10px')
                           ,list(targets = c(2,4,8), className = 'dt-center')
                           ,list(targets = c(1,3,5,9), className = 'dt-left')
                           ,list(targets = c(1,3,5), width = '250px')
                           ,list(targets = c(6,7), width = '50px')
                           ,list(targets = 8, width = '50px')
                           ,list(targets = 9, width = '50px')
                           ,list(targets = "_all", className = 'dt-left')
        )
      )
    )
  })
  })
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
        opt_table_font(font = list(gt::google_font("Roboto"))) %>%
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
      datatable(
        data,
        rownames = FALSE,
        options = list(
          dom = 't'
        ,pageLength = 20
        ,columnDefs = list(list(targets = 0, width = '5px')
                          ,list(targets = 1, width = '180px')
                          ,list(targets = "_all", className = 'dt-left')
        )
        )
      )
    }
    output$standing_norte <- renderDT({
      render_standing(datasets$nte)
    })
    output$standing_sur <- renderDT({
      render_standing(datasets$sur)
    })
    output$standing_lmb <- renderDT({
      render_standing(datasets$lmb)
    })
  })
}
