ui_guts <- function(id) {
  ns <- NS(id)
  fluidRow( 
    box(
      width = 12,
      title = "Guts",
      DTOutput(ns("woba_fip_dt"))
    ),
    box(
      width = 12,
      title = "Park Factors",
      DTOutput(ns("pf_dt"))
    )

  )
}

server_guts <- function(id, guts_data, pf_data) {
  moduleServer(id, function(input, output, session) {
        output$pf_dt <- renderDT({
        dt <- datatable(
          pf_data,
          escape = FALSE,
          rownames = FALSE,
          options = list(
            dom = 't'
            ,pageLength = 20
            ,scrollX = TRUE
          )
        )
        style_dt_numeric_columns(dt, pf_data)
      })
  
    output$woba_fip_dt <- renderDT({
      dt <- datatable(
        guts_data,
        rownames = FALSE,
        options = list(
          dom = 't'
          ,scrollX = TRUE
        )
      ) %>%
        formatRound(columns = 2:11, digits = 3)
      style_dt_numeric_columns(dt, guts_data)
    })
  })
}
