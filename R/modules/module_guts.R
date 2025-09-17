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
        datatable(
          pf_data,
          escape = FALSE,
          rownames = FALSE,
          options = list(
            dom = 't'
            ,pageLength = 20
            ,scrollX = TRUE
          )
        )
      })
  
    output$woba_fip_dt <- renderDT({
      datatable(
        guts_data,
        rownames = FALSE,
        options = list(
          dom = 't'
          ,scrollX = TRUE
        )
      )%>%
        formatRound(columns = 2:11, digits = 3)
    })
  })
}