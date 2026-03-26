ui_guts <- function(id) {
  ns <- NS(id)
  fluidRow( 
    box(
      width = 12,
      title = "Guts",
      reactableOutput(ns("woba_fip_dt"))
    ),
    box(
      width = 12,
      title = "Park Factors",
      reactableOutput(ns("pf_dt"))
    )

  )
}

server_guts <- function(id, guts_data, pf_data) {
  moduleServer(id, function(input, output, session) {
        output$pf_dt <- renderReactable({
        reactable(
          pf_data,
          searchable = FALSE,
          sortable = TRUE,
          striped = TRUE,
          compact = TRUE,
          defaultPageSize = 20,
          columns = reactable_numeric_columns(pf_data, digits = 3),
          defaultColDef = reactable_lmb_coldef(minWidth = 85, maxWidth = 145),
          theme = reactable_lmb_theme()
        )
      })
  
    output$woba_fip_dt <- renderReactable({
      reactable(
        guts_data,
        searchable = FALSE,
        sortable = TRUE,
        striped = TRUE,
        compact = TRUE,
        columns = reactable_numeric_columns(guts_data, digits = 3),
        defaultColDef = reactable_lmb_coldef(minWidth = 85, maxWidth = 145),
        theme = reactable_lmb_theme()
      )
    })
  })
}
