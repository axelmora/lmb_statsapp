ui_att <- function(id) {
  ns <- NS(id)
  fluidRow( 
    valueBox(
      value = textOutput(ns("lmb_att_avg")),
      subtitle = 'Avg Attendance',
      color = "primary"
    ),
    valueBox(
      value = textOutput(ns("lmb_cap_pct")),
      subtitle = "Avg Attendance Pct",
      color = "primary"
    ),
    valueBox(
      value = textOutput(ns("lmb_max_att")),
      subtitle = "Max Attendance",
      color = "primary"
    ),    
    reactableOutput(ns("game_att_table"))
  )
}

server_att <- function(id, att_data) {
  moduleServer(id, function(input, output, session) {
    output$game_att_table <- renderReactable({
      reactable(
        att_data,
        searchable = FALSE,
        sortable = TRUE,
        striped = TRUE,
        compact = TRUE,
        defaultPageSize = 20,
        columns = reactable_numeric_columns(att_data, digits = 3),
        defaultColDef = reactable_lmb_coldef(minWidth = 85, maxWidth = 150),
        theme = reactable_lmb_theme()
      )
    })
  
  output$lmb_att_avg <- renderText({
    round(sum(att_data$`Total Home Attendance`)/
            sum(att_data$`Home Openings`),1)
  })
  
  output$lmb_cap_pct <- renderText({
    round(((sum(att_data$`Total Home Attendance`)/
              sum(att_data$`Home Openings`))*100)/
            mean(att_data$Capacity),1)
  })
  
  output$lmb_max_att <- renderText({
    max(att_data$`High Home Attendance`)
  })
    
  })
}
