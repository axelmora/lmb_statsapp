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
    DTOutput(ns("game_att_table"))
  )
}

server_att <- function(id, att_data) {
  moduleServer(id, function(input, output, session) {
    output$game_att_table <- renderDT({
    datatable(
      att_data
      ,escape = FALSE
      ,rownames = FALSE,
      options = list(
        dom = 't'
        ,pageLength = 20
        ,columnDefs = list(list(targets = 0, width = '150x')
                           ,list(targets = 1, width = '200px')
                           ,list(targets = c(2:7), width = '5px')
                           ,list(targets = "_all", className = 'dt-left')
        )
        ,scrollX = FALSE
      )
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