#' @title moduleSummaryServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleSummaryServer
moduleSummaryServer <- function(input, output, session, rv, input_re){
  observe({
    req(rv$summary)
    
    print(rv$summary)
    
    if (length(rv$summary) > 0){
      output$summary_table <- renderTable({
        rv$summary_table
      })
    }
  })
  
  observe({
    req(rv$community_total)
    
    output$summary_total <- renderTable({
      rv$community_total
    })
  })
  
  observe({
    req(rv$community_difference)
    
    output$summary_differences <- renderTable({
      rv$community_difference
    })
  })
}


#' @title moduleSummaryUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleSummaryUI
moduleSummaryUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Summary",
        tableOutput(ns("summary_table")),
        width = 6
      ),
      box(
        title = "Total Community Expenditures",
        tableOutput(ns("summary_total")),
        width = 6
      )
    ),
    fluidRow(
      box(
        title = "Community Differences",
        tableOutput(ns("summary_differences")),
        width = 6
      )
    )
  )
}
