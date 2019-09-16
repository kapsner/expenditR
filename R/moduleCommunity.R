#' @title moduleCommunityServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleCommunityServer
moduleCommunityServer <- function(input, output, session, rv, input_re){
  
  observe({
    req(rv$community_data)
    
    output$community_expenditures <- DT::renderDataTable({
      DT::datatable(data = rv$community_data, editable = TRUE)
    })
  })
  
  observeEvent(input_re()[["moduleCommunity-community_add"]], {
    req(rv$roommates)
    
    print("Add row")
    
    if (input_re()[["moduleCommunity-community_money"]] > 0){
      
      if (nrow(rv$community_data) == 0){
        addr <- TRUE
      } else {
        addrow <- data.table::data.table(
          cbind(
            "Buyer" = input_re()[["moduleCommunity-community_buyer"]],
            "Note" = input_re()[["moduleCommunity-community_note"]],
            "Date" = as.character(input_re()[["moduleCommunity-community_date"]]),
            "Money" = input_re()[["moduleCommunity-community_money"]]
          ))
        
        tempdat <- rv$community_data
        data.table::setkey(data.table::setDT(tempdat), "Buyer", "Note", "Date", "Money")
        data.table::setkey(data.table::setDT(addrow), "Buyer", "Note", "Date", "Money")
        
        tempdat[, ("exists") := FALSE][addrow, ("exists") := TRUE]
        
        addr <- ifelse(any(tempdat[,get("exists")]), FALSE, TRUE)
        
        tempdat <- NULL
        addrow <- NULL
      }
      
      if (addr){
        rv$community_data <- rbind(rv$community_data[,("exists"):=NULL],
                                   cbind(
                                     "Buyer" = input_re()[["moduleCommunity-community_buyer"]],
                                     "Note" = input_re()[["moduleCommunity-community_note"]],
                                     "Date" = as.character(input_re()[["moduleCommunity-community_date"]]),
                                     "Money" = input_re()[["moduleCommunity-community_money"]]
                                   ))
        jsonlite::write_json(rv$community_data, paste0(storedir, "community_data.JSON"))
      } else {
        showModal(modalDialog(
          title = "Error",
          "This row already exists.",
          footer = modalButton("OK")
        ))
      }
    } else {
      showModal(modalDialog(
        title = "Error",
        "You cannot enter an amount of money of zero.",
        footer = modalButton("OK")
      ))
    }
  })
  
  observe({
    for (n in names(rv$roommates)){
      if  (n %in% unique(rv$community_data[,get("Buyer")])){
        rv$summary[[n]] <- sum(as.numeric(rv$community_data[get("Buyer")==n,get("Money")]))
      }
      
      s_dat <- t(data.table::as.data.table(rv$summary))
      s_tab <- data.table::data.table(
        cbind(
          rownames(s_dat),
          s_dat
        )
      )
      colnames(s_tab) <- c(" ", "Expenditures")
      rv$summary_table <- s_tab
      
      rv$community_total <- sum(as.numeric(rv$summary_table[,get("Expenditures")]))
    }
  })
}


#' @title moduleCommunityUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleCommunityUI
moduleCommunityUI <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Community expenditures",
        DT::dataTableOutput(ns("community_expenditures")),
        width = 12
      )
    ),
    fluidRow(
      box(
        title = "Add community expenditure",
        fluidRow(
          column(6,
                 selectInput(ns("community_buyer"), label = "Buyer", selected = NULL, choices = list(), multiple = F)
          ),
          column(6,
                 textInput(ns("community_note"), label = "Note", placeholder = "Short description of the expenditure.")
          )
        ),
        fluidRow(
          column(6,
                 dateInput(ns("community_date"), label = "Date of the expenditure", weekstart = 1)
          ),
          column(6,
                 numericInput(ns("community_money"), label = "Amount of money", min = 0, step = 0.01, value = 0)
          )
        ),
        actionButton(ns("community_add"), "Add expenditure"),
        width = 12
      )
    )
  )
}
