#' @title modulePrivateServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# modulePrivateServer
modulePrivateServer <- function(input, output, session, rv, input_re){
  
  observe({
    req(rv$private_data)
    
    output$private_expenditures <- DT::renderDataTable({
      DT::datatable(data = rv$private_data, editable = F, selection = 'single')
    })
  })
  
  observeEvent(input_re()[["modulePrivate-private_delete"]], {
    if (!is.null(input_re()[["modulePrivate-private_expenditures_rows_selected"]])){
      r <- input_re()[["modulePrivate-private_expenditures_rows_selected"]]
      rv$private_data <- rv$private_data[-r,]
      jsonlite::write_json(rv$private_data, paste0(storedir, "private_data.JSON"))
    }
  })
  
  observeEvent(input_re()[["modulePrivate-private_add"]], {
    req(rv$roommates)
    
    print("Add row")
    
    addr <- tryCatch({
      addrow <- addRowPr(buyer = input_re()[["modulePrivate-private_buyer"]],
                       debtor = input_re()[["modulePrivate-private_debtor"]],
                       note = input_re()[["modulePrivate-private_note"]],
                       date = as.character(input_re()[["modulePrivate-private_date"]]),
                       money = input_re()[["modulePrivate-private_money"]])
      
      if (nrow(rv$private_data) > 0){
      
        tempdat <- rv$private_data
        data.table::setkey(data.table::setDT(tempdat), "Buyer", "Debtor", "Note", "Date", "Money")
        data.table::setkey(data.table::setDT(addrow), "Buyer", "Debtor", "Note", "Date", "Money")
        
        tempdat[, ("exists") := FALSE][addrow, ("exists") := TRUE]
        
        addit <- ifelse(any(tempdat[,get("exists")]), FALSE, TRUE)
        
        tempdat <- NULL
        addrow <- NULL
      } else {
        addit <- TRUE
      }
      
      addit
      
    }, error = function(e){
      print(e)
      addit <- NULL
      
    }, finally = function(f){
      
      return(addit)
    })
    
    
    if (is.null(addr)){
        showModal(modalDialog(
          title = "Error",
          "Please enter a valid amount of money or a valid description.",
          footer = modalButton("OK")
        ))
    } else {
      if (addr){
        rv$private_data <- rbind(addRowPr(buyer = input_re()[["modulePrivate-private_buyer"]],
                                        debtor = input_re()[["modulePrivate-private_debtor"]],
                                          note = input_re()[["modulePrivate-private_note"]],
                                          date = as.character(input_re()[["modulePrivate-private_date"]]),
                                          money = input_re()[["modulePrivate-private_money"]]),
                                   rv$private_data[,("exists"):=NULL])
        jsonlite::write_json(rv$private_data, paste0(storedir, "private_data.JSON"))
      } else {
        showModal(modalDialog(
          title = "Error",
          "This row already exists.",
          footer = modalButton("OK")
        ))
      }
    }
  })
  
  observe({
    req(rv$private_data)

    if (nrow(rv$private_data)>0){
      outdat <- data.table::data.table()
      # iterate over buyers
      for (n in unique(rv$private_data[,get("Buyer")])){
        tmptb <- rv$private_data[get("Buyer")==n,]
        # iterate over debtors
        for (d in unique(tmptb[,get("Debtor")])){
          rv$private_summary[[n]][[d]] <- round(sum(as.numeric(tmptb[get("Debtor")==d,get("Money")])), 2)
        }
      }
      for (m in names(rv$private_summary)){
        for (o in names(rv$private_summary[[m]])){
          outdat <- rbind(outdat, cbind(m, o, rv$private_summary[[m]][[o]]))
        }
      }
      colnames(outdat) <- c("Creditor", "Debtor", "Expenditures")
      rv$private_summary_table <- outdat
    }
    
    
  })
}


#' @title modulePrivateUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# modulePrivateUI
modulePrivateUI <- function(id){
  ns <- NS(id)
  
  tagList(
    column(8,
    fluidRow(
      box(
        title = "Private expenditures",
        DT::dataTableOutput(ns("private_expenditures")),
        width = 12
      )
    )),
    column(4,
           fluidRow(
             box(
               title = "Delete selected row",
               actionButton(ns("private_delete"), "Delete"),
               width = 12
             )
           ),
           fluidRow(
             box(
               title = "Add private expenditure",
               fluidRow(
                 column(6,
                        selectInput(ns("private_buyer"), label = "Creditor", selected = NULL, choices = list(), multiple = F)
                 ),
                 column(6,
                        textInput(ns("private_note"), label = "Note", placeholder = "Short description of the expenditure.")
                 )
               ),
               fluidRow(
                 column(6,
                        selectInput(ns("private_debtor"), label = "Debtor", selected = NULL, choices = list(), multiple = F)
                 ),
                 column(6,
                        numericInput(ns("private_money"), label = "Amount of money", min = 0, step = 0.01, value = 0)
                 )
               ),
               fluidRow(
                 column(6,
                        dateInput(ns("private_date"), label = "Date of the expenditure", weekstart = 1)
                 ),
                 column(6,
                        actionButton(ns("private_add"), "Add expenditure")
                 )
               ),
               width = 12
             )
           )
    )
  )
}
