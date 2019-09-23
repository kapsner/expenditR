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
      DT::datatable(data = rv$community_data, editable = F, selection = 'single')
    })
  })
  
  observeEvent(input_re()[["moduleCommunity-community_delete"]], {
    if (!is.null(input_re()[["moduleCommunity-community_expenditures_rows_selected"]])){
      r <- input_re()[["moduleCommunity-community_expenditures_rows_selected"]]
      rv$community_data <- rv$community_data[-r,]
      jsonlite::write_json(rv$community_data, paste0(storedir, "community_data.JSON"))
    }
  })
  
  observeEvent(input_re()[["moduleCommunity-community_add"]], {
    req(rv$roommates)
    
    print("Add row")
    
    addr <- tryCatch({
      addrow <- addRow(buyer = input_re()[["moduleCommunity-community_buyer"]],
                       note = input_re()[["moduleCommunity-community_note"]],
                       date = as.character(input_re()[["moduleCommunity-community_date"]]),
                       money = input_re()[["moduleCommunity-community_money"]])
      
      if (nrow(rv$community_data) > 0){
      
        tempdat <- rv$community_data
        data.table::setkey(data.table::setDT(tempdat), "Buyer", "Note", "Date", "Money")
        data.table::setkey(data.table::setDT(addrow), "Buyer", "Note", "Date", "Money")
        
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
        rv$community_data <- rbind(addRow(buyer = input_re()[["moduleCommunity-community_buyer"]],
                                          note = input_re()[["moduleCommunity-community_note"]],
                                          date = as.character(input_re()[["moduleCommunity-community_date"]]),
                                          money = input_re()[["moduleCommunity-community_money"]]),
                                   rv$community_data[,("exists"):=NULL])
        jsonlite::write_json(rv$community_data, paste0(storedir, "community_data.JSON"))
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
    req(rv$community_data)
    
    if (nrow(rv$community_data)>0){
      for (n in names(rv$roommates)){
        if  (n %in% unique(rv$community_data[,get("Buyer")])){
          rv$summary[[n]] <- round(sum(as.numeric(rv$community_data[get("Buyer")==n,get("Money")])), 2)
        }
        
        s_dat <- t(data.table::as.data.table(rv$summary))
        s_tab <- data.table::data.table(
          cbind(
            rownames(s_dat),
            s_dat
          )
        )
        colnames(s_tab) <- c("Buyer", "Expenditures")
        # calc setdiff of names
        sd <- setdiff(names(rv$roommates), s_tab[,get("Buyer")])
        if (length(sd) > 0){
          for (m in sd){
            s_tab <- rbind(s_tab, cbind(m, 0), use.names = F)
          }
        }
        rv$summary_table <- s_tab
        
        c_tab <- data.table::data.table(
          cbind("Total",
                round(sum(as.numeric(rv$summary_table[,get("Expenditures")])), 2)
          ))
        colnames(c_tab) <- c(" ", "Sum")
        s <- round(as.numeric(c_tab[1,2])/length(rv$roommates), 2)
        c_tab <- rbind(c_tab, cbind("Sum/roommate", s), use.names = F)
        rv$community_total <- c_tab
        
        
        d_tab <- data.table::data.table()
        for (n in names(rv$roommates)){
          d <- round(as.numeric(c_tab[2,2]) - as.numeric(s_tab[get("Buyer")==n,get("Expenditures")]), 2)
          d_tab <- rbind(d_tab, cbind(n, d), use.names = F)
        }
        colnames(d_tab) <- c(" ", "Difference")
        rv$community_difference <- d_tab
      }
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
    column(8,
    fluidRow(
      box(
        title = "Community expenditures",
        DT::dataTableOutput(ns("community_expenditures")),
        width = 12
      )
    )),
    column(4,
           fluidRow(
             box(
               title = "Delete selected row",
               actionButton(ns("community_delete"), "Delete"),
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
  )
}
