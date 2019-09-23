shiny::shinyServer(function(input, output, session) {

    # define reactive values here
    rv <- shiny::reactiveValues(
        roommates = list(),
        storedir = storedir,
        no_mates = TRUE,
        no_community = TRUE,
        summary = list(),
        private_summary = list(),
        community_data = data.table::data.table(cbind(
            "Buyer" = character(0),
            "Note" = character(0),
            "Date" = character(0),
            "Money" = numeric(0)
        )),
        private_data = data.table::data.table(cbind(
            "Buyer" = character(0),
            "Debtor" = character(0),
            "Note" = character(0),
            "Date" = character(0),
            "Money" = numeric(0)
        ))
    )
    
    observe({
        if (length(rv$roommates) < 1){
            if (file.exists(paste0(storedir, "roommates.JSON"))){
                rv$roommates <- jsonlite::fromJSON(paste0(storedir, "roommates.JSON"))
            }
        }
        
        if (nrow(rv$community_data) < 1){
            if (file.exists(paste0(storedir, "community_data.JSON"))){
                rv$community_data <- data.table::as.data.table(
                    jsonlite::fromJSON(paste0(storedir, "community_data.JSON")))
            }
        }
        
        if (nrow(rv$private_data) < 1){
            if (file.exists(paste0(storedir, "private_data.JSON"))){
                rv$private_data <- data.table::as.data.table(
                    jsonlite::fromJSON(paste0(storedir, "private_data.JSON")))
            }
        }
    })

    # handle reset
    shiny::observeEvent(input$reset, {
        shinyjs::js$reset()
    })

    ######################
    ## Settings Tab
    ######################
    shiny::callModule(moduleSettingsServer, "moduleSettings", rv=rv, input_re=reactive({input}))
    
    observe({
        req(rv$roommates)
        
        if (length(rv$roommates) > 0){
            output$gotRoommates <- reactive({
                return(TRUE)
            })
            outputOptions(output, 'gotRoommates', suspendWhenHidden=FALSE)
            jsonlite::write_json(rv$roommates, paste0(storedir, "roommates.JSON"))
        }
    })
    
    ######################
    ## Community Expenditures Tab
    ######################
    shiny::callModule(moduleCommunityServer, "moduleCommunity", rv=rv, input_re=reactive({input}))
    
    observe({
        req(rv$no_mates)
        
        if (length(rv$roommates) > 0){
            output$menu <- shinydashboard::renderMenu({
                shinydashboard::sidebarMenu(
                    shinydashboard::menuItem("Community Expenditures", tabName = "community", icon = icon("table")),
                    shinydashboard::menuItem("Private Expenditures", tabName = "private", icon = icon("table"))
                )
            })
            rv$no_mates <- FALSE
        }
    })
    
    observe({
        req(rv$roommates)
        shiny::updateSelectInput(session, inputId = "moduleCommunity-community_buyer", choices = rv$roommates)
        shiny::updateSelectInput(session, inputId = "modulePrivate-private_buyer", choices = rv$roommates)
        shiny::updateSelectInput(session, inputId = "moduleSettings-delete_names", choices = rv$roommates)
    })
    
    
    observeEvent(input[["moduleCommunity-community_add"]], {
        updateSelectInput(session, inputId = "moduleCommunity-community_buyer", selected = NULL)
        updateTextInput(session, inputId = "moduleCommunity-community_note", value = "")
        updateDateInput(session, inputId = "moduleCommunity-community_date", value = NULL)
        updateNumericInput(session, inputId = "moduleCommunity-community_money", value = 0)
    })
    
    
    ######################
    ## Private Expenditures Tab
    ######################
    shiny::callModule(modulePrivateServer, "modulePrivate", rv=rv, input_re=reactive({input}))
    
    observeEvent(input[["modulePrivate-private_add"]], {
        updateSelectInput(session, inputId = "modulePrivate-private_buyer", selected = NULL)
        updateSelectInput(session, inputId = "modulePrivate-private_debtor", selected = NULL)
        updateTextInput(session, inputId = "modulePrivate-private_note", value = "")
        updateDateInput(session, inputId = "modulePrivate-private_date", value = NULL)
        updateNumericInput(session, inputId = "modulePrivate-private_money", value = 0)
    })
    
    observe({
        if (input[["modulePrivate-private_buyer"]] != ""){
            updateSelectInput(session, inputId = "modulePrivate-private_debtor", choices = rv$roommates[setdiff(names(rv$roommates), input[["modulePrivate-private_buyer"]])]) 
        }
    })
    
    ######################
    ## Community Expenditures Tab
    ######################
    shiny::callModule(moduleSummaryServer, "moduleSummary", rv=rv, input_re=reactive({input}))
    
    observe({
        req(rv$no_community)
        
        if (nrow(rv$community_data) > 0){
            output$menu <- shinydashboard::renderMenu({
                shinydashboard::sidebarMenu(
                    shinydashboard::menuItem("Community Expenditures", tabName = "community", icon = icon("table")),
                    shinydashboard::menuItem("Private Expenditures", tabName = "private", icon = icon("table")),
                    shinydashboard::menuItem("Expenditures Summary", tabName = "summary", icon = icon("table"))
                )
            })
            rv$no_community <- FALSE
        }
    })
})
