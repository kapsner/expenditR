shiny::shinyServer(function(input, output, session) {

    # define reactive values here
    rv <- shiny::reactiveValues(
        roommates = list(),
        storedir = storedir,
        no_mates = TRUE,
        community_data = data.table::data.table(cbind(
            "Buyer" = character(0),
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
                    shinydashboard::menuItem("Community Expenditures", tabName = "community", icon = icon("table"))
                )
            })
            rv$no_mates <- FALSE
        }
    })
    
    observe({
        req(rv$roommates)
        shiny::updateSelectInput(session, inputId = "moduleCommunity-community_buyer", choices = rv$roommates)
        shiny::updateSelectInput(session, inputId = "moduleSettings-delete_names", choices = rv$roommates)
    })
    
    
    observeEvent(input[["moduleCommunity-community_add"]], {
        updateSelectInput(session, inputId = "moduleCommunity-community_buyer", selected = NULL)
        updateTextInput(session, inputId = "moduleCommunity-community_note", value = "")
        updateDateInput(session, inputId = "moduleCommunity-community_date", value = NULL)
        updateNumericInput(session, inputId = "moduleCommunity-community_money", value = 0)
    })
    
})
