#' @title moduleSettingsServer
#'
#' @param input Shiny server input object
#' @param output Shiny server output object
#' @param session Shiny session object
#' @param rv The global 'reactiveValues()' object, defined in server.R
#' @param input_re The Shiny server input object, wrapped into a reactive expression: input_re = reactive({input})
#'
#' @export
#'
# moduleSettingsServer
moduleSettingsServer <- function(input, output, session, rv, input_re){
  
  observeEvent(input_re()[["moduleSettings-add_name"]], {
    if (input_re()[["moduleSettings-roommate_name"]] == "" || grepl(" |[[:punct:]]", input_re()[["moduleSettings-roommate_name"]])){
      showModal(modalDialog(
        title = "Error",
        "Please enter a valid nickname. It may not contain empty spaces or punctuation chars.",
        footer = modalButton("OK")
      ))
    } else {
      if (is.null(rv$roommates[[input_re()[["moduleSettings-roommate_name"]]]])){
        rv$roommates[[input_re()[["moduleSettings-roommate_name"]]]] <- input_re()[["moduleSettings-roommate_name"]]
        jsonlite::write_json(rv$roommates, paste0(storedir, "roommates.JSON"))
      } else {
        showModal(modalDialog(
          title = "Error",
          "Name already exists",
          footer = modalButton("OK")
        ))
      }
      updateTextInput(session, inputId = "moduleSettings-roommate_name",
                      value = NULL)
    }
    if (input_re()$tabs == "settings"){
      updateTabItems(session, "tabs", "settings")
    }
  })
  
  observe({
    req(rv$roommates)
    
    output$roommate_table <- renderTable({
      t <- data.table::data.table(rv$roommates)
      colnames(t) <- "Roommates"
      t
    })
  })
  
  observeEvent(input_re()[["moduleSettings-remove_name"]], {
    if (length(rv$roommates) > 1){
      rv$roommates <- rv$roommates[names(rv$roommates) != input_re()[["moduleSettings-delete_names"]]]
      jsonlite::write_json(rv$roommates, paste0(storedir, "roommates.JSON"))
    } else {
      rv$roommates <- NULL
      file.remove(paste0(storedir, "roommates.JSON"))
    }
  })
}


#' @title moduleSettingsUI
#'
#' @param id A character. The identifier of the shiny object
#'
#' @export
#'
# moduleSettingsUI
moduleSettingsUI <- function(id){
  ns <- NS(id)
  
  tagList(
    # first row
    fluidRow(
      box(
        title = "Add new roommate's name",
        textInput(ns("roommate_name"), label = "Nickname", placeholder = "Enter new roommates nickname"),
        actionButton(ns("add_name"), label = "Add name"),
        width = 8
      ),
      conditionalPanel(
        condition = "output['gotRoommates']",
        box(
          title = "Current roommates",
          tableOutput(ns("roommate_table")),
          width = 4
        )
      )
    ),
    fluidRow(
      conditionalPanel(
        condition = "output['gotRoommates']",
        box(
          title = "Remove roommate",
          selectInput(ns("delete_names"), label = "Remove this name", selected = NULL, choices = list(), multiple = F),
          actionButton(ns("remove_name"), label = "Remove roommate"),
          width = 8
        )
      )
    )
  )
}
