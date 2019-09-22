shiny::shinyUI(
  shiny::tagList(
    shinydashboard::dashboardPage(
      skin = "black",
      
      # Application title
      shinydashboard::dashboardHeader(title = "ExpenditR"),
      
      shinydashboard::dashboardSidebar(
        
        # Include shinyjs in the UI Sidebar
        shinyjs::useShinyjs(),
        
        #Sidebar Panel
        shinydashboard::sidebarMenu(id = "tabs",
                                    shinydashboard::menuItemOutput("menu"),
                                    shiny::tags$hr(),
                                    shinydashboard::menuItem("Settings", tabName = "settings", icon = icon("cogs")),
                                    shiny::actionButton("reset", "Reset")
        ),
        shiny::div(class = "sidebar-menu", style = "position:fixed; bottom:0; left:0; white-space: normal; text-align:left;
                                                                              padding: 9.5px 9.5px 9.5px 9.5px; margin: 6px 10px 6px 10px; box-sizing:border-box; heigth: auto; width: 230px;",
                   shiny::HTML("\u00A9 Lorenz A. Kapsner</i>"))
      ),
      
      shinydashboard::dashboardBody(
        
        # Include shinyjs in the UI Body
        shinyjs::useShinyjs(),
        
        # js reset function
        # https://stackoverflow.com/questions/25062422/restart-shiny-session
        shinyjs::extendShinyjs(script = "reset.js", functions = "reset"), # Add the js code to the page
        
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "settings",
                                  moduleSettingsUI("moduleSettings")
          ),
          
          shinydashboard::tabItem(tabName = "community",
                                  moduleCommunityUI("moduleCommunity")
          ),
          
          shinydashboard::tabItem(tabName = "private", 
                                  modulePrivateUI("modulePrivate")
          ),
          
          shinydashboard::tabItem(tabName = "summary", 
                                  moduleSummaryUI("moduleSummary")
          )
        )
      )
    )))
