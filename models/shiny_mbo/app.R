# devtools::install_github("systats/tidyMBO")
pacman::p_load(
  shiny, shiny.semantic, semantic.dashboard, DT, dplyr, tibble, stringr, 
  shinyWidgets, shinyjs, h2o, keras, tidyTX, purrr, tidyr, tidyMBO, magrittr
)
#devtools::install_github("Appsilon/shiny.semantic")
#devtools::install_github("Appsilon/semantic.dashboard")
source("mods/table_mod.R")
source("mods/perform_mod.R")

ui <- function() {
  shinyUI(
    dashboard_page(
      dashboard_header(
        #dropdownMenuOutput("dropdown"),
        # dropdownMenu (type = "notifications",
        #              taskItem("Project progress...", 50.777, color = "red")),
        # dropdownMenu(icon = uiicon("red warning sign"),
        #              notificationItem("This is an important notification!", color = "red")),
        actionButton("save", class = "ui yellow inverted button", label = "Save")# icon = uiicon("exit")  
      ),
      dashboard_sidebar(
        #size = "",
        sidebarMenu(
          menuItem(
            tabName = "data", 
            text = "Data Import", 
            icon = uiicon("cloud")
          ),
          menuItem(
            tabName = "explore", 
            text = "Exploration", 
            icon = uiicon("search")
          )
        ),
        selectInput("slot", label = NULL, choices = 1:15, selected = 1)
      ),
      dashboard_body(
        tabItems(
          selected = 1,
          tabItem(tabName = "data",
             table_mod_UI("table")
          ),
          tabItem(tabName = "explore",
            #shinyjs::useShinyjs(),
            perform_mod_UI("perform")
            #table_mod_UI("table")
          )
        )
      ), 
      theme = "cyborg"
    )
  )
}

server <- function(input, output, session) {
  
  values <- reactiveValues(data = list(), temp = NULL)
  
  observe({
    values$temp <- callModule(table_mod, "table")
  })
  
  observeEvent(input$save, {
    if(!is.null(values$temp)){
      values$data[[input$slot]] <- values$temp()
    }
  })
  
  observe({
    callModule(perform_mod, "perform", data = reactive({ values$data[[input$slot]] }))
  })
  
  # observe({
  #   callModule(
  #     module = eda_server,
  #     id = "exp1",
  #     data = values$dat[[input$slot]]
  #   )
  # })
  
  # output$dropdown <- renderDropdownMenu({
  #   dropdownMenu(messageItem("User", "Test message", color = "teal", style = "min-width: 200px"),
  #                messageItem("Users", "Test message", color = "teal", icon = "users"),
  #                messageItem("See this", "Another test", icon = "warning", color = "red"))
  # })
}

shinyApp(ui = ui(), server = server)
