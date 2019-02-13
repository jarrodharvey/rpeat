shinyUI(fluidPage(theme = "rpeat_custom.css",
                  
                  shinyjs::useShinyjs(),
                  
                  includeScript("www/message-handler.js"),
                  
                  titlePanel(
                    fluidRow(
                      column(1, offset = 5, headerPanel(tags$b("RPEAT", id = "appHeader"))
                      )
                    ), windowTitle = "RPEAT"
                  ),
                  
                  fluidRow(id = "splashScreen",
                           
                           source("Splash_Screen.R")
                           
                  ),
                  
                  shinyjs::hidden(fluidRow(id = "selectMetadata",
                                           
                                           source("Select_Metadata.R")
                                           
                  ))
                  
  )

)