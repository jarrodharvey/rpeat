shinyUI(fluidPage(
  
  fluidRow(
    
    column(8,
           
           tags$p(htmlOutput("colCount")),
           
           tags$p("Try to limit the metadata you select to just those elements that you know for sure you will need to capture for preservation."),
           
           tags$p("RPEAT has generated some ",
                  tags$a(href = "http://dublincore.org/documents/dces/",
                         "Dublin Core Metadata"), 
                  "and selected this by default.")
           
    ),
    
    column(4,
           
           actionLink("selectAll","Select All"),
           fileInput("acceptableMetadata", "Upload a text file that contains the metadata you want to include (one element per line)", accept = c(".txt"), buttonLabel = "Upload list")
    )
  ),
  
  fluidRow(
    tabsetPanel(type = "tabs",
      tabPanel("Select",
        tags$div(class = "multicol",
                   checkboxGroupInput("chooseMetadataElements", "Select sensible metadata for Archivematica", choices = character(0), inline = TRUE)
        )
      ),
      tabPanel("Preview",
        dataTableOutput("metadataPreview")
      )
    )
  ),
  
  fluidRow(
    
    column(1, offset = 5,
           actionButton("createTransfer", "Create Transfer")
    )
    
    
  )
  
))