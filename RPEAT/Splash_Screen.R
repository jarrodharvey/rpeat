shinyUI(fluidPage(
  
  fluidRow(column(width=5, img(src= "what_RPEAT_does.PNG", width = "100%")),
           
  column(width = 7, 
         
           h2("RecordPoint Export to Archivematica Transfer"),
           
           p("This application is for ",
                  tags$a(href = "https://www.recordpoint.com/",
                         "RecordPoint"),
                  " users who have identified ",
                  tags$a(href = "http://docs.recordpoint.com/display/R4/Finalise+and+Finalise+Archive+in+the+Active+Site",
                         "Finalise Archive"), 
                  " records that require long-term access and are therefore candidates for ",
                  tags$a(href = "http://www.dpconline.org/handbook",
                         "digital preservation"),
                  "."),
           
           p("It can convert a ",
                  tags$a(href = "http://docs.recordpoint.com/display/R4/Exporting+Records%2C+File+and+Boxes",
                         "RecordPoint Electronic Record Export"),
                  " to an ",
                  tags$a(href = "https://www.archivematica.org/en/docs/archivematica-1.6/user-manual/transfer/transfer/#create-submission",
                         "Archivematica Transfer"), 
                  ", allowing ", 
                  tags$a(href = "https://www.archivematica.org/en/",
                         "Archivematica"), 
                  " to run its digital preservation workflows."),
           
           p("It will generate ", 
                  a(href = "https://www.archivematica.org/en/docs/archivematica-1.6/user-manual/transfer/import-metadata/#import-metadata",
                         "'simple object' metadata describing each binary"),
                  ". It will also select only the most recent version of each Finalised record for preservation."),
           
           p("The application will only work with exports of Electronic Records, not with Electronic Files."),
           
           checkboxInput("preventDuplication", "Check this box if you would like to block conversion of records that are already in AIPs.", value = TRUE, width = NULL),
          
         if(export_dir_mounted) {
           fluidRow(
            selectInput(
               "selectExport",
               "Select a RecordPoint Export from the list.",
               list.dirs(exports_directory, recursive = FALSE, full.names = FALSE)
              ),
             actionButton("startButton", "Start!")
           )
           
         } else {
            fileInput("zipUpload", "Upload zipped RecordPoint export",
                     multiple = FALSE,
                     accept = ".zip")
         },
          
          tags$p(),
         
           htmlOutput("fileList"),
           
           shinyjs::hidden(
             actionButton("viewMetadataList", "Continue")
           )

      ))
  

  
  )
)
