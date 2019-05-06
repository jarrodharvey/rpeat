shinyServer(function(input, output, session) {
  
  output$metadataPreview <- renderDataTable(
    datatable(
      metadataTable <- metadata %>%
        select(input$chooseMetadataElements),
      options = list(pageLength = 50)
    )
  )
  
  observeEvent(input$acceptableMetadata, {
    
    inFile <- input$acceptableMetadata
    
    if (is.null(inFile))
      return(NULL)
    
    userMetadata <- scan(inFile$datapath, what="", sep="\n")
    
    # 'bad entries' as in, 'metadata listed in the file that does not exist'
    badEntries <- userMetadata[!(userMetadata %in% names(metadata))]
    
    lapply(badEntries, function(nonExistentMetadata) {
      showNotification(paste0("The metadata element '", nonExistentMetadata, "' does not exist, so it was not selected."))
      # Having all of the notifications relating to nonexistent metadata appear simultaneously is disconcerting, 
      # so a one-second delay has been programmed in between each.
      Sys.sleep(1)
    })
    
    updateCheckboxGroupInput(session, "chooseMetadataElements",
                             choices = sort(names(metadata)),
                             selected = userMetadata)
    
  })
  
  observeEvent(input$createTransfer, {
    
    if (!dir.exists(exportLoc)) {
      # It's possible that the export in 'working directory' has been nuked somehow - if so this button should throw
      # an error message.
      session$sendCustomMessage(type = 'testmessage',
                                message = paste0("This is an unlikely scenario indeed, but remember that zip file that was just extracted? It's gone now. You will need to start from scratch, sorry."))
      session$reload()
    }
    
    selectedMetadata <-  input$chooseMetadataElements
    
    if (is.null(selectedMetadata)) {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Please choose some metadata before you create this transfer.")
      return()
    }
    
    shinyjs::disable("createTransfer")
    
    mostRecentVersions <- filesAndRecordNumbers %>% 
      group_by(`Record Number`) %>%
      filter(`Record Version` == max(`Record Version`)) %>%
      .[!duplicated(.$`Record Number`, fromLast = TRUE),]
    
    assign("mostRecentVersions", mostRecentVersions, envir = .GlobalEnv)
    
    outputCSV <- metadata %>%
      # Record Number will be appended automatically as the second column, so it is removed here along with
      # the metadata not selected by the user.
      select(selectedMetadata, -one_of("Record Number")) %>%
      # Archivematica requires the path to each object to go in front of the object's file name.
      cbind(filename = paste0("objects/", mostRecentVersions$filename), "Record Number" = mostRecentVersions$`Record Number`, .)
    
    buildTransferPackage(outputCSV)
    
    message_text <- switch(Sys.info()[["sysname"]],
                                "Windows" = paste0("The transfer package has been built successfully! It should now be available in ", path.expand("~"), "/RecordPoint_Exports"),
                                "Linux" = "The transfer package has been built successfully! Sign in to Archivematica and it should now be available in /home/RecordPoint_Transfers.",
                                stop("I don't recognise the system.")
    )
    
    session$sendCustomMessage(type = 'testmessage',
                              message = message_text)
    
    session$reload()
  })
  
  observeEvent(input$selectAll, {
    if (length(input$chooseMetadataElements) == ncol(metadata)) {
      updateCheckboxGroupInput(session,"chooseMetadataElements", choices=sort(names(metadata)))
    } else {
      updateCheckboxGroupInput(session, "chooseMetadataElements", choices=sort(names(metadata)), selected = names(metadata))
    }
  })
  
  observeEvent(input$viewMetadataList, {
    withProgress(message = 'Getting Record Metadata...', min = 0, max = nrow(filesAndRecordNumbers), {
      lapply(unique(filesAndRecordNumbers$`Record Number`), getMetadata) %>%
        bind_rows(.) %>%
        removeEmptyColumns(.) %>%
        addDublinCore(., session) %>%
        assign("metadata", ., env = .GlobalEnv)
    })
    updateCheckboxGroupInput(session, "chooseMetadataElements",
                             choices = sort(names(metadata)),
                             # Dublin Core metadata selected by default 
                             selected = grep("^dc\\.", names(metadata), value = TRUE, perl = TRUE))
    output$colCount <- renderUI({ 
      HTML(paste0("With <strong>", ncol(metadata), "</strong> elements found (<strong>", metadataElementsOriginalCount - ncol(metadata), "</strong> universally empty metadata fields were removed), some cleanup may be required."))
    })
    shinyjs::hide("splashScreen")
    shinyjs::show("selectMetadata")
  })
  
  observeEvent(c(input$zipUpload, input$startButton), {
    export_title <- if (export_dir_mounted) { 
      # For some reason input$startButton fires initially without being clicked,
      # so need to check that it has NOT been clicked
      if (isolate(input$startButton) == 0) {
        return()
      }
      isolate(input$selectExport) 
    } else {
      isolate(input$zipUpload$name)
    }
    if (dir.exists(paste0("/home/RecordPoint_Transfers/", str_replace(export_title, ".zip", ""), "_RPEAT"))) {
      session$sendCustomMessage(type = 'testmessage',
                                message = "You have already done this transfer!")
      return()
    }
    if (!export_dir_mounted) {
     # Upload a file called 'bypass.zip' to bypass extraction - useful for testing!
      if (export_title != "bypass.zip") {
        if (file.exists("workingDir")) {
          unlink("workingDir", recursive = TRUE, force = TRUE)
        }
        dir.create("workingDir")
        zipContents <- unzip(input$zipUpload$datapath, list = TRUE)
        zipFiles <- zipContents[zipContents$Length > 0,]$Name
        zipDirs <- zipContents[zipContents$Length == 0,]$Name
        exportName <- str_replace(input$zipUpload$name, ".zip", "")
        withProgress(
          lapply(zipFiles, function(zipFile) {
            fileName <- sub(".*/", "", zipFile)
            unzip(input$zipUpload$datapath, files = zipFile, exdir = "workingDir")
            incProgress(1, detail = paste("Adding", fileName))
          }
          ), min = 0, max = length(zipFiles), message = "Extracting files to working directory...")
      } else {
        print(getwd())
        dirsInWorking <- list.dirs("workingDir", FALSE, FALSE)
        if (length(dirsInWorking) != 1) {
          session$sendCustomMessage(type = 'testmessage',
                                    message = "Make sure that there is exactly one folder in your working directory.")
          return()
        }
        exportName <- dirsInWorking
      } 
    } else {
      if (file.exists("workingDir")) {
        unlink("workingDir", recursive = TRUE, force = TRUE)
      }
      dir.create("workingDir")
      showNotification(
        "Now copying the export folder. This may take some time depending on the size of the folder, so please be patient...",
        duration = NULL,
        type = "message",
        closeButton = FALSE,
        id = "exportCopyNotification"
      )
      shinyjs::disable("startButton")
      file.copy(
        paste0(exports_directory, "/", export_title),
        "./workingDir",
        recursive = TRUE,
        copy.mode = FALSE,
        copy.date = TRUE
      )
      removeNotification(id = "exportCopyNotification")
      shinyjs::enable("startButton")
      exportName <- export_title
    }
    assign("exportLoc", paste0("./workingDir/", exportName), env = .GlobalEnv)
    # The directory in Position 1 will be the export location itself, so remove it.
    contents <- list.dirs(exportLoc, recursive = TRUE) %>% .[2:length(.)]
    # All folders in an export will begin with either "Record_" or "Binary_". There is something wrong
    # with the export if that isn't the case.
    verificationPattern <- paste0("^", exportLoc, "/(Binary_|Record_)")
    if (all(grepl(verificationPattern, contents))) {
      binaryFolders <- grep("/Binary_", contents, value = TRUE)
      withProgress(lapply(binaryFolders, getFilesAndRecordNumbers), min = 0, max = length(binaryFolders), message = "Gathering data on record binaries...") %>%
        bind_rows(.) %>%
        assign("filesAndRecordNumbers", ., envir = .GlobalEnv)
      if (!all(filesAndRecordNumbers$filename %in% sub("^.*/", "", list.files(exportLoc, recursive = TRUE)))) {
        session$sendCustomMessage(type = 'testmessage',
                                  message = "It looks like some of your binaries are missing from their respective binary folders. Every binary directory should contain a binary!")
        missingList <- filesAndRecordNumbers[!filesAndRecordNumbers$filename %in% sub("^.*/", "", list.files(exportLoc, recursive = TRUE)),] %>%
          apply(., 1, function(row) {
            paste(row[["filename"]], "from", row[["Binary ID"]])
          }) %>%
          paste(., collapse = ", ") 
        output$fileList <- renderUI({
          HTML(paste0("<b>The following records are missing their binaries: </b>", missingList, "."))
        })
        return()
      }
      if(input$preventDuplication & Sys.info()[["sysname"]] != "Windows") {
        # R is attempting to connect to the host through the container!
	      # To allow this, run the following command in the host:
        # docker network create -d bridge --subnet 192.168.0.0/24 --gateway 192.168.0.1 dockernet
	      # ShinyProxy's application.yml file should also have the container-network set as dockernet
        connect(es_host="192.168.0.1")
        aipData <- Search(index = "aips", type = "aip", size = 10000)$hits$hits
        alreadyPreservedRecords <- lapply(aipData, getPreservedRecords) %>% unlist(.)
        filesAndRecordNumbers$identifier <- sub("^.*\\((.*)\\)\\..*$", "\\1", filesAndRecordNumbers$filename)
        rejectedRecords <- intersect(filesAndRecordNumbers$identifier, alreadyPreservedRecords) 
        rejectedRecords <-  filesAndRecordNumbers[filesAndRecordNumbers$identifier %in% rejectedRecords,] %>%
          .$filename %>%
          sub("\\([a-z0-9]{32}\\)\\.", ".", .) %>%
          unique(.) 
        rejectedRecords_recordNumbers <- filesAndRecordNumbers[filesAndRecordNumbers$identifier %in% alreadyPreservedRecords,]$`Record Number`
        rejectedRecordText <- paste(rejectedRecords, collapse = ", ") 
        filesAndRecordNumbers <- filesAndRecordNumbers[!filesAndRecordNumbers$`Record Number` %in% rejectedRecords_recordNumbers,] %>%
          assign("filesAndRecordNumbers", ., envir = .GlobalEnv)
      } else {
        rejectedRecords <- c()
      }
      acceptedRecords <- gsub("\\([a-z0-9]{32}\\)\\.", ".", filesAndRecordNumbers$filename) %>%
        unique(.) %>%
        setdiff(., rejectedRecords) %>%
        paste(., collapse = ", ") 
      acceptedRecordsCount <- countCharOccurrences(",", acceptedRecords)
      output$fileList <- renderUI({ 
	if (acceptedRecordsCount > 0) {
	   line1 <- paste0("<b>The following records will be repackaged according to Archivematica Transfer requirements:</b> ", acceptedRecords, ".")
        } else {
	   line1 <- "<b>All records submitted are already in Archivematica's AIPs!</b>"
	}
        if (length(rejectedRecords) > 0) {
          line2 <- paste0("<font color='red'><b>The following records were found to already exist within AIPs and will not be repackaged:</b> ", rejectedRecordText, ".</font>")
        } else {
          if (!isolate(input$preventDuplication)) {
            line2 <- "<font color='green'><b>You have elected not to prevent duplicate digital objects, so potential duplicates have not been filtered out.</b></font>"
          } else if (Sys.info()[["sysname"]] == "Windows") {
            line2 <- "<font color='purple'><b>You are running this app in Windows, so a deduplication check will not be run.</b></font>"
          } else {
            line2 <- "<font color='green'><b>No records were found to be duplicates.</b></font>"
          }
        }
        line3 <- "Okay to continue?"
        HTML(paste(line1, line2, '<br/>', sep = '<br/><br/>'))
      })
	if (acceptedRecordsCount > 0) {
         shinyjs::show("viewMetadataList")
	}
      
    } else {
      session$sendCustomMessage(type = 'testmessage',
                                message = "Did you select a RecordPoint export? All folders in the export should begin with either 'Binary_' or 'Record_'.")
    }
  })
  
})
