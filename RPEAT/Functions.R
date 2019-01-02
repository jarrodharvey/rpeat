addDublinCore <- function(metadata, session) {
  userLogin <- get_user_login(session)
  metadata <- metadata %>% mutate(
    dc.title = tools::file_path_sans_ext(Name),
    dc.source = paste0("Created in ", `Record Source System`, " and captured by RecordPoint version ", recordPointVersion, "."),
    dc.creator = sub("\\d*;#", "", `Created By`),
    dc.date = paste("Created:", Created, "Modified:", Modified) %>%
      gsub("T\\d{2}:\\d{2}:\\d{2}.0000000Z", "", .),
    dc.provenance = paste("Repackaged for Archivematica by", userLogin, "on", Sys.Date())
  )
}
buildTransferPackage <- function(metadataCSV) {
  packageName <- str_replace(exportLoc, "./workingDir/", "") %>%
    paste0(., "_RPEAT")
  file.copy("Transfer_Skeleton", "./workingDir", recursive=TRUE)
  packageLocation <- paste0("./workingDir/", packageName)
  file.rename("./workingDir/Transfer_Skeleton", packageLocation)
  moveBinariesToObjectsFolder(packageLocation)
  # Export can be deleted now that the XMLs have been read and the binaries have been moved
  unlink(exportLoc, recursive=TRUE)
  if (!"dc.identifier" %in% metadataCSV) {
    metadataCSV$dc.identifier <- uniqueIdentifiers
  }
  metadataCSV$filename <- sub("\\([a-z0-9]{32}\\)\\.", ".", metadataCSV$filename)
  write.csv(metadataCSV, paste0(packageLocation, "/metadata/metadata.csv"), row.names = FALSE)
  writeStackInfo(packageLocation)
  setwd(packageLocation)
  removeUniqueIDsFromFilenames(packageLocation)
  moveTransferPackage(packageName, packageLocation)
}
filterOutEmptyColumn <- function(colName, df) {
  # Iterate through columns to determine which ones are completely empty.
  # They can then get filtered out for the user's convenience.
  allRowsEmpty <- all(df[[colName]] == "")
  if (is.na(allRowsEmpty)) {
    return(colName)
  }
  if (allRowsEmpty) {
    return(NA)
  }
  return(colName)
}
getBinaryElement <- function(xmlContent, elementName) {
  # The binary's "Container" value will be its Record Number. Need to cycle through the
  # binary's properties until the Container is found.
  xmlContent$Properties$ArrayOfMetaData %>%
    lapply(function(metadata) {
      if (metadata$Name == elementName) {
        return(metadata$Value)
      }
    }) %>%
    # The list returned is mostly NULL values, besides the Container.
    # Unlisting filters out all NULL values.
    unlist(.) %>%
    # We don't want this object to have a title, so select the first item to make it
    # stand alone.
    .[[1]] %>%
    # Now our data looks like "R0673552118|Record;#R0673552118". Remove everything
    # after the pipe symbol.
    str_replace("\\|.*$", "")
}
getFilesAndRecordNumbers <- function(binaryFolder) {
  # This function can establish a link between a 'binary' folder and a 'record' folder.
  # This information is used later when filtering out duplicate binaries.
  xmlFile <- list.files(binaryFolder, "\\.recordpoint$") %>% 
    paste0(binaryFolder, "/", .)
  xmlContent <- xmlParse(xmlFile, options = HUGE) %>%
    xmlToList(.)
  filename <- xmlContent$FileName
  # Used as part of the Transfer's metadata
  if (!exists("recordPointVersion")) {
    assign("recordPointVersion", xmlContent$.attrs[["RecordPointVersion"]], envir = .GlobalEnv)
  }
  # Need to know the record version to ensure only the latest version of the record will be preserved
  recordVersion <- getBinaryElement(xmlContent, "Record Version") %>%
    as.character(.) %>%
    as.numeric(.)
  recordNumber <- getBinaryElement(xmlContent, "Container")
  incProgress(1, detail = paste("Verifying", recordNumber))
  return(tibble("Record Version" = recordVersion, filename, "Record Number" = recordNumber, "Binary ID" = sub("^.*/", "", binaryFolder)))
}
getMetadata <- function(recordNumber) {
  metadataFolder <- paste0(exportLoc, "/Record_", recordNumber)
  xmlFile <- list.files(metadataFolder, "\\.xml$") %>%
    paste0(metadataFolder, "/", .)
  xmlContent <- xmlParse(xmlFile, options = HUGE)
  metadata <- getNodeSet(xmlContent, "/Metadata/ActiveSiteProperties/property")
  df <- xmlToDataFrame(metadata) %>%
    # Only interested in the 'name' and 'value' elements of the metadata property.
    # Won't need to capture 'type' in position 2.
    .[,c(1,3)] %>%
    # We need the data to be wide, not long.
    t(.) %>%
    as.tibble(.)
  # The tibble starts off with the column names 'V1', 'V2', etc. Need to rename them to be
  # the same as the first row.
  names(df) <- df[1,]
  # Now the first row is no longer needed, as it contains redundant column headings.
  df <- df[c(-1),]
  incProgress(1, detail = paste("Gathering metadata for", recordNumber))
  return(df)
}
getPreservedRecords <- function(aip) {
  digitalObjects <- aip$`_source`$mets$`ns0:mets_dict_list`[[1]]$`ns0:dmdSec_dict_list`
  if (any(grepl("dc:identifier", digitalObjects))) {
    return(lapply(digitalObjects, function(object) {
      object$`ns0:mdWrap_dict_list`[[1]]$`ns0:xmlData_dict_list`[[1]]$`ns3:dublincore_dict_list`[[1]]$`dc:identifier`
    }) %>% unlist(.))
  } else {
    return(NULL)
  }
}
get_user_login <- function(session) {
  if (Sys.getenv("SHINYPROXY_USERNAME") != "") {
    userlogin <- Sys.getenv("SHINYPROXY_USERNAME") 
  } else if (!is.null(session$user)) {
    userlogin <- session$user
  } else {
    userlogin <- Sys.info()[["user"]]
  }
}
moveBinariesToObjectsFolder <- function(packageLocation) {
  binaries <- list.files(exportLoc, recursive = TRUE, full.names = TRUE) %>% 
    .[sub("^.*/", "", .) %in% mostRecentVersions$filename]
  assign("uniqueIdentifiers", sub("^.*\\(([a-z0-9]{32})\\)\\..*$", "\\1", binaries), envir = .GlobalEnv)
  objectsDir <- paste0(packageLocation, "/objects")
  withProgress(
    lapply(binaries, function(binary) {
      binaryTitle <- sub("^.*/", "", binary)
      incProgress(1, detail = paste("Transferring", binaryTitle))
      file.move(binary, objectsDir)
    }),
    min = 0,
    max = nrow(metadata),
    message = "Moving the most recent version of each binary to the new SIP..."
  )
}
moveTransferPackage <- function(packageName, packageLocation) {
  if (dir.exists("/home/RecordPoint_Transfers")) {
    writeDir <- "/home/RecordPoint_Transfers"
  } else {
    writeDir <- path.expand("~")
  }
  writeDir <- paste0(writeDir, "/", packageName)
  dir.create(writeDir)
  file.copy(".", writeDir, recursive = TRUE)
  # Set permissions so that Archivematica can have access
  Sys.chmod(writeDir, "775")
  Sys.chmod(paste0(writeDir, "/metadata"), "775")
  Sys.chmod(paste0(writeDir, "/metadata/submissionDocumentation"), "775")
  Sys.chmod(list.files(paste0(writeDir, "/metadata/submissionDocumentation"), full.names = TRUE), "775")
}
removeEmptyColumns <- function(input) {
  # For the user's later reference, store how many metadata elements there were initially
  assign("metadataElementsOriginalCount", ncol(input), envir = .GlobalEnv)
  # Could not find a similar function to remove empty columns, so created this one.
  # nonEmptyColumns <- lapply(names(input), filterOutEmptyColumn, input)
  #return(input[,!is.na(nonEmptyColumns)])
  apply(input, 2, function(x) gsub("^$|^ $", NA, x)) %>%
    .[, colSums(is.na(.)) == 0] %>%
    as.tibble(.)
}
removeUniqueIDsFromFilenames <- function(packageLocation) {
  lapply(list.files("objects", full.names = TRUE), function(object) {
    file.rename(object, sub("\\([a-z0-9]{32}\\)\\.", ".", object))
  }) 
}
writeStackInfo <- function(packageLocation) {
  stackInfoText <- c("This file contains information relating to the environment in which RPEAT was run, to aid in reproducibility if needed.",
                     "", "",
                     "###SYSTEM INFORMATION###",
                     "", "",
                     capture.output(Sys.info()),
                     "", "",
                     "###R SESSION INFORMATION###",
                     "", "",
                     capture.output(sessionInfo()))
  lapply(stackInfoText, function(section) {
    write(section, file = paste0(packageLocation, "/metadata/submissionDocumentation/Stack_Info.txt"), append = TRUE)
  })
}