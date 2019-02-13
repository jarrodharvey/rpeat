source("Package_Loader.R")
source("Functions.R")

exports_directory <- switch(Sys.info()[["sysname"]],
                            "Windows" = "~/RecordPoint_Exports",
                            "Linux" = "/mnt/RecordPoint_Exports",
                            stop("I don't recognise the system.")
)

export_dir_mounted <- dir.exists(exports_directory) & !identical(list.files(exports_directory), character(0))