#' @title Prepares spacial data files to be loaded
#'  
#' @description \code{haawe} Organizes loaded data and generates an R script to load the data.
#' 
#' @details stub
#' 
#' @param x A keyword for the data to be loaded or alternatively the URL where the data is located
#' @param keyname The keyword to be assigned to the data in case that x is a URL
#' 
#' @return Generated R script saved in the data directory of the kokua package.
#'
#' @author Edward Greg Huang <edwardgh@@berkeley.edu>
#' @export

haawe <- function(x, keyname = NULL, overwrite = FALSE) { # takes key/url and in case of url also a keyname argument
    data(dataKeys, package = 'kokua') #  Loads tracker dataframe of spacial data
    if (is.null(keyname)) { #  x is a key
        keyData <- dataKeys[grep(x, dataKeys$key), ] #  Searches for all data corresponding to x argument
        if (nrow(keyData) == 0) { #  Stop if there are no unloaded matches
            stop('There are no unloaded data for the key specified') 
        }
        if (overwrite == FALSE & ! is.null(getLoaded(x))) {
            for (match in getLoaded(x)) {
                message(match, ' is already loaded.')
            }
            message('')
            keyData <- keyData[ ! keyData$key %in% getLoaded(x), ]
            if (nrow(keyData) == 0) {
                stop('All data for the specified query is already loaded. Overwrite not authorized.')
            }
        }
        invisible(mapply(.loadData, keyData$url, keyData$key, .fileExt(keyData$url), file.path(.libPaths(), 'kokua', 'data'))) #  Downloads each previously unloaded file with helper function
    } else {
        stopifnot(is.character(keyname)) #  Verifies that keyname is a character string
        if (overwrite == FALSE & ! is.null(getLoaded(keyname, exact == TRUE))) { 
            stop('Data is already loaded for the key specified. Overwrite not authorized.')
        }
    invisible(.loadData(x, keyname, .fileExt(x), dest = file.path(.libPaths(), 'kokua', 'data')))
    }
}


.loadData <- function(url, name, ext, dest) {
    filename <- paste(name, ext, sep = '.') #  Constructs full filename
    if (! file.exists(file.path(dest, name))) { #  If folder does not exist in target directory
        dir.create(file.path(dest, name)) #  Creates new folder in the /data directory for file
    }
    path <- file.path(dest, filename) #  Constructs filepath
    message('Attempting to download ', name, '...')
    attempt <- tryCatch(download.file(url = as.character(url), destfile = path, mode = 'wb'),
                 error = function(cond) {
                    message('Download failed for the URL: ', url)
                    message('Original error message:')
                    message(cond)
                    unlink(file.path(dest, name))
                    return(NULL)
                 }, warning = function(cond) {
                    message(cond)
                    message('')
                    return(NULL)
                 })#  Downloads to target directory
    if (is.null(attempt)) {
        return(NULL)
    }
    if (length(unzip(path, list = TRUE)) > 0) { #  Checks if file is compressed
        unzip(path, exdir = file.path(dest, name)) #  Unzips file to same directory and keeps original compressed file
        file.remove(path) #  Optional cleanup.
    } 
    unlink(file.path(dest, name, '__MACOSX'), recursive = TRUE)
    files <- list.files(path = file.path(dest, name) , recursive = TRUE)
    fileInfo <- mapply(.readSelect, files, mapply(.fileExt, files))
    fileInfo <- fileInfo[ ! sapply(fileInfo, is.null)]
    stopifnot(length(fileInfo) == 1)
    fileInfo <- unlist(fileInfo)
    
    if (grepl("/", fileInfo[1], fixed = TRUE)) {
        subdirectory <- gsub('/.*', '', fileInfo[1])
        fileInfo[1] <- gsub('.*/', '', fileInfo[1])
    } else {
        subdirectory <- ''
    }
    
    readFun <- .scriptSelect(fileInfo[1], fileInfo[2])
    loadString <- c(sprintf('oldwd <- setwd("%s")', paste0(file.path(dest, name, subdirectory))),
                    paste0(name, ' <- ', readFun), 
                    .scriptSelect(fileInfo[1], fileInfo[2], proj = TRUE, name = name), 
                    'setwd(oldwd) \n')
    
    #  If download was successful, the user is notified
    writeLines(loadString, file.path(.libPaths(), 'kokua', 'data', paste0(name, '.R')))
    message('Attempting to load and reproject ', name, '...')
    data(list = name)
    message(name, ' reprojected to the following coordinate reference system: +proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
    message(writeLines(c(paste0(filename, ' successfully loaded. Downloaded data may be viewed by running: plot(', name,')'), '')))
}

.readSelect <- function(file, ext) { #  Finds and returns target filename and file extension
    switch(ext,
           'bil' = c(file, ext),
           'shp' = c(file, ext)
           )
}

.scriptSelect <- function(file, ext, proj = FALSE, name = NULL) {
    if (proj == FALSE) {
        switch(ext,
               'bil' = paste0("raster('", file, "')"),
               'shp' = paste0("readOGR('.', '", gsub(paste0(name, '.', ext), '', file), "')")
        # 'tif' = 
        # 'kml' = 
        # support for addtional extensions to be added
        )
    } else {
        switch(ext,
               # 'bil' = paste0('projectRaster(', name, ', crs = CRS("', '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0', '"))'),
               'bil' = paste0('crs(', name, ') <- "+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"'),
               'shp' = paste0(name, ' <- spTransform(', name,', CRS("', '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0', '"))')
               # 'tif' = 
               # 'kml' = 
               # support for addtional extensions to be added
        )
    }
}

.fileExt <- function(path) { #  Retrieves the file extension of a string
    pos <- regexpr('\\.([[:alnum:]]+)$', path)
    ifelse(pos > -1L, substring(path, pos + 1L), '')
}

getLoaded <- function(data = NULL, exact = FALSE) { #  Retrieves names of loaded data or optionally queries through the list of loaded data
    loaded <- list.dirs(path = file.path(.libPaths(), 'kokua', 'data'), full.names = FALSE, recursive = FALSE)
    if (is.null(data)) {
        results <- loaded[loaded != ""]
    } else if (exact == TRUE) {
        results <- loaded[which(loaded == data)]
    } else {
        results <- grep(data, loaded, value = TRUE)   
    }
    if (identical(results, character(0))) {
        return(NULL)
    }
    return(results)
}
