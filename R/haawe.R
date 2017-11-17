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

haawe <- function(x, keyname = NULL) { # takes key/url and in case of url also a keyname argument
    data(dataKeys, package = 'kokua') #  Loads tracker dataframe of spacial data
    if (is.null(keyname)) { #  x is a key
        keyData <- dataKeys[grep(x, dataKeys$key), ] #  Searches for all data corresponding to x argument
        unloaded <- keyData[, keyData$loaded == FALSE] #  Finds unloaded data corresponding to `key`
        if (length(unloaded) == 0) { #  Stop if there are no unloaded matches
            stop('There are no unloaded data for the key specified') 
        } #  If the data aren't all loaded, load them with .loadData
        loadString <- c(mapply(.loadData, unloaded$url, unloaded$key, .fileExt(unloaded$url), file.path(.libPaths(), 'kokua', 'data'))) #  Downloads each previously unloaded file with helper function
        # dataKeys[dataKeys$key %in% unloaded$key, 'loaded'] <- TRUE #  If successful, grep the keys in unloaded and change their $loaded value to TRUE
    } else {
        stopifnot(is.character(keyname)) #  Verifies that keyname is a character string
        loadString <- .loadData(x, keyname, .fileExt(x), dest = file.path(.libPaths(), 'kokua', 'data'))
        # newRow <- data.frame(key = keyname, url = x, loaded = TRUE)
        # dataKeys <- rbind(dataKeys, newRow)
    }
    # save(dataKeys, file = file.path(.libPaths(), 'kokua', 'data', 'dataKeys.RData')) #  Updates dataKeys file to reflect newly loaded data
    # return(eval(parse(text = loadString)))
    return(loadString)
}

## this version of the function uses keyword strings to download data. it needs a place to store
## those keyword strings and their corresponding URLs. For that we can make a exported object
## in the hdimDB package, something like:
# #' @export
# dataKeys <- data.frame(key = c('geol_niihau', 'geol_kauai'),
#                        url = c('http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/niihau.zip',
#                                'http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/kauai.zip'),
#                        loaded = c(FALSE, FALSE))

## then when the user uses `hdimDB` to load data, the function will check `dataKeys` to see if those
## data are already loaded, if they aren't the function will load the data and update the `loaded`
## column in `dataKeys`

.loadData <- function(url, name, ext, dest) {
    filename <- paste(name, ext, sep = '.') #  Constructs full filename
    if (!file.exists(file.path(dest, name))) { #  If folder does not exist in target directory
        dir.create(file.path(dest, name)) #  Creates new folder in the /data directory for file
    }
    path <- file.path(dest, filename) #  Constructs filepath
    download.file(url = as.character(url), destfile = path, mode = 'wb') #  Downloads to target directory
    if (length(unzip(path, list = TRUE)) > 0) { #  Checks if file is compressed
        unzip(path, exdir = file.path(dest, name)) #  Unzips file to same directory and keeps original compressed file
        # file.remove(path) #  Optional cleanup.
    } 
    unlink(file.path(dest, name, "__MACOSX"), recursive = TRUE)
    files <- list.files(path = file.path(dest, name) , recursive = TRUE)
    
    fileInfo <- mapply(.readSelect, files, mapply(.fileExt, files))
    fileInfo <- fileInfo[!sapply(fileInfo, is.null)]
    stopifnot(length(fileInfo) == 1)
    fileInfo <- unlist(fileInfo)
    
    if (grepl("/", fileInfo[1], fixed = TRUE)) {
        subdirectory <- gsub("/.*", "", fileInfo[1])
        fileInfo[1] <- gsub(".*/", "", fileInfo[1])
    } else {
        subdirectory <- ''
    }
    
    readFun <- .scriptSelect(fileInfo[1], fileInfo[2])
    ## 
    # readFun <- suppressWarnings(.readSelect(list.files(dest, recursive = TRUE))) #  Constructs lists of strings of load functions for each file
    # readFun <- readFun[!sapply(readFun, is.null)]

    # use `writeLines` to put together (and save to /data) a simple R script that loads to datafile(s), something like:
    loadString <- c(sprintf('oldwd <- setwd("%s")', paste0(file.path(dest, name, subdirectory))),
                    paste0(name, ' <- ', readFun), 
                    .scriptSelect(fileInfo[1], fileInfo[2], proj = TRUE, name = name), 
                    'setwd(oldwd) \n')
    
    # eval(parse(text = loadString))
    #  If download was successful, the user is notified
    writeLines(loadString, file.path(.libPaths(), 'kokua', 'data', paste0(name, '.R')))
    data(name)
    cat(paste0(filename, ' successfully loaded. Downloaded data may be viewed by running: plot(', name,')'))
    return(loadString)
    # invisible(paste0(name, '.R'))
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
               'bil' = paste0('projectRaster(', name, ', crs = CRS("', '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0', '"))'),
               # 
               'bil' = paste0('crs(', name, ') <- "+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"'),
               'shp' = paste0(name, ' <- spTransform(', name,', CRS("', '+proj=utm +zone=4 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0', '"))')
               # 'tif' = 
               # 'kml' = 
               # support for addtional extensions to be added
        )
    }
}

.fileExt <- function(path) { #  Retrieves the file ex   tension of a string
    pos <- regexpr("\\.([[:alnum:]]+)$", path)
    ifelse(pos > -1L, substring(path, pos + 1L), "")
}

getLoaded <- function(data = NULL) {
    loaded <- list.dirs(path = file.path(.libPaths(), 'kokua', 'data'), recursive = FALSE, full.names = FALSE)
    if (is.null(data)) {
        return(loaded[loaded != ""])
    }
    return(grepl(data, loaded))
}
