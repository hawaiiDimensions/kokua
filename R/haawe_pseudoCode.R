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

haawe_pseudo <- function(x, keyname = NULL) { # takes key/url and in case of url also a keyname argument
    data(dataKeys, package = 'hdimDB') #  Loads tracker dataframe of spacial data
    if (length(grep(x, dataKeys$key) > 0)) { #  x is a key
        keyData <- dataKeys[grep(x, dataKeys$key), ] #  Searches for all data corresponding to x argument
        unloaded <- keyData[, keyData$loaded == FALSE] #  Finds unloaded data corresponding to `key`
        if (length(unloaded) == 0) { #  Stop if there are no unloaded matches
            stop('There are no unloaded data for the key specified') 
        } #  If the data aren't all loaded, load them with .loadData
        mapply(.loadData, unloaded$url, 
                          unloaded$key, 
                          .fileExt(unloaded$url), 
                          file.path(.libPaths(), 'hdimDB', 'data', paste(x, Sys.time()))) #  Downloads each previously unloaded file with helper function
        dataKeys[dataKeys %in% unloaded, 'loaded'] <- TRUE #  If successful, grep the keys in unloaded and change their $loaded value to TRUE
    } else {
        stopifnot(is.character(keyname)) #  Verifies that keyname is a character string
        .loadData(x, keyname, .fileExt(x), dest = file.path(.libPaths(), 'hdimDB', 'data', keyname))
        newRow <- data.frame(key = keyname, url = x, loaded = TRUE)
        dataKeys <- rbind(dataKeys, newRow)
    }
    save(dataKeys, file = file.path(.libPaths(), 'hdimDB', 'data', 'dataKeys.RData')) #  Updates dataKeys file to reflect newly loaded data
}

.loadData <- function(url, name, ext, dest) {
    filename <- paste(name, ext, sep = '.') #  Constructs full filename
    if (!file.exists(dest)) { #  If folder does not exist in target directory
        dir.create(dest) #  Creates new folder in the /data directory for file
    }
    path <- file.path(dest, filename) #  Constructs filepath
    download.file(url = as.character(url), destfile = path, mode = 'wb') #  Downloads to target directory
    if (length(unzip(path, list = TRUE)) > 0) { #  Checks if file is compressed
        unzip(path, exdir = dest) #  Unzips file to same directory and keeps original compressed file
        file.remove(path) #  Optional cleanup.
    } 
    # if `path` is to a single file, figure out its file extension
    # if `path` is a directory, look inside at what the file extensions are inside the directory
    # (might need to recursively do this...that's something we should talk about on Thursday)
    
    # once you know the file extensions, figure out which spatial data reading 
    # function (`raster` or `readOGR`) you need and combine this info, with the filename 
    # info into a string that, if read by R, would load the data. 
    # ....now that you have that (save the string object to a variable called `readFun`)....
    readFun <- suppressWarnings(.readSelect(list.files(dest, recursive = TRUE))) #  Constructs lists of strings of load functions for each file
    readFun <- readFun[!sapply(readFun, is.null)]
    stopifnot(length(readFun) == 1)
    readFun <- unlist(readFun)
    # use `writeLines` to put together (and save to /data) a simple R script that loads to datafile(s), something like:
    writeLines(con = file.path(.libPaths(), 'hdimDB', 'data', paste0(name, '.R'),
               text = c(sprintf('oldwd <- setwd(%s)', file.path(.libPaths(), 'hdimDB', 'data')),
               readFun, # this should be a STRING that you make above
                        # when you figure out the file extension and
                        # which function is needed for reading in
               'setwd(oldwd) \n')))
    #  If download was successful, the user is notified
    cat(paste0(filename, ' successfully loaded. Downloaded data can be loaded into R by running: data(', name,'.R)'))
    invisible(paste0(name, '.R'))
}

.readSelect <- function(files) { #  Returns string of correct load function when given spacial filename 
    # For example, 
    # if you detected a file named `kauai.bil` you would need a string that says:
    # 'raster(kauai.bil)'
    # if you detected a file called `hawaii_state_geol_ageClean.shp` you would need:
    # readOGR('.', 'hawaii_state_geol_ageClean')
    
    # looks in directory and prioritizes '.bil'
    exts <- mapply(.fileExt, files) #  Finds extensions
    scripts <- mapply(.scriptSelect, files, exts)
    scripts <- scripts[!is.na(scripts)]
    return(scripts)
}

.scriptSelect <- function(f, ext) {
    switch(ext,
          'bil' = paste0("raster(", f, ")"),
          'shp' = paste0("readOGR('.', ", gsub(paste0('.', ext), '', f), ")"))
    # support for addtional extensions to be added
}

.fileExt <- function(path) { #  Retrieves the file extension of a string
    pos <- regexpr("\\.([[:alnum:]]+)$", path)
    ifelse(pos > -1L, substring(path, pos + 1L), "")
}
