## this version of the function uses keyword strings to download data. it needs a place to store
## those keyword strings and their corresponding URLs. For that we can make a exported object
## in the kokua package, something like:
# #' @export
# dataKeys <- data.frame(key = c('geol_niihau', 'geol_kauai'),
#                        url = c('http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/niihau.zip',
#                                'http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/kauai.zip'),
#                        loaded = c(FALSE, FALSE))

## then when the user uses `haawe` to load data, the function will check `dataKeys` to see if those
## data are already loaded, if they aren't the function will load the data and update the `loaded`
## column in `dataKeys`

haawe_pseudo <- function(key) {
    dest <- file.path(.libPaths(), 'kokua', 'data', key) #  Finds path to /data folder in kokua installation directory
    
    ## check if `key` corresponds to data that are already loaded
    ## note: use `grep` for matching the argument `key` to the column `key` in `dataKeys`
    
    keyData <- dataKeys[grep(key, dataKeys$key), ] #  Searches for data corresponding to `key`
    
    ## by using `grep` we can let the user either load, e.g., all geol data (by specifying `key = 'geol'`)
    ## or load a specific data file, e.g., `key = 'geol_niihau'`
    ## if the data aren't all loaded, load them as you've done in the current `haawe` function
    
    unloaded <- keyData[, keyData$unloaded == FALSE] #  Finds unloaded data corresponding to `key`
    mapply(print, mapply(.loadData, unloaded$url, unloaded$key, .fileExt(unloaded$url))) #  Downloads each previously unloaded file with helper function
}

.loadData <- function(url, name = NULL, ext) {
    data <- paste(name, ext, sep = '.') 
    if (data %in% list.files(dest)) { #  Checks if file already exists in target directory
        stop(paste(data, 'already exists in target directory', sep = ' ')) 
    } else { #  If file does not exist in target directory, proceed with download
        if (!file.exists(dest)) { #  If folder does not exist in target directory
            dir.create(dest) #  Creates new folder in the /data directory for file
        }
        path <- file.path(dest, data)
        download.file(url = url, destfile = path, mode = 'wb') #  Downloads to target directory
        if (length(unzip(path, list = TRUE)) > 0) { #  Checks if file is compressed
            unzip(path, exdir = dest) #  Unzips file to same directory and keeps original compressed file
            # file.remove(path) #  Optional cleanup.
        } 
        # if `path` is to a single file, figure out its file extension
        # if `path` is a directory, look inside at what the file extensions are inside the directory
        # (might need to recursively do this...that's something we should talk about on Thursday)

        # once you know the file extensions, figure out which spatial data reading 
        # function (`raster` or `readOGR`) you need and combine this info, with the filename 
        # info into a string that, if read by R, would load the data. 
        # ....now that you have that (save the string object to a variable called `readFun`)....
        readFun <- mapply(.readSelect, list.files(dest, recursive = TRUE)) #  Constructs lists of strings of load functions for each file
        readFun <- readFun[!sapply(readFun, is.null)] #  Drops NULL values
        
        # use `writeLines` to put together (and save to /data) a simple R script that loads to datafile(s), something like:
        
        # mapply(writeLines, readFun, MoreArgs = list(con = file.path(.libPaths(), 'kokua', 'data', paste0(name, '.R'),
        #                                             text = c(sprintf('oldwd <- setwd(%s)', file.path(.libPaths(), 'kokua', 'data')), readFun, # this should be a STRING that you make above
        #                                                                                                                                        # when you figure out the file extension and
        #                                                                                                                                        # which function is needed for reading in
        #                                                                                                  'setwd(oldwd) \n'))))
    }
    #  If download was successful, the user is notified
    cat(paste0(data, ' successfully loaded. Downloaded data can be loaded into R by running: data(', path, ')'))
    invisible(path)
}

.readSelect <- function(file) { #  Returns string of correct load function when given spacial filename 
    # For example, 
    # if you detected a file named `kauai.bil` you would need a string that says:
    # 'raster(kauai.bil)'
    # if you detected a file called `hawaii_state_geol_ageClean.shp` you would need:
    # readOGR('.', 'hawaii_state_geol_ageClean')
    
    ext <- .fileExt(file) #  Finds extension
    script <- switch(ext,
                     'bil' = paste0("raster(", file, ")"),
                     'shp' = paste0("readOGR('.', ", gsub(paste0('.', ext), '', file), ")"))
    # support for addtional extensions to be added
    return(script)
}

.fileExt <- function(path) { #  Retrieves the file extension of a string
    pos <- regexpr("\\.([[:alnum:]]+)$", path)
    ifelse(pos > -1L, substring(path, pos + 1L), "")
}