## this version of the function uses keyword strings to download data. it needs a place to store
## those keyword strings and their corresponding URLs.  For that we can make a exported object
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
    dest <- file.path(.libPaths(), 'kokua', 'data') #  Finds path to /data folder in kokua installation directory
    
    ## check if `key` corresponds to data that are already loaded
    ## note: use `grep` for matching the argument `key` to the column `key` in `dataKeys`
    
    keyData <- dataKeys[grep(key, dataKeys$key), ] #  Searches for data corresponding to `key`
    
    ## by using `grep` we can let the user either load, e.g., all geol data (by specifying `key = 'geol'`)
    ## or load a specific data file, e.g., `key = 'geol_niihau'`
    ## if the data aren't all loaded, load them as you've done in the current `haawe` function
    
    unloaded <- keyData[, keyData$unloaded == FALSE] #  Finds unloaded data corresponding to `key`
    return (mapply(.loadData, unloaded$url, unloaded$key, file_ext(unloaded$url))) #  Downloads each previously unloaded file with helper function
    
    ## in order to make loading the data easier, the function should also write an R script for each 
    ## datafile it loads.  Having this R script will allow the user to just run `data(script_name)` 
    ## in order to load the data. The R script for each datafile should look something like this:
    # object_name <- readOGR(datafile) # ...or...
    # object_name <- raster(datafile)  # depending on the file extension of the data in native format
}

.loadData <- function(url, filename = NULL, extension){
    datafile <- paste(filename, extension, sep = '.') 
    if (datafile %in% list.files(dest)) { #  Checks if file already exists in target directory
        stop(paste(datafile, 'already exists in target directory', sep = ' ')) 
    } else { #  If file does not exist in target directory, proceed with download
        path <- file.path(dest, datafile)
        download.file(url = url, destfile = path, mode = 'wb') # Downloads to target directory
        if (length(unzip(path, list = TRUE)) > 0) { #  Checks if file is compressed
            unzip(path, exdir = dest) #  Unzips file to same directory and keeps original compressed file
        }
    }
    #  If download was successful, the user is notified
    cat(paste(datafile, 'successfully loaded. Downloaded data can be loaded into R by running: data(', filename, ')', sep = ''))
    
    
    
    invisible(path)
}

.file_ext <- function(path) {
    pos <- regexpr("\\.([[:alnum:]]+)$", path)
    ifelse(pos > -1L, substring(path, pos + 1L), "")
}
