#' @title Loads geospacial data files into the kokua package directory
#'  
#' @description \code{haawe} takes the URL of a raw data file and downloads it to the local kokua data directory. If the file is compressed, haawe automatically unzips the file.
#' 
#' @details haawe organizes and centralizes geospacial data for later analysis
#' 
#' @param url The URL of the data to be downloaded
#' @param filename A name the user wishes to assign to the data to be downloaded. Defaults to `NULL`
#' @param extension The file extension of the data to be downloaded
#' 
# @example 
# testURL <- 'http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/niihau.zip' 
# haawe(testURL, 'niihau', '.zip')
#'
#' @return File path string and printed confirmation of successful data download or error message
#'
#' @author Edward G. Huang <edwardgh@@berkeley.edu>
#' @export

haawe <- function(url, filename = NULL, extension){
    dest <- file.path(.libPaths(), 'kokua', 'data') #  Finds path to /data folder in kokua installation directory
    datafile <- paste(filename, extension, sep = '') 
    if (datafile %in% list.files(dest)) { #  Checks if file already exists in target directory
        stop(paste(datafile, 'already exists in target directory', sep = ' ')) 
    } else { #  If file does not exist in target directory, proceed with download
        path <- file.path(dest, datafile)
        download.file(url = url, destfile = path, mode = 'wb') # Downloads to target directory
        if (length(unzip(path, list = TRUE)) > 0) { #  Checks if file is compressed
            unzip(path, exdir = dest) #  Unzips file to same directory and keeps original compressed file
            ## ajr: this looks great for now...if we come across files that aren't compressed we
            ## can deal with that as it arrises
        }
    }
    #  If download was successful, the user is notified
    cat(paste(datafile, 'successfully loaded. Downloaded data can be loaded into R by running: data(', path, ')', sep = ''))
    invisible(path)
}

