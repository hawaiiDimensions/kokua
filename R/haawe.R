#' @title Loads geospacial data files into the kokua package directory
#'  
#' @description \code{haawe} takes the URL of a raw data file and downloads it to the local kokua data directory. If the file is compressed, haawe automatically unzips the file.
#' 
#' @details haawe organizes and centralizes geospacial data for later analysis
#' 
#' @param url The URL of the data to be downloaded
#' @param name A name the user wishes to assign to the data to be downloaded. Defaults to `NULL`
#' @param ext The file extension of the data to be downloaded
#' 
# @example 
# testURL <- 'http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/niihau.zip' 
# haawe(testURL, 'niihau', '.zip')
#'
#' @return File path string and printed confirmation of successful data download or error message
#'
#' @author Edward G. Huang <edwardgh@@berkeley.edu>
#' @export

haawe <- function(url, name = NULL, ext) {
    dest <- file.path(.libPaths(), 'kokua', 'data', name) #  Finds path to /data folder in kokua installation directory
    datafile <- paste(name, ext, sep = '') 
    if (datafile %in% list.files(dest)) { #  Checks if file already exists in target directory
        stop(paste(datafile, 'already exists in target directory', sep = ' ')) 
    } else { #  If file does not exist in target directory, proceed with download
        if (!file.exists(dest)) { #  If folder does not exist in target directory
            dir.create(dest) # Creates new folder in the /data directory for file
        }
        path <- file.path(dest, datafile)
        download.file(url = url, destfile = path, mode = 'wb') # Downloads to target directory
        if (length(unzip(path, list = TRUE)) > 0) { #  Checks if file is compressed
            unzip(path, exdir = dest) #  Unzips file to same directory and keeps original compressed file
            file.remove(path)
        }
        # if `path` is to a single file, figure out its file extension
        
        # if `path` is a directory, look inside at what the file extensions are inside the directory 
        # (might need to recursively do this...that's something we should talk about on Thursday)
        # once you know the file extensions, figure out which spatial data reading 
        # function (`raster` or `readOGR`) you need and combine this info, with the filename 
        # info into a string that, if read by R, would load the data.  
        # 
        # For example, 
        # if you detected a file named `kauai.bil` you would need a string that says:
        # 'raster(kauai.bil)'
        # if you detected a file called `hawaii_state_geol_ageClean.shp` you would need:
        # readOGR('.', 'hawaii_state_geol_ageClean')
        # ....now that you have that (save the string object to a variable called `readFun`)....
        # use `writeLines` to put together (and save to /data) a simple R script that loads to datafile(s), something like:
        # writeLines(con = file.path(.libPaths(), 'kokua', 'data', paste0(filename, '.R'), 
        #                            text = c(sprinftf('oldwd <- setwd(%s)', 
        #                                              file.path(.libPaths(), 'kokua', 'data')), 
        #                                     readFun, # this should be a STRING that you make above
        #                                              # when you figure out the file extension and
        #                                              # which function is needed for reading in
        #                                     'setwd(oldwd) \n'))
        
    }
    #  If download was successful, the user is notified
    cat(paste(datafile, ' successfully loaded. Downloaded data can be loaded into R by running: data(', path, ')', sep = ''))
    invisible(path)
}

.file_ext <- function(path) {
    pos <- regexpr("\\.([[:alnum:]]+)$", path)
    ifelse(pos > -1L, substring(path, pos + 1L), "")
}