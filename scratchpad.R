library(RCurl)

foo <- proc.time()
dataURL <- "https://github.com/hawaiiDimensions/geodata/raw/master/test.RData"
load(url(dataURL))
repoTime <- proc.time() - foo

setwd("/Users/EdwardH/Dropbox/hawaiiDimensions/geodata")
foo <- proc.time()
load("~/Dropbox/hawaiiDimensions/geodata/test.RData")
localTime <- proc.time() - foo

repoTime - localTime 
# user  system elapsed 
# 0.097   0.270  21.785 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## sourced from https://gist.github.com/holstius/6631918
read.kml <- function(file, layers) {
    require(sp)
    require(rgdal)
    read.layer <- function (layer_name) {
        spobj <- rgdal::readOGR(dsn=file, layer=layer_name)
        coords <- coordinates(spobj)
        colnames(coords) <- c('x', 'y', 'z')[1:ncol(coords)]
        df <- data.frame(coords, spobj@data)
        transform(df, layer=layer_name)
    }
    Reduce(rbind, lapply(layers, read.layer))
}

write.kml <- function (spobj, dsn, layer, var.name, col=bpy.colors(20)) {
    require(maptools)
    dir.create(dsn)
    old_wd <- setwd(dsn)
    ge_grid <- GE_SpatialGrid(spobj)
    layer <- str_replace(layer, ".kml$", "")
    png(
        file = str_c(layer, ".png"),
        width = ge_grid$width,
        height = ge_grid$height, 
        bg = "transparent"
    )
    par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    image(spobj, var.name)
    dev.off()
    kml <- kmlOverlay(ge_grid, str_c(layer, ".kml"), str_c(layer, ".png"))
    setwd(old_wd)
    return(kml)
}
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Loading Files ##
# https://geoscripting-wur.github.io/IntroToRaster/
download.file(url = githubURL, destfile = 'hdim_sites.kml', method = 'auto')

kmlURL <- "https://github.com/hawaiiDimensions/geodata/raw/master/sites/dimensions_plots.kml"
readRDS(url(kmlURL))

library(RCurl)
source(getURLContent(kmlURL))

raster(getURLContent(rasterURL)) # ?

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# readings
# http://neondataskills.org/R/Raster-Data-In-R/
# https://science.nature.nps.gov/im/datamgmt/statistics/r/fundamentals/index.cfm
# http://www.image.ucar.edu/GSP/Software/Netcdf/
# 
