library(plotrix)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
devtools::install_github('ajrominger/socorro')
library(socorro)

## =========================
## chrono, elev, precip maps
## =========================

## set-up colors
rain.colors <- colorRampPalette(brewer.pal(9, 'YlGnBu'))
chrono.colors <- colorRampPalette(hsv(c(0.1, 0, 0.05), c(0.7, 0.9, 0.3), c(1, 0.8, 0.3)))
elev.colors <- colorRampPalette(gray(c(0, 0.1, 10^seq(-0.75, 0, length = 10))))


## get chrono and dimensions sites loaded
eval(parse(text=c('{', readLines('~/Dropbox/hawaiiDimensions/geoData/maps/sites_map.R', n=35), '}')))

age.unit <- rep('mya', length(hi.geo.poly$AGE_RANGE))
age.unit[grep('A.D.', hi.geo.poly$AGE_RANGE)] <- 'yr'
age.unit[grep('yr', hi.geo.poly$AGE_RANGE)] <- 'ya'

age.val <- as.character(hi.geo.poly$AGE_RANGE)
age.val <- gsub(' to ', '-', age.val)
age.val <- gsub('A.D.|[[:alpha:]]| |,|/|>', '', age.val)
age.val[age.val == ''] <- NA
age.val <- sapply(strsplit(age.val, '-'), function(x) mean(as.numeric(x)))
age.val[age.unit == 'ya'] <- age.val[age.unit == 'ya'] / 10^6
age.val[age.unit == 'yr'] <- (2015 - age.val[age.unit == 'yr']) / 10^6

## get precip
setwd('~/Dropbox/hawaiiDimensions/geodata/site_selection/StateRFGrids_mm2/staterf_mmann')
precip <- raster('w001001.adf')
precipP <- rasterToPolygons(cut(precip, breaks = 20), dissolve = TRUE)
precipP <- spTransform(precipP, CRS(proj4string(islands)))

## get elevation
setwd('~/Dropbox/hawaiiDimensions/geoData/site_selection/elrange_n83')
elev <- readOGR('.', 'elrange_n83')
elev <- spTransform(elev, CRS(proj4string(islands)))


## get puu makaala and focal region boundaries
puum <- SpatialPoints(matrix(c(-155.241361, 19.506630), nrow = 1), 
                      CRS('+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'))
bigI <- matrix(c(-155.4, -155.14, -155.14, -155.4,
                 19.595, 19.595, 19.32, 19.32), ncol = 2)
bigI <- SpatialPolygons(
    Srl = list(Polygons(srl = list(Polygon(bigI)), ID = 0)), 
    pO = 1L, 
    proj4string = CRS('+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'))


## =========================
## plot maps and site layout
## =========================


setwd('~/Dropbox/Research/grants/macrosystems/figs')

jpeg(filename='fig_map.jpg', width=10, height=12, units='in', res=400)
close.screen(all.screens = TRUE)

split.screen(figs=matrix(c(0, 1, 0.6, 1,
                           0, 1, 0.3, 0.7,
                           0, 1, 0, 0.4), nrow = 3, byrow = TRUE), erase=FALSE)


## chrono age

par(mar=rep(0, 4), bg = 'transparent')
plot(islands, col = 'gray30')
plot(hi.geo.poly[!is.na(age.val), ],
     col = chrono.colors(15)[hi.geo.poly$AGE_GROUP[!is.na(age.val)] + 1],
     border = chrono.colors(15)[hi.geo.poly$AGE_GROUP[!is.na(age.val)] + 1],
     add = TRUE)
plot(islands, add = TRUE)

plot(sites, add=TRUE, pch=21, col='white', bg='gray')
plot(spTransform(puum, CRS(proj4string(islands))), add=TRUE, pch=24, col='white', bg='black', cex = 2)

# plot(spTransform(bigI, CRS(proj4string(islands))), add = TRUE, lwd = 3, border = 'white')
# plot(spTransform(bigI, CRS(proj4string(islands))), add = TRUE, lwd = 1)

split.screen(figs=matrix(c(0, 0.5, 0.4, 0.7), nrow=1), erase=FALSE)
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA, bg = 'transparent')
plot(1, xlim=c(min(age.val, na.rm = TRUE), 6.4), ylim=c(0, 1.5), log='x',type='n',
     axes = FALSE, xlab='Substrate age (My)', ylab='', xaxs='i', yaxs='i')

a <- seq(par('usr')[1], par('usr')[2], length = 21)
rect(xleft = 10^a[-1], xright = 10^a[-21], ybottom = 0, ytop = 0.6, col = chrono.colors(20), border = NA)
rect(10^min(a), 0, 10^max(a), 0.6)

logAxis(1, labels=FALSE)
axis(side=1, at=c(10^(-4:0), 6))

close.screen(4)


## elevation

screen(2, new = FALSE)
par(mar=rep(0, 4), bg = 'transparent')
plot(islands, col = 'black')
col.elev <- elev.colors(length(unique(rowSums(elev@data[, c('LOWELEV', 'HIGHELEV')]))))
plot(elev, col = col.elev[as.numeric(as.factor(rowSums(elev@data[, c('LOWELEV', 'HIGHELEV')]) / 2))],
     border = col.elev[as.numeric(as.factor(rowSums(elev@data[, c('LOWELEV', 'HIGHELEV')]) / 2))],
     add = TRUE)
plot(islands)

plot(sites, add=TRUE, pch=21, col='white', bg='gray')
plot(spTransform(puum, CRS(proj4string(islands))), add=TRUE, pch=24, col='white', bg='black', cex = 2)

# plot(spTransform(bigI, CRS(proj4string(islands))), add = TRUE, lwd = 3, border = 'white')
# plot(spTransform(bigI, CRS(proj4string(islands))), add = TRUE, lwd = 1)

split.screen(figs=matrix(c(0, 0.5, 0.4, 0.7), nrow=1), erase=FALSE)
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA, bg = 'transparent')
plot(1, xlim=c(0, max(elev$HIGHELEV) * 0.3048), ylim=c(0, 1.5),type='n',
     axes = FALSE, xlab='Elevation (m)', ylab='', xaxs='i', yaxs='i')

a <- seq(par('usr')[1], par('usr')[2], length = 21)
rect(xleft = a[-1], xright = a[-21], ybottom = 0, ytop = 0.6, col = elev.colors(20), border = NA)
rect(min(a), 0, max(a), 0.6)
axis(1)

close.screen(4)


## precip

screen(3, new = FALSE)
par(mar=rep(0, 4), bg = 'transparent')
plot(islands)
plot(precipP, col = rain.colors(20)[precipP$layer], border = rain.colors(20)[precipP$layer], add = TRUE)
plot(islands)

plot(sites, add=TRUE, pch=21, col='white', bg='gray')
plot(spTransform(puum, CRS(proj4string(islands))), add=TRUE, pch=24, col='white', bg='black', cex = 2)

# plot(spTransform(bigI, CRS(proj4string(islands))), add = TRUE, lwd = 3, border = 'white')
# plot(spTransform(bigI, CRS(proj4string(islands))), add = TRUE, lwd = 1)

split.screen(figs=matrix(c(0, 0.5, 0.4, 0.7), nrow=1), erase=FALSE)
par(mar=c(2, 1, 0, 1) + 1, mgp=c(2,0.5,0), xpd=NA, bg = 'transparent')
plot(1, xlim=range(values(precip), na.rm = TRUE), ylim=c(0, 1.5),type='n',
     axes = FALSE, xlab='Annual precipitation (mm)', ylab='', xaxs='i', yaxs='i')

a <- seq(par('usr')[1], par('usr')[2], length = 21)
rect(xleft = a[-1], xright = a[-21], ybottom = 0, ytop = 0.6, col = rain.colors(20), border = NA)
rect(min(a), 0, max(a), 0.6)
axis(1)

close.screen(4)

dev.off()
close.screen(all.screens = TRUE)


## =======================
## map of one focal region
## =======================

bigI <- matrix(c(-155.4, -155.14, -155.14, -155.4,
                 19.595, 19.595, 19.28, 19.28), ncol = 2)
bigI <- SpatialPolygons(
    Srl = list(Polygons(srl = list(Polygon(bigI)), ID = 0)), 
    pO = 1L, 
    proj4string = CRS('+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs'))



pdf('fig_submap.pdf', width = 4, height = 4)
plot(spTransform(bigI, CRS(proj4string(islands))))
plot(precipP, col = rain.colors(20)[precipP$layer], border = rain.colors(20)[precipP$layer], add = TRUE)
plot(elev, add = TRUE)
plot(spTransform(puum, CRS(proj4string(islands))), add=TRUE, pch=24, col='white', bg='black', cex = 2)
dev.off()
