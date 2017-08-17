dataKeys <- data.frame(key = c('geol_niihau', 'geol_kauai'),
                       url = c('http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/niihau.zip',
                               'http://gis.ess.washington.edu/data/raster/tenmeter/hawaii/kauai.zip'),
                       loaded = c(FALSE, FALSE))

save(dataKeys, file = file.path(.libPaths(), 'hdimDB', 'data', 'dataKeys.RData')) #  Updates dataKeys file to reflect newly loaded data

haawe('geol')

library(raster)
data(geol_niihau, package = 'hdimDB')
