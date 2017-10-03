oldwd <- setwd("/Library/Frameworks/R.framework/Versions/3.4/Resources/library/hdimDB/data")
islands <- readOGR('.', islands/islands)
setwd(oldwd) 

