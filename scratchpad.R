library(RCurl)

foo <- proc.time()
dataURL <- "https://github.com/hawaiiDimensions/kokua/raw/master/data/islands.RData"
load(url(dataURL))
repoTime <- proc.time() - foo

setwd("/Users/EdwardH/Dropbox/hawaiiDimensions/kokua/data")
foo <- proc.time()
load("~/Dropbox/hawaiiDimensions/kokua/data/islands.RData")
localTime <- proc.time() - foo

repoTime - localTime 