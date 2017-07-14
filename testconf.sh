# kokua R CRAN data package installation shell script

#! /bin/sh
## sources single file from gitHub repository hawaiiDimensions/kokua
## need to look into options to download entire sub-directory from kokua.
wget -O ~/kokuaData "https://github.com/hawaiiDimensions/kokua/blob/master/data/hawaii_state_geol_ageClean.RData?raw=true"
