# reproject PRISM data from 4km to the paleon lat long grid

library(raster)
library(rgdal)
library(dplyr)


years = seq(1895,2016) 
variables = c('ppt')
months = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
format = 'tif'
toRaster <- raster("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/paleon.unit.ll_01.tif")
outdir <- '/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/'

  
# for ppt:
  inpath <- "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/ppt/"
  
  in_files <- paste0("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/ppt/", list.files(path = "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/ppt/",pattern=paste(".*_",".*\\.bil$", sep = "")))
  
  in_files.short <- list.files(path = "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/ppt/",pattern=paste(".*_",".*\\.bil$", sep = ""))
  
  out_files <- paste0(outdir, variables, '/PRISM_', variables, '_stable_paleonll_', substring(in_files.short, 24, 27), substring(in_files.short, 28, 29))
  
  test_run <- data.frame(data.in = in_files, data.out = out_files) 
  
  reproject <- function(x) {writeRaster(projectRaster(from = raster(as.character(x["data.in"])), to = toRaster),
                                        filename = paste0(x["data.out"], '.', format), overwrite=T)}
  
  system.time(apply( test_run, MARGIN = 1, reproject ))

# for Tmax:
  variables<- c("tmax")
 
  inpath <- "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/tmax/"
  
  in_files <- paste0("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/tmax/", list.files(path = "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/tmax/",pattern=paste(".*_",".*\\.bil$", sep = "")))
  
  in_files.short <- list.files(path = "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/tmax/",pattern=paste(".*_",".*\\.bil$", sep = ""))
  
  out_files <- paste0(outdir, variables, '/PRISM_', variables, '_stable_paleonll_', substring(in_files.short, 25, 28), substring(in_files.short, 29, 30))
  
  test_run <- data.frame(data.in = in_files, data.out = out_files) 
  #test_run<- test_run[1:2,]
  reproject <- function(x) {writeRaster(projectRaster(from = raster(as.character(x["data.in"])), to = toRaster),
                                        filename = paste0(x["data.out"], '.', format), overwrite=T)}
  
  system.time(apply( test_run, MARGIN = 1, reproject ))

# for Tmean
  variables<- c("tmean")
  
  in_files <- paste0("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/tmean/", list.files(path = "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/tmean/",pattern=paste(".*_",".*\\.bil$", sep = "")))
  in_files.short <- list.files(path = "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/Climate/tmean/",pattern=paste(".*_",".*\\.bil$", sep = ""))
  
  out_files <- paste0(outdir, variables, '/PRISM_', variables, '_stable_paleonll_', substring(in_files.short, 26, 29), substring(in_files.short, 30, 31))
  
  test_run <- data.frame(data.in = in_files, data.out = out_files) 
  #test_run<- test_run[1:2,]
  reproject <- function(x) {writeRaster(projectRaster(from = raster(as.character(x["data.in"])), to = toRaster),
                                        filename = paste0(x["data.out"], '.', format), overwrite=T)}
  
  system.time(apply( test_run, MARGIN = 1, reproject ))
  



