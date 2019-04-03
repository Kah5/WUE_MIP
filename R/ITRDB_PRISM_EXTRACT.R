# Script to pull climate data from 8km aggregated prism data and extract the time series needed for the ITRDB sites:
library(raster)
library(sp)
library(tidyr)
# read in the ITRDB file
rwl.age.ll <- readRDS( "Data/ITRDB/rwl.ages.df.nona_spatial.rds")

# get the LatLon for the grid cell:
toRaster <- "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/paleon.unit.ll_01.tif"
paleon.ll <- raster("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/paleon.unit.ll_01.tif")
lat.lon <- raster::extract(paleon.ll, rwl.age.ll[,c("Longitude", "Latitude")], cellnumber = TRUE, df = TRUE)
y <- data.frame(rasterToPoints(paleon.ll))
xy <- xyFromCell(paleon.ll, cell = lat.lon$cells)
lat.lon$x <- xy[,"x"]
lat.lon$y <- xy[,"y"]
lat.lon$Longitude <- rwl.age.ll$Longitude
lat.lon$Latitude <- rwl.age.ll$Latitude

lat.lon.uni <- unique(lat.lon[, c("x", "y")])

# --------------Extract Climate Data---------------------
workingdir <- "/Users/kah/Documents/WUE_MIP/WUE_MIP"
# ------------------------------- 1. Maximum Temperature ----------------------------------------------------
sample.yrs <- 1895:2015
# A: Monthly temperatures for the full range
setwd("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/tmax")

filenames <- list.files(pattern=paste(".tif$", sep = ""))
#filenames <- filenames [substring(filenames, first = 26, last = 29) %in% sample.yrs]# use substring to index filenames that match the years designated:

extract.ll.Tmax <- function(x){
        s <- raster(x) #make all into a raster
        df.s <- data.frame(rasterToPoints(s))
        # extract data for lat long:
        df.select <- dplyr::left_join(df.s, lat.lon.uni, by = c("x", "y"))
        colnames(df.select) <- c("x", "y", "Tmax")
        df.select$year <- substring(x, 28, 31)
        df.select$month <- substring(x, 32, 33)
        cat(".")
        df.select
        
} 


files <- data.frame(filenames)
# should take ~ 10 minutes to run for 1400 files in our dataset
system.time(tmax.list <- apply(as.data.frame(files), MARGIN= 1, FUN=extract.ll.Tmax))

tmax.itrdb.df <- do.call(rbind, tmax.list)

setwd("/Users/kah/Documents/WUE_MIP/WUE_MIP/")
saveRDS(tmax.itrdb.df, paste0("Data/ITRDB/PRISM/tmax/Tmax_1895_2016_extracted_ITRDB.rds"))

# ------------------------------- 1. Monthly Precip ----------------------------------------------------

# A: Monthly Precipitation for the whole US
setwd("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/ppt")

filenames <- list.files(pattern=paste(".tif$", sep = ""))
#filenames <- filenames [substring(filenames, first = 26, last = 29) %in% sample.yrs]# use substring to index filenames that match the years designated:

extract.ll.ppt <- function(x){
  s <- raster(x) #make all into a raster
  df.s <- data.frame(rasterToPoints(s))
  # extract data for lat long:
  df.select <- dplyr::left_join(df.s, lat.lon.uni, by = c("x", "y"))
  colnames(df.select) <- c("x", "y", "ppt")
  df.select$year <- substring(x, 27, 30)
  df.select$month <- substring(x, 31, 32)
  cat(".")
  df.select
  
} 


files <- data.frame(filenames)
# should take ~ 10 minutes to run for 1400 files in our dataset
system.time(ppt.list <- apply(as.data.frame(files), MARGIN= 1, FUN=extract.ll.ppt))

ppt.itrdb.df <- do.call(rbind, ppt.list)


write.csv(ppt.itrdb.df, paste0("/Data/ITRDB/PRISM/ppt/ppt_1895_2016_extracted_ITRDB.csv"))
