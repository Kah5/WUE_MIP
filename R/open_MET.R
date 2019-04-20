library(ncdf4)
library(abind)
library(dplyr)
library(tidyr)
library(reshape2)

# script to read in daily met drivers and summarise them by month 
# this is mostly to have met data to correlate with linkages, but we can also compare to outputs from guess & ed
# this is based off some helpful code from Christy Rollinson here: https://github.com/PalEON-Project/MIP_Utils/tree/master/Phase2_region/met_utils


#------------read in daily precip driver data and summarise by monthly---------------------
# open driver data & combine:
basedir <- "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/MET/"
files <- list.files(paste0(basedir, "precipf"))
nchar.var  <- nchar("precipf")
yrs       <- as.numeric(unique(substr(files, nchar.var+2, nchar.var+5)))
files[1]

# create a function to open the files and summarise, then we will lapply 
open.summarise.precipf <- function(files){
  temp.name  <- strsplit(files,"_")
  year <- as.numeric(temp.name[[1]][2])
  mon  <- as.numeric(substring(temp.name[[1]][3],1,2))
  print(year)
  
  nc.file <- nc_open(paste(basedir,"precipf","/",files,sep=""))
  data.df <- ncvar_get(nc.file, "precipf")
  time.df <- ncvar_get(nc.file, "time" )    
  lat     <- ncvar_get(nc.file, "lat" )
  lon     <- ncvar_get(nc.file, "lon" )
  nc_close(nc.file)
  dimnames(data.df) <- list( lon, lat,time.df)
  
  data.df <- melt(data.df)
  colnames(data.df) <- c("lon", "lat", "days_since_850", "precipf")
  data.df$month <- mon
  data.df$year <- year

  # summarise to monthly
  data.summarised <- data.df %>% group_by(lon, lat, month, year) %>% dplyr::summarise(precipf_sum = sum(precipf, na.rm =TRUE),
                                                                                      precipf_mean = mean(precipf, na.rm =TRUE))
  
  data.summarised
}


precip.summaries <- lapply(files, open.summarise.precipf)
precip.summaries.df <- do.call(rbind, precip.summaries)
summary(precip.summaries.df)
# save dat.new
saveRDS(precip.summaries.df,"/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/MET/precipf.summary.rds")

#------------read in daily temperature driver data and summarise by monthly---------------------
# open driver data & combine:
basedir <- "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/MET/"
files <- list.files(paste0(basedir, "tair"))
nchar.var  <- nchar("tair")
yrs       <- as.numeric(unique(substr(files, nchar.var+2, nchar.var+5)))


# create a function to open the files and summarise, then we will lapply 
open.summarise.tair <- function(files){
  temp.name  <- strsplit(files,"_")
  year <- as.numeric(temp.name[[1]][2])
  mon  <- as.numeric(substring(temp.name[[1]][3],1,2))
  print(year)
  
  nc.file <- nc_open(paste(basedir,"tair","/",files,sep=""))
  data.df <- ncvar_get(nc.file, "tair")
  time.df <- ncvar_get(nc.file, "time" )    
  lat     <- ncvar_get(nc.file, "lat" )
  lon     <- ncvar_get(nc.file, "lon" )
  nc_close(nc.file)
  dimnames(data.df) <- list( lon, lat, time.df)
  
  data.df <- melt(data.df)
  colnames(data.df) <- c("lon", "lat", "days_since_850", "tair")
  data.df$month <- mon
  data.df$year <- year
  # summarise to monthly
  data.summarised <- data.df %>% group_by(lon, lat, month, year) %>% dplyr::summarise(tair_mean = mean(tair, na.rm =TRUE),
                                                                                      tair_max = max(tair, na.rm=TRUE),
                                                                                      tair_min = min(tair, na.rm=TRUE))
  
  data.summarised
}


tair.summaries <- lapply(files, open.summarise.tair)
tair.summaries.df <- do.call(rbind, tair.summaries)
summary(tair.summaries.df)
# save dat.new
saveRDS(tair.summaries.df,"/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/MET/tair.summary.rds")



