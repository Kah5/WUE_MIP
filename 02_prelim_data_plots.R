library(raster)
library(plyr)
library(ggplot2)
library(reshape2)
# read in model outputs that were extracted

ED2.CO2 <- readRDS(file = "Data/ED_montly_CO2.RDS")
ED2.GPP <- readRDS(file = "Data/ED_montly_gpp.RDS")
ED2.gwbi <- readRDS(file = "Data/ED_montly_gwbi.RDS")
ED2.tair <- readRDS(file = "Data/ED_montly_tair.RDS")
ED2.transp <- readRDS(file = "Data/ED_montly_transp.RDS")
ED2.Fire <- readRDS(file = "Data/ED_montly_Fire.RDS")
#test<- raster(ED2.CO2)

ed.co2<- ED2.CO2$CO2
ed.gpp<- ED2.GPP$GPP
ed.gwbi<- ED2.gwbi$GWBI


yr <- "850"
dens1850 <- ed.co2[,,yr]
tab <- melt(dens1850)

#find the indices for lat lons where we have data:

colnames(tab) <- c("lat", "lon", "CO2")
ggplot(tab, aes(x = lon, y = lat, fill = CO2))+geom_raster()+theme_bw()+coord_equal()




lat <- "35.2"
lon <- "-99.8"
CO2 <- ed.co2[1,1,]
tab <- melt(CO2)

# create for lookup
lats <- data.frame(lat = as.numeric(dimnames(ed.co2)$lat),
                   latrow = 1:30)

lons <- data.frame(lon = as.numeric(dimnames(ed.co2)$lon),
                   lonrow = 1:80)

datain <- tab[!is.na(tab$value),]

datain<- merge(datain, lats, by = "lat")
datain <- merge(datain, lons, by = "lon")

# function to extract data from grid cells that have data (there is likely a way to do this better):

extractnonna <-function(datain, x){
  test <- matrix(0,nrow = 13932, ncol=nrow(datain))
  test<- data.frame(test)
  for(i in 1:nrow(datain)){
    test[,i] <- x[datain[i,]$latrow, datain[i,]$lonrow,]
    colnames(test[i]) <- datain[i,]$ID
  }
  test
}

co2 <- extractnonna(datain = datain, x = ed.co2)
GPP <- extractnonna(datain = datain, x = ed.gpp)
GWBI <- extractnonna(datain = datain, x = ed.gwbi)
#co2<- extractnonna(datain = datain, x = ed.co2)

#test <- adply(ED2.CO2$CO2, c(1,2,3))
#plot(ED2.GPP$GPP, ED2.CO2$CO2)
#plot(ED2.gwbi$GWBI, ED2.CO2$CO2)
