library(raster)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(zoo)

load("Data/PalEON_siteInfo_all.RData")
# read in model outputs that were extracted

files <- list.files("Data/ED2/")

for(i in 1:length(files)){
assign(x = unlist(strsplit(files[i],split = '.rds')), value = readRDS(paste0("Data/ED2/",files[i])))
}

ED2.CO2 <- readRDS(file ="Data/ED2/ED2.CO2.rds")
ED2.GPP <- readRDS(file = "Data/ED2/ED2.GPP.rds")
ED2.gwbi <- readRDS(file = "Data/ED2/ED2.gwbi.RDS")
#ED2.tair <- readRDS(file = "Data/ED2/ED2")
ED2.qair <- readRDS(file = "Data/ED2/ED2.qair.rds")
ED2.transp <- readRDS(file = "Data/ED2/ED2.transp.rds")
ED2.Fire <- readRDS(file = "Data/ED2/ED2.Fire.rds")
ED2.LAI <- readRDS(file = "Data/ED2/ED2.LAI.rds")
#ED2.evap <- readRDS(file = "Data/ED2/ED2.evap.rds")
#ED2.npp <- readRDS(file = "Data/ED2/ED2.npp.rds")
#ED2.bai <- readRDS(file = "Data/ED2/ED2.BA.rds")
ED2.precipf <- readRDS(file = "Data/ED2/ED2.precipf.rds")

# laod pft specific variables:
#ED2.BA <- readRDS(file = "Data/ED_monthly_BA_nona.RDS")
#ED2.dens <- readRDS(file = "Data/ED_monthly_Dens_nona.RDS")
#ED2.Fcomp <- readRDS(file = "Data/ED_monthly_Fcomp_nona.RDS")

# this converts them into arrays, there is likely an easier way, but using array(unlist(L), dim = c(nrow(L[[1]]), ncol(L[[1]]), length(L))) takes too long
#ed.co2 <- ED2.CO2$CO2
#ed.gpp <- ED2.GPP$GPP
#ed.gwbi <- ED2.gwbi$GWBI
#ed.tair <- ED2.tair$tair
#ed.transp <- ED2.transp$Transp
#ed.fire <- ED2.Fire$Fire
#ed.LAI <- ED2.LAI$LAI
#ed.evap <- ED2.evap$Evap
#ed.qair <- ED2.qair$qair
#ed.precip <- ED2.precipf$precipf
#ed.bai <- ED2.bai$BAI


#ed.Fcomp <- ED2.Fcomp$Fcomp
#ed.Dens <- ED2.dens$Dens
#ed.BA <- ED2.BA$BA

# ----------------get lats and long dimensions----------------------
#yr <- "850"
#co2850 <- ed.co2[,,yr]
#tab <- melt(co2850)

#find the indices for lat lons where we have data:


#colnames(tab) <- c("lat", "lon", "CO2")
ggplot(paleon, aes(x = lon, y = lat, fill = notes))+geom_raster()+theme_bw()+coord_equal()


# create a dataframe with lat and lon values for lookup later
#lats <- data.frame(lat = as.numeric(dimnames(ed.co2)$lat),
 #                  latrow = 1:30)

#lons <- data.frame(lon = as.numeric(dimnames(ed.co2)$lon),
 #                  lonrow = 1:80)

# only get the grid cells that the model has been run at
#datain <- tab[!is.na(tab$value),]

#datain<- merge(datain, lats, by = "lat")
#datain <- merge(datain, lons, by = "lon")
#datain$ID <- 1:40 
#datain$site.name <- paste0("site", datain$ID)


# make a simple reference map of where whe have grid cells and their names:
states <- map_data("state")
states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
states <- spTransform(states,CRSobj = '+init=epsg:4326')
mapdata <- data.frame(states)

png(height = 8, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/ED_grid_map.png"))
ggplot(datain, aes(x = lon, y = lat))+geom_raster()+geom_text(aes(label=site.name),hjust=0, vjust=0)+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60))
dev.off()

write.csv(datain, paste0(getwd(), "/Data/ED_site_list_lat_lon.csv"))
# function to extract data from grid cells that have data (there is likely a way to do this better):

extractnonna <-function(datain, x){
  test <- matrix(0,nrow = 13932, ncol=nrow(datain))
  test<- data.frame(test)
  for(i in 1:nrow(datain)){
    test[,i] <- x[datain[i,]$latrow, datain[i,]$lonrow,]
    colnames(test[i]) <- datain[i,]$site.name
  }
  
  colnames(test) <- datain$site.name
  
  # get the years listed for ED monthly runs
  timevec <- attributes(ed.co2)$dimnames$time
  test$Yearmo <- timevec
  test  <- separate(data = test, col = Yearmo, into = c("Year", "mo"), sep = "\\.") # separate the year and month
  test[is.na(test$mo),]$mo <- 0
  
  # now convert the decimals into  months 1:12:
  time.match <- data.frame(mo = unique(test$mo), Month = 1:12)
  test <- merge(test, time.match, by = "mo")
  test
}


CO2 <- extractnonna(datain = datain, x = ed.co2)
GPP <- extractnonna(datain = datain, x = ed.gpp)
GWBI <- extractnonna(datain = datain, x = ed.gwbi)
tair <- extractnonna(datain = datain, x = ed.tair)
qair <- extractnonna(datain = datain, x = ed.qair)
transp <- extractnonna(datain = datain, x = ed.transp)
fire <- extractnonna(datain = datain, x = ed.fire)
lai <- extractnonna(datain = datain, x = ed.LAI)
evap <- extractnonna(datain = datain, x = ed.evap)
precip <- extractnonna(datain = datain, x = ed.precip)
bai <- extractnona(datain = datain, x = ed.bai)
#soilmoist <- extractnonna(datain = datain, x = ed.soilmoist)

#------------------------------------------------ preliminary plots:--------------------------------------------

# plot the seasonal cycle for each variable at each site and save in outputs/preliminaryplots
plot.seasonal <- function(df, name){
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/ED2_", name, "_seasonal_site.png"))
  m <- melt(df, id.vars=c("Year", "Month", "mo"))
  print(ggplot(data = m, aes(x = Month, y = value))+geom_point()+facet_wrap(~variable,  ncol = 5))
  dev.off()
}


plot.seasonal(GPP, "GPP")
plot.seasonal(GWBI, "GWBI")
plot.seasonal(tair, "Tair")
plot.seasonal(transp, "Transp")
plot.seasonal(fire, "Fire")
plot.seasonal(lai, "LAI")
plot.seasonal(evap, "Evap")
plot.seasonal(qair, "Qair")
#plot.seasonal(soilmoist, "SoilMoist")
plot.seasonal(precip, "Precipf")
# calculate the means for the years:

plot.yrmean.ts <- function(df, name){
  m <- melt(df, id.vars=c("Year", "Month", "mo"))
  yrmeans<-dcast(m, Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/ED2_", name, "_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = Year, y = value, color = variable))+geom_line())
  dev.off()
}

plot.yrmean.ts(GPP, "GPP")
plot.yrmean.ts(GWBI, "GWBI")
plot.yrmean.ts(tair, "Tair")
plot.yrmean.ts(qair, "Qair")
plot.yrmean.ts(transp, "Transp")
plot.yrmean.ts(fire, "Fire")
plot.yrmean.ts(lai, "LAI")
plot.yrmean.ts(evap, "Evap")
plot.yrmean.ts(CO2, "CO2")
plot.yrmean.ts(precip, "Precip")
#plot.yrmean.ts(soilmoist, "SoilMoist")

# plot and calulate only summer JJA yearly values:

plot.JJA.ts <- function(df, name){
  m <- melt(df, id.vars=c("Year", "Month", "mo"))
  yrmeans<-dcast(m[m$Month %in% c(6,7,8),], Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/ED2_", name, "_JJA_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = Year, y = value, color = variable))+geom_line())
  dev.off()
}


plot.JJA.ts (GPP, "GPP")
plot.JJA.ts (GWBI, "GWBI")
plot.JJA.ts (tair, "Tair")
plot.JJA.ts (qair, "Qair")
plot.JJA.ts (transp, "Transp")
plot.JJA.ts (fire, "Fire")
plot.JJA.ts (lai, "LAI")
plot.JJA.ts (evap, "Evap")
plot.JJA.ts (CO2, "CO2")
plot.JJA.ts(precip, "Precip")
#plot.JJA.ts(soilmoist, "SoilMoist")
# plot moving averages in ggplot:
#rollmean(, 50, fill=0)
# work on finding the total annual gpp for each year 
# also find the summer gpp and GWBI?



# ---------------------saving as CSV or RDS files for WUE calculations-----------------
saveRDS(GPP, "Data/extracted/ED_monthly_GPP.RDS")
#saveRDS(NPP, "Data/extracted/ED_monthly_NPP.RDS")
saveRDS(CO2, "Data/extracted/ED_monthly_CO2.RDS")
saveRDS(GWBI, "Data/extracted/ED_monthly_GWBI.RDS")
saveRDS(tair, "Data/extracted/ED_monthly_tair.RDS")
saveRDS(qair, paste0(getwd(),"/Data/extracted/ED_monthly_qair.RDS"))
saveRDS(precip, paste0(getwd(),"/Data/extracted/ED_monthly_precip.RDS"))
#saveRDS(soilmoist, paste0(getwd(),"/Data/extracted/ED_monthly_soilmoist.RDS"))

saveRDS(transp, "Data/extracted/ED_monthly_Transp.RDS")
saveRDS(evap, "Data/extracted/ED_monthly_evap.RDS")
saveRDS(lai, "Data/extracted/ED_monthly_lai.RDS")
saveRDS(fire, "Data/extracted/ED_monthly_fire.RDS")
