library(raster)
library(plyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(zoo)

load("Data/PalEON_siteInfo_all.RData")
# read in model outputs that were extracted for ED2

files <- list.files("Data/ED2/")

# read in all the ED2 files
for(i in 1:length(files)){
assign(x = unlist(strsplit(files[i],split = '.rds')), value = readRDS(paste0("Data/ED2/",files[i])))
}



# ----------------get lats and long dimensions----------------------

ggplot(paleon, aes(x = lon, y = lat, fill = notes))+geom_raster()+theme_bw()+coord_equal()


# make a simple reference map of where whe have grid cells and their names:
states <- map_data("state")
states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
states <- spTransform(states,CRSobj = '+init=epsg:4326')
mapdata <- data.frame(states)

png(height = 8, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/ED_grid_map.png"))
ggplot(paleon, aes(x = lon, y = lat))+geom_raster()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60))
dev.off()

  # get the years listed for ED monthly runs
#13932 data points = 1161 years
  timevec <- 1:13932
  month <- rep(1:12, 1161)
  yearsince  <- rep(0:1160, each =12)
  year <- yearsince + 850
  

 
#-------------------------------------------ED2 preliminary plots:--------------------------------------------

# plot the seasonal cycle for each variable at each site and save in outputs/preliminaryplots
plot.seasonal <- function(df, name){
  df <- data.frame(df)
  colnames(df) <- paleon$site
  df$year <- year
  df$month <- month
  #df <- df[!is.na(df),]
  
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/ED2_", name, "_seasonal_site.png"))
  m <- melt(df, id.vars=c("year", "month"))
  m<- m[complete.cases(m),]
  print(ggplot(data = m[1:100000,], aes(x = month, y = value, color = variable))+geom_point())
  dev.off()
}


# not run
#plot.seasonal(ED2.GPP, "GPP")
#plot.seasonal(ED2.GWBI, "GWBI")
#plot.seasonal(ED2.tair, "Tair")
#plot.seasonal(ED2.transp, "Transp")
#plot.seasonal(ED2.fire, "Fire")
#plot.seasonal(ED2.lai, "LAI")
#plot.seasonal(ED2.evap, "Evap")
#plot.seasonal(ED2.qair, "Qair")
#plot.seasonal(soilmoist, "SoilMoist")
#plot.seasonal(ED2.precip, "Precipf")
# calculate the means for the years:

plot.yrmean.ts <- function(df, name){
  df <- data.frame(df)
  colnames(df) <- paleon$latlon
  df$year <- year
  df$month <- month
  m <- melt(df, id.vars=c("year", "month"))
  yrmeans<-dcast(m, year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "year")
  m2$Year <- as.numeric(m2$year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/", name, "_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = year, y = value, color = variable))+geom_line()+ theme(legend.position="none"))
  dev.off()
}

plot.yrmean.ts(ED2.GPP, "ED2.GPP")
plot.yrmean.ts(ED2.GWBI, "ED2.GWBI")
plot.yrmean.ts(ED2.tair, "ED2.Tair")
plot.yrmean.ts(ED2.qair, "ED2.Qair")
plot.yrmean.ts(ED2.Transp, "ED2.Transp")
plot.yrmean.ts(ED2.Fire, "ED2.Fire")
plot.yrmean.ts(ED2.LAI, "ED2.LAI")
plot.yrmean.ts(ED2.Evap, "ED2.Evap")
plot.yrmean.ts(ED2.CO2, "ED2.CO2")
plot.yrmean.ts(ED2.precipf, "ED2.Precip")
#plot.yrmean.ts(soilmoist, "SoilMoist")

# plot and calulate only summer JJA yearly values:

plot.JJA.ts <- function(df, name){
  df <- data.frame(df)
  colnames(df) <- paleon$latlon
  df$year <- year
  df$month <- month
  m <- melt(df, id.vars=c("year", "month"))
  yrmeans<-dcast(m[m$month %in% c(6,7,8),], year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "year")
  m2$year <- as.numeric(m2$year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/", name, "_JJA_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = year, y = value, color = variable))+geom_line()+ theme(legend.position="none"))
  dev.off()
}


plot.JJA.ts (ED2.GPP, "ED2.GPP")
plot.JJA.ts (ED2.GWBI, "ED2.GWBI")
plot.JJA.ts (ED2.tair, "ED2.Tair")
plot.JJA.ts (ED2.qair, "ED2.Qair")
plot.JJA.ts (ED2.Transp, "ED2.Transp")
plot.JJA.ts (ED2.Fire, "ED2.Fire")
plot.JJA.ts (ED2.LAI, "ED2.LAI")
plot.JJA.ts (ED2.Evap, "ED2.Evap")
plot.JJA.ts (ED2.CO2, "ED2.CO2")
plot.JJA.ts(ED2.precipf, "ED2.Precip")
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


#-------------------------LPJ-GUESS preliminary plots--------------------
# remove the ED2 variables:
rm(list = ls())

load("Data/PalEON_siteInfo_all.RData")
# read in model outputs that were extracted for ED2

files <- list.files("Data/LPJ-GUESS/")

# read in all the LPJ-GUESS files
for(i in 1:length(files)){
  assign(x = unlist(strsplit(unlist(strsplit(files[i],split = '.rds')),split = 'LPJ-'))[2], value = readRDS(paste0("Data/LPJ-GUESS/",files[i])))
}

timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850

# reload function
plot.yrmean.ts <- function(df, name){
  df <- data.frame(df)
  colnames(df) <- paleon$latlon
  df$year <- year
  df$month <- month
  m <- melt(df, id.vars=c("year", "month"))
  yrmeans<-dcast(m, year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "year")
  m2$Year <- as.numeric(m2$year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/", name, "_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = year, y = value, color = variable))+geom_line()+ theme(legend.position="none"))
  dev.off()
}

plot.yrmean.ts(GUESS.GPP, "GUESS.GPP")
#plot.yrmean.ts(GUESS.GWBI, "GUESS.GWBI") # GWBI is pft specifi in LPJ
plot.yrmean.ts(GUESS.tair, "GUESS.Tair")
plot.yrmean.ts(GUESS.Transp, "GUESS.Transp")
plot.yrmean.ts(GUESS.Fire, "GUESS.Fire")
plot.yrmean.ts(GUESS.LAI, "GUESS.LAI")
plot.yrmean.ts(GUESS.Evap, "GUESS.Evap")
plot.yrmean.ts(GUESS.CO2, "GUESS.CO2")
plot.yrmean.ts(GUESS.precipf, "GUESS.Precip")


# plot jja meanas as a timesearies
plot.JJA.ts <- function(df, name){
  df <- data.frame(df)
  colnames(df) <- paleon$latlon
  df$year <- year
  df$month <- month
  m <- melt(df, id.vars=c("year", "month"))
  yrmeans<-dcast(m[m$month %in% c(6,7,8),], year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "year")
  m2$year <- as.numeric(m2$year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/", name, "_JJA_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = year, y = value, color = variable))+geom_line()+ theme(legend.position="none"))
  dev.off()
}


plot.JJA.ts (GUESS.GPP, "Guess.GPP")
#plot.JJA.ts (GUESS.GWBI, "GWBI") # GWBI is pft specific in LPJ
plot.JJA.ts (GUESS.tair, "Tair")
plot.JJA.ts (GUESS.Transp, "Transp")
plot.JJA.ts (GUESS.Fire, "Fire")
plot.JJA.ts (GUESS.LAI, "LAI")
plot.JJA.ts (GUESS.Evap, "Evap")
plot.JJA.ts (GUESS.CO2, "CO2")
plot.JJA.ts(GUESS.precipf, "Precip")