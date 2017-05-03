
library(ggplot2)
#library(reshape)
library(plantecophys)
library(reshape2)

# if data data isn't already loaded, load the necessary data

GPP <- readRDS(paste0(getwd(),"/Data/extracted/ED_monthly_GPP.RDS"))
#saveRDS(NPP, "Data/extracted/ED_monthly_NPP.RDS")
CO2<- readRDS(paste0(getwd(),"/Data/extracted/ED_monthly_CO2.RDS"))
qair<- readRDS(paste0(getwd(),"/Data/extracted/ED_monthly_qair.RDS"))
tair <- readRDS(paste0(getwd(),"/Data/extracted/ED_monthly_tair.RDS"))
transp <- readRDS(paste0(getwd(),"/Data/extracted/ED_monthly_Transp.RDS"))
lai <- readRDS(paste0(getwd(),"/Data/extracted/ED_monthly_lai.RDS"))


site.list <- colnames(CO2)[2:41]# vector of number of sites
#for calculating iWUE GPP/T*VPD
summary(qair[2:41])
summary(tair[2:41])


#---------Calculate RH and VPD---------------------


#function to calculate relative humidity from qair and tair
qair2rh <- function(qair, temp, press = 101.325){
  es <-  6.112 * exp((17.67 * (temp-273.15))/((temp-273.15) +243.5 ))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
 return(rh)
}

#funciton to calculate VPD vpd calculation
qair2vpd <- function(qair, temp, press = 101.325){
  es <-  6.112 * exp((17.67 * (temp-273.15))/((temp-273.15) +243.5 ))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0

vpd <- (es- (rh * es))
return(vpd)
}


RHtoVPD()

# calculate a VPD list
RH <- VPD <- VPD2 <- CO2
for(s in 1:length(site.list)){
RH[,c(site.list[s])] <- qair2rh(qair[,c(site.list[s])], tair[,c(site.list[s])], press = 101.325)
VPD[,c(site.list[s])] <- qair2vpd(qair[,c(site.list[s])], tair[,c(site.list[s])], press = 101.325)
}

for(s in 1:length(site.list)){
  VPD2[,c(site.list[s])] <- RHtoVPD(RH[,c(site.list[s])], tair[,c(site.list[s])]-273.15, Pa = 101)
}
#names(RH)<- names(VPD)<- site.list



# -----------------------calculate canopy conductance------------- 
# need to correct this/make sure it is right
canconduct <- function(tair, Transp, LAI, VPD){
  ((115.8 + 0.4226*(tair-273.15))*((Transp*1000/LAI)/VPD)*0.0001)
  }

#models <- c("clm45","clm.bgc", "ed2", "ed.lu", "sibcasa")

Gc <- CO2
for (s in 1:length(site.list)){
  Gc[,c(site.list[s])] <- canconduct(tair[,c(site.list[s])], transp[,c(site.list[s])], lai[,c(site.list[s])], VPD[,c(site.list[s])])
}

#-----------------calculate WUE---------------------------------

# IWUE = intrinsic WUE
# WUEt
# WUEi

# calculate WUE using loop
IWUE <- WUEt<- WUEi<- CO2
for (s in 1:length(site.list)){
  IWUE[,c(site.list[s])] <- (GPP[,c(site.list[s])]*1000/transp[,c(site.list[s])])*(VPD2[,c(site.list[s])])
  WUEt[,c(site.list[s])] <- GPP[,c(site.list[s])]*1000/transp[,c(site.list[s])]
  WUEi[,c(site.list[s])] <- GPP[,c(site.list[s])]*1000/Gc[,c(site.list[s])] # convert to kg/m2/s
}


#---------------------Do yearly + montly aggregation plots------------------

# make plots similar to the rest of the variables
plot.yrmean.ts <- function(df, name){
  m <- melt(df, id.vars=c("Year", "Month", "mo"))
  yrmeans<-dcast(m, Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/ED2_", name, "_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = Year, y = value, color = variable))+geom_line())
  dev.off()
}

plot.yrmean.ts(IWUE, "IWUE")
plot.yrmean.ts(WUEi, "WUEi")
plot.yrmean.ts(WUEt, "WUEt")

# plot seasonal cycles
plot.seasonal <- function(df, name){
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/ED2_", name, "_seasonal_site.png"))
  m <- melt(df, id.vars=c("Year", "Month", "mo"))
  print(ggplot(data = m, aes(x = Month, y = value))+geom_point()+facet_wrap(~variable,  ncol = 5))
  dev.off()
}


plot.seasonal(IWUE, "IWUE")
plot.seasonal(WUEi, "WUEi")
plot.seasonal(WUEt, "IWUEt")

# plot june july auguest means
plot.JJA.ts <- function(df, name){
  m <- melt(df, id.vars=c("Year", "Month", "mo"))
  yrmeans<-dcast(m[m$Month %in% c(6,7,8),], Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  m2$Year <- as.numeric(m2$Year)
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/ED2_", name, "_JJA_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = Year, y = value, color = variable))+geom_line())
  dev.off()
}


plot.JJA.ts (IWUE, "IWUE")
plot.JJA.ts (WUEi, "WUEi")
plot.JJA.ts (WUEt, "WUEt")

# save the WUE values as RDS files
saveRDS(IWUE, paste0(getwd(),"/Data/extracted/ED_monthly_IWUE.RDS"))
saveRDS(WUEi, paste0(getwd(),"/Data/extracted/ED_monthly_WUEi.RDS"))
saveRDS(WUEt, paste0(getwd(),"/Data/extracted/ED_monthly_WUEt.RDS"))
