# This script calculates 3 versions of WUE outlined in Beer et al
library(ggplot2)
#library(reshape)
#install.packages("plantecophys")
#library(plantecophys)
library(reshape2)

# if data data isn't already loaded, load the necessary data
load("Data/PalEON_siteInfo_all.RData")


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

#function to calculate VPD vpd calculation
qair2vpd <- function(qair, temp, press = 101.325){
  es <-  6.112 * exp((17.67 * (temp-273.15))/((temp-273.15) +243.5 ))
  e <- qair * press / (0.378 * qair + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  
  vpd <- (es- (rh * es))
  return(vpd)
}

# function to calculate WUE: 
calc.WUE <- function(model){
  
  if(model == "ED2"){
    qair <- readRDS("Data/ED2/ED2.qair.rds")
    tair <- readRDS("Data/ED2/ED2.tair.rds")
    GPP <- readRDS("Data/ED2/ED2.GPP.rds")
    CO2 <- readRDS("Data/ED2/ED2.CO2.rds")
    transp <- readRDS("Data/ED2/ED2.Transp.rds")
    LAI <- readRDS("Data/ED2/ED2.LAI.rds")
    CO2 <- data.frame(CO2)
    colnames(CO2) <- paleon$num
    site.list <- colnames(CO2)# vector of number of sites
    # calculate a VPD list
    RH <- VPD <- VPD2 <- CO2
    
    
    
    for(s in 1:length(site.list)){
      RH[,s] <- qair2rh(qair[,s], tair[,s], press = 101.325)
      VPD[,s] <- qair2vpd(qair[,s], tair[,s], press = 101.325)
    }
    
    # for(s in 1:length(site.list)){
    #  VPD2[,s] <- RHtoVPD(RH[,s], tair[,s]-273.15, Pa = 101)
    #}
    
    
    
    # -----------------------calculate canopy conductance------------- 
    # need to double check this calculation:
    canconduct <- function(tair, Transp, LAI, VPD){
      ((115.8 + 0.4226*(tair-273.15))*((Transp*1000/LAI)/VPD)*0.0001)
    }
    
    
    Gc <- CO2
    
    
    for (s in 1:length(site.list)){
      Gc[,s] <- canconduct(tair[,s], transp[,s], LAI[,s], VPD[,s])
    }
    
    #-----------------calculate WUE---------------------------------
    
    # IWUE
    # WUEt
    # WUEi: Intrinsic WUE
    
    tair <- readRDS("Data/ED2/ED2.tair.rds")
    GPP <- readRDS("Data/ED2/ED2.GPP.rds")
    CO2 <- readRDS("Data/ED2/ED2.CO2.rds")
    CO2 <- data.frame(CO2)
    colnames(CO2) <- paleon$num
    site.list <- colnames(CO2)
    transp <- readRDS("Data/ED2/ED2.Transp.rds")
    LAI <- readRDS("Data/ED2/ED2.LAI.rds")
    # calculate WUE using loop
    IWUE <- WUEt <- WUEi <- CO2
    if(model == "ED2"){
      sec2mo <- 60*60*24*30
      for (s in 1:length(site.list)){
        IWUE[,s] <- GPP[,s]*1000/(transp[,s])*(VPD[,s])
        WUEt[,s] <- GPP[,s]*1000/(transp[,s])
        WUEi[,s] <- GPP[,s]*1000/Gc[,s] # convert to kg/m2/s
      }
      
      saveRDS(IWUE, "Data/ED2/ED2.IWUE.rds")
      saveRDS(WUEt, "Data/ED2/ED2.WUEt.rds")
      saveRDS(WUEi, "Data/ED2/ED2.WUEi.rds")
      
    }else{
     
      # for LPJGUESS
      #qair <- readRDS("Data/LPJ-GUESS/")
      tair <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.tair.rds")
      GPP <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.GPP.rds")
      CO2 <- readRDS("Data/ED2/ED2.CO2.rds")
      CO2 <- data.frame(CO2)
      colnames(CO2) <- paleon$num
      site.list <- colnames(CO2)
      transp <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Transp.rds")
      LAI <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.LAI.rds")
      
      # calculate a VPD list
      RH <- VPD <- VPD2 <- CO2
      
      # we dont have qair for LPJ, so can only calculate WUEt 
      
      
      #-----------------calculate WUE---------------------------------
      
      # IWUE 
      # WUEt
      # WUEi
      
      # calculate WUE using loop
      WUEt <- CO2
      
      
      for (s in 1:length(site.list)){
        #IWUE[,s] <- GPP[,s]*1000/transp[,s]*(VPD2[,s])
        WUEt[,s] <- GPP[,s]*1000/(transp[,s])
        #WUEi[,s] <- GPP[,s]*1000/Gc[,s] # convert to kg/m2/s
      }
      
      #saveRDS(IWUE, "Data/ED2/ED2.IWUE.rds")
      saveRDS(WUEt, "Data/LPJ-GUESS/LPJ-GUESS.WUEt.rds")
      #saveRDS(WUEi, "Data/ED2/ED2.WUEi.rds")
    }
    
    
    }
 
  
    
}


calc.WUE ("ED2")
calc.WUE ("LPJ-GUESS")

#---------------------Do yearly + montly aggregation plots------------------
# read in the WUE files:
ED2.WUEi <- readRDS("Data/ED2/ED2.WUEi.rds")
ED2.WUEt <- readRDS("Data/ED2/ED2.WUEt.rds")
ED2.IWUE <- readRDS("Data/ED2/ED2.IWUE.rds")

GUESS.WUEt <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.WUEt.rds")

# make plots similar to the rest of the variables
plot.yrmean.ts <- function(df, name){
  df <- data.frame(df)
  colnames(df) <- paleon$num
  df$year <- year
  df$month <- month
  m <- melt(df, id.vars=c("year", "month"))
  yrmeans<-dcast(m, year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "year")
  m2$year <- as.numeric(m2$year)
  
  png(height = 7, width = 18, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/", name, "_mean_timeseries_site.png"))
  print(ggplot(data = m2, aes(x = year, y = value, color = variable))+geom_line())
  dev.off()
}

plot.yrmean.ts(ED2.IWUE, "ED2_IWUE")
plot.yrmean.ts(ED2.WUEi, "ED2_WUEi")
plot.yrmean.ts(ED2.WUEt, "ED2_WUEt")
plot.yrmean.ts(GUESS.WUEt, "LPJ-GUESS_WUEt")

# plot seasonal cycles
plot.seasonal <- function(df, name){
  df <- data.frame(df)
  colnames(df) <- paleon$site
  df$year <- year
  df$month <- month
  #df <- df[!is.na(df),]
  
  png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/", name, "_seasonal_site.png"))
  m <- melt(df, id.vars=c("year", "month"))
  m<- m[complete.cases(m),]
  print(ggplot(data = m[1:100000,], aes(x = month, y = value, color = variable))+geom_point())
  dev.off()
}

plot.seasonal(ED2.IWUE, "ED2_IWUE")
plot.seasonal(ED2.WUEi, "ED2_WUEi")
plot.seasonal(ED2.WUEt, "ED2_IWUEt")
plot.seasonal(GUESS.WUEt, "GUESS_IWUEt")

# plot june july auguest means
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


plot.JJA.ts (ED2.IWUE, "ED2_IWUE")
plot.JJA.ts (ED2.WUEi, "ED2_WUEi")
plot.JJA.ts (ED2.WUEt, "ED2_WUEt")
plot.JJA.ts (GUESS.WUEt, "LPJ.GUESS_WUEt")


# save the WUE values as RDS files
#saveRDS(IWUE, paste0(getwd(),"/Data/extracted/ED_monthly_IWUE.RDS"))
#saveRDS(WUEi, paste0(getwd(),"/Data/extracted/ED_monthly_WUEi.RDS"))
#saveRDS(WUEt, paste0(getwd(),"/Data/extracted/ED_monthly_WUEt.RDS"))

# -------------find rate of change (1850-2010) per each grid cell----------------
timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850
paleon$site <- paste0("X", paleon$num)

data = ED2.WUEi
model = "ED2"
wue = "WUEi"




slope.WUE.inc <- function(data ,model, wue){
  site.list <- paste0("X", paleon$num)
  
  
  df <- data.frame(data)
  colnames(df) <- site.list
  df <- df[!sapply(df, function(x)all(is.na(x)))]
  site.listnona <- colnames(df)
  df$Year <- year
  df$Month <- month
  
  
  
  m <- melt(df, id.vars=c("Year", "Month"))
  yrmeans <- dcast(m[m$Month %in% c(6,7,8),], Year ~ variable, mean)
  m2 <- melt(yrmeans, id.vars= "Year")
  
  m1850 <- m2[m2$Year %in% 1850:2010,]
  
  #ggplot(m1850, aes(x = Year, y = value, color = variable))+geom_point()+ theme(legend.position="none")
  lm.slope <- data.frame(site = site.listnona,
                         slope = NA, 
                         int = NA)
  #test <- df[!sapply(df, function(x)all(is.na(x)))]
  
  for(i in 1:length(site.listnona)){
    testlm <- lm(value ~ Year,data = na.omit(m1850[m1850$variable %in% site.listnona[i],]))
    lm.slope[i,]$slope <- testlm$coefficients[2]
    lm.slope[i,]$int <- testlm$coefficients[1]
  }
  
  #lm.slope$num <- as.character(lm.slope$num)
  lm.slope.m <- merge(lm.slope, paleon, by = "site")
  saveRDS(lm.slope.m, paste0("outputs/data/", model, "/", model,".", wue,"_slope.rds"))
  
  # mapdata for plotting
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states)<-~long+lat
  class(states)
  proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  png(height = 4, width = 8, units = "in", res=300, paste0("outputs/preliminaryplots/WUE/", model,"_", wue, "_slope_map.png"))
  print(ggplot(lm.slope.m, aes(x=lon, y = lat, fill = slope))+
          scale_fill_gradient(high= "red", low = "blue")+
          geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50))+
          theme_bw()+ggtitle(paste0(model, " slope of ", wue, " 1850-2010 ")))
  dev.off()
}

# run this funciton:

slope.WUE.inc(ED2.IWUE, "ED2", "IWUE")
slope.WUE.inc(ED2.WUEi, "ED2", "WUEi")
slope.WUE.inc(ED2.WUEt, "ED2", "WUEt")

slope.WUE.inc(GUESS.WUEt, "GUESS", "WUEt")
