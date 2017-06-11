#----------- looking into NPP, tair, precip sensitivities ---------------

# 1. Calculating sensitivity of NPP to tair and precip
# 2. Is NPP sensitive to tair or precip?
# 3. How do sensitivities vary across space?
# 4. How do sensitivities vary through time?

#----------1. Calculating sensitivty of NPP to tair and precip----------
# load required data

load("Data/PalEON_siteInfo_all.RData")

timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850

NPP <- readRDS("Data/ED2/ED2.NPP.rds")
precip <- readRDS("Data/ED2/ED2.precipf.rds")
tair <- readRDS("Data/ED2/ED2.tair.rds")
soil <- readRDS("Data/ED2/ED2.SoilMoist.rds")

# assign dimnames
dimnames(NPP) <- list(year, paleon$num)
dimnames(precip) <- list(year, paleon$num)
dimnames(tair) <- list(year, paleon$num)

# aggregate to growing season NPP
source("R/get.yrmeans.R")
NPP.gs <- get.JJAmeans(NPP, "NPP")
pr.jun <- get.mo.means(precip, mo = c("6"),"prJun")
pr.jul <- get.mo.means(precip,mo = c("7"), "prJul")
pr.aug <- get.mo.means(precip,mo = c("8"), "prAug")

t.jun <- get.mo.means(tair, mo = c("6"),"tJun")
t.jul <- get.mo.means(tair,mo = c("7"), "tJul")
t.aug <- get.mo.means(tair,mo = c("8"), "tAug")

npp.all <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(NPP.gs, pr.jun, pr.jul, pr.aug, t.jun, t.jul, t.aug))

npp.m <- melt(npp.all, id.vars =c("Year", "Site", "NPP"))


# plot NPP vs all climate parameters (all data)
ggplot(npp.m, aes(NPP, value))+geom_point()+facet_wrap(~variable, scales="free")

# plot NPP vs all climate parameters (all data, but colored by sites)
ggplot(npp.m, aes(NPP, value, color = Site))+geom_point()+facet_wrap(~variable, scales="free")+theme(legend.position = "none")

paleon$Site <- paste0("X", paleon$num)

# next we want to generate an overall sensitivity of NPP to each monthly climate driver
# need to set the colors the same
plot.npp.sens <- function(df, clim, period){
  a <- df[df$variable %in% clim & df$Year %in% period,]
  slope.table <- data.frame(site = unique(a$Site),
                            pval = NA, 
                            slope = NA)
  
  # find the slopes of relationship between NPP and climate variable for each grid cell:
  for(i in 1:length(paleon$num)){
    if(is.na(a[i,]$NPP)){
      pval <- NA
      slope <- NA
    }else{
      mod <- summary( lm(NPP ~ value,data = a[a$Site %in% slope.table[i,]$site,]) )
      pval <- mod$coefficients[2,4]
      slope <- mod$coefficients[2,1]
    }
    slope.table[i,]$pval <- pval
    slope.table[i,]$slope <- slope
  }
  
  paleon$site <- paste0("X", paleon$num)
  
  # merge paleon to site to plot:
  slope.xy <- merge(paleon, slope.table, by = "site")
  
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states)<-~long+lat
  class(states)
  proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  png(height=4, width = 7, units="in",res=300 ,paste0("outputs/preliminaryplots/sensitivity/npp/ED2_npp_", clim, "_slopes_",period[1],"_",period[length(period)],".png"))
  print(ggplot(slope.xy, aes(x = lon, y=lat, fill= slope))+geom_raster()+scale_fill_gradient(low = "blue", high = "red", limits=c(0.00025, 0.0025))+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle(paste0('Slope of NPP ', clim, " relationship")))
  
  dev.off()
  }


# write out these to plots
plot.npp.sens(npp.m, "prJun", 850:2010)
plot.npp.sens(npp.m, "prJul", 850:2010)
plot.npp.sens(npp.m, "prAug", 850:2010)
plot.npp.sens(npp.m, "tJun", 850:2010)
plot.npp.sens(npp.m, "tJul", 850:2010)
plot.npp.sens(npp.m, "tAug", 850:2010)

# for period = 1901:2010
plot.npp.sens(npp.m, "prJun", 1901:2010)
plot.npp.sens(npp.m, "prJul", 1901:2010)
plot.npp.sens(npp.m, "prAug", 1901:2010)
plot.npp.sens(npp.m, "tJun", 1901:2010)
plot.npp.sens(npp.m, "tJul", 1901:2010)
plot.npp.sens(npp.m, "tAug", 1901:2010)

#for period = 850-1900
plot.npp.sens(npp.m, "prJun", 850:1900)
plot.npp.sens(npp.m, "prJul", 850:1900)
plot.npp.sens(npp.m, "prAug", 850:1900)
plot.npp.sens(npp.m, "tJun", 850:1900)
plot.npp.sens(npp.m, "tJul", 850:1900)
plot.npp.sens(npp.m, "tAug", 850:1900)



#------------ using AGB increment instead of NPP---------------
#----------- looking into NPP, tair, precip sensitivities ---------------

# 1. Calculating sensitivity of NPP to tair and precip
# 2. Is NPP sensitive to tair or precip?
# 3. How do sensitivities vary across space?
# 4. How do sensitivities vary through time?

#----------1. Calculating sensitivty of NPP to tair and precip----------
# load required data
library(raster)
library(dplyr)
library(reshape2)
load("Data/PalEON_siteInfo_all.RData")

timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850

# calculating AGB increment:
AGB <- readRDS("Data/ED2/ED2.AGB.rds")
precip <- readRDS("Data/ED2/ED2.precipf.rds")
tair <- readRDS("Data/ED2/ED2.tair.rds")

# assign dimnames
dimnames(AGB) <- list(year, paleon$num)
dimnames(precip) <- list(year, paleon$num)
dimnames(tair) <- list(year, paleon$num)

# aggregate to growing season NPP
source("R/get.yrmeans.R")
AGB.gs <- get.JJAmeans(AGB, "AGB")
agb.long <- dcast(AGB.gs, formula = Year ~ Site)
agbi <- agb.long


# need to calculate the difference in AGB between each year at each site:
for(i in 1:length(paleon$num)){
  for(t in 1:length(yrs)){
    if(t ==1){
      agbi[t,i+1] <- 0
    }else{
  agbi[t,i+1] <- agb.long[t,i+1] - agb.long[t-1,i+1]
    }
  }
}

#melt the agbi:
agbi.m <- melt(agbi, id.vars = "Year")
colnames(agbi.m) <- c("Year", "Site", "agbi")
saveRDS(agbi, "outputs/data/ED2/ED2.agbi.rds")

# also look at soil moisture:
moist <- readRDS("Data/ED2/ED2.SoilMoist.rds")
dimnames(moist) <- list(timevec, paleon$num, 1:12)

# for now, lets just sum the total amount of soil moisture:
totMoist <-  precip

for(i in 1:length(paleon$num)){
  
  moist.site <- data.frame(moist[,i,])
  totMoist[,i] <- rowSums(moist.site, na.rm=TRUE)
  
}

saveRDS(totMoist ,"outputs/data/ED2/ED2.totMoist.rds")




pr.jun <- get.mo.means(precip, mo = c("6"),"prJun")
pr.jul <- get.mo.means(precip, mo = c("7"), "prJul")
pr.aug <- get.mo.means(precip, mo = c("8"), "prAug")
pr.jja <- get.mo.means(precip, mo = c("6","7","8"),"prJJA")


# rough conversion of precipitaiton rate to mm precip per month
# precip rate: kgH20/m2/day
pr.jun[,3] <- pr.jun[,3]*(30*60*60*24)
pr.jul[,3] <- pr.jul[,3]*(30*60*60*24)
pr.aug[,3] <- pr.aug[,3]*(30*60*60*24)
pr.jja[,3] <- pr.jja[,3]*(30*60*60*24)

t.jun <- get.mo.means(tair, mo = c("6"),"tJun")
t.jul <- get.mo.means(tair,mo = c("7"), "tJul")
t.aug <- get.mo.means(tair,mo = c("8"), "tAug")


sm.jun <- get.mo.means(totMoist, mo = c("6"),"smJun")
sm.jul <- get.mo.means(totMoist, mo = c("7"),"smJul")
sm.aug <- get.mo.means(totMoist, mo = c("8"),"smAug")

AGB.all <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(
  agbi.m, pr.jun, pr.jul, pr.aug, t.jun, t.jul, t.aug,
  sm.jun, sm.jul, sm.aug))

AGB.m <- melt(AGB.all, id.vars =c("Year", "Site", "agbi"))


# plot NPP vs all climate parameters (all data)
X11(width = 12)
ggplot(AGB.m, aes(agbi, value))+geom_point()+facet_wrap(~variable, scales="free")

# plot NPP vs all climate parameters (all data, but colored by sites)
ggplot(AGB.m, aes(agbi, value, color = Site))+geom_point()+facet_wrap(~variable, scales="free")+theme(legend.position = "none")

ggplot(AGB.m, aes(Year, agbi, color = Site))+geom_point()+theme(legend.position = "none")

paleon$Site <- paste0("X", paleon$num)

# next we want to generate an overall sensitivity of NPP to each monthly climate driver
# need to set the colors the same

plot.agbi.sens <- function(df, clim, period){
  
  a <- df[df$variable %in% clim & df$Year %in% period,]
  slope.table <- data.frame(site = unique(a$Site),
                            pval = NA, 
                            slope = NA)
  
  # find the slopes of relationship between NPP and climate variable for each grid cell:
  for(i in 1:length(paleon$num)){
    if(is.na(a[i,]$agbi)){
      pval <- NA
      slope <- NA
    }else{
      mod <- summary( lm(agbi ~ value, data = a[a$Site %in% slope.table[i,]$site,]) )
      pval <- mod$coefficients[2,4]
      slope <- mod$coefficients[2,1]
    }
    slope.table[i,]$pval <- pval
    slope.table[i,]$slope <- slope
  }
  
  paleon$site <- paste0("X", paleon$num)
  
  slope.table$sigslope <- ifelse(slope.table$pval < 0.05, slope.table$slope, 0)
  
  # merge paleon to site to plot:
  slope.xy <- merge(paleon, slope.table, by = "site")
  
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states) <- ~long+lat
  class(states)
  proj4string(states) <- CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  png(height=4, width = 7, units="in",res=300 ,paste0("outputs/preliminaryplots/sensitivity/agbi/ED2_agbi_", clim, "_slopes_",period[1],"_",period[length(period)],".png"))
  print(ggplot(slope.xy, aes(x = lon, y=lat, fill= sigslope))+geom_raster()+#scale_fill_gradient(low = "blue", high = "red", limits=c(0.00025, 0.0025))+
          geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle(paste0('Slope of NPP ', clim, " relationship")))
  
  dev.off()
}


# write out these to plots
plot.agbi.sens(AGB.m, "prJun", 850:2010)
plot.agbi.sens(AGB.m, "prJul", 850:2010)
plot.agbi.sens(AGB.m, "prAug", 850:2010)
plot.agbi.sens(AGB.m, "tJun", 850:2010)
plot.agbi.sens(AGB.m, "tJul", 850:2010)
plot.agbi.sens(AGB.m, "tAug", 850:2010)

# for period = 1901:2010
plot.agbi.sens(AGB.m, "prJun", 1901:2010)
plot.agbi.sens(AGB.m, "prJul", 1901:2010)
plot.agbi.sens(AGB.m, "prAug", 1901:2010)
plot.agbi.sens(AGB.m, "tJun", 1901:2010)
plot.agbi.sens(AGB.m, "tJul", 1901:2010)
plot.agbi.sens(AGB.m, "tAug", 1901:2010)

# for period = 1901:2010
plot.agbi.sens(AGB.m, "prJun", 1801:1900)
plot.agbi.sens(AGB.m, "prJul", 1801:1900)
plot.agbi.sens(AGB.m, "prAug", 1801:1900)
plot.agbi.sens(AGB.m, "tJun", 1801:1900)
plot.agbi.sens(AGB.m, "tJul", 1801:1900)
plot.agbi.sens(AGB.m, "tAug", 1801:1900)

#for period = 850-1900
plot.agbi.sens(AGB.m, "prJun", 850:1900)
plot.agbi.sens(AGB.m, "prJul", 850:1900)
plot.agbi.sens(AGB.m, "prAug", 850:1900)
plot.agbi.sens(AGB.m, "tJun", 850:1900)
plot.agbi.sens(AGB.m, "tJul", 850:1900)
plot.agbi.sens(AGB.m, "tAug", 850:1900)


# lets just look at the Pearson Climate correlation:
plot.agbi.cor <- function(df, clim, period){
  
  a <- df[df$variable %in% clim & df$Year %in% period,]
  slope.table <- data.frame(site = unique(a$Site),
                            pval = NA, 
                            cor = NA)
  
  # find the slopes of relationship between NPP and climate variable for each grid cell:
  for(i in 1:length(paleon$num)){
    
    if(is.na(a[i,]$agbi)){
      pval <- NA
      cor <- NA
    }else{
      df.a <- a[a$Site %in% slope.table[i,]$site,]
      mod <- cor.test(df.a$agbi, df.a$value)
      #mod <- summary( lm(agbi ~ value, data = a[a$Site %in% slope.table[i,]$site,]) )
      pval <- mod$p.value
      cor <- mod$estimate
    }
    slope.table[i,]$pval <- pval
    slope.table[i,]$cor <- cor
  }
  
  paleon$site <- paste0("X", paleon$num)
  
  slope.table$sigcor <- ifelse(slope.table$pval < 0.05, slope.table$cor, NA)
  
  # merge paleon to site to plot:
  slope.xy <- merge(paleon, slope.table, by = "site")
  
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states) <- ~long+lat
  class(states)
  proj4string(states) <- CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  png(height=4, width = 7, units="in",res=300 ,paste0("outputs/preliminaryplots/sensitivity/agbi/ED2_agbi_", clim, "_cors_",period[1],"_",period[length(period)],".png"))
  print(ggplot(slope.xy, aes(x = lon, y=lat, fill= sigcor))+geom_raster()+#scale_fill_gradient(low = "blue", high = "red", limits=c(0.00025, 0.0025))+
          geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle(paste0('Pearson Correlations between ', clim, " and AGBI")))
  
  dev.off()
  
  colnames(slope.xy)[14] <- paste0(clim, "_sigcor")
  saveRDS(slope.xy, paste0("outputs/preliminaryplots/sensitivity/agbi/ED2/cors/GUESS_", clim, "_AGB_", period[1], "_", period[length(period)],".rds" ))
  
}

plot.agbi.cor(AGB.m, "prJun", 1901:2010)
plot.agbi.cor(AGB.m, "prJul", 1901:2010)
plot.agbi.cor(AGB.m, "prAug", 1901:2010)
plot.agbi.cor(AGB.m, "tJun", 1901:2010)
plot.agbi.cor(AGB.m, "tJul", 1901:2010)
plot.agbi.cor(AGB.m, "tAug", 1901:2010)
plot.agbi.cor(AGB.m, "smJun", 1901:2010)
plot.agbi.cor(AGB.m, "smJul", 1901:2010)
plot.agbi.cor(AGB.m, "smAug", 1901:2010)

plot.agbi.cor(AGB.m, "prJun", 850:1800)
plot.agbi.cor(AGB.m, "prJul", 850:1800)
plot.agbi.cor(AGB.m, "prAug", 850:1800)
plot.agbi.cor(AGB.m, "tJun", 850:1800)
plot.agbi.cor(AGB.m, "tJul", 850:1800)
plot.agbi.cor(AGB.m, "tAug", 850:1800)
plot.agbi.cor(AGB.m, "smJun", 850:1800)
plot.agbi.cor(AGB.m, "smJul", 850:1800)
plot.agbi.cor(AGB.m, "smAug", 850:1800)

# next step here:
# what is agbi increment in LPJ-GUESS sensitive to?


# ------------------------------for LPJ-GUESS--------------------------------:
# do all the above correlations/processing steps, but for LPJ-GUESS:
load("Data/PalEON_siteInfo_all.RData")

timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850
yrs <- 850:2010
pft.guess = c("BNE", "BINE", "BNS", "BIBS", "TeBS", "BeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")

# calculating AGB increment:
AGB <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.AGB.rds")
precip <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.precipf.rds")
tair <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.tair.rds")

# assign dimnames
dimnames(AGB) <- list(yr, paleon$num, pft.guess)
dimnames(precip) <- list(year, paleon$num)
dimnames(tair) <- list(year, paleon$num)

# Guess is at yearly agb already, lets just use that
source("R/get.yrmeans.R")
AGB.yr <- data.frame(AGB[,,"Total"])
AGB.yr$Year <- yrs
# put AGB.yr at the beginngin of the df:
require(dplyr)
AGB.yr <- AGB.yr %>%
  dplyr::select(Year, everything())

#agb.long <- dcast(AGB.gs, formula = Year ~ Site)
agbi <- AGB.yr


# need to calculate the difference in AGB between each year at each site:
for(i in 1:length(paleon$num)){
  for(t in 1:length(yrs)){
    if(t ==1){
      agbi[t,i+1] <- 0
    }else{
      agbi[t,i+1] <- AGB.yr[t,i+1] - AGB.yr[t-1,i+1]
    }
  }
}

#melt the agbi:
agbi.m <- melt(agbi, id.vars = "Year")
colnames(agbi.m) <- c("Year", "Site", "agbi")
saveRDS(agbi, "outputs/data/GUESS/GUESS.agbi.rds")

# also look at soil moisture:
moist <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.SoilMoist.rds")
dimnames(moist) <- list(timevec, paleon$num, 1:2)

# for now, lets just sum the total amount of soil moisture:
totMoist <-  precip

for(i in 1:length(paleon$num)){
  
  moist.site <- data.frame(moist[,i,])
  totMoist[,i] <- rowSums(moist.site, na.rm=TRUE)
  
}

saveRDS(totMoist ,"outputs/data/GUESS/GUESS.totMoist.rds")




pr.jun <- get.mo.means(precip, mo = c("6"),"prJun")
pr.jul <- get.mo.means(precip, mo = c("7"), "prJul")
pr.aug <- get.mo.means(precip, mo = c("8"), "prAug")
pr.jja <- get.mo.means(precip, mo = c("6","7","8"),"prJJA")


# rough conversion of precipitaiton rate to mm precip per month
# precip rate: kgH20/m2/day
pr.jun[,3] <- pr.jun[,3]*(30*60*60*24)
pr.jul[,3] <- pr.jul[,3]*(30*60*60*24)
pr.aug[,3] <- pr.aug[,3]*(30*60*60*24)
pr.jja[,3] <- pr.jja[,3]*(30*60*60*24)

t.jun <- get.mo.means(tair, mo = c("6"),"tJun")
t.jul <- get.mo.means(tair,mo = c("7"), "tJul")
t.aug <- get.mo.means(tair,mo = c("8"), "tAug")


sm.jun <- get.mo.means(totMoist, mo = c("6"),"smJun")
sm.jul <- get.mo.means(totMoist, mo = c("7"),"smJul")
sm.aug <- get.mo.means(totMoist, mo = c("8"),"smAug")

AGB.all <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(
  agbi.m, pr.jun, pr.jul, pr.aug, t.jun, t.jul, t.aug,
  sm.jun, sm.jul, sm.aug))

AGB.m <- melt(AGB.all, id.vars =c("Year", "Site", "agbi"))

paleon$Site <- paste0("X", paleon$num)

# next we want to generate an overall sensitivity of NPP to each monthly climate driver
# need to set the colors the same

plot.agbi.sens <- function(df, clim, period){
  
  a <- df[df$variable %in% clim & df$Year %in% period,]
  slope.table <- data.frame(site = unique(a$Site),
                            pval = NA, 
                            slope = NA)
  
  # find the slopes of relationship between NPP and climate variable for each grid cell:
  for(i in 1:length(paleon$num)){
    if(is.na(a[i,]$agbi)){
      pval <- NA
      slope <- NA
    }else{
      mod <- summary( lm(agbi ~ value, data = a[a$Site %in% slope.table[i,]$site,]) )
      pval <- mod$coefficients[2,4]
      slope <- mod$coefficients[2,1]
    }
    slope.table[i,]$pval <- pval
    slope.table[i,]$slope <- slope
  }
  
  paleon$site <- paste0("X", paleon$num)
  
  slope.table$sigslope <- ifelse(slope.table$pval < 0.05, slope.table$slope, 0)
  
  # merge paleon to site to plot:
  slope.xy <- merge(paleon, slope.table, by = "site")
  
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states) <- ~long+lat
  class(states)
  proj4string(states) <- CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  png(height=4, width = 7, units="in",res=300 ,paste0("outputs/preliminaryplots/sensitivity/agbi/ED2_agbi_", clim, "_slopes_",period[1],"_",period[length(period)],".png"))
  print(ggplot(slope.xy, aes(x = lon, y=lat, fill= sigslope))+geom_raster()+#scale_fill_gradient(low = "blue", high = "red", limits=c(0.00025, 0.0025))+
          geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle(paste0('Slope of NPP ', clim, " relationship")))
  
  dev.off()
}


plot.agbi.cor <- function(df, clim, period){
  load("Data/PalEON_siteInfo_all.RData")
  paleon$site <- paste0("X", paleon$num)
  a <- df[df$variable %in% clim & df$Year %in% period,]
  slope.table <- data.frame(site = unique(a$Site),
                            pval = NA, 
                            cor = NA)
  
  # find the slopes of relationship between NPP and climate variable for each grid cell:
  for(i in 1:length(paleon$num)){
    
    if(is.na(a[i,]$agbi)){
      pval <- NA
      cor <- NA
    }else{
      df.a <- a[a$Site %in% slope.table[i,]$site,]
      mod <- cor.test(df.a$agbi, df.a$value)
      #mod <- summary( lm(agbi ~ value, data = a[a$Site %in% slope.table[i,]$site,]) )
      pval <- mod$p.value
      cor <- mod$estimate
    }
    slope.table[i,]$pval <- pval
    slope.table[i,]$cor <- cor
  }
  
  
  paleon$site <- paste0("X", paleon$num)
  
  slope.table$sigcor <- ifelse(slope.table$pval < 0.05, slope.table$cor, NA)
  
  #colnames(slope.table) <- c("site", "pval", "cor", paste0(clim, "_sigcor"))
  # merge paleon to site to plot:
  slope.xy <- merge(paleon, slope.table, by = "site")
 
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states) <- ~long+lat
  class(states)
  proj4string(states) <- CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  png(height=4, width = 7, units="in",res=300 ,paste0("outputs/preliminaryplots/sensitivity/agbi/GUESS/GUESS_agbi_", clim, "_cors_",period[1],"_",period[length(period)],".png"))
  print(ggplot(slope.xy, aes(x = lon, y=lat, fill= sigcor))+geom_raster()+#scale_fill_gradient(low = "blue", high = "red", limits=c(0.00025, 0.0025))+
          geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle(paste0('Pearson Correlations between ', clim, " and AGBI")))
  
  dev.off()
  colnames(slope.xy)[13] <- paste0(clim, "_sigcor")
  saveRDS(slope.xy, paste0("outputs/preliminaryplots/sensitivity/agbi/GUESS/cors/GUESS_", clim, "_AGB_", period[1], "_", period[length(period)],".rds" ))
  
}

plot.agbi.cor(AGB.m, "prJun", 1901:2010)
plot.agbi.cor(AGB.m, "prJul", 1901:2010)
plot.agbi.cor(AGB.m, "prAug", 1901:2010)
plot.agbi.cor(AGB.m, "tJun", 1901:2010)
plot.agbi.cor(AGB.m, "tJul", 1901:2010)
plot.agbi.cor(AGB.m, "tAug", 1901:2010)
plot.agbi.cor(AGB.m, "smJun", 1901:2010)
plot.agbi.cor(AGB.m, "smJul", 1901:2010)
plot.agbi.cor(AGB.m, "smAug", 1901:2010)

plot.agbi.cor(AGB.m, "prJun", 850:1800)
plot.agbi.cor(AGB.m, "prJul", 850:1800)
plot.agbi.cor(AGB.m, "prAug", 850:1800)
plot.agbi.cor(AGB.m, "tJun", 850:1800)
plot.agbi.cor(AGB.m, "tJul", 850:1800)
plot.agbi.cor(AGB.m, "tAug", 850:1800)
plot.agbi.cor(AGB.m, "smJun", 850:1800)
plot.agbi.cor(AGB.m, "smJul", 850:1800)
plot.agbi.cor(AGB.m, "smAug", 850:1800)

plot.agbi.cor(AGB.m, "prJun", 1800:1900)
plot.agbi.cor(AGB.m, "prJul", 1800:1900)
plot.agbi.cor(AGB.m, "prAug", 1800:1900)
plot.agbi.cor(AGB.m, "tJun", 1800:1900)
plot.agbi.cor(AGB.m, "tJul", 1800:1900)
plot.agbi.cor(AGB.m, "tAug", 1800:1900)
plot.agbi.cor(AGB.m, "smJun", 1800:1900)
plot.agbi.cor(AGB.m, "smJul", 1800:1900)
plot.agbi.cor(AGB.m, "smAug", 1800:1900)

# to do: make a map of the highest correlation at each grid cell
map.highest.cor <- function(model, yr){
  load("Data/PalEON_siteInfo_all.RData")
  paleon$site <- paste0("X",paleon$num)
  paleon2 <- paleon
  cor.dir <- paste0("C:/Users/JMac/Documents/Kelly/MIP/WUE_MIP/WUE_MIP/outputs/preliminaryplots/sensitivity/agbi/",model,"/cors/")
  cor.files <- list.files(path = cor.dir, pattern=paste0({yr[1]}, "_", {yr[length(yr)]}, ".rds"))
    
    
    for (i in 1:length(cor.files)){
      cor.table <- readRDS(paste0(cor.dir, cor.files[i]))
      
      paleon2  <- merge(cor.table[, c(1:4,13)], paleon2, by = c("site", "num", "lon", "lat") )
    rm(cor.table)
      }
    
    # find the parameter with highest correlation to AGBI:
    paleon2[is.na(paleon2)]<- 0
    colnames(paleon2)[5:13] <- c("JunTair", "JulTair", "AugTair", "JunMoist", "JulMoist", "AugMoist", "JunPr","JulPr","AugPr" )
    paleon2$highest <- colnames(paleon2[,5:13])[max.col(abs(paleon2[,5:13]),ties.method="first")]
    
    # map out these in space:
    states <- map_data("state")
    states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
    coordinates(states) <- ~long+lat
    class(states)
    proj4string(states) <- CRS("+proj=longlat +datum=NAD83")
    states <- spTransform(states,CRSobj = '+init=epsg:4326')
    mapdata <- data.frame(states)
    
    png(height=4, width = 7, units="in",res=300 ,paste0("outputs/preliminaryplots/sensitivity/agbi/", model,"/",model,"_agbi_highest_cors_",period[1],"_",period[length(period)],".png"))
    print(ggplot(paleon2, aes(x = lon, y=lat, fill= highest))+geom_raster()+scale_fill_manual(values=c('#b2182b','#d6604d','#f4a582','#8c510a','#bf812d',
                                                                                               '#dfc27d',
                                                                                               '#80cdc1','#35978f',
                                                                                               '#01665e'),
                                                                                             limits = c("JunTair", "JulTair", "AugTair", "JunMoist", "JulMoist", "AugMoist", "JunPr","JulPr","AugPr"))+
            geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle(paste0('Highest correlation between AGBI and climate',yr[1],"-",yr[length(yr)])))
    
    dev.off()
}

map.highest.cor(model = "GUESS", yr=1901:2010)
map.highest.cor(model = "GUESS", yr=1800:1900)
map.highest.cor(model = "GUESS", yr=850:1800)

# for ED
#ggplot(paleon, aes(lon, lat, fill = highest))+geom_raster()
# also what is the relationship between denisty and AGBI
