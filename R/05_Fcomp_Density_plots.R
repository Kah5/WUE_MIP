# This script will look at Fcomp, Density, possibly BA? trends through time
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)
library(tidyr)

# load the necessary data:


# load the pft specific data:
ED2.Dens <- readRDS("D:/Kelly/WUE_MIP/Data/ED2/ED2.Dens.rds")
ED2.Fcomp <- readRDS("D:/Kelly/WUE_MIP/Data/ED2/ED2.Fcomp.RDS")
ED2.CO2 <- readRDS('D:/Kelly/WUE_MIP/Data/ED2/ED2.CO2.rds')
load("D:/Kelly/WUE_MIP/Data/PalEON_siteInfo_all.RData")

timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850
# try unlist ot convert Fcomp to a df
#df <- data.frame(matrix(unlist(Fcomp), nrow=13932, byrow=T),stringsAsFactors=FALSE)
#test <- do.call(rbind, lapply(Fcomp, data.frame, stringsAsFactors=FALSE))

#convert list to array
pft.lab=c("grass.c4", "tropic.early", "tropic.mid", "tropic.late", "grass.c3.temp", "pine.north", "pine.south", "conifer.late", "temp.decid.early", "temp.decid.mid", "temp.decid.late","ag1", "ag2", "ag3", "ag4","grass.c3.subtrop","Araucaria")

Fcomp<- ED2.Fcomp
Dens <- ED2.Dens
CO2 <- ED2.CO2


dimnames(Fcomp) <- list(timevec, paleon$num, pft.lab)
dimnames(Dens) <- list(timevec, paleon$num, pft.lab)
#grass <- ED2.Fcomp[,,"grass.c4"]
#dens <- dens$Dens



# plot pfts that occurred in ED runs:


# reduce to the actual pfts present:
pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )
Fcomp.r <- Fcomp[,,pfts]
Dens.r <- Dens[,,pfts]

# plots of Fcomp at each site (for ED2):

for(i in 1:length(paleon$num)){
  png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Fcomp/ED2_Fcomp_",paleon[i,]$latlon, ".png"))
  plot(Fcomp.r[,i,"pine.north"], ylim = c(0,1.5), col = 'red', ylab = "Fcomp", xlab = "Months since 850")
  points(Fcomp.r[,i,"conifer.late"], col = "forestgreen")
  points(Fcomp.r[,i,"temp.decid.early"], col = "lightblue")
  points(Fcomp.r[,i,"temp.decid.mid"], col = 'blue')
  points(Fcomp.r[,i,"temp.decid.late"], col = "orange")
  points(Fcomp.r[,i,"grass.c3.temp"], col = "black")
  legend('topleft',legend=c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
                            "temp.decid.late", "grass.c3.temp" ), 
         col = c('red', 'forestgreen', 'lightblue', 'blue', 'orange', 'black'), pch=16)
  dev.off()
}


# make the plot for density:
for(i in 1:length(paleon$num)){
  png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/ED2_Dens_",paleon[i,]$latlon, ".png"))
  plot(Dens.r[,i,"pine.north"] , ylim=c(10000, 1000000), col = 'red', ylab = "Dens", xlab = "Months since 850")
  points(Dens.r[,i,"conifer.late"], col = "forestgreen")
  points(Dens.r[,i,"temp.decid.early"], col = "lightblue")
  points(Dens.r[,i,"temp.decid.mid"], col = 'blue')
  points(Dens.r[,i,"temp.decid.late"], col = "orange")
  points(Dens.r[,i,"grass.c3.temp"], col = "black")
  legend('topleft',legend=c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
                            "temp.decid.late", "grass.c3.temp" ), 
         col = c('red', 'forestgreen', 'lightblue', 'blue', 'orange', 'black'), pch=16)
  dev.off()
}

# Histograms of total density by site:
for(i in 1:length(paleon$num)){
  png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/ED2_Dens_",paleon[i,]$latlon, ".png"))
  dens.site <- data.frame(Dens.r[,i,])
  dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
  print(ggplot(dens.site, aes(Totaldens))+geom_histogram()+theme_bw())
  dev.off()
  
  site.m <- melt(dens.site, id.vars = c("Totaldens"))
  png(height = 4, width= 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/ED2_Dens_byPFT", paleon[i,]$latlon, '.png'))
  print(ggplot(site.m, aes(value, fill= variable))+geom_histogram()+facet_wrap(~variable)+theme_bw())
  dev.off()
}

# make a dataframe of total density and density sd to get an idea of how much each grid cell varies:
library(modes)
Dens.mean <- data.frame(num = paleon$num, 
                        lon = paleon$lon, 
                        lat = paleon$lat, 
                        latlon = paleon$latlon,
                        mean = NA, 
                        sd = NA, 
                        pval= NA, 
                        BC = NA)

for(i in 1:length(paleon$num)){
  dens.site <- data.frame(Dens.r[,i,])
  dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
  Dens.mean[i,]$mean <- mean(dens.site$Totaldens, na.rm=TRUE)
  Dens.mean[i,]$sd <- sd(dens.site$Totaldens, na.rm=TRUE)
  Dens.mean[i,]$pval <- diptest::dip.test(na.omit(density(dens.site$Totaldens)$y))$p
  Dens.mean[i,]$BC <- bimodality_coefficient(na.omit(dens.site$Totaldens)) 
}

# save the file for future use:
saveRDS(Dens.mean, paste0(getwd(), "/outputs/data/ED2/ED2.meandens.rds"))

# plot the mean density out on a map:
states <- map_data("state")
states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
states <- spTransform(states,CRSobj = '+init=epsg:4326')
mapdata <- data.frame(states)

cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")

# map of mean density:
png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_mean_dens_map.png"))
ggplot(Dens.mean, aes(x = lon, y = lat, fill = mean))+geom_raster()+
  scale_fill_gradientn(colours = cbpalette, limits = c(0,3000000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw()+ ggtitle('Mean total density')
dev.off()

rbpalette<- c('#67001f',
  '#b2182b',
  '#d6604d',
  '#f4a582',
  '#fddbc7',
  '#d1e5f0',
  '#92c5de',
  '#4393c3',
  '#2166ac',
  '#053061')
# map of sd of density:
png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_sd_dens_map.png"))
ggplot(Dens.mean, aes(x = lon, y = lat, fill = sd))+geom_raster()+
  scale_fill_gradientn(colours = rev(rbpalette), limits = c(0,500000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('SD of total density')
dev.off()

# map of places with significantly bimodal distribution in tree density over time:

bimodal <- ifelse(Dens.mean$BC > 0.55 & Dens.mean$pval <= 0.05, "Bimodal", 'Unimodal')
Dens.mean$bimodal <- bimodal

png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_bimodal_time_map.png"))
ggplot(Dens.mean, aes(x = lon, y = lat, fill = bimodal))+geom_raster()+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Places where density has bimodal distn.')
dev.off()


# compare this to a map of fire frequency:
fire <- readRDS(paste0(getwd(),'/Data/ED2/ED2.Fire.rds'))
dimnames(fire) <- list(timevec, paleon$num)
df.fire <- data.frame(fire)
fire.tots <- Dens.mean
fire.tots$Fire.tots <- colSums(fire, na.rm=TRUE) # find the total number of fires at each grid cell
fire.tots$countfires <- colSums(fire != 0, na.rm=TRUE)

png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_Fire_emmissions_map.png"))
ggplot(fire.tots, aes(x = lon, y = lat, fill = Fire.tots))+geom_raster()+
  scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total Fire Emmissions kgC/m2/s", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total Fire emmissions 850-2011')
dev.off()


png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_fire_counts_map.png"))
ggplot(fire.tots, aes(x = lon, y = lat, fill = countfires))+geom_raster()+
  scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total number of Fires", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total number of fires 850-2011')
dev.off()

# basic plots of fire emmissions vs mean density and sd:
ggplot(fire.tots, aes(x = Fire.tots, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Total fire emmissions 850-2011")
ggplot(fire.tots, aes(x = Fire.tots, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Total fire emmissions 850-2011")

# plots of fire counts vs. mean density and sd:
ggplot(fire.tots, aes(x = countfires, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Number of fires 850-2011")
ggplot(fire.tots, aes(x = countfires, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Number of fires 850-2011")

#----------- What is the sensitivity of total density to CO2 in ED2?-------------
atm.co2<- CO2[,1]

sens.mean <- data.frame(num = paleon$num, 
                        lon = paleon$lon, 
                        lat = paleon$lat, 
                        latlon = paleon$latlon,
                        corCO2 = NA, 
                        mean=NA,
                        sd = NA
                        )

for(i in 1:length(paleon$num)){
  dens.site <- data.frame(Dens.r[,i,])
  dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
  sens.mean[i,]$corCO2 <- cor(dens.site$Totaldens, atm.co2)
  sens.mean[i,]$mean <- mean(dens.site$Totaldens, na.rm= TRUE)
  sens.mean[i,]$sd <- sd(dens.site$Totaldens, na.rm=TRUE)
}

# it looks like in ED, the west is more highly positively corrllated with CO2:
png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_dens_co2_cor_map.png"))
ggplot(sens.mean, aes(x = lon, y = lat, fill = corCO2))+geom_raster()+
  scale_fill_gradientn(colours = rev(rbpalette), limits = c(-1,1), name ="correlation coefficient", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Correlation of Total Density with CO2')
dev.off()

#------------- Are WUE increases higher in the West then?-------------------
WUEt <- readRDS(paste0(getwd(), "/Data/ED2/ED2.WUEt.rds"))
WUEi <- readRDS(paste0(getwd(), "/Data/ED2/ED2.WUEi.rds"))
IWUE <- readRDS(paste0(getwd(), "/Data/ED2/ED2.IWUE.rds"))


WUE.cor.mean <- data.frame(num = paleon$num, 
                        lon = paleon$lon, 
                        lat = paleon$lat, 
                        latlon = paleon$latlon,
                        corIWUE = NA, 
                        corWUEi = NA,
                        corWUEt = NA,
                        mean=NA,
                        sd = NA
)

for(i in 1:length(paleon$num)){
  dens.site <- data.frame(Dens.r[,i,])
  dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
  WUE.cor.mean[i,]$corIWUE <- cor(dens.site$Totaldens, IWUE[,i])
  WUE.cor.mean[i,]$corWUEi <- cor(dens.site$Totaldens, WUEi[,i])
  WUE.cor.mean[i,]$corWUEt <- cor(dens.site$Totaldens, WUEt[,i])
  WUE.cor.mean[i,]$mean <- mean(dens.site$Totaldens, na.rm= TRUE)
  WUE.cor.mean[i,]$sd <- sd(dens.site$Totaldens, na.rm=TRUE)
}

# map out the correlations with WUE
png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_dens_WUE_cor_map.png"))
ggplot(WUE.cor.mean, aes(x = lon, y = lat, fill = corIWUE))+geom_raster()+
  scale_fill_gradientn(colours = rev(rbpalette), limits = c(-1,1), name ="correlation coefficient", na.value = 'darkgrey')+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Correlation of Total Density with WUE')
dev.off()