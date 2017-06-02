# This script will look at Fcomp, Density, possibly BA? trends through time
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)
library(tidyr)

# load the necessary data:


# load the pft specific data:
ED2.Dens <- readRDS("D:/Kelly/WUE_MIP/Data/ED2/ED2.Dens.rds")
ED2.Fcomp <- readRDS("D:/Kelly/WUE_MIP/Data/ED2/ED2.Fcomp.RDS")

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
  png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/ED2_Dens_",paleon[i,]$latlon, ".png"))
  dens.site <- data.frame(Dens.r[,i,])
  dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
  ggplot(dens.site, aes(Totaldens))+geom_histogram()+theme_bw()
  dev.off()
  
  site.m <- melt(dens.site, id.vars = c("Totaldens"))
  png(height = 4, width= 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/ED2_Dens_byPFT", paleon[i,]$latlon, '.png'))
  ggplot(site.m, aes(value, fill= variable))+geom_histogram()+facet_wrap(~variable)+theme_bw()
  dev.off()
}

# make a dataframe of total density and density sd to get an idea of how much each grid cell varies:
Dens.mean <- data.frame(num = paleon$num, 
                        lon = )
dens.site <- data.frame(Dens.r[,i,])
dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
Dens.mean[i,]$mean <- mean(dens.site$Total.dens, na.rm=TRUE)
Dens.mean[i,]$sd <- sd(dens.site$Total.dens, na.rm=TRUE)
