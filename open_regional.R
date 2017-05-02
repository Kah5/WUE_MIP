# -------------------------------------------------------
# Scripts to read in, combine, & map output from the PalEON Regional Runs
# Created: Christy Rollinson, crollinson@gmail.com, Feb 2016
# -------------------------------------------------------


# -----------------------------
# libraries we're going to be using
# -----------------------------
library(ncdf4)
library(raster)
#library(car)
library(abind)
library(ggplot2)
library(reshape2)
# -----------------------------

# -----------------------------
# Setting paths & directories
# -----------------------------
setwd("C:/Users/JMac/Documents/Kelly/MIP/WUE_MIP/")
path.figs <- "phase2_model_output/Exploratory_Figs"
if(!dir.exists(path.figs)) dir.create(path.figs)

path.guess <- "phase2_model_output/LPJ-GUESS_annual.nc"
#path.jules <- "phase2_model_output/jules"
#path.wsl   <- "phase2_model_output/LPJ-WSL/LPJ-WSL.v1"

sec2yr <- 60*60*24*365

mo.yr <- vector()
for(y in 850:2010){
  mo.yr <- c(mo.yr, rep(y, 12))
}
length(mo.yr)
mo.yr[1:37]

# -----------------------------


# -----------------------------
# 1. LPJ-GUESS
# -----------------------------
# Looking at a few annual things first
guess.ann <- nc_open(file.path(path.guess, "LPJ-GUESS_annual.nc"))
summary(guess.ann$var)

guess.out <- list()
guess.out$lat <- ncvar_get(guess.ann, "lat")
guess.out$lon <- ncvar_get(guess.ann, "lon")
guess.out$Year <- ncvar_get(guess.ann, "time") + 850
guess.out$PFT <- ncvar_get(guess.ann, "PFT")
for(v in names(guess.ann$var)){
  guess.out[[v]] <- ncvar_get(guess.ann, v)  
}

dim(guess.out$Fcomp)

# pull out density
Dens <- guess.out$Dens
# oull out fire
Fire <- guess.out$Fire
# pull out GWBI--Gross Woody biomass increment(akin to tree ring width increment)
# in KgC/m2/mo
FPC <- guess.out$FPC
Fcomp <- guess.out$Fcomp
Mort <- guess.out$Mort
Estab <- guess.out$Estab
BA <- guess.out$BA
# lets pull out density for year == 1850


plotdes <- function(Dens, yearno, PFT){
Year <- yearno+850
dens1850 <- Dens[,,,yearno]
#dens1850$evg <- rowSums
tab <- melt(dens1850)

colnames(tab) <- c("pft", "lon", "lat", "Dens")

# plot the density of each PFT for 1850
ggplot(tab[tab$pft == PFT,], aes(x = lon, y = lat, fill = Dens))+geom_raster()+theme_bw()+ggtitle(paste0("Total Density ", Year))
#ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}





pft <- data.frame(pft = 1:13, 
                  names = 1:13)
pft$names <- as.character(guess.out$PFT)
colnames(pft) <- c("pft", "names")
pft$fullnames <- c("Boreal needleleaf evergreeen", "Boreal needleleaf evergreen shade intolerant", 
                   "Boreal needleleaf summergreen", "Boreal needleleaf summergreen shade intolerant", 
                   "Temperate broadleaf summergreen", "Temperate broadleaf summergreen shade intolerant",
                   "Temperate broadleaf evergreen", "Tropical broadleaf evergreen", "Tropical broadleaf evergreen shade intolerant", 
                   "Tropical broadleaf raingreen", "C3 grass", "C4 grass", "Total")

plotlatlon <- function(Dens, lon,lat){
  #Year <- yearno+850
  dens1850 <- Dens[,lon,lat,]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c("pft", "year", "Dens")
  tab <- merge(tab, pft, by = "pft")
  # plot the density of each PFT for 1850
  ggplot(tab, aes(x = year, y = Dens))+geom_point()+theme_bw()+ggtitle(paste0("Total Density ","lat = ", lat,"lon" =lon))+ 
    facet_wrap(~names) + scale_color_manual(name = "PFT", values = )
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}

pdf("transect_1_density_LPJ_lat_20.pdf")
plotlatlon(Dens = Dens, lon = 1, lat = 20) # lower density in past
plotlatlon(Dens = Dens, lon = 5, lat = 20) # lower density in past
plotlatlon(Dens = Dens, lon = 10,lat = 20) # MN area
plotlatlon(Dens = Dens, lon = 15,lat = 20) # MN area
plotlatlon(Dens = Dens, lon = 20,lat = 20) # wisconsin area
plotlatlon(Dens = Dens, lon = 21,lat = 20) # wisconsin area
plotlatlon(Dens = Dens, lon = 25,lat = 20) # wisconsin area
plotlatlon(Dens = Dens, lon = 30,lat = 20) # michigan area
dev.off()

pdf("transect_2_denisty_LPJ_lat_13.pdf")
plotlatlon(Dens = Dens, lon = 1, lat = 13) # lower density in past
plotlatlon(Dens = Dens, lon = 5, lat = 13) # lower density in past
plotlatlon(Dens = Dens, lon = 10,lat = 13) # MN area
plotlatlon(Dens = Dens, lon = 15,lat = 13) # MN area
plotlatlon(Dens = Dens, lon = 20,lat = 13) # wisconsin area
plotlatlon(Dens = Dens, lon = 21,lat = 13) # wisconsin area
plotlatlon(Dens = Dens, lon = 25,lat = 13) # wisconsin area
plotlatlon(Dens = Dens, lon = 30,lat = 13) # michigan area
dev.off()


plotlatlonone <- function(fact, lon,lat, name){
  #Year <- yearno+850
  dens1850 <- fact[,lon,lat,]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c("pft", "year", name)
  colnames(tab) <- c("pft", "year", name)
  tab <- merge(tab, pft, by = "pft")
  # plot the density of each PFT for 1850
  #pft # that show up in LPJ guess: 1,2, 4,5,6,7, 11, 13
  pftsin <- c(1,2,4,5,6,7, 11)
  tab <- tab[tab$pft %in% pftsin,]
  ggplot(tab, aes(x = year, y = tab[,c(name)], color = fullnames))+geom_line()+theme_bw()+
    scale_color_manual(name = "PFT", values = c('#e41a1c',
      '#377eb8',
      '#984ea3',
      'forestgreen',
      '#ff7f00',
      'black',
      '#a65628'))+
    ggtitle(paste(name, "lat = ", lat, "lon =", lon)) + ylab(name) +xlab("Years after 850 AD")+ 
    theme(legend.position = "bottom")+guides(fill=guide_legend(ncol =2,byrow=FALSE))
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}

pdf("transect_1_density_one_LPJ_lat_20.pdf")
plotlatlonone(fact = Dens, lon = 1, lat = 20, name = "Dens") # lower density in past
plotlatlonone(fact = Dens, lon = 5, lat = 20, name = "Dens") # lower density in past
plotlatlonone(fact = Dens, lon = 10,lat = 20, name = "Dens") # MN area
plotlatlonone(fact = Dens, lon = 15,lat = 20, name = "Dens") # MN area
plotlatlonone(fact = Dens, lon = 20,lat = 20, name = "Dens") # wisconsin area
plotlatlonone(fact = Dens, lon = 21,lat = 20, name = "Dens") # wisconsin area
plotlatlonone(fact = Dens, lon = 25,lat = 20, name = "Dens") # wisconsin area
plotlatlonone(fact = Dens, lon = 30,lat = 20, name = "Dens") # michigan area
dev.off()

pdf("transect_2_denisty_one_LPJ_lat_13.pdf")
plotlatlonone(fact = Dens, lon = 1, lat = 13, name = "Dens") # lower density in past
plotlatlonone(fact = Dens, lon = 5, lat = 13, name = "Dens") # lower density in past
plotlatlonone(fact = Dens, lon = 10,lat = 13, name = "Dens") # MN area
plotlatlonone(fact = Dens, lon = 15,lat = 13, name = "Dens") # MN area
plotlatlonone(fact = Dens, lon = 20,lat = 13, name = "Dens") # wisconsin area
plotlatlonone(fact = Dens, lon = 21,lat = 13, name = "Dens") # wisconsin area
plotlatlonone(fact = Dens, lon = 25,lat = 13, name = "Dens") # wisconsin area
plotlatlonone(fact = Dens, lon = 30,lat = 13, name = "Dens") # michigan area
dev.off()

# working with fires

plotlatlonfire <- function(fact, lon,lat, name){
  #Year <- yearno+850
  dens1850 <- fact[lon,lat,]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c(name)
  tab$year <- 1:1161
  # plot the density of each PFT for 1850
  #pft # that show up in LPJ guess: 1,2, 4,5,6,7, 11, 13
  
  ggplot(tab, aes(x = year, y = tab[,c(name)]))+geom_line()+theme_bw()+
    ggtitle(paste(name, "lat = ", lat, "lon =", lon)) + ylab(name) +xlab("Years after 850 AD")+ 
    theme(legend.position = "bottom")+guides(fill=guide_legend(ncol =2,byrow=FALSE))
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}

plotfire <- function(fact, yearno,name){
  Year <- yearno+850
  dens1850 <- fact[,,yearno]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c('lat','lon',name)
  
  # plot the density of each PFT for 1850
  #pft # that show up in LPJ guess: 1,2, 4,5,6,7, 11, 13
  
  ggplot(tab, aes(x = lat, y = lon,fill= tab[,c(name)]))+geom_raster()+theme_bw()+
    ggtitle(paste("Pfire for", Year)) + ylab(name) 
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}

plotfire(Fire, 1000, "Fire")
plotfire(Fire, 1010, "Fire")
plotfire(Fire, 1020, "Fire")
plotfire(Fire, 1030, "Fire")
plotfire(Fire, 1050, "Fire")
plotfire(Fire, 1090, "Fire")
plotfire(Fire, 1100, "Fire")
plotfire(Fire, 1161, "Fire")




pdf("transect_1_fire_one_LPJ_lat_20.pdf")
plotlatlonfire(fact = Fire, lon = 1, lat = 20, name = "Fire") # lower density in past
plotlatlonfire(fact = Fire, lon = 5, lat = 20, name = "Fire") # lower Fireity in past
plotlatlonfire(fact = Fire, lon = 10,lat = 20, name = "Fire") # MN area
plotlatlonfire(fact = Fire, lon = 15,lat = 20, name = "Fire") # MN area
plotlatlonfire(fact = Fire, lon = 20,lat = 20, name = "Fire") # wisconsin area
plotlatlonfire(fact = Fire, lon = 21,lat = 20, name = "Fire") # wisconsin area
plotlatlonfire(fact = Fire, lon = 25,lat = 20, name = "Fire") # wisconsin area
plotlatlonfire(fact = Fire, lon = 30,lat = 20, name = "Fire") # michigan area
dev.off()

pdf("transect_2_Fire_one_LPJ_lat_13.pdf")
plotlatlonone(fact = Fire, lon = 1, lat = 13, name = "Fire") # lower Fireity in past
plotlatlonone(fact = Fire, lon = 5, lat = 13, name = "Fire") # lower Fireity in past
plotlatlonone(fact = Fire, lon = 10,lat = 13, name = "Fire") # MN area
plotlatlonone(fact = Fire, lon = 15,lat = 13, name = "Fire") # MN area
plotlatlonone(fact = Fire, lon = 20,lat = 13, name = "Fire") # wisconsin area
plotlatlonone(fact = Fire, lon = 21,lat = 13, name = "Fire") # wisconsin area
plotlatlonone(fact = Fire, lon = 25,lat = 13, name = "Fire") # wisconsin area
plotlatlonone(fact = Fire, lon = 30,lat = 13, name = "Fire") # michigan area
dev.off()


# places that were lower density in the past, show some increases in total density in the 20th cent.
# this is what we would expect, 
# places with higher density in the past don't have much of a noticible shift
# do these shifts corespond to higher increases in WUE in the prev. low denisty places?


# for 1850
pdf("Total_density_plots.pdf")
plotdes(Dens = Dens, yearno = 800, PFT = 13)
plotdes(Dens = Dens, yearno = 900, PFT = 13)
plotdes(Dens = Dens, yearno = 950, PFT = 13)
plotdes(Dens = Dens, yearno = 1000, PFT = 13)
plotdes(Dens = Dens, yearno = 1010, PFT = 13)
plotdes(Dens = Dens, yearno = 1060, PFT = 13)
plotdes(Dens = Dens, yearno = 1090, PFT = 13)
plotdes(Dens = Dens, yearno = 1110, PFT = 13)
plotdes(Dens = Dens, yearno = 1130, PFT = 13)
plotdes(Dens = Dens, yearno = 1150, PFT = 13)
plotdes(Dens = Dens, yearno = 1160, PFT = 13)
dev.off()

dens.hists <- function(Dens, yearno){
  Year <- yearno+850
  dens1850 <- Dens[,,,yearno]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c("pft", "lat","lon", "Dens")
  tab <- merge(tab, pft, by = "pft")
  # plot the density of each PFT for 1850
  ggplot(tab[tab$pft == 13,], aes(x = Dens))+geom_histogram()+theme_bw()+ ggtitle(paste0("Histogram of Total Density across space ", Year ))+xlab("Total Density")
    
  #facet_wrap(~names) + scale_color_manual(name = "PFT", values = )
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}

dens.hists(Dens, 950)# 1800
dens.hists(Dens, 1000)# 1850
dens.hists(Dens, 1050)# 1900
dens.hists(Dens, 1100)
dens.hists(Dens, 1161)#2011


# look at density histograms of a grid cell throuh time
dens.hists.time <- function(Dens, lat,lon){
  #Year <- yearno+850
  dens1850 <- Dens[,lat,lon,]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c("pft", "year", "Dens")
  tab <- merge(tab, pft, by = "pft")
  # plot the density of each PFT for 1850
  ggplot(tab[tab$pft == 13,], aes(x = Dens))+geom_histogram()+theme_bw()+ ggtitle(paste0("Histogram of Total Density 850-2011 at lat = ",lat, "lon = ",lon  ))+xlab("Total Density")
  
  #facet_wrap(~names) + scale_color_manual(name = "PFT", values = )
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}

# plot histograms of temporal densitys in time:
# not stem density is supposedly in trees/ha, but these are very high estimates
# perhaps there is a unit issue, or perhaps the grass stem density is inflating the total estimates
pdf("Transect_1_dens_hists_850_2011.pdf")
dens.hists.time(Dens, lon = 1, lat = 20)
dens.hists.time(Dens, lon = 5, lat = 20)
dens.hists.time(Dens, lon = 10, lat = 20)
dens.hists.time(Dens, lon = 15, lat = 20)
dens.hists.time(Dens, lon = 20, lat = 20)
dens.hists.time(Dens, lon = 25, lat = 20)
dens.hists.time(Dens, lon = 30, lat = 20)
dev.off()

pdf("Transect_2_dens_hists_850_2011.pdf")
dens.hists.time(Dens, lon = 1, lat = 13)
dens.hists.time(Dens, lon = 5, lat = 13)
dens.hists.time(Dens, lon = 10, lat = 13)
dens.hists.time(Dens, lon = 15, lat = 13)
dens.hists.time(Dens, lon = 20, lat = 13)
dens.hists.time(Dens, lon = 25, lat = 13)
dens.hists.time(Dens, lon = 30, lat = 13)
dev.off()

# extracting GWBI

##################3
plotgwbi <- function(GWBI, yearno, PFT){
  Year <- yearno+850
  gwbi1850 <- GWBI[,,,yearno]
  #dens1850$evg <- rowSums
  tab <- melt(gwbi1850)
  
  colnames(tab) <- c("pft", "lon", "lat", "GWBI")
  
  # plot the density of each PFT for 1850
  ggplot(tab[tab$pft == PFT,], aes(x = lon, y = lat, fill = GWBI))+geom_raster()+theme_bw()+ggtitle(paste0("Total Density ", Year))
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}
plotgwbi(GWBI, 1000, 13)
plotgwbi(GWBI, 1150, 13)



#use the function plotlatlonone to plot GWBI at a variety of sites
pdf("transect_1_GWBI_one_LPJ_lat_20.pdf")
plotlatlonone(fact = GWBI, lon = 1, lat = 20, name = "GWBI") # lower density in past
plotlatlonone(fact = GWBI, lon = 5, lat = 20, name = "GWBI") # lower density in past
plotlatlonone(fact = GWBI, lon = 10,lat = 20, name = "GWBI") # MN area
plotlatlonone(fact = GWBI, lon = 15,lat = 20, name = "GWBI") # MN area
plotlatlonone(fact = GWBI, lon = 20,lat = 20, name = "GWBI") # wisconsin area
plotlatlonone(fact = GWBI, lon = 21,lat = 20, name = "GWBI") # wisconsin area
plotlatlonone(fact = GWBI, lon = 25,lat = 20, name = "GWBI") # wisconsin area
plotlatlonone(fact = GWBI, lon = 30,lat = 20, name = "GWBI") # michigan area
dev.off()

pdf("transect_2_GWBI_one_LPJ_lat_13.pdf")
plotlatlonone(fact = GWBI, lon = 1, lat = 13, name = "GWBI") # lower density in past
plotlatlonone(fact = GWBI, lon = 5, lat = 13, name = "GWBI") # lower density in past
plotlatlonone(fact = GWBI, lon = 10,lat = 13, name = "GWBI") # MN area
plotlatlonone(fact = GWBI, lon = 15,lat = 13, name = "GWBI") # MN area
plotlatlonone(fact = GWBI, lon = 20,lat = 13, name = "GWBI") # wisconsin area
plotlatlonone(fact = GWBI, lon = 21,lat = 13, name = "GWBI") # wisconsin area
plotlatlonone(fact = GWBI, lon = 25,lat = 13, name = "GWBI") # wisconsin area
plotlatlonone(fact = GWBI, lon = 30,lat = 13, name = "GWBI") # michigan area
dev.off()
# plot out from 1800-2011

plotlatlonmod <- function(fact, lon,lat, name){
  #Year <- yearno+850
  dens1850 <- fact[,lon,lat,]
  #dens1850$evg <- rowSums
  tab <- melt(dens1850)
  
  colnames(tab) <- c("pft", "year", name)
  colnames(tab) <- c("pft", "year", name)
  tab <- merge(tab, pft, by = "pft")
  # plot the density of each PFT for 1850
  #pft # that show up in LPJ guess: 1,2, 4,5,6,7, 11, 13
  pftsin <- c(1,2,4,5,6,7, 11)
  tab <- tab[tab$pft %in% pftsin,]
  tab <- tab[tab$year %in% 950:1161,]
  ggplot(tab, aes(x = year, y = tab[,c(name)], color = fullnames))+geom_line()+theme_bw()+
    scale_color_manual(name = "PFT", values = c('#e41a1c',
                                                '#377eb8',
                                                '#984ea3',
                                                'forestgreen',
                                                '#ff7f00',
                                                'black',
                                                '#a65628'))+
    ggtitle(paste(name, "lat = ", lat, "lon =", lon)) + ylab(name) +xlab("Years after 850 AD")+ 
    theme(legend.position = "bottom")+guides(fill=guide_legend(ncol =2,byrow=FALSE))
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}

pdf("transect_1_GWBI_one_LPJ_lat_20_1800_2011.pdf")
plotlatlonmod(fact = GWBI, lon = 1, lat = 20, name = "GWBI") # lower density in past
plotlatlonmod(fact = GWBI, lon = 5, lat = 20, name = "GWBI") # lower density in past
plotlatlonmod(fact = GWBI, lon = 10,lat = 20, name = "GWBI") # MN area
plotlatlonmod(fact = GWBI, lon = 15,lat = 20, name = "GWBI") # MN area
plotlatlonmod(fact = GWBI, lon = 20,lat = 20, name = "GWBI") # wisconsin area
plotlatlonmod(fact = GWBI, lon = 21,lat = 20, name = "GWBI") # wisconsin area
plotlatlonmod(fact = GWBI, lon = 25,lat = 20, name = "GWBI") # wisconsin area
plotlatlonmod(fact = GWBI, lon = 30,lat = 20, name = "GWBI") # michigan area
dev.off()

pdf("transect_2_GWBI_one_LPJ_lat_13_1800_2011.pdf")
plotlatlonmod(fact = GWBI, lon = 1, lat = 13, name = "GWBI") # lower density in past
plotlatlonmod(fact = GWBI, lon = 5, lat = 13, name = "GWBI") # lower density in past
plotlatlonmod(fact = GWBI, lon = 10,lat = 13, name = "GWBI") # MN area
plotlatlonmod(fact = GWBI, lon = 15,lat = 13, name = "GWBI") # MN area
plotlatlonmod(fact = GWBI, lon = 20,lat = 13, name = "GWBI") # wisconsin area
plotlatlonmod(fact = GWBI, lon = 21,lat = 13, name = "GWBI") # wisconsin area
plotlatlonmod(fact = GWBI, lon = 25,lat = 13, name = "GWBI") # wisconsin area
plotlatlonmod(fact = GWBI, lon = 30,lat = 13, name = "GWBI") # michigan area
dev.off()



#use the function plotlatlonone to plot Fcomp at a variety of sites
pdf("transect_1_Fcomp_one_LPJ_lat_20.pdf")
plotlatlonone(fact = Fcomp, lon = 1, lat = 20, name = "Fcomp") # lower density in past
plotlatlonone(fact = Fcomp, lon = 5, lat = 20, name = "Fcomp") # lower density in past
plotlatlonone(fact = Fcomp, lon = 10,lat = 20, name = "Fcomp") # MN area
plotlatlonone(fact = Fcomp, lon = 15,lat = 20, name = "Fcomp") # MN area
plotlatlonone(fact = Fcomp, lon = 20,lat = 20, name = "Fcomp") # wisconsin area
plotlatlonone(fact = Fcomp, lon = 21,lat = 20, name = "Fcomp") # wisconsin area
plotlatlonone(fact = Fcomp, lon = 25,lat = 20, name = "Fcomp") # wisconsin area
plotlatlonone(fact = Fcomp, lon = 30,lat = 20, name = "Fcomp") # michigan area
dev.off()

pdf("transect_2_Fcomp_one_LPJ_lat_13.pdf")
plotlatlonone(fact = Fcomp, lon = 1, lat = 13, name = "Fcomp") # lower density in past
plotlatlonone(fact = Fcomp, lon = 5, lat = 13, name = "Fcomp") # lower density in past
plotlatlonone(fact = Fcomp, lon = 10,lat = 13, name = "Fcomp") # MN area
plotlatlonone(fact = Fcomp, lon = 15,lat = 13, name = "Fcomp") # MN area
plotlatlonone(fact = Fcomp, lon = 20,lat = 13, name = "Fcomp") # wisconsin area
plotlatlonone(fact = Fcomp, lon = 21,lat = 13, name = "Fcomp") # wisconsin area
plotlatlonone(fact = Fcomp, lon = 25,lat = 13, name = "Fcomp") # wisconsin area
plotlatlonone(fact = Fcomp, lon = 30,lat = 13, name = "Fcomp") # michigan area
dev.off()

# # plot fcomp 

plotfcomp <- function(Fcomp, yearno){
  Year <- yearno+850
  gwbi1850 <- Fcomp[,,,yearno]
  #dens1850$evg <- rowSums
  tab <- melt(gwbi1850)
  
  colnames(tab) <- c("pft", "lon", "lat", "Fcomp")
  tab <- merge(tab, pft, by = "pft")
  pftsin <- c(1,2,4,5,6,7, 11)
  tab <- tab[tab$pft %in% pftsin,]
  # plot the density of each PFT for 1850
  ggplot(tab, aes(x = lon, y = lat, fill = Fcomp))+geom_raster()+theme_bw()+
    ggtitle(paste0("Fractional Composition of PFT's ", Year))+facet_wrap(~fullnames)+
    scale_fill_gradient(low = "white", high = "forestgreen")
  #ggplot(tab, aes(x = lon, y = lat, fill = Dens))+geom_raster()+facet_wrap(~pft)
}
#X11(width = 12)
#pdf("LPJ-Guess_Fcomp_maps.pdf")
png(height = 8, width = 13, units = 'in', res= 300, "Fcomp_1800_LPJ_guess.png")
plotfcomp(Fcomp, 950)
dev.off()

png(height = 8, width = 13, units = 'in', res= 300, "Fcomp_1850_LPJ_guess.png")
plotfcomp(Fcomp, 1000)
dev.off()

png(height = 8, width = 13, units = 'in', res= 300, "Fcomp_2000_LPJ_guess.png")
plotfcomp(Fcomp, 1150)
dev.off()
# next we want to extract GPP and ET to calculate WUE
# this may be in the montly LPJ runs
# we then want to relate 

unique(guess.out$PFT)

# -----------------------------

# -----------------------------
# 2. LPJ-WSL
# -----------------------------
# Getting a list of the files we have
files.wsl <- dir(file.path(path.wsl), ".nc")

# Creating a list to store things in
wsl.out <- list()

# Adding some of the global variables
wsl.temp <- nc_open(file.path(path.wsl, files.wsl[1]))
wsl.out$lat <- ncvar_get(wsl.temp, "lat")
wsl.out$lon <- ncvar_get(wsl.temp, "lon")

wsl.out$Month <- ncvar_get(wsl.temp, "Month")
nc_close(wsl.temp)

for(i in 1:length(files.wsl)){
  print(paste0(" ====== ", "Processing File: ", files.wsl[i], " ====== "))
  wsl.temp <- nc_open(file.path(path.wsl, files.wsl[i]))
  if(i == 1){
    wsl.out$Year <- ncvar_get(wsl.temp, "Year")
    for(v in names(wsl.temp$var)){
      wsl.out[[v]] <- ncvar_get(wsl.temp, v)  
    }
  } else {
    wsl.out$Year <- c(wsl.out$Year, ncvar_get(wsl.temp, "Year"))
    for(v in names(wsl.temp$var)){
      wsl.out[[v]] <- abind(wsl.out[[v]], ncvar_get(wsl.temp, v), along=3)
    }    
  }
  nc_close(wsl.temp)
}

summary(wsl.out)
dim(wsl.out$Fcomp)
dim(wsl.out$NPP)
# -----------------------------

# -----------------------------
# 3. Jules
# -----------------------------


# -----------------------------

# -----------------------------
# Summarizing Fcomp
# -----------------------------
# -----------
# LPJ-GUESS
# -----------
for(y in 1:dim(guess.out$Fcomp)[3]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.evg <- stack(data.frame(apply(guess.out$Fcomp[c(1,2,7,8,9),,y,], c(2,3), FUN=sum)))
  names(dat.evg) <- c("Fcomp", "Year")
  dat.evg$Year <- as.numeric(substr(dat.evg$Year,2,nchar(paste(dat.evg$Year)))) + 849
  dat.evg$lat  <- guess.out$lat[y]
  dat.evg$lon  <- guess.out$lon
  dat.evg$PFT  <- as.factor("Evergreen")      
  
  dat.decid <- stack(data.frame(apply(guess.out$Fcomp[c(3:6,10),,y,], c(2,3), FUN=sum)))
  names(dat.decid) <- c("Fcomp", "Year")
  dat.decid$Year <- as.numeric(substr(dat.decid$Year,2,nchar(paste(dat.decid$Year)))) + 849
  dat.decid$lat  <- guess.out$lat[y]
  dat.decid$lon  <- guess.out$lon
  dat.decid$PFT  <- as.factor("Deciduous")
  
  dat.grass <- stack(data.frame(apply(guess.out$Fcomp[c(11:12),,y,], c(2,3), FUN=sum)))
  names(dat.grass) <- c("Fcomp", "Year")
  dat.grass$Year <- as.numeric(substr(dat.grass$Year,2,nchar(paste(dat.grass$Year)))) + 849
  dat.grass$lat  <- guess.out$lat[y]
  dat.grass$lon  <- guess.out$lon
  dat.grass$PFT  <- "Grass"        
  
  if(y==1) guess.fcomp <- rbind(dat.evg, dat.decid, dat.grass) else guess.fcomp <- rbind(guess.fcomp, dat.evg, dat.decid, dat.grass)
}
summary(guess.fcomp)

# lets try to make this run without taking up as much memory
bylat <- function(y)
  {
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.evg <- stack(data.frame(apply(guess.out$Fcomp[c(1,2,7,8,9),,y,], c(2,3), FUN=sum)))
  names(dat.evg) <- c("Fcomp", "Year")
  dat.evg$Year <- as.numeric(substr(dat.evg$Year,2,nchar(paste(dat.evg$Year)))) + 849
  dat.evg$lat  <- guess.out$lat[y]
  dat.evg$lon  <- guess.out$lon
  dat.evg$PFT  <- as.factor("Evergreen")    
  dat.evg

  dat.decid <- stack(data.frame(apply(guess.out$Fcomp[c(3:6,10),,y,], c(2,3), FUN=sum)))
  names(dat.decid) <- c("Fcomp", "Year")
  dat.decid$Year <- as.numeric(substr(dat.decid$Year,2,nchar(paste(dat.decid$Year)))) + 849
  dat.decid$lat  <- guess.out$lat[y]
  dat.decid$lon  <- guess.out$lon
  dat.decid$PFT  <- as.factor("Deciduous")
  
  dat.grass <- stack(data.frame(apply(guess.out$Fcomp[c(11:12),,y,], c(2,3), FUN=sum)))
  names(dat.grass) <- c("Fcomp", "Year")
  dat.grass$Year <- as.numeric(substr(dat.grass$Year,2,nchar(paste(dat.grass$Year)))) + 849
  dat.grass$lat  <- guess.out$lat[y]
  dat.grass$lon  <- guess.out$lon
  dat.grass$PFT  <- "Grass"        
  
  if(y==1) guess.fcomp <- rbind(dat.evg, dat.decid, dat.grass) else guess.fcomp <- rbind(guess.fcomp, dat.evg, dat.decid, dat.grass)
  guess.fcomp
  }
summary(guess.fcomp)



test <- apply(seq_along(1:2), FUN = bylat(y))

# Graphing Fraction PFT
X11 (width = 12)
ggplot(data=guess.fcomp[guess.fcomp$Year %in% c(850, 1850, 2010),]) +
  facet_grid(Year~PFT) +
  geom_raster(aes(x=lon, y=lat, fill=Fcomp)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-GUESS") +
  coord_equal(ratio=1)
# -----------

# -----------
# LPJ-WSL
# -----------
for(y in 1:dim(wsl.out$Fcomp)[2]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.evg <- stack(data.frame(apply(wsl.out$Fcomp[,y,,c(1,3,4,6)], c(1,2), FUN=sum)))
  names(dat.evg) <- c("Fcomp", "Year")
  dat.evg$Year <- as.numeric(substr(dat.evg$Year,2,nchar(paste(dat.evg$Year)))) + 849
  dat.evg$lat  <- wsl.out$lat[y]
  dat.evg$lon  <- wsl.out$lon
  dat.evg$PFT  <- as.factor("Evergreen")      
  
  dat.decid <- stack(data.frame(apply(wsl.out$Fcomp[,y,,c(2,5,7)], c(1,2), FUN=sum)))
  names(dat.decid) <- c("Fcomp", "Year")
  dat.decid$Year <- as.numeric(substr(dat.decid$Year,2,nchar(paste(dat.decid$Year)))) + 849
  dat.decid$lat  <- wsl.out$lat[y]
  dat.decid$lon  <- wsl.out$lon
  dat.decid$PFT  <- as.factor("Deciduous")
  
  dat.grass <- stack(data.frame(apply(wsl.out$Fcomp[,y,,c(8:9)], c(1,2), FUN=sum)))
  names(dat.grass) <- c("Fcomp", "Year")
  dat.grass$Year <- as.numeric(substr(dat.grass$Year,2,nchar(paste(dat.grass$Year)))) + 849
  dat.grass$lat  <- wsl.out$lat[y]
  dat.grass$lon  <- wsl.out$lon
  dat.grass$PFT  <- "Grass"        
  
  if(y==1) wsl.fcomp <- rbind(dat.evg, dat.decid, dat.grass) else wsl.fcomp <- rbind(wsl.fcomp, dat.evg, dat.decid, dat.grass)
}

# Graphing Fraction PFT
ggplot(data=wsl.fcomp[wsl.fcomp$Year %in% c(850, 1850, 2010),]) +
  facet_grid(Year~PFT) +
  geom_raster(aes(x=lon, y=lat, fill=Fcomp)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-WSL") +
  coord_equal(ratio=1)
# -----------


# New Code for looking at Density--KH
for(y in 1:dim(guess.out$Dens)[3]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.evg <- stack(data.frame(apply(guess.out$Dens[c(1,2,7,8,9),,y,], c(2,3), FUN=sum)))
  names(dat.evg) <- c("Dens", "Year")
  dat.evg$Year <- as.numeric(substr(dat.evg$Year,2,nchar(paste(dat.evg$Year)))) + 849
  dat.evg$lat  <- guess.out$lat[y]
  dat.evg$lon  <- guess.out$lon
  dat.evg$PFT  <- as.factor("Evergreen")      
  
  dat.decid <- stack(data.frame(apply(guess.out$Dens[c(3:6,10),,y,], c(2,3), FUN=sum)))
  names(dat.decid) <- c("Dens", "Year")
  dat.decid$Year <- as.numeric(substr(dat.decid$Year,2,nchar(paste(dat.decid$Year)))) + 849
  dat.decid$lat  <- guess.out$lat[y]
  dat.decid$lon  <- guess.out$lon
  dat.decid$PFT  <- as.factor("Deciduous")
  
  dat.grass <- stack(data.frame(apply(guess.out$Dens[c(11:12),,y,], c(2,3), FUN=sum)))
  names(dat.grass) <- c("Dens", "Year")
  dat.grass$Year <- as.numeric(substr(dat.grass$Year,2,nchar(paste(dat.grass$Year)))) + 849
  dat.grass$lat  <- guess.out$lat[y]
  dat.grass$lon  <- guess.out$lon
  dat.grass$PFT  <- "Grass"        
  
  if(y==1) guess.dens <- rbind(dat.evg, dat.decid, dat.grass) else guess.dens <- rbind(guess.dens, dat.evg, dat.decid, dat.grass)
}
summary(guess.dens)

ggplot(data=guess.dens[guess.dens$Year %in% c(850, 1850, 2010),]) +
  facet_grid(Year~PFT) +
  geom_raster(aes(x=lon, y=lat, fill=Dens)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-GUESS Density") +
  coord_equal(ratio=1)
write.csv(guess.dens, "phase2_model_output/LPJ-GUESS_annual.nc/dens.csv")
# -----------
# Comparing Multiple Models
# -----------
wsl.fcomp$Model   <- as.factor("LPJ-WSL")
guess.fcomp$Model <- as.factor("LPJ-GUESS")

fcomp <- rbind(guess.fcomp, wsl.fcomp)
png(file.path(path.figs, "Fcomp_Evergreen_0850_1850_2010.png"), height=8.5, width=11, units="in", res=180)
ggplot(data=fcomp[fcomp$Year %in% c(850, 1850, 2010) & fcomp$PFT=="Evergreen",]) +
  facet_grid(Year~Model) +
  geom_raster(aes(x=lon, y=lat, fill=Fcomp)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  scale_fill_gradientn(limits=c(0,1), colours=c("black", "green3")) +
  ggtitle("Evergreen") +
  coord_equal(ratio=1)
dev.off()

png(file.path(path.figs, "Fcomp_Deciduous_0850_1850_2010.png"), height=8.5, width=11, units="in", res=180)
ggplot(data=fcomp[fcomp$Year %in% c(850, 1850, 2010) & fcomp$PFT=="Deciduous",]) +
  facet_grid(Year~Model) +
  geom_raster(aes(x=lon, y=lat, fill=Fcomp)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  scale_fill_gradientn(limits=c(0,1), colours=c("black", "dodgerblue2")) +
  ggtitle("Deciduous") +
  coord_equal(ratio=1)
dev.off()

png(file.path(path.figs, "Fcomp_Grass_0850_1850_2010.png"), height=8.5, width=11, units="in", res=180)
ggplot(data=fcomp[fcomp$Year %in% c(850, 1850, 2010) & fcomp$PFT=="Grass",]) +
  facet_grid(Year~Model) +
  geom_raster(aes(x=lon, y=lat, fill=Fcomp)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  scale_fill_gradientn(limits=c(0,1), colours=c("black", "goldenrod2")) +
  ggtitle("Grass") +
  coord_equal(ratio=1)
dev.off()
# -----------

# -----------------------------


# -----------------------------
# Summarizing NPP
# -----------------------------
# -----------
# LPJ-GUESS
# -----------
dim(guess.out$NPP)

for(y in 1:dim(guess.out$NPP)[3]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(guess.out$NPP[13,,y,]))
  names(dat.temp) <- c("NPP", "Year")
  dat.temp$Year <- as.numeric(substr(dat.temp$Year,2,nchar(paste(dat.temp$Year)))) + 849
  dat.temp$lat  <- guess.out$lat[y]
  dat.temp$lon  <- guess.out$lon
  
  if(y==1) guess.npp <- dat.temp else guess.npp <- rbind(guess.npp, dat.temp)
}
guess.npp$NPP.yr <- guess.npp$NPP*sec2yr
summary(guess.npp)
mean(guess.npp$NPP.yr, na.rm=T)
max(guess.npp$NPP.yr, na.rm=T)

mean(guess.npp$NPP, na.rm=T)

# Graphing
ggplot(data=guess.npp[guess.npp$Year %in% c(850, 1850, 2010),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=NPP.yr)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-GUESS") +
  coord_equal(ratio=1)
# -----------

# -----------
# LPJ-WSL
# -----------
dim(wsl.out$NPP)

for(y in 1:dim(wsl.out$NPP)[2]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(wsl.out$NPP[,y,]))
  names(dat.temp) <- c("NPP", "Year")
  dat.temp$Year <- mo.yr
  dat.temp$lat  <- wsl.out$lat[y]
  dat.temp$lon  <- wsl.out$lon
  dat.temp$NPP.yr <- dat.temp$NPP*sec2yr
  
  dat.temp <- aggregate(dat.temp[,c("NPP", "NPP.yr")], by=dat.temp[,c("lat", "lon", "Year")], FUN=mean, na.rm=T)
  
  if(y==1) wsl.npp <- dat.temp else wsl.npp <- rbind(wsl.npp, dat.temp)
}
wsl.npp$NPP.yr <- wsl.npp$NPP*sec2yr
summary(wsl.npp)
dim(wsl.npp)

mean(wsl.npp$NPP.yr, na.rm=T)
max(wsl.npp$NPP.yr, na.rm=T)

mean(wsl.npp$NPP, na.rm=T)

# Graphing
ggplot(data=wsl.npp[wsl.npp$Year %in% c(850, 1850, 2010),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=NPP.yr)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-wsl") +
  coord_equal(ratio=1)
# -----------

# -----------
# Comparing Models
# -----------
wsl.npp$Model   <- as.factor("LPJ-WSL")
guess.npp$Model <- as.factor("LPJ-GUESS")

guess.npp <- guess.npp[,c("lat", "lon", "Year", "NPP", "NPP.yr", "Model")]

npp <- rbind(guess.npp, wsl.npp)

png(file.path(path.figs, "NPP_0850_1850_2010.png"), height=8.5, width=11, units="in", res=180)
ggplot(data=npp[npp$Year %in% c(850, 1850, 2010),]) +
  facet_grid(Year~Model) +
  geom_raster(aes(x=lon, y=lat, fill=NPP.yr)) +
  scale_fill_gradientn(colours=c("black", "green3")) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  coord_equal(ratio=1)
dev.off()

# -----------

# -----------------------------


# -----------------------------
# Summarizing NEE
# -----------------------------
# -----------
# LPJ-GUESS
# -----------
dim(guess.out$NEE)

for(y in 1:dim(guess.out$NEE)[2]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(guess.out$NEE[,y,]))
  names(dat.temp) <- c("NEE", "Year")
  dat.temp$Year <- as.numeric(substr(dat.temp$Year,2,nchar(paste(dat.temp$Year)))) + 849
  dat.temp$lat  <- guess.out$lat[y]
  dat.temp$lon  <- guess.out$lon
  
  if(y==1) guess.NEE <- dat.temp else guess.NEE <- rbind(guess.NEE, dat.temp)
}
guess.NEE$NEE.yr <- guess.NEE$NEE*sec2yr
summary(guess.NEE)
mean(guess.NEE$NEE.yr, na.rm=T)
max(guess.NEE$NEE.yr, na.rm=T)

mean(guess.NEE$NEE, na.rm=T)

summary(guess.NEE[guess.NEE$Year==2009,])
# Graphing
ggplot(data=guess.NEE[guess.NEE$Year %in% c(850, 1850, 2000),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=NEE.yr)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-GUESS") +
  coord_equal(ratio=1)
# -----------

# -----------
# LPJ-WSL
# -----------
dim(wsl.out$NEE)

for(y in 1:dim(wsl.out$NEE)[2]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(wsl.out$NEE[,y,]))
  names(dat.temp) <- c("NEE", "Year")
  dat.temp$Year <- mo.yr
  dat.temp$lat  <- wsl.out$lat[y]
  dat.temp$lon  <- wsl.out$lon
  dat.temp$NEE.yr <- dat.temp$NEE*sec2yr
  
  dat.temp <- aggregate(dat.temp[,c("NEE", "NEE.yr")], by=dat.temp[,c("lat", "lon", "Year")], FUN=mean, na.rm=T)
  
  if(y==1) wsl.NEE <- dat.temp else wsl.NEE <- rbind(wsl.NEE, dat.temp)
}
wsl.NEE$NEE.yr <- wsl.NEE$NEE*sec2yr
summary(wsl.NEE)
dim(wsl.NEE)

mean(wsl.NEE$NEE.yr, na.rm=T)
max(wsl.NEE$NEE.yr, na.rm=T)

mean(wsl.NEE$NEE, na.rm=T)

# Doing Biome Classifications
ggplot(data=wsl.NEE[wsl.NEE$Year %in% c(850, 1850, 2000),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=NEE.yr)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-wsl") +
  coord_equal(ratio=1)
# -----------

# -----------
# Comparing Models
# -----------
wsl.NEE$Model   <- as.factor("LPJ-WSL")
guess.NEE$Model <- as.factor("LPJ-GUESS")

guess.NEE <- guess.NEE[,c("lat", "lon", "Year", "NEE", "NEE.yr", "Model")]

NEE <- rbind(guess.NEE, wsl.NEE)

png(file.path(path.figs, "NEE_0850_1850_2000.png"), height=8.5, width=11, units="in", res=180)
ggplot(data=NEE[NEE$Year %in% c(850, 1850, 2000),]) +
  facet_grid(Year~Model) +
  geom_raster(aes(x=lon, y=lat, fill=NEE.yr)) +
  scale_fill_gradient2(low="green3", mid="gray10", high="red3") +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  coord_equal(ratio=1)
dev.off()
# -----------
# -----------------------------


# -----------------------------
# Summarizing TotLivBiom
# -----------------------------
# -----------
# LPJ-GUESS
# -----------
dim(guess.out$TotLivBiom)

for(y in 1:dim(guess.out$TotLivBiom)[3]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(guess.out$TotLivBiom[13,,y,]))
  names(dat.temp) <- c("TotLivBiom", "Year")
  dat.temp$Year <- as.numeric(substr(dat.temp$Year,2,nchar(paste(dat.temp$Year)))) + 849
  dat.temp$lat  <- guess.out$lat[y]
  dat.temp$lon  <- guess.out$lon
  
  if(y==1) guess.TotLivBiom <- dat.temp else guess.TotLivBiom <- rbind(guess.TotLivBiom, dat.temp)
}
summary(guess.TotLivBiom)
mean(guess.TotLivBiom$TotLivBiom.yr, na.rm=T)
max(guess.TotLivBiom$TotLivBiom.yr, na.rm=T)

mean(guess.TotLivBiom$TotLivBiom, na.rm=T)

summary(guess.TotLivBiom[guess.TotLivBiom$Year==2009,])
# Graphing
ggplot(data=guess.TotLivBiom[guess.TotLivBiom$Year %in% c(850, 1850, 2000),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=TotLivBiom)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-GUESS") +
  coord_equal(ratio=1)

write.csv(guess.TotLivBiom, "phase2_model_output/LPJ-GUESS_annual.nc/TotLivBiom.csv")
# -----------

# -----------
# LPJ-WSL
# -----------
dim(wsl.out$TotLivBiom)

for(y in 1:dim(wsl.out$TotLivBiom)[2]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(wsl.out$TotLivBiom[,y,]))
  names(dat.temp) <- c("TotLivBiom", "Year")
  dat.temp$Year <- wsl.out$Year
  dat.temp$lat  <- wsl.out$lat[y]
  dat.temp$lon  <- wsl.out$lon
  
  if(y==1) wsl.TotLivBiom <- dat.temp else wsl.TotLivBiom <- rbind(wsl.TotLivBiom, dat.temp)
}
summary(wsl.TotLivBiom)
dim(wsl.TotLivBiom)

mean(wsl.TotLivBiom$TotLivBiom.yr, na.rm=T)
max(wsl.TotLivBiom$TotLivBiom.yr, na.rm=T)

mean(wsl.TotLivBiom$TotLivBiom, na.rm=T)

# Doing Biome Classifications
ggplot(data=wsl.TotLivBiom[wsl.TotLivBiom$Year %in% c(850, 1850, 2000),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=TotLivBiom)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-wsl") +
  coord_equal(ratio=1)
# -----------

# -----------
# Comparing Models
# -----------
wsl.TotLivBiom$Model   <- as.factor("LPJ-WSL")
guess.TotLivBiom$Model <- as.factor("LPJ-GUESS")

TotLivBiom <- rbind(guess.TotLivBiom, wsl.TotLivBiom)

png(file.path(path.figs, "TotLivBiom_0850_1850_2000.png"), height=8.5, width=11, units="in", res=180)
ggplot(data=TotLivBiom[TotLivBiom$Year %in% c(850, 1850, 2000),]) +
  facet_grid(Year~Model) +
  geom_raster(aes(x=lon, y=lat, fill=TotLivBiom)) +
  scale_fill_gradientn(colors=c("black", "green3")) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  coord_equal(ratio=1)
dev.off()
# -----------
# -----------------------------