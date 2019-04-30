# This script will look at Fcomp, Density, possibly BA? trends through time
# Author: Kelly Heilman
library(reshape2)
library(ggplot2)
library(tidyr)
library(gganimate)
library(dplyr)

# load the necessary data:
rm(list=ls()) # clear envt

# load the pft specific data:
ED2.Dens <- readRDS("Data/ED2/ED2.Dens.rds")
ED2.Fcomp <- readRDS("Data/ED2/ED2.Fcomp.RDS")
ED2.CO2 <- readRDS('Data/ED2/ED2.CO2.rds')
load("Data/PalEON_siteInfo_all.RData")

GUESS.Dens <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Dens.rds")
GUESS.Fcomp <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Fcomp.rds")

GUESS.CO2 <- ED2.CO2



yrlyvar <- (0:1160) + 850

# make plots for ED2:
timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850
# try unlist ot convert Fcomp to a df
#df <- data.frame(matrix(unlist(Fcomp), nrow=13932, byrow=T),stringsAsFactors=FALSE)
#test <- do.call(rbind, lapply(Fcomp, data.frame, stringsAsFactors=FALSE))

#convert list to array
pft.lab=c("grass.c4", "tropic.early", "tropic.mid", "tropic.late", "grass.c3.temp", "pine.north", "pine.south", "conifer.late", "temp.decid.early", "temp.decid.mid", "temp.decid.late","ag1", "ag2", "ag3", "ag4","grass.c3.subtrop","Araucaria")


Fcomp <- ED2.Fcomp
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


# plots of Fcomp at each site (for ED2): Not run b/c they take awhile

# for(i in 1:length(paleon$num)){
#   png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Fcomp/ED2_Fcomp_",paleon[i,]$latlon, ".png"))
#   plot(Fcomp.r[,i,"pine.north"], ylim = c(0,1.5), col = 'red', ylab = "Fcomp", xlab = "Months since 850")
#   points(Fcomp.r[,i,"conifer.late"], col = "forestgreen")
#   points(Fcomp.r[,i,"temp.decid.early"], col = "lightblue")
#   points(Fcomp.r[,i,"temp.decid.mid"], col = 'blue')
#   points(Fcomp.r[,i,"temp.decid.late"], col = "orange")
#   points(Fcomp.r[,i,"grass.c3.temp"], col = "black")
#   legend('topleft',legend=c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
#                             "temp.decid.late", "grass.c3.temp" ), 
#          col = c('red', 'forestgreen', 'lightblue', 'blue', 'orange', 'black'), pch=16)
#   dev.off()
# }
# 
# 
# # make the plot for density:
# for(i in 1:length(paleon$num)){
#   png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/ED2_Dens_",paleon[i,]$latlon, ".png"))
#   plot(Dens.r[,i,"pine.north"] , ylim=c(10000, 1000000), col = 'red', ylab = "Dens", xlab = "Months since 850")
#   points(Dens.r[,i,"conifer.late"], col = "forestgreen")
#   points(Dens.r[,i,"temp.decid.early"], col = "lightblue")
#   points(Dens.r[,i,"temp.decid.mid"], col = 'blue')
#   points(Dens.r[,i,"temp.decid.late"], col = "orange")
#   points(Dens.r[,i,"grass.c3.temp"], col = "black")
#   legend('topleft',legend=c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
#                             "temp.decid.late", "grass.c3.temp" ), 
#          col = c('red', 'forestgreen', 'lightblue', 'blue', 'orange', 'black'), pch=16)
#   dev.off()
# }
# 

#save Dens.r and Fcomp.r
saveRDS(Dens.r, "Data/ED2.Dens.pftonly.rds")
saveRDS(Fcomp.r, "Data/ED2.Fcomp.pftonly.rds")


# make plots for LPJ-GUESS:
Dens.lpj <- data.frame(GUESS.Dens)
dim(GUESS.Fcomp)
guess.pft.lab <- c("BNE", "BINE", "BNS", "BIBS", "TeBS", "TelBS", "TeBE",
                   "TrBE", "TrlBE", "TrBR", "C3G", "C4G", "Total")
                   
dimnames(GUESS.Fcomp) <- list(yrlyvar, paleon$num, guess.pft.lab)
dimnames(GUESS.Dens) <- list(yrlyvar, paleon$num, guess.pft.lab)

guess.fcomp <- melt(GUESS.Fcomp)
colnames(guess.fcomp) <- c("Year", "Site", "PFT", "Fcomp")

guess.dens <- melt(GUESS.Dens)
colnames(guess.dens) <- c("Year", "Site", "PFT", "Dens")

ggplot(guess.dens[guess.dens$Site == 100,], aes(Year, Dens, color = PFT))+geom_point()

guess.dens.wide <- guess.dens %>% spread(key= PFT, value = Dens)
colnames(guess.dens.wide)[3:length(guess.dens.wide)] <- paste0(colnames(guess.dens.wide)[3:length(guess.dens.wide)], ".Dens")



#save Dens.r and Fcomp.r
saveRDS(guess.dens.wide, "Data/GUESS.Dens.pft.wide.rds")
saveRDS(guess.dens, "Data/GUESS.Dens.pft.rds")
saveRDS(guess.fcomp, "Data/GUESS.Fcomp.pft.rds")


#----------- read  LINKAGES fcomp:

# no density file for LINKAGES
LINK.Fcomp <- readRDS("Data/LINKAGES/PalEON_regional_LINKAGES.Fcomp.rds")
#ED2.Fcomp <- readRDS("Data/ED2/ED2.Fcomp.RDS")
#ED2.CO2 <- readRDS('Data/ED2/ED2.CO2.rds')
load("Data/PalEON_siteInfo_all.RData")


LINK.CO2<- GUESS.CO2 <- ED2.CO2

yrlyvar <- (0:1160) + 850

# make plots for ED2:
timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850

#convert list to array
pft.link=c("beech","chestnut","elm","fir","hemlock","pignut hickory","red maple","sugar maple","red spruce",
"poplar","tamarack","white ash","white oak","white pine","yellow birch")

dimnames(LINK.Fcomp) <- list(yrlyvar, paleon$num, pft.link)


link.Fcomp <- melt(LINK.Fcomp)
colnames(link.Fcomp) <- c("Year", "Site", "PFT", "Fcomp")

# remove sites with just NA for species
link.Fcomp$Fcomp <- ifelse(link.Fcomp$Fcomp == -9999, NA, link.Fcomp$Fcomp)
link.Fcomp.nona <- link.Fcomp[!is.na(link.Fcomp$Fcomp),]


saveRDS(link.Fcomp.nona, "Data/LINKAGES.Fcomp.pft.rds")
#ggplot(link.Fcomp.nona[link.Fcomp.nona$Site %in% "127",], aes(Year, Fcomp, color = PFT))+geom_line()


#--------------------Make GIF maps of Fcomp over time-------------------
# for ED2, using gganimate--this is the visualize how veg is changing across the region
ED2.fcomp <- melt(Fcomp)
head(ED2.fcomp) # year, site, pft, Fcomp
colnames(ED2.fcomp) <- c("time", "Site", "PFT", "Fcomp")
ED2.fcomp$Year <- year 
ED2.fcomp$Month <- month 
ED2.fcomp$num <- as.numeric(ED2.fcomp$Site)
# map out each of PFT's changes separately:
ED2.fcomp.ll <- left_join(paleon, ED2.fcomp, by = "num")

# ED2 PFTS
# "pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
#           "temp.decid.late", "grass.c3.temp" )

# but the ones in the model are:
# c("BNE" ,"BINE","BNS", "BIBS", "TeBS","TelBS","TeBE", "C3G", "Total" )

# .............................Get PFT with highest Fcomp over time for ED2 ..............................
by.fcomp <- ED2.fcomp.ll %>% group_by( Year, Site, PFT ) %>% dplyr::summarise(Fcompy = mean( Fcomp), 
                                                                             Fcomp.sd = sd(Fcomp))

by.fcomp2 <-  by.fcomp %>% dplyr::select(Year, Site, PFT, Fcompy) %>% group_by(Year, Site ) %>% spread(key = PFT, value = Fcompy)
saveRDS(by.fcomp2, "outputs/data/ED2/ED2_mean_yearly_fcomp.rds")
highest.fcomp <- by.fcomp %>% dplyr::select(Year, Site, PFT, Fcompy) %>% group_by(Year, Site) %>% filter(Fcompy == max(Fcompy))

# get latlon again
ED2.sites <- unique(ED2.fcomp.ll[, c("num", "lon", "lat", "Site")])
highest.fcomp <- left_join(ED2.sites, highest.fcomp, by = "Site")
by.fcomp <- left_join(ED2.sites, by.fcomp2, by = "Site")
by.fcomp.nona <- by.fcomp[!is.na(by.fcomp$grass.c4),]

by.fcomp2<- by.fcomp[!is.na(by.fcomp$grass.c4),]
library(maps)
library(sp)
library(rgeos)

all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(all_states)<-~long+lat
class(all_states)

ca = map_data("world", "Canada")
coordinates(ca)<-~long+lat
ca.data <- data.frame(ca)
mapdata <- data.frame(all_states)

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plyr)
library(rnaturalearth)
#  Assuming you have a path 'Maps' that you store your spatial files in.  This
#  is all downloaded from <a href=>http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

# lakes
ne_lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical')
sp::plot(ne_lakes, col = 'blue')

# rivers
ne_rivers <- ne_download(scale = 110, type = 'rivers_lake_centerlines', category = 'physical')
sp::plot(ne_rivers, col = 'blue')

# coast:
ne_coast <- ne_download(scale = 110, type = 'coastline', category = 'physical')
sp::plot(ne_coast, col = 'blue')

# states:
ne_state <- ne_download(scale = 110, type = 'states', category = 'cultural')

nat.earth <- stack('/Users/kah/Documents/TreeRings/data/NE2_50M_SR_W/NE2_50M_SR_W/NE2_50M_SR_W.tif')



#  I have a domain I'm interested in
quick.subset <- function(x, longlat){
  
  # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)
  
  x.f = fortify(x, region="id")
  x.join = plyr::join(x.f, x@data, by="id")
  
  x.subset <- subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
                       x.join$lat > longlat[3] & x.join$lat < longlat[4])
  
  x.subset
}


domain <- c(-100,-61, 35, 49)
lakes.subset <- quick.subset(ne_lakes, domain)
river.subset <- quick.subset(ne_rivers, domain)
coast.subset <- quick.subset(ne_coast, domain)
state.subset <- quick.subset(ne_state, c(-105,-61, 35, 49))
nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE2_50M_SR_W.1,
                                       NE2_50M_SR_W.2,
                                       NE2_50M_SR_W.3,
                                       1))
# plot out map
cbPalette <- c('#a6611a',
               '#dfc27d',
               '#80cdc1',
               '#018571')

NEmap <- ggplot()+
  geom_raster(data = rast.table, aes(x = x, y = y, fill = NE2_50M_SR_W.1)) +scale_fill_gradientn(colours = rev(cbPalette), guide = FALSE)+
  
  geom_path(data=state.subset, aes(x = long, y = lat, group = group), color = 'grey40')+
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  xlab('') + ylab('')+ coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61)) 

#"pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
#           "temp.decid.late", "grass.c3.temp" )

highest.fcomp$Year<- as.numeric(highest.fcomp$Year)
highest.fcomp <- highest.fcomp[!is.na(highest.fcomp$Year),]

map.highest <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = highest.fcomp, aes(lon, lat, fill = PFT))+
  scale_fill_manual( values = c("pine.north"="#7fc97f",
                                "temp.decid.late"="#beaed4",
                                "temp.decid.mid"="#fdc086",
                                "conifer.late"="#ffff99",
                                "temp.decid.early"="#386cb0",
                                "grass.c3.temp"="#f0027f",
                                "temp.decid.late"="#bf5b17"))+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))


pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )



# for each year & site get the PFT with highest Fcomp:
map.highest.gif <- map.highest + transition_time(time = Year) +
  labs(title = "Dominant PFT Year: {frame_time}") 
map.highest.gif <- gganimate::animate(map.highest.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_highest_pft_ED2.gif"), map.highest.gif)


# .............................map out Fcomp for each PFT over time..............................

#------ for pine.north 
map.pine.north <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data= by.fcomp.nona, aes(lon, lat, fill = pine.north))+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

# for each year & site get the PFT with highest Fcomp:
map.pine.north.gif <- map.pine.north + transition_time(Year) +
  labs(title = "pine.north Fcomp Year: {frame_time}") 
pine.north.gif <- gganimate:: animate(map.pine.north.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_pine.north_ED2.gif"), pine.north.gif)

# -------for conifer.late 
map.conifer.late <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data= by.fcomp.nona, aes(lon, lat, fill = conifer.late))+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

# for each year & site get the PFT with highest Fcomp:
map.conifer.late.gif <- map.conifer.late + transition_time(Year) +
  labs(title = "conifer.late Fcomp Year: {frame_time}") 
conifer.late.gif <- gganimate:: animate(map.conifer.late.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_conifer.late_ED2.gif"), conifer.late.gif)

# ------for temp.decid.mid 
map.temp.decid.mid <-ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data= by.fcomp.nona, aes(lon, lat, fill = temp.decid.mid ))+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

# for each year & site get the PFT with highest Fcomp:
map.temp.decid.mid.gif <- map.temp.decid.mid + transition_time(Year) +
  labs(title = "temp.decid.mid Fcomp Year: {frame_time}") 
temp.decid.mid.gif <- gganimate:: animate(map.temp.decid.mid.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_temp.decid.mid_ED2.gif"), temp.decid.mid.gif)


#------------- for temp.decid.early 
map.temp.decid.early <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data= by.fcomp.nona, aes(lon, lat, fill = temp.decid.early ))+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

# for each year & site get the PFT with highest Fcomp:
map.temp.decid.early.gif <- map.temp.decid.early + transition_time(Year) +
  labs(title = "temp.decid.early Fcomp Year: {frame_time}") 
temp.decid.early.gif <- gganimate:: animate(map.temp.decid.early.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_temp.decid.early_ED2.gif"), temp.decid.early.gif)

#----------- for temp.decid.late 
map.temp.decid.late <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data= by.fcomp.nona, aes(lon, lat, fill = temp.decid.late ))+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

# for each year & site get the PFT with highest Fcomp:
map.temp.decid.late.gif <- map.temp.decid.late + transition_time(Year) +
  labs(title = "temp.decid.late Fcomp Year: {frame_time}") 
temp.decid.late.gif <- gganimate:: animate(map.temp.decid.late.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_temp.decid.late_ED2.gif"), temp.decid.late.gif)

#--------- for grass.c3.temp 
map.grass.c3.temp <-ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data= by.fcomp.nona, aes(lon, lat, fill = grass.c3.temp ))+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

# for each year & site get the PFT with highest Fcomp:
map.grass.c3.temp.gif <- map.grass.c3.temp + transition_time(Year) +
  labs(title = "grass.c3.temp Fcomp Year: {frame_time}") 
grass.c3.temp.gif <- gganimate:: animate(map.grass.c3.temp.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_grass.c3.temp_ED2.gif"), grass.c3.temp.gif)





#--------------------Make GIF maps of Fcomp over time for GUESS-------------------
# for GUESS, using gganimate--this is the visualize how veg is changing across the region

head(guess.fcomp) # year, site, pft, Fcomp
guess.fcomp$num <- as.numeric(guess.fcomp$Site)
# map out each of PFT's changes separately:
guess.fcomp.ll <- left_join(paleon, guess.fcomp, by = "num")

# 
# BNE = Boreal needleleaved evergreen
# BINE = Boreal needleleaved evergreen (shade intolerant)
# BNS = Boreal needleleved summergreen
# BIBS = Boreal broadleaved summergreen (shade intolerant)
# TeBS = Temperate broadleaved summergreen
# TeIBS = Temperate broadleaved summergreen (shade intolerant)
# TeBE = Temperate broadleved evergreen
# TrBE = Tropical broadleaved evergreen
# TrIBE = Tropical broadleaved evergreen (shade intolerant)
# TrBR = Tropical broadleaved raingreen
# C3G = C3 grass
# C4G = C4 grass
# Total = sum of al PFTs

# but the ones in the model are:
# c("BNE" ,"BINE","BNS", "BIBS", "TeBS","TelBS","TeBE", "C3G", "Total" )

# .............................Get PFT with highest Fcomp over time for GUESS ..............................
by.fcomp <- guess.fcomp.ll %>% group_by(Year, Site) %>% spread(PFT, Fcomp)
highest.fcomp <- guess.fcomp.ll %>% filter (!PFT %in% "Total") %>% group_by(Year, Site) %>% filter(Fcomp == max(Fcomp))

# unique highest fcomp = c(BIBS  TeBS  BINE  BNE   TelBS C3G   TeBE)


library(maps)
library(sp)
library(rgeos)

all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(all_states)<-~long+lat
class(all_states)

ca = map_data("world", "Canada")
coordinates(ca)<-~long+lat
ca.data <- data.frame(ca)
mapdata <- data.frame(all_states)

library(raster)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plyr)
library(rnaturalearth)
#  Assuming you have a path 'Maps' that you store your spatial files in.  This
#  is all downloaded from <a href=>http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

# lakes
ne_lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical')
sp::plot(ne_lakes, col = 'blue')

# rivers
ne_rivers <- ne_download(scale = 110, type = 'rivers_lake_centerlines', category = 'physical')
sp::plot(ne_rivers, col = 'blue')

# coast:
ne_coast <- ne_download(scale = 110, type = 'coastline', category = 'physical')
sp::plot(ne_coast, col = 'blue')

# states:
ne_state <- ne_download(scale = 110, type = 'states', category = 'cultural')

nat.earth <- stack('/Users/kah/Documents/TreeRings/data/NE2_50M_SR_W/NE2_50M_SR_W/NE2_50M_SR_W.tif')



#  I have a domain I'm interested in
quick.subset <- function(x, longlat){
  
  # longlat should be a vector of four values: c(xmin, xmax, ymin, ymax)
  x@data$id <- rownames(x@data)
  
  x.f = fortify(x, region="id")
  x.join = plyr::join(x.f, x@data, by="id")
  
  x.subset <- subset(x.join, x.join$long > longlat[1] & x.join$long < longlat[2] &
                       x.join$lat > longlat[3] & x.join$lat < longlat[4])
  
  x.subset
}


domain <- c(-100,-61, 35, 49)
lakes.subset <- quick.subset(ne_lakes, domain)
river.subset <- quick.subset(ne_rivers, domain)
coast.subset <- quick.subset(ne_coast, domain)
state.subset <- quick.subset(ne_state, c(-105,-61, 35, 49))
nat.crop <- crop(nat.earth, y=extent(domain))

rast.table <- data.frame(xyFromCell(nat.crop, 1:ncell(nat.crop)),
                         getValues(nat.crop/255))

rast.table$rgb <- with(rast.table, rgb(NE2_50M_SR_W.1,
                                       NE2_50M_SR_W.2,
                                       NE2_50M_SR_W.3,
                                       1))
# plot out map
cbPalette <- c('#a6611a',
               '#dfc27d',
               '#80cdc1',
               '#018571')

NEmap <- ggplot()+
  geom_raster(data = rast.table, aes(x = x, y = y, fill = NE2_50M_SR_W.1)) +scale_fill_gradientn(colours = rev(cbPalette), guide = FALSE)+
  
  geom_path(data=state.subset, aes(x = long, y = lat, group = group), color = 'grey40')+
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  xlab('') + ylab('')+ coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61)) 



map.highest <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = highest.fcomp, aes(lon, lat, fill = PFT))+
  scale_fill_manual( values = c("BIBS"="#7fc97f",
                                "TeBS"="#beaed4",
                                "BINE"="#fdc086",
                                "BNE"="#ffff99",
                                "TelBS"="#386cb0",
                                "C3G"="#f0027f",
                                "TeBE"="#bf5b17"))+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))




# for each year & site get the PFT with highest Fcomp:
map.highest.gif <- map.highest + transition_time(Year) +
  labs(title = "Dominant PFT Year: {frame_time}") 
map.highest.gif <- animate(map.highest.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_highes_pft_GUESS.gif"), map.highest.gif)


# .............................map out Fcomp for each PFT over time..............................
map.BNE <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "BNE",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:
map.BNE.gif <- map.BNE + transition_time(Year) +
  labs(title = "BNE (Boreal needleleaved evergreen) Fcomp Year: {frame_time}") 
BNE.gif <- animate(map.BNE.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_BNE_GUESS.gif"), BNE.gif)

# ------ for BINE
map.BINE <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "BINE",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:

map.BINE.gif <- map.BINE + transition_time(Year) +
  labs(title = "BINE Boreal needleleaved evergreen (shade intolerant) Fcomp Year: {frame_time}") 
BINE.gif <- animate(map.BINE.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_BINE_GUESS.gif"), BINE.gif)

#------ for BNS 

# do the
map.BNS <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "BNS",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:

map.BNS.gif <- map.BNS + transition_time(Year) +
  labs(title = "BNS Boreal needleleaved summergreen Fcomp Year: {frame_time}") 
BNS.gif <- animate(map.BNS.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_BNS_GUESS.gif"), BNS.gif)


#------ for BIBS

# do the
map.BIBS <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "BIBS",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:

map.BIBS.gif <- map.BIBS + transition_time(Year) +
  labs(title = "BIBS Boreal broadleaved summergreen Fcomp Year: {frame_time}") 
BIBS.gif <- animate(map.BIBS.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_BIBS_GUESS.gif"), BIBS.gif)

#------ for TeBS

# do the
map.TeBS <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "TeBS",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:

map.TeBS.gif <- map.TeBS + transition_time(Year) +
  labs(title = "TeBS Temperate broadleaved summergreen Fcomp Year: {frame_time}") 
TeBS.gif <- animate(map.TeBS.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_TeBS_GUESS.gif"), TeBS.gif)


#------ for TelBS

# do the
map.TelBS <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "TelBS",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:

map.TelBS.gif <- map.TelBS + transition_time(Year) +
  labs(title = "TelBS Temperate broadleaved summergreen (shade intolerant) Fcomp Year: {frame_time}") 
TelBS.gif <- animate(map.TelBS.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_TelBS_GUESS.gif"), TelBS.gif)

#------ for TeBE

# do the
map.TeBE <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "TeBE",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:

map.TeBE.gif <- map.TeBE + transition_time(Year) +
  labs(title = "TeBE Temperate broadleved evergreen Fcomp Year: {frame_time}") 
TeBE.gif <- animate(map.TeBE.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_TeBE_GUESS.gif"), TeBE.gif)


#------ for TeBE

# do the
map.C3G <- ggplot(guess.fcomp.ll[guess.fcomp.ll$PFT %in% "C3G",], aes(lon, lat, fill = Fcomp))+geom_raster()+
  scale_fill_gradientn(colors = c("#fff7fb","#ece2f0", "#d0d1e6","#a6bddb","#67a9cf","#3690c0","#02818a","#016c59","#014636"), limits= c(0,1)  )

# for each year & site get the PFT with highest Fcomp:

map.C3G.gif <- map.C3G + transition_time(Year) +
  labs(title = "C3G C3 grass Fcomp Year: {frame_time}") 
C3G.gif <- animate(map.C3G.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_C3G_GUESS.gif"), C3G.gif)




#--------------------Make GIF maps of Fcomp over time for LINKAGES-------------------
# for GUESS, using gganimate--this is the visualize how veg is changing across the region

head(link.Fcomp.nona) # year, site, pft, Fcomp
link.Fcomp.nona$num <- as.numeric(link.Fcomp.nona$Site)
# map out each of PFT's changes separately:
link.fcomp.ll <- left_join(paleon, link.Fcomp.nona, by = "num")


# .............................Get PFT with highest Fcomp over time for LINKAGES ..............................
link.by.fcomp <- link.fcomp.ll %>% group_by(Year, Site) %>% spread(PFT, Fcomp)
link.highest.fcomp <- link.fcomp.ll %>% group_by(Year, Site) %>% filter(Fcomp == max(Fcomp))

# unique highest fcomp = c(BIBS  TeBS  BINE  BNE   TelBS C3G   TeBE)


library(maps)
library(sp)
library(rgeos)

all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(all_states)<-~long+lat
class(all_states)

ca = map_data("world", "Canada")
coordinates(ca)<-~long+lat
ca.data <- data.frame(ca)
mapdata <- data.frame(all_states)


NEmap <- ggplot()+
  geom_raster(data = rast.table, aes(x = x, y = y, fill = NE2_50M_SR_W.1)) +scale_fill_gradientn(colours = rev(cbPalette), guide = FALSE)+
  
  geom_path(data=state.subset, aes(x = long, y = lat, group = group), color = 'grey40')+
  geom_path(data=lakes.subset, aes(x = long, y = lat, group = group), color = 'blue') +
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#ADD8E6') +
  scale_alpha_discrete(range=c(1,0)) +
  xlab('') + ylab('')+ coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61)) 


unique(link.highest.fcomp$PFT)

map.highest.link <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = link.highest.fcomp, aes(lon, lat, fill = PFT))+
  scale_fill_manual(values = c("hemlock" = "#a6cee3",
  "fir" = "#1f78b4",
  "white pine"="#b2df8a",
  "tamarack"="#33a02c",
  "red maple"="#fb9a99",
  "sugar maple"="#e31a1c",
  "pignut hickory"="#fdbf6f",
  "white oak"= "#ff7f00",
  "elm"= "#cab2d6",
  "beech"="#6a3d9a",
  "white ash"="#ffff99", 
  "poplar"="#b15928",
  "red spruce"="black", "chestnut"="#800026"))+
  coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))




# for each year & site get the PFT with highest Fcomp:
map.highest.gif <- map.highest.link + transition_time(Year) +
  labs(title = "Dominant PFT Year: {frame_time}") 
map.highest.gif <- gganimate::animate(map.highest.gif)
anim_save(filename=paste0(getwd(), "/outputs/preliminaryplots/gifs/Fcomp_highes_pft_link.gif"), map.highest.gif)


# .............................map out Fcomp for each PFT over time..............................

make_fcomp_gif_link <- function (x, species){
  
    map.BNE <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
      geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+
      geom_raster(data = x[x$PFT %in% species,], aes(lon, lat, fill = Fcomp))+
      scale_fill_gradientn(colors = c("#f7fcfd",
        "#e0ecf4",
        "#bfd3e6",
        "#9ebcda",
        "#8c96c6",
        "#8c6bb1",
        "#88419d",
        "#810f7c",
        "#4d004b"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))+theme_bw()
    
    # for each year & site get the PFT with highest Fcomp:
    map.BNE.gif <- map.BNE + transition_time(Year) +
      labs(title = paste(species, "Fcomp Year: {frame_time}") )
    BNE.gif <- gganimate::animate(map.BNE.gif)
    anim_save(filename=paste0(getwd(), paste0("/outputs/preliminaryplots/gifs/Fcomp_",species,"_link.gif")), BNE.gif)

}

# should use apply, but manually writing them out here
make_fcomp_gif_link(link.fcomp.ll, "beech")
make_fcomp_gif_link(link.fcomp.ll, "chestnut")
make_fcomp_gif_link(link.fcomp.ll, "elm")
make_fcomp_gif_link(link.fcomp.ll, "fir")
make_fcomp_gif_link(link.fcomp.ll, "hemlock")
make_fcomp_gif_link(link.fcomp.ll, "pignut hickory")
make_fcomp_gif_link(link.fcomp.ll, "red maple")
make_fcomp_gif_link(link.fcomp.ll, "sugar maple")
make_fcomp_gif_link(link.fcomp.ll, "red spruce")
make_fcomp_gif_link(link.fcomp.ll, "poplar")
make_fcomp_gif_link(link.fcomp.ll, "tamarack")
make_fcomp_gif_link(link.fcomp.ll, "white ash")
make_fcomp_gif_link(link.fcomp.ll, "white oak")
make_fcomp_gif_link(link.fcomp.ll, "yellow birch")

#-----------------------Do sites switch PFT in response to climate and CO2?-------------
# 1. quantify the number of pft switches--Do they vary over time or across space in a meaninful way?
# 2. quantify the type of pft switches--Does CO2 favor 



#----------------------Are there sites where density is bimodal?----------------

# make histograms of density by site and model
# plot histograms for each grid cell based on the model:
make.hists<- function(model){
if(model == "ED2"){
  Dens <- readRDS("Data/ED2/ED2.Dens.rds")
  CO2 <- ED2.CO2
  dimnames(Dens) <- list(timevec, paleon$num, pft.lab)
  pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
          "temp.decid.late", "grass.c3.temp" )
  Dens.r <- Dens[,,pfts]
  
  # loop to calculate the total density and output histograms
  for(i in 1:length(paleon$num)){
    png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_",paleon[i,]$latlon, ".png"))
    dens.site <- data.frame(Dens.r[,i,])
    dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
    print(ggplot(dens.site, aes(Totaldens))+geom_histogram()+theme_bw())
    dev.off()
    
    site.m <- melt(dens.site, id.vars = c("Totaldens"))
    png(height = 4, width= 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_byPFT", paleon[i,]$latlon, '.png'))
    print(ggplot(site.m, aes(value, fill= variable))+geom_histogram()+facet_wrap(~variable)+theme_bw())
    dev.off()
  }
  
}else{
  Dens <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Dens.rds")
  CO2 <- ED2.CO2
  yr <- 850:2010 # guess density is yearly
  pft.guess=c("BNE", "BINE", "BNS", "BIBS", "TeBS", "BeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
  dimnames(Dens) <- list(yr, paleon$num, pft.guess)
  
  pfts <- c("BNE" ,"BINE","BNS", "BIBS",   
            "TeBS","BeIBS","TeBE", "C3G", "Total" )
  Dens.r <- Dens[,,pfts]
  # loop that takes total 
  for(i in 1:length(paleon$num)){
    png(height=7, width = 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_",paleon[i,]$latlon, ".png"))
    dens.site <- data.frame(Dens.r[,i,])
    #dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
    print(ggplot(dens.site, aes(Total))+geom_histogram()+theme_bw())
    dev.off()
    
    site.m <- melt(dens.site, id.vars = c("Total"))
    png(height = 4, width= 7, units = 'in', res=300, paste0(getwd(), "/outputs/preliminaryplots/Dens/hists/",model,"_Dens_byPFT", paleon[i,]$latlon, '.png'))
    print(ggplot(site.m, aes(value, fill= variable))+geom_histogram()+facet_wrap(~variable)+theme_bw())
    dev.off()
  }
  
}
}

make.hists(model = "ED2")
make.hists(model = "GUESS")

# there are some sites that have bimodal distn of tree density, but 
# these may be due to different species distributions


# -----------------What grid cells are significantly "bimodal"?--------------

# make one big function that finds the mean density and SD nd maps these out 
# make a dataframe of total density and density sd to get an idea of how much each grid cell varies:
# this function also outputs the df of mean density and sd that is used in the map.fires function below

make.dens.maps <- function(model) {
  if(model == "ED2"){
  library(modes)
  Dens <- readRDS("Data/ED2/ED2.Dens.rds")
  CO2 <- ED2.CO2
  dimnames(Dens) <- list(timevec, paleon$num, pft.lab)
  pfts <- c("pine.north" ,"conifer.late","temp.decid.early", "temp.decid.mid",   
            "temp.decid.late", "grass.c3.temp" )
  Dens.r <- Dens[,,pfts]
  
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

  bimodal <- ifelse(Dens.mean$BC > 0.55 & Dens.mean$pval <= 0.05, "Bimodal", 'Unimodal')
  Dens.mean$bimodal <- bimodal
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
  print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = mean))+geom_raster()+
    scale_fill_gradientn(colours = cbpalette, limits = c(0,3000000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw()+ ggtitle('Mean total density'))
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
  print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = sd))+geom_raster()+
    scale_fill_gradientn(colours = rev(rbpalette), limits = c(0,500000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('SD of total density'))
  dev.off()
  
  # map of places with significantly bimodal distribution in tree density over time:
  
  
  
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_bimodal_time_map.png"))
  print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = bimodal))+geom_raster()+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Places where density has bimodal distn.'))
  dev.off()
  
  }else{ 
    
    ## if the model is LPJ-GUESS, then...
    # read in LPJ density again
    Dens <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Dens.rds")
    CO2 <- ED2.CO2
    yr <- 850:2010 # guess density is yearly
    pft.guess=c("BNE", "BINE", "BNS", "BIBS", "TeBS", "BeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
    dimnames(Dens) <- list(yr, paleon$num, pft.guess)
    
    pfts <- c("BNE" ,"BINE","BNS", "BIBS",   
              "TeBS","BeIBS","TeBE", "C3G", "Total" )
    Dens.r <- Dens[,,pfts]
    
    
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
      Dens.mean[i,]$mean <- mean(dens.site$Total, na.rm=TRUE)
      Dens.mean[i,]$sd <- sd(dens.site$Total, na.rm=TRUE)
      Dens.mean[i,]$pval <- diptest::dip.test(na.omit(density(dens.site$Total)$y))$p
      Dens.mean[i,]$BC <- bimodality_coefficient(na.omit(dens.site$Total)) 
    }
    
    bimodal <- ifelse(Dens.mean$BC > 0.55 & Dens.mean$pval <= 0.05, "Bimodal", 'Unimodal')
    Dens.mean$bimodal <- bimodal
    
    # save the file for future use:
    saveRDS(Dens.mean, paste0(getwd(), "/outputs/data/GUESS/GUESS.meandens.rds"))
    
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
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/GUESS_mean_dens_map.png"))
    print( ggplot(Dens.mean, aes(x = lon, y = lat, fill = mean))+geom_raster()+
      scale_fill_gradientn(colours = cbpalette, limits = c(700,3000), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw()+ ggtitle('Mean total density'))
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
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/GUESS_sd_dens_map.png"))
    print( ggplot(Dens.mean, aes(x = lon, y = lat, fill = sd))+geom_raster()+
      scale_fill_gradientn(colours = rev(rbpalette), limits = c(100,900), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('SD of total density'))
    dev.off()
    
    # map of places with significantly bimodal distribution in tree density over time:
    
    bimodal <- ifelse(Dens.mean$BC > 0.55 & Dens.mean$pval <= 0.05, "Bimodal", 'Unimodal')
    Dens.mean$bimodal <- bimodal
    
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/GUESS_bimodal_time_map.png"))
    print(ggplot(Dens.mean, aes(x = lon, y = lat, fill = bimodal))+geom_raster()+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Places where density has bimodal distn.'))
    dev.off()
  }
}

# saves maps in preliminaryplots/Dens/maps
# note the color scale differences in tree density between models
make.dens.maps(model = "ED2")
make.dens.maps(model = "GUESS")

# ---------------------How does fire frequency vary across space?-----------------
# also how does it relate to mean density?

# need to fix this function:
#Error in eval(expr, envir, enclos) : object 'bimodal' not found 

map.fires <- function(model){
  
  ## for ED2:
  if(model == "ED2"){
    
    # compare this to a map of fire frequency:
    fire <- readRDS(paste0(getwd(),'/Data/ED2/ED2.Fire.rds'))
    dimnames(fire) <- list(timevec, paleon$num)
    df.fire <- data.frame(fire)
    fire.tots <- readRDS(paste0(getwd(), "/outputs/data/ED2/ED2.meandens.rds"))
    fire.tots$Fire.tots <- colSums(fire, na.rm=TRUE) # find the total number of fires at each grid cell
    fire.tots$countfires <- colSums(fire != 0, na.rm=TRUE)
    
    # plot the mean density out on a map:
    states <- map_data("state")
    states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
    coordinates(states)<-~long+lat
    class(states)
    proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
    states <- spTransform(states,CRSobj = '+init=epsg:4326')
    mapdata <- data.frame(states)
    
    cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
    
    
    # plot a map of total fire emmissions across space:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_Fire_emmissions_map.png"))
      print(ggplot(fire.tots, aes(x = lon, y = lat, fill = Fire.tots))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total Fire Emmissions kgC/m2/s", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total Fire emmissions 850-2011'))
    dev.off()
    
    # plot a map of the total instances of fire across space
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_fire_counts_map.png"))
      print(ggplot(fire.tots, aes(x = lon, y = lat, fill = countfires))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total number of Fires", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total number of fires 850-2011'))
    dev.off()
    
    # basic plots of fire emmissions vs mean tree density and sd throught time:
    pdf(paste0("outputs/preliminaryplots/Dens/", model,"_density_vs_fire.pdf"))
      print(ggplot(fire.tots, aes(x = Fire.tots, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Total fire emmissions 850-2011"))
      print(ggplot(fire.tots, aes(x = Fire.tots, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Total fire emmissions 850-2011"))
      
      # plots of fire counts vs. mean density and sd through time:
      print(ggplot(fire.tots, aes(x = countfires, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Number of fires 850-2011"))
      print(ggplot(fire.tots, aes(x = countfires, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Number of fires 850-2011"))
    dev.off()
    
  }else{
    ## For LPJ-GUESS:
    
    # compare this to a map of fire frequency:
    fire <- readRDS(paste0(getwd(),'/Data/LPJ-GUESS/LPJ-GUESS.Fire.rds'))
    dimnames(fire) <- list(yr, paleon$num)
    df.fire <- data.frame(fire)
    fire.tots <- readRDS(paste0(getwd(), "/outputs/data/GUESS/GUESS.meandens.rds"))
    fire.tots$Fire.tots <- colSums(fire, na.rm=TRUE) # find the total number of fires at each grid cell
    fire.tots$countfires <- colSums(fire != 0, na.rm=TRUE)
    
    # plot the mean density out on a map:
    states <- map_data("state")
    states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
    coordinates(states)<-~long+lat
    class(states)
    proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
    states <- spTransform(states,CRSobj = '+init=epsg:4326')
    mapdata <- data.frame(states)
    
    cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
    
    
    # plot a map of total fire emmissions across space:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/", model,"_Fire_emmissions_map.png"))
    print(ggplot(fire.tots, aes(x = lon, y = lat, fill = Fire.tots))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total Fire Emmissions kgC/m2/s", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total Fire emmissions 850-2011'))
    dev.off()
    
    # plot a map of the total instances of fire across space
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/",model,"_fire_counts_map.png"))
    print(ggplot(fire.tots, aes(x = lon, y = lat, fill = countfires))+geom_raster()+
      scale_fill_gradient(low = "#fee090", high = '#99000d',name ="Total number of Fires", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Total number of fires 850-2011'))
    dev.off()
    
    # basic plots of fire emmissions vs mean tree density and sd throught time:
    pdf(paste0("outputs/preliminaryplots/Dens/", model,"_density_vs_fire.pdf"))
    print(ggplot(fire.tots, aes(x = Fire.tots, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Total fire emmissions 850-2011"))
    print(ggplot(fire.tots, aes(x = Fire.tots, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Total fire emmissions 850-2011"))
    
    # plots of fire counts vs. mean density and sd through time:
    print(ggplot(fire.tots, aes(x = countfires, y = mean, color = bimodal))+geom_point()+theme_bw()+xlab("Mean total density")+ylab("Number of fires 850-2011"))
    print(ggplot(fire.tots, aes(x = countfires, y = sd, color = bimodal))+geom_point()+theme_bw()+xlab("SD total density")+ylab("Number of fires 850-2011"))
    dev.off()
  }
}

map.fires(model = "ED2")
map.fires(model = "GUESS")


#----------- What is the sensitivity of total density to CO2 in ED2?-------------
# use ED2 CO2 for CO2 (can't find the output for LPJ-GUESS)

WUE.cor.co2 <- function(model){
  if(model == "ED2"){
  atm.co2 <- CO2[,1]
  # df for saving sensitiviey
  sens.mean <- data.frame(num = paleon$num, 
                        lon = paleon$lon, 
                        lat = paleon$lat, 
                        latlon = paleon$latlon,
                        corCO2 = NA, 
                        mean=NA,
                        sd = NA
                        )

  for(i in 1:length(paleon$num)){
    Dens.r <- readRDS(paste0(getwd(), "/Data/ED2/ED2.Dens.rds"))
    dens.site <- data.frame(Dens.r[,i,])
    dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
    sens.mean[i,]$corCO2 <- cor(dens.site$Totaldens, atm.co2)
    sens.mean[i,]$mean <- mean(dens.site$Totaldens, na.rm= TRUE)
    sens.mean[i,]$sd <- sd(dens.site$Totaldens, na.rm=TRUE)
  }

  rbpalette<- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0',
                '#92c5de','#4393c3','#2166ac','#053061')
  
# it looks like in ED, the west is more highly positively corrllated with CO2:
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_dens_co2_cor_map.png"))
  ggplot(sens.mean, aes(x = lon, y = lat, fill = corCO2))+geom_raster()+
    scale_fill_gradientn(colours = rev(rbpalette), limits = c(-1,1), name ="correlation coefficient", na.value = 'darkgrey')+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Correlation of Total Density with CO2')
  dev.off()

  }else{
    
    # for GUESS
    atm.co2 <- CO2[,1]
    # df for saving sensitiviey
    sens.mean <- data.frame(num = paleon$num, 
                            lon = paleon$lon, 
                            lat = paleon$lat, 
                            latlon = paleon$latlon,
                            corCO2 = NA, 
                            mean=NA,
                            sd = NA
    )
    
    for(i in 1:length(paleon$num)){
      Dens.r <- readRDS(paste0(getwd(), "/Data/LPJ-GUESS/LPJ-GUESS.Dens.rds"))
      dens.site <- data.frame(Dens.r[,i,])
      dens.site$Total <- dens.site[,,13] 
      #dens.site$Totaldens <- rowSums(dens.site, na.rm=TRUE)
      sens.mean[i,]$corCO2 <- cor(dens.site$Total, atm.co2)
      sens.mean[i,]$mean <- mean(dens.site$Total, na.rm= TRUE)
      sens.mean[i,]$sd <- sd(dens.site$Total, na.rm=TRUE)
    }
    
    rbpalette<- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#d1e5f0',
                  '#92c5de','#4393c3','#2166ac','#053061')
    
    # it looks like in ED, the west is more highly positively corrllated with CO2:
    png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/",model,"_dens_co2_cor_map.png"))
    ggplot(sens.mean, aes(x = lon, y = lat, fill = corCO2))+geom_raster()+
      scale_fill_gradientn(colours = rev(rbpalette), limits = c(-1,1), name ="correlation coefficient", na.value = 'darkgrey')+
      geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Correlation of Total Density with CO2')
    dev.off()
  }
}
WUE.cor.co2(model = "ED2")
WUE.cor.co2(model = "GUESS")



# from here on out, this is test code:

#------what is the sensitivity of Relativized Density to precip and Tair?--------------

#atm.co2 <- CO2[,1]

# read in the saved mean density file
mean.dens <- readRDS("outputs/data/ED2/ED2.meandens.rds")

# need to relativize the the density data:
# making the Relative Density file for ED2:
TotalDens <- CO2
RelDens <- CO2 
Dens.r <- readRDS(paste0(getwd(), "/Data/ED2/ED2.Dens.rds"))


for(i in 1:length(paleon$num)){
  
  dens.site <- data.frame(Dens.r[,i,])
  TotalDens[,i] <- rowSums(dens.site, na.rm=TRUE)
  RelDens[,i] <- TotalDens[,i]/mean(TotalDens[,i], na.rm=TRUE)
}

saveRDS(RelDens ,"outputs/data/ED2/ED2.RelDens.rds")


# making the Relative Density file for GUESS:
RelDens <- CO2
Dens.r <- readRDS(paste0(getwd(), "/Data/LPJ-GUESS/LPJ-GUESS.Dens.rds"))

for(i in 1:length(paleon$num)){
  dens.site <- data.frame(Dens.r[,i,])
  TotalDens[,i] <- dens.site[,13]
  RelDens[,i] <- TotalDens[,i]/mean(TotalDens[,i], na.rm=TRUE)
}

saveRDS(RelDens, "outputs/data/GUESS/GUESS.RelDens.rds")

# okay now lets look at sensitivity of "Relative Density" to Tair and Precip and WUE

# for model == "ED2"

ED.reldens <- readRDS("outputs/data/ED2/ED2.RelDens.rds")
ED.tair <- readRDS("Data/ED2/ED2.tair.rds")
ED.precip <- readRDS("Data/ED2/ED2.precipf.rds")
ED.IWUE <- readRDS("Data/ED2/ED2.IWUE.rds")
ED.WUEi <- readRDS("Data/ED2/ED2.WUEi.rds")
ED.WUEt <- readRDS("Data/ED2/ED2.WUEt.rds")
ED.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")

# get the mean relative density (not sure if this is right--double check)
sec2yr <- 1*60*60*24*365.25
source("R/get.yrmeans.R")

reldens.y <- get.yrmeans(ED.reldens, "Rel.Dens")
tair.y <- get.yrmeans(ED.tair, "Tair")
tair.y$Tair.C <- tair.y$Tair - 273.15
precipf.y <- get.yrmeans(ED.precip, "precip")
precipf.y$precip.mm <- precipf.y$precip*sec2yr # convert to mm

IWUE.y <- get.yrmeans(ED.IWUE, "IWUE")
WUEi.y <- get.yrmeans(ED.WUEi, "WUEi")
WUEt.y <- get.yrmeans(ED.WUEt, "WUEt")
CO2.y <- get.yrmeans(ED.CO2, "CO2")


# use reduce to merge these all together
all.y <- Reduce(function(x, y) merge(x, y, by = c("Year", "Site"),all=TRUE), list(reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                 tair.y, precipf.y))

# save the all.y
saveRDS(all.y, "outputs/data/ED2/ED2.alldat.yrmeans.rds")


# this function currently takes awhile
# plot.sens.subset <- function(df, xname, yname, yrs){
#   df <- df[,c("Year", "Site", xname, yname)]
#   colnames(df) <- c("Year", "Site", "x", "y")
#   df <- df[df$Year %in% yrs, ]
#   lim <- quantile(df$x, .99, na.rm=T) # so we dont plot the outliers
#   
#   png(height = 12, width = 12, units= "in", res = 100, file = paste0(getwd(),"/outputs/preliminaryplots/sensitivity/ED2_", xname,"_", yname,"_",yrs[1],"_",yrs[length(yrs)],"_sens.png"))
#   print(ggplot(data = df, aes(x = x, y = y, color = Site))+geom_point()+xlim(0,lim)+
#           ylab(yname)+ xlab(xname)+stat_smooth(color = "black") +theme_bw()+ theme(legend.position="none") )
#   dev.off()
#   
# # write site level data to a pdf:
#   pdf(paste0(getwd(),"/outputs/preliminaryplots/sensitivity/ED2_", xname,"_", yname,"_",yrs[1],"_",yrs[length(yrs)],"_sens_site.pdf"), 7, 5)
#   for (i in 1:length(unique(df$Site))) {
#     print(ggplot(df[df$Site %in% df$Site[i:(i+24)], ], 
#                  aes(x, y)) + 
#             geom_point() +
#             facet_wrap(~ Site, ncol = 5, nrow = 5) +
#             xlim(0,lim)+
#             ylab(yname)+ xlab(xname)+stat_smooth(color = "black") +theme_bw())
#   }
#   dev.off()
# }
# 
# 
# plot.sens(all.y, "CO2", "Rel.Dens")
# plot.sens(all.y, "Rel.Dens", "IWUE")
# plot.sens(all.y, "Rel.Dens", "WUEi")
# plot.sens(all.y, "Rel.Dens", "WUEt")
# plot.sens(all.y, "Rel.Dens", "Tair")
# plot.sens(all.y, "Rel.Dens", "precip")
# 
# # making the plots for the CO2 dominated era:
# 
# plot.sens.subset(all.y, "Rel.Dens", "CO2", 850:1800)
# plot.sens.subset(all.y, "Rel.Dens", "CO2", 1800:2010)
# plot.sens.subset(all.y, "Rel.Dens", "IWUE", 850:1800)
# plot.sens.subset(all.y, "Rel.Dens", "WUEi", 850:1800)
# plot.sens.subset(all.y, "Rel.Dens", "WUEt", 850:1800)
# plot.sens.subset(all.y, "Rel.Dens", "Tair", 850:1800)
# plot.sens.subset(all.y, "Rel.Dens", "precip", 850:1800)

# The above plots are all interesteing since precip, tair, WUE, density all vary by site. 
# may be helpful to determine a site specific sensitivity to CO2, precip, WUE
# OR relativize by mean of all sites at all time periods?



#-----------------Plot site level sensitivities to climat-------------
png(height = 20, width = 20, units = "in", res = 200, "outputs/preliminaryplots/sensitivity/ED2_TairC_relDens.png")
ggplot(all.y, aes(Tair.C, Rel.Dens))+geom_point()+facet_wrap(~Site)
dev.off()

png(height = 20, width = 20, units = "in", res = 200, "outputs/preliminaryplots/sensitivity/ED2_precipmm_relDens.png")
ggplot(all.y, aes(precip.mm, Rel.Dens))+geom_point()+facet_wrap(~Site)
dev.off()

png(height = 20, width = 20, units = "in", res = 200, "outputs/preliminaryplots/sensitivity/ED2_TairC_relDens.png")
ggplot(all.y, aes(CO2, Rel.Dens))+geom_point()+facet_wrap(~Site)
dev.off()

# read in the agbi + total dens as well:
dens.agbi.site <- readRDS( "outputs/data/ED2/ED2.agbi.dens.site.rds")
#dens.agbi <- readRDS( "outputs/data/ED2/ED2.agbi.rds")

all.df <- left_join(all.y, dens.agbi.site, by = c("Year", "Site"))

# in general, CO2, precip, and Tair all affect agbi at sites:
ggplot(all.df, aes(CO2, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(precip.mm, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(Tair.C, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(CO2, GS_agb, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_agb, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")

ggplot(all.df, aes(Tair.C, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(precip.mm, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(CO2, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_gwbi, Dens, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_gwbi, GS_agb, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")


ggplot(all.df, aes(CO2, Dens))+geom_point()+stat_smooth()+stat_smooth()+theme(legend.position = "none")
library(mgcv)

agbi.p.gam <- gam(agbi ~ s(precip.mm), data = all.df)
agbi.p.t.gam <- gam(agbi ~ s(precip.mm) + s(Tair.C), data = all.df)
agbi.p.t.c.gam <- gam(agbi ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(Year), data = all.df)
summary(agbi.p.t.c.gam)
plot(agbi.p.t.c.gam)

dens.p.t.c.gam <- gam(Dens ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(Year), data = all.df)
summary(dens.p.t.c.gam)

agb.p.t.c.gam <- gam(GS_agb ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(Year) + s(Dens), data = all.df)
summary(agb.p.t.c.gam)
plot(agb.p.t.c.gam)

rel.dens.p.t.c.gam <- gam(Rel.Dens ~ s(precip.mm) + s(Tair.C) + s(CO2) + s(agbi), data = all.df)
summary(rel.dens.p.t.c.gam)
plot(rel.dens.p.t.c.gam)

agbi.p.t.c.glm <- lm(agbi ~ precip.mm + Tair.C + CO2 + Site, data = all.df)
summary(agbi.p.t.c.glm )
saveRDS(all.df, "outputs/data/ED2/dens_agbi_climate_ED2.rds")



# -------------write out the dens_agbi_climate_GUESS.rds ----------------------


GUESS.reldens <- readRDS("outputs/data/GUESS/GUESS.RelDens.rds")
GUESS.tair <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.tair.rds")
GUESS.precip <- readRDS("Data/LPJ-GUESS/lpj-guess.precipf.rds")
GUESS.IWUE <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.IWUE.rds")
GUESS.WUEt <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.IWUEet.rds")
#GUESS.WUEt <- readRDS("Data/LPJ-GUESS/GUESS.WUEt.rds")
GUESS.CO2 <- ED2.CO2

# get the mean relative density (not sure if this is right--double check)
sec2yr <- 1*60*60*24*365.25
source("R/get.yrmeans.R")

reldens.y <- get.yrmeans(GUESS.reldens, "Rel.Dens")
tair.y <- get.yrmeans(GUESS.tair, "Tair")
tair.y$Tair.C <- tair.y$Tair - 273.15
precipf.y <- get.yrmeans(GUESS.precip, "precip")
precipf.y$precip.mm <- precipf.y$precip*sec2yr # convert to mm

IWUE.y <- get.yrmeans(GUESS.IWUE, "IWUE")
WUEi.y <- get.yrmeans(GUESS.WUEi, "WUEi")
WUEt.y <- get.yrmeans(GUESS.WUEt, "WUEt")
CO2.y <- get.yrmeans(GUESS.CO2, "CO2")


# use rGUESSuce to merge these all together
all.y <- Reduce(function(x, y) merge(x, y, by = c("Year", "Site"),all=TRUE), list(reldens.y, IWUE.y, WUEt.y, CO2.y,
                                                                                  tair.y, precipf.y))

# save the all.y
saveRDS(all.y, "outputs/data/GUESS/GUESS.alldat.yrmeans.rds")



# read in the agbi + total dens as well:
dens.agbi.site <- readRDS( "outputs/data/GUESS/GUESS.agbi.dens.site.rds")
#dens.agbi <- readRDS( "outputs/data/GUESS/GUESS.agbi.rds")

all.df <- left_join(all.y, dens.agbi.site, by = c("Year", "Site"))

# in general, CO2, precip, and Tair all affect agbi at sites:
ggplot(all.df, aes(CO2, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(precip.mm, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(Tair.C, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(CO2, GS_agb, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_agb, agbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")

ggplot(all.df, aes(Tair.C, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(precip.mm, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes(CO2, GS_gwbi, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_gwbi, Dens, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")
ggplot(all.df, aes( GS_gwbi, GS_agb, color = Site))+geom_point()+stat_smooth()+theme(legend.position = "none")

#-----What is sensitivity to climate when we relativize the climate data----

# for model == "ED2"

ED.reldens <- readRDS("outputs/data/ED2/ED2.RelDens.rds")
ED.tair <- readRDS("Data/ED2/ED2.tair.rds")
ED.precip <- readRDS("Data/ED2/ED2.precipf.rds")
ED.IWUE <- readRDS("Data/ED2/ED2.IWUE.rds")
ED.WUEi <- readRDS("Data/ED2/ED2.WUEi.rds")
ED.WUEt <- readRDS("Data/ED2/ED2.WUEt.rds")
ED.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")

# get the mean relative density (not sure if this is right--double check)
# function to map out WUE increase across space:
map.WUE.inc <- function(WUEtype, var){
  
  png(height = 5, width = 8, units = "in", res=300, paste0(getwd(),"/outputs/preliminaryplots/WUE/ED2_",var,"inc_rel_pre1800.png"))
  print(ggplot(IWUEinc, aes(Year, IWUE, color = Site))+geom_point()+theme(legend.position = "none")+theme_bw()+theme(legend.position="none"))
  dev.off()
  
  a <- dcast(IWUEinc, Year ~ Site)
  
  
  slope.table <- data.frame(site = colnames(a[,2:length(a)]),
                            pval = NA, 
                            slope = NA)
  
  # find the slopes for WUE for 
  for(i in 1:length(paleon$num)){
    if(is.na(a[,i+1])){
      pval <- NA
      slope <- NA
    }else{
      mod <- summary( lm(a[,i+1] ~ Year,data = a) )
      pval <- mod$coefficients[2,4]
      slope <- mod$coefficients[2,1]
    }
    slope.table[i,]$pval <- pval
    slope.table[i,]$slope <- slope
  }
  
  paleon$site <- paste0("X", paleon$num)
  
  # merge paleon to site to plot:
  slope.xy <- merge(paleon, slope.table, by = "site")
  
  ggplot(slope.xy, aes(x = lon, y=lat, fill= slope))+geom_raster()
  
  
  states <- map_data("state")
  states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
  coordinates(states)<-~long+lat
  class(states)
  proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
  states <- spTransform(states,CRSobj = '+init=epsg:4326')
  mapdata <- data.frame(states)
  
  cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
  
  
  # map out the correlations with WUE
  png(height = 4, width = 8, units = 'in',res=200,paste0(getwd(),"/outputs/preliminaryplots/Dens/maps/ED_",var,"_inc_map.png"))
  print(ggplot(slope.xy, aes(x = lon, y=lat, fill= slope))+geom_raster()+
          scale_fill_gradient(low = "blue", high = "red", name ="slope (WUE increase/year)", na.value = 'darkgrey')+
          geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-100,-60), ylim=c(34,50)) + theme_bw() + ggtitle('Slope of relative WUE increase/year'))
  dev.off()
}

map.WUE.inc(IWUEinc, "IWUE")
map.WUE.inc(WUEiinc, "WUEi")
map.WUE.inc(WUEtinc, "WUEt")

# get the relative jjameans for all the datavariables
source("R/get.yrmeans.R")
get.relative.jjameans <- function (model){
  if(model == "ED2"){
    ED.reldens <- readRDS("outputs/data/ED2/ED2.RelDens.rds")
    ED.tair <- readRDS("Data/ED2/ED2.tair.rds")
    ED.precip <- readRDS("Data/ED2/ED2.precipf.rds")
    ED.IWUE <- readRDS("Data/ED2/ED2.IWUE.rds")
    ED.WUEi <- readRDS("Data/ED2/ED2.WUEi.rds")
    ED.WUEt <- readRDS("Data/ED2/ED2.WUEt.rds")
    ED.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")
    ED.LAI <- readRDS("Data/ED2/ED2.LAI.rds")
    ED.AGB <- readRDS("Data/ED2/ED2.AGB.rds")
    
    
    
    reldens.y <- get.JJAmeans(ED.reldens, "Rel.Dens")
    tair.y <- get.JJAmeans(ED.tair, "Tair")
    precipf.y <- get.JJAmeans(ED.precip, "precip")
    IWUE.y <- get.JJAmeans(ED.IWUE, "IWUE")
    WUEi.y <- get.JJAmeans(ED.WUEi, "WUEi")
    WUEt.y <- get.JJAmeans(ED.WUEt, "WUEt")
    CO2.y <- get.JJAmeans(ED.CO2, "CO2")# save the all.y
    LAI.y <- get.JJAmeans(ED.LAI, "LAI")# save the all.y
    AGB.y <- get.JJAmeans(ED.AGB, "AGB")
    
    all.jja <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                     tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja, "outputs/data/ED2/ED2.alldat.jjameans.rds")
    
    
    
    
    #relativize <- function(df, var){
     # test <- dcast(df, Year ~ Site)
      #Relvalue <- test
      
    #  for(i in 1:length(paleon$num)){
     #   Relvalue[,i+1] <- test[,i+1]/mean(test[,i+1], na.rm=TRUE)
      #}
      #m3 <- melt(Relvalue, id.vars = "Year")
      #colnames(m3) <- c("Year", "Site", var)
      #m3
    #}
    
   # tair.r <- relativize(tair.y, "tair")
  #  precipf.r <- relativize(precipf.y, "precipf")
   # IWUE.r <- relativize(IWUE.y, "IWUE")
  #  WUEi.r <- relativize(WUEi.y, "WUEi")
   # WUEt.r <- relativize(WUEt.y, "WUEt")
    
    # use reduce to merge these all together
    #jja.y <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y,  CO2.y, IWUE.r, WUEi.r, WUEt.r,
     #                                                                tair.r, precipf.r))
    
    
    # sensitivity analyses still need some work:
    
    #------------- Are WUE increases higher in the West then?-------------------
    # look at increases in WUE relative to 850-1800 mean:
    # using the WUEi.y summer growing season averages
    
    relativize.period <- function(df,period, var){
    
      test <- dcast(df, Year ~ Site)
      Relvalue <- test
      
      for(i in 1:length(paleon$num)){
        Relvalue[,i+1] <- test[,i+1]/mean(test[test$Year %in% period ,i+1], na.rm=TRUE)
      }
      m3 <- melt(Relvalue, id.vars = "Year")
      colnames(m3) <- c("Year", "Site", paste0("rel_",var))
      m3
    }
    
    IWUEinc <- relativize.period(IWUE.y, 850:1800, "IWUE")
    WUEiinc <- relativize.period(WUEi.y, 850:1800, "WUEi")
    WUEtinc <- relativize.period(WUEt.y, 850:1800, "WUEt")
    # rename variables:
    
    
    # save the jja relative wue, density and overall precipitation to a single df:
    all.jja.rel <-  Reduce(function(x, y) merge(x, y, by =,all=TRUE), list(reldens.y, IWUEinc, WUEiinc, WUEtinc,reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                           tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja.rel, "outputs/data/ED2/ED2.all.jja.rel.rds")
    
    }else{
    
    # if the model == LPJ.GUESS
    G.reldens <- readRDS("outputs/data/GUESS/GUESS.RelDens.rds")
    G.tair <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.tair.rds")
    G.precip <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.precipf.rds")
    G.IWUE <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.IWUE.rds")
    G.WUEi <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.WUEi.rds")
    G.WUEt <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.WUEt.rds")
    G.CO2 <- readRDS("Data/ED2/ED2.CO2.rds")
    G.LAI <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.LAI.rds")
    G.AGB <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.AGB.rds")
    
    reldens.y <- get.JJAmeans(G.reldens, "Rel.Dens")
    tair.y <- get.JJAmeans(G.tair, "Tair")
    precipf.y <- get.JJAmeans(G.precip, "precip")
    IWUE.y <- get.JJAmeans(G.IWUE, "IWUE")
    WUEi.y <- get.JJAmeans(G.WUEi, "WUEi")
    WUEt.y <- get.JJAmeans(G.WUEt, "WUEt")
    CO2.y <- get.JJAmeans(G.CO2, "CO2")# save the all.y
    LAI.y <- get.JJAmeans(G.LAI, "LAI")
    #AGB.y <- get.JJAmeans(G.AGB, "AGB")
    dimnames(G.AGB) <- list(yr, paleon$num, pft.guess)
    totAGB.y <- data.frame(G.AGB[,,"Total"])
    totAGB.y$Year <- yr
    AGB.y <- melt(totAGB.y, id.vars = c("Year"))
    colnames(AGB.y) <- c("Year", "Site", "AGB")
    
    all.jja <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                       tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja, "outputs/data/GUESS/GUESS.alldat.jjameans.rds")
    
  
    #relativize <- function(df, var){
      #test <- dcast(df, Year ~ Site)
      #Relvalue <- test
      
    #  for(i in 1:length(paleon$num)){
     #   Relvalue[,i+1] <- test[,i+1]/mean(test[,i+1], na.rm=TRUE)
      #}
    #  m3 <- melt(Relvalue, id.vars = "Year")
     # colnames(m3) <- c("Year", "Site", var)
      #m3
    #}
    
    #tair.r <- relativize(tair.y, "tair")
    #precipf.r <- relativize(precipf.y, "precipf")
    #IWUE.r <- relativize(IWUE.y, "IWUE")
    #WUEi.r <- relativize(WUEi.y, "WUEi")
    #WUEt.r <- relativize(WUEt.y, "WUEt")
    
    # use reduce to merge these all together
    #jja.y <- Reduce(function(x, y) merge(x, y, by = ,all=TRUE), list(reldens.y,  CO2.y, IWUE.r, WUEi.r, WUEt.r,
     #                                                                tair.r, precipf.r))
    
    
    # sensitivity analyses still need some work:
    
    #------------- Are WUE increases higher in the West then?-------------------
    # look at increases in WUE relative to 850-1800 mean:
    # using the WUEi.y summer growing season averages
    
    relativize.period <- function(df,period, var){
      
      test <- dcast(df, Year ~ Site)
      Relvalue <- test
      
      for(i in 1:length(paleon$num)){
        Relvalue[,i+1] <- test[,i+1]/mean(test[test$Year %in% period ,i+1], na.rm=TRUE)
      }
      m3 <- melt(Relvalue, id.vars = "Year")
      colnames(m3) <- c("Year", "Site", paste0("rel_",var))
      m3
    }
    
    IWUEinc <- relativize.period(IWUE.y, 850:1800, "IWUE")
    WUEiinc <- relativize.period(WUEi.y, 850:1800, "WUEi")
    WUEtinc <- relativize.period(WUEt.y, 850:1800, "WUEt")
    # rename variables:
    
    
    # save the jja relative wue, density and overall precipitation to a single df:
    all.jja.rel <-  Reduce(function(x, y) merge(x, y, by =,all=TRUE), list(reldens.y, IWUEinc, WUEiinc, WUEtinc,reldens.y, IWUE.y, WUEi.y, WUEt.y, CO2.y,
                                                                           tair.y, precipf.y, LAI.y, AGB.y))
    
    saveRDS(all.jja.rel, "outputs/data/GUESS/GUESS.all.jja.rel.rds")
  }
}

get.relative.jjameans(model = "ED2")
get.relative.jjameans(model = "GUESS")


#-------------- Is the increase in WUE related to changes in density?-----------
# also are these increases linked to spatial patterns in precip and temperature

# Main Question: Is climate or stand structure more important in determining WUE increase?

# Steps for analysis:

# 1. Get a value for each growing season/year at each point (above)
# 2. Take all the points together and model of WUE based on tree density (relative), CO2, temp, precip


# for jja mean data:
#jja.y <- readRDS("outputs/data/ED2/ED2.alldat.yrmeans.rds")

make_post_1800_plots <- function(model){
  if(model == "ED2"){

  jja.y <- readRDS("outputs/data/ED2/ED2.all.jja.rel.rds")
  
  # get the years from 1800 - 2010:
  jja.subset <- jja.y[jja.y$Year %in% 1800:2010, ]
  
  
  
  # Plot basic Trends through time
  
  
  # maybe plot these by mean precipf, mean tair, and mean WUE?
  
  
  # Q: What is the effect of WUE, precip, tair on rel. density?
  # basic plots over the whole domain
  
  # find the mean precip rate
  precip <- jja.subset[,c("Site", "precip", "Year")]
  pr.means <- aggregate(precip ~ Site, data = precip, FUN = mean)
  colnames(pr.means) <- c("Site", "mean.precipf")
  jja.subset <- merge(jja.subset, pr.means, by = "Site")# add site means to the jja.subset df
  
  # print these all to a pdf:
  pdf("outputs/preliminaryplots/post_1800_changes/ED2/post_1800_timeseries.pdf")
  
  ggplot(jja.subset, aes(Rel.Dens, Tair, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(Rel.Dens, IWUE, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(Rel.Dens, precip, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(LAI, precip, color = Site))+geom_point()+theme(legend.position = "none")
  ggplot(jja.subset, aes(AGB, precip, color = Site))+geom_point()+theme(legend.position = "none")
  
  
  
  # now lets make some prelimary plots colored by mean precip
  
  ggplot(jja.subset, aes(Rel.Dens, Tair, color = mean.precipf ))+geom_point()
  ggplot(jja.subset, aes(Rel.Dens, precip, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Rel.Dens, CO2, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Rel.Dens, IWUE, color = mean.precipf))+geom_point()
  dev.off()
  
  
  # Plot basic Trends through time colored by mean precipf
  pdf("outputs/preliminaryplots/post_1800_changes/ED2/post_1800_timeseries_by_precip.pdf")
  
  ggplot(jja.subset, aes(Year, Tair, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Year, CO2, color = mean.precipf))+geom_point()
  ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  # based on these plots, it looks like places with lower mean precipf over 1800-2010 show larger inc in density:
  # are these also places with increases in WUE
  # these plots are omitting several outliers:
  
  ggplot(jja.subset, aes(Year, IWUE, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, WUEt, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, WUEi, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  dev.off()
  
  
  
  ggplot(jja.subset, aes(Rel.Dens, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Rel.Dens, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Rel.Dens, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(AGB, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(AGB, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(AGB, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  
  # lets also look at the places the increase relative to mean annual temperature
  Tair <- jja.subset[,c("Site", "Tair", "Year")]
  tair.means <- aggregate(Tair ~ Site, data = Tair, FUN = mean)
  colnames(tair.means) <- c("Site", "mean.tair")
  jja.subset <- merge(jja.subset, tair.means, by = "Site")# add site means to the jja.subset df
  
  
  pdf("outputs/preliminaryplots/post_1800_changes/ED2/post_1800_timeseries_by_tair.pdf")
  
  # plot relative increase in WUE jja temp subset:
  ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  # plot relative increase in density with jja temp subset:
  ggplot(jja.subset, aes(Year, Tair, color = mean.tair))+geom_point()
  ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()
  ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
  
  dev.off()
  
  # lets make these outputs better and on the same png file:
  source("R/grid_arrange_shared_legend.R")
  
  rd <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  pr <- ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()+theme_bw()
  IWUEp <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEtp <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEip <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  
  #X11(width =12)
  library(grid)
  library(gridExtra)
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeserise_by_precip.png")
  grid_arrange_shared_legend(rd, pr, WUEtp, nrow=3, ncol=1, position = "right")
  dev.off()
  
  # now lets subset the reldens and WUE increases by precip
  rdhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with higher than average precip")+ylab("Relative Density")
  rdlow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with lower than average precip")+ylab("Relative Density")
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(rdhigh, rdlow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot ts of AGB increases colored by precip 
  ahigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Relative Density")+ylim(0,45)
  alow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Relative Density")+ylim(0,45)
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_AGB_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(ahigh, alow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot ts of LAI increases colored by precip
  lhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Relative Density")+ylim(0,12)
  llow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Relative Density")+ylim(0,12)
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_LAI_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(lhigh, llow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot high and lows for LAI
  lhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+ylim(0,11)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("LAI")
  llow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+ylim(0, 11)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("LAI")
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(lhigh, llow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # plot high and lows for AGB:
  ahigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Aboveground Biomass")
  alow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Aboveground Biomass")
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_AGB_timeseries_highlo_precip.png")
  grid_arrange_shared_legend(ahigh, alow, nrow=2, ncol=1, position = "right")
  dev.off()
  
  # do the same thing for WUE
  IWUEphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at high precip sites")+ylim(0,4)
  WUEtphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at high precip sites")+ylim(0,4)
  WUEiphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at high precip sites")+ylim(0,20)
  
  IWUEplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at low precip sites")+ylim(0,4)
  WUEtplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at low precip sites")+ylim(0,4)
  WUEiplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at low precip sites")+ylim(0,20)
  
  png(height = 12, width = 12, units = "in",res=200,"outputs/preliminaryplots/post_1800_changes/ED2/post1800_WUE_timeseries_by_precip.png")
  grid_arrange_shared_legend(IWUEphigh, IWUEplow, WUEtphigh, WUEtplow, WUEiphigh, WUEiplow, ncol=2, nrow=3, position = "right")
  dev.off()
  
  
  # plot out the temperatures:
  rdt <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  prt <- ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()+theme_bw()
  IWUEt <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEtt <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  WUEit <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
  
  png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/ED2/post1800_dens_timeserise_by_tair.png")
  grid_arrange_shared_legend(rdt, prt, WUEtt, nrow=3, ncol=1, position = "right")
  dev.off()
  
  saveRDS(jja.subset, "outputs/data/ED2/jja.subset.rds")
  
  }else{
    
    # for LPJ-GUESS:
    
    jja.y <- readRDS("outputs/data/GUESS/GUESS.all.jja.rel.rds")
    
    # get the years from 1800 - 2010:
    jja.subset <- jja.y[jja.y$Year %in% 1800:2010, ]
    
    
    # Q: What is the effect of WUE, precip, tair on rel. density?
    # basic plots over the whole domain
    #paleon$Site <- paste0("X", paleon$num)
    #GUESS.jja<- merge(jja.subset, paleon, by = "Site")
    #png("GUESS.map.png")
    #ggplot(GUESS.jja, aes(x = lon, y = lat, fill=mean.precipf))+geom_raster()
    #dev.off()
    #saveRDS(GUESS.jja, "GUESS.jja.rds")
    #jja.ED <- readRDS("outputs/data/ED2/ED2.all.jja.rel.rds")
    
    # get the years from 1800 - 2010:
   # jja.subset <- jja.y[jja.ED$Year %in% 1800:2010, ]
    
    
    
    # print these all to a pdf:
    pdf("outputs/preliminaryplots/post_1800_changes/GUESS/post_1800_timeseries.pdf")
    
    ggplot(jja.subset, aes(Rel.Dens, Tair, color = Site))+geom_point()+theme(legend.position = "none")
    ggplot(jja.subset, aes(Rel.Dens, IWUE, color = Site))+geom_point()+theme(legend.position = "none")
    ggplot(jja.subset, aes(Rel.Dens, precip, color = Site))+geom_point()+theme(legend.position = "none")
    ggplot(jja.subset, aes(LAI, precip, color = Site))+geom_point()+theme(legend.position = 'none')
    ggplot(jja.subset, aes(AGB, IWUE, color = Site))+geom_point()+theme(legend.position = "none")
    
    dev.off()
    
    precip <- jja.subset[,c("Site", "precip", "Year")]
    pr.means <- aggregate(precip ~ Site, data = precip, FUN = mean)
    colnames(pr.means) <- c("Site", "mean.precipf")
    jja.subset <- merge(jja.subset, pr.means, by = "Site")# add site means to the jja.subset df
    
    
    # just merging by had here because it wasn't working above
    # add the relative increases in WUE for ED
    
    # now lets make some prelimary plots of 
    
    ggplot(jja.subset, aes(Rel.Dens, Tair, color = mean.precipf ))+geom_point()
    ggplot(jja.subset, aes(Rel.Dens, precip, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Rel.Dens, CO2, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Rel.Dens, IWUE, color = mean.precipf))+geom_point()
    
    # Plot basic Trends through time colored by mean precipf
    pdf("outputs/preliminaryplots/post_1800_changes/GUESS/post_1800_timeseries_by_precip.pdf")
    
    ggplot(jja.subset, aes(Year, Tair, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Year, CO2, color = mean.precipf))+geom_point()
    ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, AGB, color = mean.precipf))+geom_point()
    
    # based on these plots, it looks like places with lower mean precipf over 1800-2010 show larger inc in density:
    # are these also places with increases in WUE
    # these plots are omitting several outliers:
    
    ggplot(jja.subset, aes(Year, IWUE, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, WUEt, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, WUEi, color = mean.precipf))+ylim(0,100)+geom_point()+scale_color_gradient(low = "red", high = "blue")
    
    ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    
    dev.off()
    
   
    # lets also look at the places the increase relative to mean annual temperature
    Tair <- jja.subset[,c("Site", "Tair", "Year")]
    tair.means <- aggregate(Tair ~ Site, data = Tair, FUN = mean)
    colnames(tair.means) <- c("Site", "mean.tair")
    jja.subset <- merge(jja.subset, tair.means, by = "Site")# add site means to the jja.subset df
    
    
    pdf("outputs/preliminaryplots/post_1800_changes/GUESS/post_1800_timeseries_by_tair.pdf")
    
    # plot relative increase in WUE jja temp subset:
    ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    
    # plot relative increase in density with jja temp subset:
    ggplot(jja.subset, aes(Year, Tair, color = mean.tair))+geom_point()
    ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()
    ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")
    ggplot(jja.subset, aes(Year, AGB, color = mean.tair))+geom_point()
    dev.off()
    
    # lets make these outputs better and on the same png file:
    source("R/grid_arrange_shared_legend.R")
    
    rd <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    pr <- ggplot(jja.subset, aes(Year, precip, color = mean.precipf))+geom_point()+theme_bw()
    IWUEp <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEtp <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEip <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    
    
    #X11(width =12)
    library(grid)
    library(gridExtra)
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_dens_timeserise_by_precip.png")
    grid_arrange_shared_legend(rd, pr, WUEtp, nrow=3, ncol=1, position = "right")
    dev.off()
    
    # now lets subset the reldens and WUE increases by precip
    rdhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with higher than average precip")+ylab("Relative Density")
    rdlow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, Rel.Dens, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ylim(0,4)+ggtitle("Sites with lower than average precip")+ylab("Relative Density")
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_dens_timeseries_highlo_precip.png")
    grid_arrange_shared_legend(rdhigh, rdlow, nrow=2, ncol=1, position = "right")
    dev.off()
    
    # now lets subset the AGB and WUE increases by precip
    ahigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+ylim(0,13)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("Aboveground Biomass")
    alow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, AGB, color = mean.precipf))+geom_point()+ylim(0,13)+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("Aboveground Biomass")
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_AGB_timeseries_highlo_precip.png")
    grid_arrange_shared_legend(ahigh, alow, nrow=2, ncol=1, position = "right")
    dev.off()
    
    lhigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+ylim(0,6)+theme_bw()+ggtitle("Sites with higher than average precip")+ylab("LAI")
    llow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, LAI, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+ylim(0,6)+theme_bw()+ggtitle("Sites with lower than average precip")+ylab("LAI")
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_LAI_timeseries_highlo_precip.png")
    grid_arrange_shared_legend(lhigh, llow, nrow=2, ncol=1, position = "right")
    dev.off()
    
    # do the same thing for WUE
    IWUEphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at high precip sites")+ylim(0,4)
    WUEtphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at high precip sites")+ylim(0,4)
    WUEiphigh <- ggplot(jja.subset[jja.subset$mean.precipf >0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at high precip sites")+ylim(0,20)
    
    IWUEplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_IWUE, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative IWUE at low precip sites")+ylim(0,4)
    WUEtplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEt, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEt at low precip sites")+ylim(0,4)
    WUEiplow <- ggplot(jja.subset[jja.subset$mean.precipf <= 0.00003580,], aes(Year, rel_WUEi, color = mean.precipf))+geom_point()+scale_color_gradient(low = "red", high = "blue", limits=c(range(jja.subset$mean.precipf)))+theme_bw()+ggtitle("Relative WUEi at low precip sites")+ylim(0,20)
    
    png(height = 12, width = 12, units = "in",res=200,"outputs/preliminaryplots/post_1800_changes/GUESS/post1800_WUE_timeseries_by_precip.png")
    grid_arrange_shared_legend(IWUEphigh, IWUEplow, WUEtphigh, WUEtplow, WUEiphigh, WUEiplow, ncol=2, nrow=3, position = "right")
    dev.off()
    
    
    # plot out the temperatures:
    rdt <- ggplot(jja.subset, aes(Year, Rel.Dens, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    prt <- ggplot(jja.subset, aes(Year, precip, color = mean.tair))+geom_point()+theme_bw()
    IWUEt <- ggplot(jja.subset, aes(Year, rel_IWUE, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEtt <- ggplot(jja.subset, aes(Year, rel_WUEt, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    WUEit <- ggplot(jja.subset, aes(Year, rel_WUEi, color = mean.tair))+geom_point()+scale_color_gradient(low = "red", high = "blue")+theme_bw()
    
    png(height = 12, width = 12, units = "in", res = 200, "outputs/preliminaryplots/post_1800_changes/GUESS/post1800_dens_timeserise_by_tair.png")
    grid_arrange_shared_legend(rdt, prt, WUEtt, nrow=3, ncol=1, position = "right")
    dev.off()
    
    saveRDS(jja.subset, "outputs/data/GUESS/jja.subset.rds")
    
  }
  
  

  
  
  
  
  
# now to quantitativily analysize theses
# fit a gam?
# relative density increases over time
denst <- gam(Rel.Dens ~ s(Year), data = jja.subset)
plot(time)

tairt <- gam( Tair ~ s(Year), data = jja.subset )
plot(tairt)

precipt <- gam(precip ~ s(Year) + random(list(Site)), data = jja.subset)
plot(precipt)

# using a gam
g <- gam(Rel.Dens ~ s(Year) + s(WUEt) + s(precip) + s(Tair), data = jja.subset)
summary(g)
plot(g)
