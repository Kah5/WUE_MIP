# script for plotting Paleon model output locations, my tree ring sites, and ITRDB tree ring sites:
library(sp)
library(raster)
library(ggplot2)
library(maps)
library(rgeos)

# read in the spatial points data for the Tree ring data:
TREERING_PALEON_GRID <- read.csv("/Users/kah/Documents/TreeRings/data/KH_Treering_sites_PALEON_model_grid.csv - KH_Treering_sites_PALEON_model_grid.csv-2.csv")

# find the closest grid cell:
# read in the Paleon model sites
load("Data/PalEON_siteInfo_all.RData")
TR.locs <- TREERING_PALEON_GRID[TREERING_PALEON_GRID$Site.code %in% c( "AVO",  "BON",  "COR",  "ENG",  "GLA","GLL",  "GLL1", "GLL2", "GLL3", "GLL4", "HIC",  "MOU",  "PLE",  "PVC", 
                                                                       "STC",  "TOW",  "UNC"  ),]
TR.sites <- merge(paleon, TREERING_PALEON_GRID, by.x = "latlon", by.y = "latlon_PALEON")


# read in ITRDB sites:
rwl.age.ll <- readRDS( "Data/ITRDB/rwl.ages.df.nona_spatial.rds")
rwl.age.ll.unique <- unique(rwl.age.ll[, c("Longitude", "Latitude", "studyCode", "SPEC.CODE")])



all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(all_states)<-~long+lat
class(all_states)

ca = map_data("world", "Canada")
coordinates(ca)<-~long+lat
ca.data <- data.frame(ca)
mapdata <- data.frame(all_states)


# get the lakes to overlay:
library(rnaturalearth)
#  Assuming you have a path 'Maps' that you store your spatial files in.  This
#  is all downloaded from <a href=>http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

# lakes
ne_lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical')
sp::plot(ne_lakes, col = 'blue')
ne_lakes <- rgdal::readOGR('/Users/kah/Documents/TreeRings/data/ne_50m_lakes/ne_50m_lakes.shp')



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

#----------------------- MAP of tree ring and itrdb sites------------------------
all.itrdb.tr.sites <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = 'steelblue2')+ 
  geom_raster(data = paleon, aes(lon, lat), fill = "grey10", alpha = 0.9)+
  geom_point(data = TR.locs, aes(x = longitude, y = latitude), color = "red", fill = "red", shape = 24, size = 5)+
  geom_point(data = rwl.age.ll.unique, aes(x = Longitude, y = Latitude), color = "blue", fill = "blue", shape = 24, size = 5)+
  coord_cartesian(ylim = c(35.5, 49), xlim= c(-99,-66))+theme_bw(base_size = 22)+ylab("")+xlab("")


png(height = 12, width = 18, units = 'in', res=500,"outputs/PALEON_MODEL_MAP_with_ITRDB.png")
all.itrdb.tr.sites
dev.off()

TR.locs$SPEC.CODE <- "QUMA"

all.itrdb.tr.sites.byspec <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = 'steelblue2')+ 
  geom_raster(data = paleon, aes(lon, lat), fill = "grey10", alpha = 0.9)+
  geom_point(data = TR.locs, aes(x = longitude, y = latitude, color = SPEC.CODE, fill = SPEC.CODE), shape = 24, size = 5)+
  geom_point(data = rwl.age.ll.unique, aes(x = Longitude, y = Latitude, color = SPEC.CODE, fill = SPEC.CODE),  shape = 24, size = 5)+
  coord_cartesian(ylim = c(35.5, 49), xlim= c(-99,-66))+theme_bw(base_size = 22)+ylab("")+xlab("")


png(height = 12, width = 20, units = 'in', res=500,"outputs/PALEON_MODEL_MAP_with_ITRDB_by_species.png")
all.itrdb.tr.sites.byspec
dev.off()

# now color by ED2 PFT 

taxa.trans <- read.csv("data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv")
ITRDB.pft <- left_join(taxa.trans, rwl.age.ll.unique, by = "SPEC.CODE")
TR.pft <- left_join(taxa.trans, TR.locs, by = "SPEC.CODE")


all.itrdb.tr.sites.byED <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = 'steelblue2')+ 
  geom_raster(data = paleon, aes(lon, lat), fill = "grey10", alpha = 0.9)+
  geom_point(data = TR.pft, aes(x = longitude, y = latitude, color = ED.PFT, fill = ED.PFT), shape = 24, size = 5)+
  geom_point(data = ITRDB.pft, aes(x = Longitude, y = Latitude, color = ED.PFT, fill = ED.PFT),  shape = 24, size = 5)+
  coord_cartesian(ylim = c(35.5, 49), xlim= c(-99,-66))+theme_bw(base_size = 22)+ylab("")+xlab("")


png(height = 12, width = 20, units = 'in', res=500,"outputs/PALEON_MODEL_MAP_with_ITRDB_by_ED_PFT.png")
all.itrdb.tr.sites.byED
dev.off()
# now color by GUESS PFT

all.itrdb.tr.sites.byGUESS <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = 'steelblue2')+ 
  geom_raster(data = paleon, aes(lon, lat), fill = "grey10", alpha = 0.9)+
  geom_point(data = TR.pft, aes(x = longitude, y = latitude, color = LPJ.GUESS.PFT, fill = LPJ.GUESS.PFT), shape = 24, size = 5)+
  geom_point(data = ITRDB.pft, aes(x = Longitude, y = Latitude, color = LPJ.GUESS.PFT, fill = LPJ.GUESS.PFT),  shape = 24, size = 5)+
  coord_cartesian(ylim = c(35.5, 49), xlim= c(-99,-66))+theme_bw(base_size = 22)+ylab("")+xlab("")


png(height = 12, width = 20, units = 'in', res=500,"outputs/PALEON_MODEL_MAP_with_ITRDB_by_LPJ_GUESS_PFT.png")
all.itrdb.tr.sites.byGUESS
dev.off()

# color by PALEON taxa level

all.itrdb.tr.sites.byPALEON <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = 'steelblue2')+ 
  geom_raster(data = paleon, aes(lon, lat), fill = "grey10", alpha = 0.9)+
  geom_point(data = TR.pft, aes(x = longitude, y = latitude, color = PALEON, fill = PALEON), shape = 24, size = 5)+
  geom_point(data = ITRDB.pft, aes(x = Longitude, y = Latitude, color = PALEON, fill = PALEON),  shape = 24, size = 5)+
  coord_cartesian(ylim = c(35.5, 49), xlim= c(-99,-66))+theme_bw(base_size = 22)+ylab("")+xlab("")


png(height = 12, width = 20, units = 'in', res=500,"outputs/PALEON_MODEL_MAP_with_ITRDB_by_PALEON_Taxa.png")
all.itrdb.tr.sites.byPALEON
dev.off()
