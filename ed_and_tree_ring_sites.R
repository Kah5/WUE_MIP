library(ggplot2)
library(sp)
library(raster)
library(rgdal)
# comparison of my sites with christy's ed runs

# read in christy's ed runs
ed.sites <- read.csv("C:/Users/JMac/Documents/Kelly/MIP/WUE_MIP/WUE_MIP/Paleon_MIP_Phase2_ED_Order_Status.csv")
ggplot(ed.sites, aes(x = lon, y = lat, fill = location ))+geom_raster()

ed.sites$status <- 'not run'
ed.sites[ed.sites$location %in% "geo",]$status <- 'done'
ed.sites[ed.sites$spininital %in% "ERROR",]$status <- "ERROR"

kellysites <- read.csv("C:/Users/JMac/Documents/Kelly/TreeRings/outputs/lat_long_sites_progress.csv")



all_states <- map_data("state")
states <- subset(all_states, region %in% c( "north dakota",'south dakota' ,"illinois", "minnesota", "wisconsin", "iowa", 
                                             'michigan', 'missouri', 'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
#mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata<-data.frame(states)

png(height = 3, width = 7, units = 'in', res = 300, "KH_sites_ED_regional_runs.png")
ggplot()+geom_raster(data = ed.sites, aes(x = lon, y = lat, fill = status ) )+ 
  geom_point(data = kellysites, aes(x = longitude, y = latitude))+
  #geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group),colour = "darkgrey", fill = NA)+
  theme_bw()+coord_cartesian()
dev.off()

coordinates(kellysites) <- ~longitude + latitude
proj4string(kellysites)<- "+init=epsg:4326"

ed <- ed.sites[,c('lon', 'lat', 'num')]
coordinates(ed) <- ~lon + lat
gridded(ed) <- TRUE
proj4string(ed)<- "+init=epsg:4326"
rast <- raster(ed)
plot(rast)
# extract the cell numbers of the tree ring sites
kellysites$num <- extract(x = rast, kellysites)

# use these numbers to merge with num from ed.sitese
TR_ed <- merge(kellysites, ed.sites, by = 'num')
TR.df <- data.frame(TR_ed)

write.csv(TR.df, "Tree_ring_sites_PALEON_model_grid.csv")

ggplot(TR.df, aes(x = longitude, y = latitude, color = location))+geom_point()
