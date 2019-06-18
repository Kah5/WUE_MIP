# script to find grid cells and ITRDB sites with similar mean annual climate spaces:
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
all.met <- readRDS( paste0(getwd(),"/Data/MET/all.met.summary.rds"))
# merge climate and growth for ED2:
colnames(all.met)[3] <- "Year"


all.met.summary <- all.met %>% group_by(lon, lat) %>% summarise(MAP = mean(precip_total.mm, na.rm=TRUE),
                                                                MAP.wy = mean(precip_total_wtr_yr.mm, na.rm=TRUE), 
                                                                tair_max_1 = mean(tair_max_1, na.rm=TRUE),
                                                                tair_max_2 = mean(tair_max_2, na.rm=TRUE),
                                                                tair_max_3 = mean(tair_max_3, na.rm=TRUE),
                                                                tair_max_4 = mean(tair_max_4, na.rm=TRUE),
                                                                tair_max_5 = mean(tair_max_5, na.rm=TRUE),
                                                                tair_max_6 = mean(tair_max_6, na.rm=TRUE),
                                                                tair_max_7 = mean(tair_max_7, na.rm=TRUE),
                                                                tair_max_8 = mean(tair_max_8, na.rm=TRUE),
                                                                tair_max_9 = mean(tair_max_9, na.rm=TRUE),
                                                                tair_max_10 = mean(tair_max_10, na.rm=TRUE),
                                                                tair_max_11 = mean(tair_max_11, na.rm=TRUE),
                                                                tair_max_12 = mean(tair_max_12, na.rm=TRUE))

ggplot(all.met.summary, aes(lon, lat, fill = MAP.wy))+geom_raster()
ggplot(all.met.summary, aes(lon, lat, fill = tair_max_6))+geom_raster()

ggplot(all.met.summary, aes(MAP.wy,tair_max_6, color = lon))+geom_point()

# now summarise for grid cells included in ED, GUESS, and LINKAGEs:
all.ED <- readRDS(paste0(getwd(),"/outputs/data/ED2/ED2.gwbi.pft.all.met.rds"))

grid.met.summary <- all.ED %>% filter(Year >= 1895) %>% group_by(lon, lat) %>% summarise(MAP.met = mean(precip_total.mm, na.rm=TRUE),
                                                                MAP.wy.met = mean(precip_total_wtr_yr.mm, na.rm=TRUE), 
                                                                tair_max_1.met = mean(tair_max_1- 273.15, na.rm=TRUE) ,
                                                                tair_max_2.met = mean(tair_max_2- 273.15, na.rm=TRUE),
                                                                tair_max_3.met = mean(tair_max_3- 273.15, na.rm=TRUE),
                                                                tair_max_4.met = mean(tair_max_4- 273.15, na.rm=TRUE),
                                                                tair_max_5.met = mean(tair_max_5- 273.15, na.rm=TRUE),
                                                                tair_max_6.met = mean(tair_max_6- 273.15, na.rm=TRUE),
                                                                tair_max_7.met = mean(tair_max_7- 273.15, na.rm=TRUE),
                                                                tair_max_8.met = mean(tair_max_8- 273.15, na.rm=TRUE),
                                                                tair_max_9.met = mean(tair_max_9- 273.15, na.rm=TRUE),
                                                                tair_max_10.met = mean(tair_max_10- 273.15, na.rm=TRUE),
                                                                tair_max_11.met = mean(tair_max_11- 273.15, na.rm=TRUE),
                                                                tair_max_12.met = mean(tair_max_12- 273.15, na.rm=TRUE))





# get prism summary: 
rwl.itrdb.clim.nona <- readRDS( paste0(getwd(),"/Data/ITRDB/full.clim.prism.rds"))

all.prism.summary <- rwl.itrdb.clim.nona %>% group_by(Longitude, Latitude) %>% summarise(MAP.prism = mean(ppt_total, na.rm=TRUE),
                                                                              MAP.wy.prism = mean(ppt_MAP.wy, na.rm=TRUE), 
                                                                              tair_max_1.prism = mean(tmax_01, na.rm=TRUE),
                                                                              tair_max_2.prism = mean(tmax_02, na.rm=TRUE),
                                                                              tair_max_3.prism = mean(tmax_03, na.rm=TRUE),
                                                                              tair_max_4.prism = mean(tmax_04, na.rm=TRUE),
                                                                              tair_max_5.prism= mean(tmax_05, na.rm=TRUE),
                                                                              tair_max_6.prism = mean(tmax_06, na.rm=TRUE),
                                                                              tair_max_7.prism = mean(tmax_07, na.rm=TRUE),
                                                                              tair_max_8.prism = mean(tmax_08, na.rm=TRUE),
                                                                              tair_max_9.prism = mean(tmax_09, na.rm=TRUE),
                                                                              tair_max_10.prism = mean(tmax_10, na.rm=TRUE),
                                                                              tair_max_11.prism = mean(tmax_11, na.rm=TRUE),
                                                                              tair_max_12.prism = mean(tmax_12, na.rm=TRUE))


ggplot(all.prism.summary, aes(Longitude, Latitude, color = MAP.wy.prism))+geom_point()
ggplot(all.prism.summary, aes(Longitude, Latitude, color = tair_max_6.prism))+geom_point()

ggplot()+geom_point(data = grid.met.summary, aes(MAP.wy.met,tair_max_6.met), color = "blue")+
geom_point(data = all.prism.summary, aes(MAP.wy.prism, tair_max_6.prism), color = "red")+ylim(15, 35)+xlim(400, 1800)

ggplot(all.prism.summary, aes(MAP.wy.prism, tair_max_6.prism, color = Longitude))+geom_point()+ylim(15, 35)+xlim(400, 1800)

ggplot()+geom_histogram(data = grid.met.summary, aes(MAP.wy.met), fill = "red")+
  geom_histogram(data = all.prism.summary, aes(MAP.wy.prism), fill = "blue")

ggplot()+geom_histogram(data = grid.met.summary, aes(tair_max_6.met), fill = "red")+
  geom_histogram(data = all.prism.summary, aes(tair_max_6.prism), fill = "blue")

# need to subset and select ITRDB sites and model grid cells with comparable averages in recent years:
grid.met.summary$tmax_06_bins <- cut(grid.met.summary$tair_max_6.met, breaks=seq(15, 35, by = 1))

all.prism.summary$tmax_06_bins <- cut(all.prism.summary$tair_max_6.prism, breaks=seq(15, 35, by = 1))

tmax.ordered.cuts <- data.frame(tmax_06_bins = levels(cut(grid.met.summary[order(grid.met.summary$tair_max_6.met),]$tair_max_6.met, breaks=seq(15, 35, by = 1))),
                           tmax06.mids=seq(15.5, 34.5, by = 1))

all.prism.summary <- left_join(all.prism.summary, tmax.ordered.cuts, by = "tmax_06_bins")
grid.met.summary <- left_join(grid.met.summary, tmax.ordered.cuts, by = "tmax_06_bins")



# get bins of the prism precipitation data
grid.met.summary$wtryr_bins <- cut(grid.met.summary$MAP.wy.met, breaks=seq(400, 1450, by = 50))

all.prism.summary$wtryr_bins <- cut(all.prism.summary$MAP.wy.prism, breaks=seq(400, 1450, by = 50))

precip.ordered.cuts <- data.frame(wtryr_bins = levels(cut(grid.met.summary[order(grid.met.summary$MAP.wy.met),]$MAP.wy.met, breaks=seq(400, 1450, by = 50))),
                                wtr.mids=seq(425, 1425, by = 50))

all.prism.summary <- left_join(all.prism.summary, precip.ordered.cuts, by = "wtryr_bins")
grid.met.summary <- left_join(grid.met.summary, precip.ordered.cuts, by = "wtryr_bins")

tmax.mids.plot <- ggplot()+geom_point(data= all.prism.summary, aes(wtr.mids, tmax06.mids), color = "red", alpha = 0.5)+
  geom_point(data= grid.met.summary, aes(wtr.mids, tmax06.mids), pch = 6,color = "blue", alpha = 0.5)
tmax.mids.plot 

# get the list of sites where climate bins over lap (+/- 50mm MAP and 1 degree mean Tmax in june)
prism.short <- all.prism.summary #%>% dplyr::select(Longitude, Latitude, wtryr_bins, wtr.mids, tmax_06_bins, tmax06.mids)
met.short <- grid.met.summary #%>% dplyr::select(lon, lat, wtryr_bins, wtr.mids, tmax_06_bins, tmax06.mids)

common_envts <- merge(prism.short, met.short, by = c("wtryr_bins","wtr.mids", "tmax_06_bins", "tmax06.mids"))

saveRDS(common_envts, "outputs/data/ITRDB_MET_common_envts.rds")


#---------------------Plot mean Fcomp by PFT for the models with ITRDB by PFT in the models-----------------------------

# read in ED fcomp and map means across space
ED.fcomp <- readRDS("outputs/data/ED2/ED2_mean_yearly_fcomp.rds")
ED.fcomp.m <- melt(ED.fcomp, id.vars = c("Site", "Year"))
ED.fcomp.means <- ED.fcomp.m %>% group_by(variable, Site) %>% summarise(Fcomp = mean(value, na.rm = TRUE))
ED.fcomp.means$Site <- as.character(ED.fcomp.means$Site)
ED.fcomp.means <- left_join(ED.fcomp.means, paleon, by = "Site")

ed.trees <- c("pine.north", "conifer.late", "temp.decid.early", "temp.decid.late", "temp.decid.mid")

ggplot(ED.fcomp.means[ED.fcomp.means$variable %in% ed.trees,], aes(lon, lat, fill = Fcomp))+geom_raster()+facet_wrap(~variable)

# read in ITRDB lat long + pft categorie:
taxa.trans <- read.csv("Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv", stringsAsFactors = FALSE)
rwl.itrdb.pft <- left_join(rwl.itrdb.clim.nona, taxa.trans, by = "SPEC.CODE")
rwl.itrdb.ed.pft <- rwl.itrdb.pft %>% select("Longitude", "Latitude", "SPEC.CODE","ED.PFT")
rwl.itrdb.ed.pft <- unique(rwl.itrdb.ed.pft)
colnames(ED.fcomp.means)[1] <- "ED.PFT"

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
ne_rivers <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')
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


ED.pft.map <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_line(data = river.subset, aes(x = long, y = lat, group = group), color = "blue")+
  geom_raster(data = ED.fcomp.means[ED.fcomp.means$ED.PFT %in% ed.trees,], aes(lon, lat, fill = Fcomp))+
  geom_point(data = rwl.itrdb.ed.pft, aes(Longitude, Latitude), color = "red", pch = 18, size = 4)+facet_wrap(~ED.PFT, ncol = 1)+
  scale_fill_gradientn(colors = c("#edf8b1",
    "#c7e9b4",
    "#7fcdbb",
    "#41b6c4",
    "#1d91c0",
    "#225ea8",
    "#0c2c84"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))+theme_bw(base_size = 14)


png(height = 15, width = 5, units = "in", res = 300, "outputs/itrdb_model_compare/ed2_fcomp_itrdb_sites.png")
ED.pft.map 
dev.off()

# map LPJ-GUESS taxa:

# read in ED fcomp and map means across space
GUESS.fcomp <- readRDS("Data/GUESS.Fcomp.pft.rds")
#GUESS.fcomp.m <- melt(GUESS.fcomp, id.vars = c("Site", "Year"))
GUESS.fcomp.means <- GUESS.fcomp %>% group_by(PFT, Site) %>% dplyr::summarise(Fcomp = mean(Fcomp, na.rm = TRUE))
GUESS.fcomp.means$Site <- as.character(GUESS.fcomp.means$Site)
GUESS.fcomp.means <- left_join(GUESS.fcomp.means, paleon, by = "Site")

GUESS.trees <- c("BNE","BINE","BIBS", "TeBS", "TelBS")

ggplot(GUESS.fcomp.means[GUESS.fcomp.means$PFT %in% GUESS.trees,], aes(lon, lat, fill = Fcomp))+geom_raster()+facet_wrap(~PFT)


# read in ITRDB lat long + pft categories:
taxa.trans <- read.csv("Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv", stringsAsFactors = FALSE)
rwl.itrdb.pft <- left_join(rwl.itrdb.clim.nona, taxa.trans, by = "SPEC.CODE")
rwl.itrdb.GUESS.pft <- rwl.itrdb.pft %>% dplyr::select("Longitude", "Latitude", "SPEC.CODE","LPJ.GUESS.PFT")
rwl.itrdb.GUESS.pft <- unique(rwl.itrdb.GUESS.pft)
colnames(GUESS.fcomp.means)[1] <- "spec"


GUESS.PFT.convert <- data.frame(
  spec.abb = c("TeIB", 
               "TeBS", 
               "TeBE", 
               "BNE", 
               "BINE", 
               "BIBS"),
  
  spec = c("TelBS", 
           "TeBS", 
           "TeBE", 
           "BNE", 
           "BINE", 
           "BIBS"),
  full.spec = c("Temperate broadleaved summergreen (shade intolerant)",
                "Temperate broadleaved summergreen", 
                "Temperate broadleved evergreen", 
                "Boreal needleleaved evergreen", 
                "Boreal needleleaved evergreen (shade intolerant)", 
                "Boreal broadleaved summergreen (shade intolerant)")
  #biome = c("Temperate", "Temperate","Temperate","Boreal"),
  #leaf = c("Broadleaved", "Broadleaved","Broadleaved","Needleleaved"),
  #decidious = c("decidious", "decidious", "evergreen", "evergreen")
)

# merge rwl + names
rwl.itrdb.GUESS.pft <- merge(rwl.itrdb.GUESS.pft, GUESS.PFT.convert, by.x = "LPJ.GUESS.PFT", by.y = "full.spec")

# merge fcomp + names
GUESS.fcomp.means <- merge(GUESS.fcomp.means, GUESS.PFT.convert, by= "spec")

colnames(GUESS.fcomp.means)[14] <- "LPJ.GUESS.PFT"
GUESS.pft.map <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = GUESS.fcomp.means[GUESS.fcomp.means$spec %in% GUESS.trees,], aes(lon, lat, fill = Fcomp))+
  geom_point(data = rwl.itrdb.GUESS.pft, aes(Longitude, Latitude), color = "red", pch = 18, size = 4)+facet_wrap(~LPJ.GUESS.PFT, ncol = 1)+
  scale_fill_gradientn(colors = c("#edf8b1",
                                  "#c7e9b4",
                                  "#7fcdbb",
                                  "#41b6c4",
                                  "#1d91c0",
                                  "#225ea8",
                                  "#0c2c84"), limits= c(0,1)  )+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))+theme_bw(base_size = 14)

GUESS.pft.map

png(height = 16, width = 16, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_ED_ITRDB_fcomp.png")
cowplot::plot_grid(GUESS.pft.map, ED.pft.map, ncol = 2)
dev.off()






#-----------------------------Now compare relative autocorrelation by site and taxa----------------------------

unique(all.met)
#acf(x = all.met[all.met$lon == unique(all.met[,c("lat", "lon")])[1,2] & all.met$lat == unique(all.met[,c("lat", "lon")])[1,1],]$precip_total_wtr_yr.mm,type = "correlation", lag.max = 5)

all.met.sub <- all.met %>% filter(Year >= 1895 )
head(all.met.sub)

all.met.sub.acf <- all.met.sub %>% group_by(lat, lon) %>% arrange(Year) %>% dplyr::summarise(acf.1 = acf(precip_total_wtr_yr.mm, lag.max = 1, plot = FALSE)$acf[2],
                                                                           acf.2 = acf(precip_total_wtr_yr.mm, lag.max = 2, plot = FALSE)$acf[3])



ggplot(all.met.sub.acf, aes(lon, lat, fill = acf.1))+geom_raster()

# for ITRDB sites:

all.prism.sub <- rwl.itrdb.clim.nona %>% filter(year >= 1895 )
head(all.prism.sub)

all.prism.sub.acf <- all.prism.sub %>% group_by(Latitude, Longitude, studyCode) %>%  dplyr::summarise(acf.1 = acf(x = year, y = ppt_MAP.wy, lag.max = 1, plot = FALSE)$acf[2],
                                                                           acf.2 = acf(x = year, y = ppt_MAP.wy, lag.max = 2, plot = FALSE)$acf[3], 
                                                                           acf.5 = acf(x = year, y = ppt_MAP.wy, lag.max = 5, plot = FALSE)$acf[6])





ggplot(all.prism.sub.acf, aes(Longitude, Latitude, color = acf.1))+geom_point()

ggplot(all.prism.sub.acf, aes(acf.1))+geom_histogram()
ggplot(all.met.sub.acf, aes(acf.1))+geom_histogram()


# extract point MIP climate drivers 
# get the coordinates for the paleon grid cells of itrdb
sites.coords <- readRDS(paste0("Data/ITRDB/PRISM/tmax/Tmax_1895_2016_extracted_ITRDB_sites_only.rds"))
test.prism <- merge(all.prism.sub.acf, sites.coords, by = c("studyCode", "Latitude", "Longitude"))
test <- merge(test.prism, all.met.sub, by.x = c("Latitude", "Longitude"), by.y = c("lat", "lon"))


rwl.age.ll <- readRDS( "Data/ITRDB/rwl.ages.df.nona_spatial.rds")
rwl.age.ll.unique <- unique(rwl.age.ll[, c("Longitude", "Latitude", "studyCode", "SPEC.CODE")])

# get the LatLon for the grid cell:
toRaster <- "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/paleon.unit.ll_01.tif"
paleon.ll <- raster("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/paleon.unit.ll_01.tif")
values(paleon.ll) <- 1:ncell(paleon.ll)


all.prism.unique <- unique(all.prism.sub[,c("Longitude", "Latitude")])
lat.lon <- raster::extract(paleon.ll, all.prism.unique[,c("Longitude", "Latitude")], cellnumber = TRUE, df = TRUE, method = "simple")
#y <- data.frame(rasterToPoints(paleon.ll))

xy <- xyFromCell(paleon.ll, cell = lat.lon$cells)

lat.lon$x <- xy[,"x"]
lat.lon$y <- xy[,"y"]
lat.lon$Longitude <- all.prism.unique$Longitude
lat.lon$Latitude <- all.prism.unique$Latitude

all.met_wide.precip <- data.frame(all.met.sub) %>% dplyr::select(lon, lat, Year, precip_total_wtr_yr.mm) %>% group_by(lon, lat) %>% spread(key = Year, value = precip_total_wtr_yr.mm)
all.met_wide.temp <- data.frame(all.met.sub) %>% dplyr::select(lon, lat, Year, tair_max_6) %>% group_by(lon, lat) %>% spread(key = Year, value = tair_max_6)

#all.met.unique <- unique(all.met.sub[all.met.sub$Year ==1985,c("lon", "lat","precip_total_wtr_yr.mm")])

coordinates(all.met_wide.precip)  <- ~ lon + lat
gridded(all.met_wide.precip) <- TRUE
proj4string(all.met_wide.precip) <- "+proj=longlat +ellps=WGS84 +no_defs"
all.met.rast <- stack(all.met_wide.precip)
met.rast <- projectRaster(all.met.rast , to = paleon.ll)
met.rast.extracted <- data.frame(raster::extract(all.met.rast,lat.lon[, c("x", "y")]))
met.rast.extracted$x <- lat.lon$Longitude
met.rast.extracted$y <- lat.lon$Latitude
ggplot(met.rast.extracted, aes(x,y, fill = X2010))+geom_point()
met.rast.extracted.m <- melt(met.rast.extracted, id.vars = c("x", "y"))


coordinates(all.met_wide.temp)  <- ~ lon + lat
gridded(all.met_wide.temp) <- TRUE
proj4string(all.met_wide.temp) <- "+proj=longlat +ellps=WGS84 +no_defs"
all.met.rast <- stack(all.met_wide.temp)
met.rast <- projectRaster(all.met.rast , to = paleon.ll)
met.rast.extracted.t <- data.frame(raster::extract(all.met.rast,lat.lon[, c("x", "y")]))
met.rast.extracted.t$x <- lat.lon$Longitude
met.rast.extracted.t$y <- lat.lon$Latitude
ggplot(met.rast.extracted.t, aes(x,y, color = X2010))+geom_point()
met.rast.extracted.temp.m <- melt(met.rast.extracted.t, id.vars = c("x", "y"))
colnames(met.rast.extracted.temp.m) <- c("x", "y", "Year", "Tmax_6_met")
colnames(met.rast.extracted.m) <- c("x", "y", "Year", "Total_WY_precip_met")


merged.met.xy <- merge(met.rast.extracted.m, met.rast.extracted.temp.m, by = c("x", "y", "Year"))

all.prism.sub$Year <- paste0("X",all.prism.sub$year)

# finally merge together!
merged.met.itrdb.xy <- merge(merged.met.xy, unique(all.prism.sub[,c("Longitude", "Latitude", "Year", "year", "ppt_MAP.wy", "tmax_06")]), by.x = c("x", "y", "Year"),  by.y = c("Longitude", "Latitude", "Year"))
merged.met.itrdb.xy.nona <- merged.met.itrdb.xy[merged.met.itrdb.xy$Total_WY_precip_met > 0,]
met.prism.map <- ggplot(merged.met.itrdb.xy.nona, aes(ppt_MAP.wy, Total_WY_precip_met))+geom_point(size = 0.5)+geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+ylab("Met Driver Total Annual Precip")+xlab("Prism Total Annual Precip")

met.tmax.map<- ggplot(merged.met.itrdb.xy.nona, aes(tmax_06, Tmax_6_met-273.15))+geom_point(size = 0.5)+geom_abline(aes(intercept = 0, slope = 1), color = "red", linetype = "dashed")+ylab("Met Driver June Tmax")+xlab("Prism June Tmax")

png(height = 4, width = 8, units = "in", res = 300, "outputs/itrdb_model_compare/MET_prism_driver.plots.png")
plot_grid(met.prism.map, met.tmax.map)
dev.off()

#-------- compare  correlations of ITRDB data with MET data and with prism data:-------------
# need to read in detrended ITRDB data:



#------------Read in pearson correlation coefficients and compare correlations across common envts------------------

# for ED2
ED2.cors.df <- readRDS( "outputs/gwbi_model/ED2_gwbi_correlation_coefs_by_pft.rds")
load("Data/PalEON_siteInfo_all.RData")
paleon$Site <- as.character(paleon$num)
ED2.cors.site <- left_join(paleon[,c("lon", "lat", "Site")], ED2.cors.df, by = "Site")

ED2.cors.site.subset <- merge(common_envts, ED2.cors.site, by = c("lon", "lat"))

ggplot(ED2.cors.site.subset[ED2.cors.site.subset$month %in% "tair_max_6",], aes(tair_max_6.met, coef, color = PFT))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)

ggplot(ED2.cors.site.subset[ED2.cors.site.subset$month %in% "precip_total_wtr_yr.mm",], aes(tair_max_6.met, coef, color = PFT))+
  ylab("Correlation with Total Precipitation")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)

# for GUESS:

GUESS.cors.df <- readRDS( "outputs/gwbi_model/GUESS_gwbi_correlation_coefs_by_pft.rds")
GUESS.cors.site <- left_join(paleon[,c("lon", "lat", "Site")], GUESS.cors.df, by = "Site")
GUESS.cors.site.subset <- merge(common_envts, GUESS.cors.site, by = c("lon", "lat"))

ggplot(GUESS.cors.site.subset[GUESS.cors.site.subset$month %in% "tair_max_6",], aes(tair_max_6.met, coef, color = PFT))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)

ggplot(GUESS.cors.site.subset[GUESS.cors.site.subset$month %in% "tair_max_6",], aes(MAP.wy.met, coef, color = PFT))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)


ggplot(GUESS.cors.site.subset[GUESS.cors.site.subset$month %in% "precip_total_wtr_yr.mm",], aes(tair_max_6.met, coef, color = PFT))+
  ylab("Correlation with Total Precipitation")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)

ggplot(GUESS.cors.site.subset[GUESS.cors.site.subset$month %in% "precip_total_wtr_yr.mm",], aes(MAP.wy.met, coef, color = PFT))+
  ylab("Correlation with Total Precipitation")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)


# for ITRDB:
ITRDB.cors.df <- readRDS( "Data/ITRDB/rwl.itrdb.clim.correlations.rds" )

ITRDB.cors.df.slim <- ITRDB.cors.df %>% dplyr::select(Longitude:PALEON,tmax_01:ppt_total.wy) %>% group_by(Longitude, Latitude, SPEC.CODE, studyCode, PALEON) %>% gather(key = month,value = coef, tmax_01:ppt_total.wy)
ITRDB.cors.site.subset <- merge(common_envts, ITRDB.cors.df.slim, by = c("Longitude", "Latitude"))


# plotting ITRDB as PALEON taxa
ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "tmax_06",], aes(tair_max_6.prism, coef, color = PALEON))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PALEON)

ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "tmax_06",], aes(MAP.wy.met, coef, color = PALEON))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PALEON)


ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "ppt_total.wy",], aes(tair_max_6.met, coef, color = PALEON))+
  ylab("Correlation with Total Precipitation")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PALEON)

ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "ppt_total.wy",], aes(MAP.wy.met, coef, color = PALEON))+
  ylab("Correlation with Total Precipitation")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PALEON)




# now get the conversion of ITRDB to different PFTS:
species.num.trans <- read.csv( file = "Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv")
ITRDB.cors.site.subset.pft <- left_join(species.num.trans, ITRDB.cors.site.subset[!names(ITRDB.cors.site.subset) %in% "PALEON"], by = "SPEC.CODE")


# plotting ITRDB as ED2 pfts
ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(tair_max_6.prism, coef, color = ED.PFT))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)

ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(MAP.wy.met, coef, color = ED.PFT))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)


ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(tair_max_6.met, coef, color = ED.PFT))+
  ylab("Correlation with Total Precipitation")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)

ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(MAP.wy.met, coef, color = ED.PFT))+
  ylab("Correlation with Total Precipitation")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)


# plotting ITRDB as LPJ-GUESS pfts
ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(tair_max_6.prism, coef, color = LPJ.GUESS.PFT))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT)

ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(MAP.wy.met, coef, color = LPJ.GUESS.PFT))+geom_point(size = 0.5)+
  ylab("Correlation with June Tmax")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT)


ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(tair_max_6.met, coef, color = LPJ.GUESS.PFT))+
  ylab("Correlation with Total Precipitation")+xlab("Mean June Tmax")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT)

ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(MAP.wy.met, coef, color = LPJ.GUESS.PFT))+
  ylab("Correlation with Total Precipitation")+xlab("Mean Annual Precipitation")+stat_smooth(method = "lm")+geom_point()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT)


# now directly compare ITRDB and ED2 on the same plots
ED2.cors.compare <- ED2.cors.site.subset
ED2.cors.compare$ED.PFT <- ED2.cors.compare$PFT

ED.tmax.cor.bytmax <- ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(tair_max_6.prism, coef), color = "#d95f02")+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point(aes(color = "ITRDB"))+
  geom_point(data = ED2.cors.compare[ED2.cors.compare$month %in% "tair_max_6",], aes(tair_max_6.met, coef, color = "ED2"))+stat_smooth(data = ED2.cors.compare[ED2.cors.compare$month %in% "tair_max_6",], aes(tair_max_6.met, coef,color = "ED2"),  method = "lm")+
  ylab("Correlation with June Tmax")+xlab("Mean June Tmax")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)+ scale_colour_manual(name=" ", values=c(ED2="#7570b3", ITRDB="#d95f02"))

ED.tmax.cor.bymap <- ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(MAP.wy.met, coef, color = "ITRDB"))+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point(aes(color = "ITRDB"))+
  geom_point(data = ED2.cors.compare[ED2.cors.compare$month %in% "tair_max_6",], aes(MAP.wy.met, coef, color = "ED2"))+stat_smooth(data = ED2.cors.compare[ED2.cors.compare$month %in% "tair_max_6",], aes(MAP.wy.met, coef, color = "ED2"), method = "lm")+
  ylab("Correlation with June Tmax")+xlab("Mean Annual Precipitation")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)+ scale_colour_manual(name=" ", values=c(ED2="#7570b3", ITRDB="#d95f02"))

ED.map.cor.bytmax <- ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(tair_max_6.prism, coef, color = "ITRDB"))+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point(aes(color = "ITRDB"))+
  geom_point(data = ED2.cors.compare[ED2.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(tair_max_6.met, coef, color = "ED2"))+stat_smooth(data = ED2.cors.compare[ED2.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(tair_max_6.met, coef, color = "ED2"), method = "lm")+
  ylab("Correlation with Total Precipitation")+xlab("Mean June Tmax")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)+ scale_colour_manual(name=" ", values=c(ED2="#7570b3", ITRDB="#d95f02"))

ED.map.cor.bymap <- ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(MAP.wy.met, coef, color = "ITRDB"))+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point(aes(color = "ITRDB"))+
  geom_point(data = ED2.cors.compare[ED2.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(MAP.wy.met, coef, color = "ED2"))+stat_smooth(data = ED2.cors.compare[ED2.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(MAP.wy.met, coef, color = "ED2"), method = "lm")+
  ylab("Correlation with Total Precipitation")+xlab("Mean Annual Precipitation")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT)+ scale_colour_manual(name=" ", values=c(ED2="#7570b3", ITRDB="#d95f02"))

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_tmax_cor_by_tmax.png")
ED.tmax.cor.bytmax+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_tmax_cor_by_map.png")
ED.tmax.cor.bymap+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_map_cor_by_tmax.png")
ED.map.cor.bytmax+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_map_cor_by_map.png")
ED.map.cor.bymap+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()


# now directly compare ITRDB and LPJ-GUESS on the same plots
GUESS.cors.compare <- GUESS.cors.site.subset
GUESS.cors.compare$gwbi.pft <- GUESS.cors.compare$PFT
guess.trans <- read.csv( file = "Data/GUESS.pft.abbrev.csv")
ITRDB.cors.site.subset.pft <- left_join(guess.trans, ITRDB.cors.site.subset.pft , by = "LPJ.GUESS.PFT")
GUESS.cors.compare <- left_join(guess.trans, GUESS.cors.compare , by = "gwbi.pft")


wrapit <- function(text) {
  wtext <- paste(strwrap(text,width=40),collapse=" \n ")
  return(wtext)
}

GUESS.cors.compare $wrapped_text <- plyr::llply(GUESS.cors.compare$LPJ.GUESS.PFT, wrapit)
GUESS.cors.compare $wrapped_text <- unlist(GUESS.cors.compare$wrapped_text)

ITRDB.cors.site.subset.pft$wrapped_text <- plyr::llply(ITRDB.cors.site.subset.pft$LPJ.GUESS.PFT, wrapit)
ITRDB.cors.site.subset.pft$wrapped_text <- unlist(ITRDB.cors.site.subset.pft$wrapped_text)

# get rid of TrIBE because it has mostly zero values for growth in LPJ-GUESS
GUESS.cors.compare <- GUESS.cors.compare[!GUESS.cors.compare$gwbi.pft %in% "TrIBE.gwbi", ]  

GUESS.tmax.cor.bytmax <- ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(tair_max_6.prism, coef))+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point( aes(color = "ITRDB"))+
  geom_point(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "tair_max_6" ,], aes(tair_max_6.met, coef, color = "LPJ.GUESS"))+stat_smooth(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "tair_max_6",], aes(tair_max_6.met, coef, color = "LPJ.GUESS"), method = "lm")+
  ylab("Correlation with June Tmax")+xlab("Mean June Tmax")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT, labeller = label_wrap_gen(width=35), ncol = 3)+theme_bw(base_size = 10)+theme(panel.grid = element_blank())+ 
  scale_colour_manual(name=" ", values=c(LPJ.GUESS="#1b9e77", ITRDB="#d95f02"))

GUESS.tmax.cor.bymap <- ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "tmax_06",], aes(MAP.wy.met, coef, color = "ITRDB"))+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point( aes(color = "ITRDB"))+
  geom_point(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "tair_max_6",], aes(MAP.wy.met, coef, color = "LPJ.GUESS"))+stat_smooth(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "tair_max_6",], aes(MAP.wy.met, coef, color = "LPJ.GUESS"), method = "lm")+
  ylab("Correlation with June Tmax")+xlab("Mean Annual Precipitation")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT, labeller = label_wrap_gen(width=35), ncol = 3)+theme_bw(base_size = 10)+theme(panel.grid = element_blank())+
  scale_colour_manual(name=" ", values=c(LPJ.GUESS="#1b9e77", ITRDB="#d95f02"))

GUESS.map.cor.bytmax <-ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(tair_max_6.prism, coef, color = "ITRDB"))+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point( aes(color = "ITRDB"))+
  geom_point(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(tair_max_6.met, coef, color = "LPJ.GUESS"))+stat_smooth(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(tair_max_6.met, coef, color = "LPJ.GUESS"), method = "lm")+
  ylab("Correlation with Total Precipitation")+xlab("Mean June Tmax")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT, labeller = label_wrap_gen(width=35), ncol = 3)+theme_bw(base_size = 10)+theme(panel.grid = element_blank())+ 
  scale_colour_manual(name=" ", values=c(LPJ.GUESS="#1b9e77", ITRDB="#d95f02"))

GUESS.map.cor.bymap <- ggplot(ITRDB.cors.site.subset.pft[ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy",], aes(MAP.wy.met, coef, color = "ITRDB"))+stat_smooth(method = "lm", aes(color = "ITRDB"))+geom_point( aes(color = "ITRDB"))+
  geom_point(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(MAP.wy.met, coef, color = "LPJ.GUESS"))+stat_smooth(data = GUESS.cors.compare[GUESS.cors.compare$month %in% "precip_total_wtr_yr.mm",], aes(MAP.wy.met, coef, color = "LPJ.GUESS"), method = "lm")+
  ylab("Correlation with Total Precipitation")+xlab("Mean Annual Precipitation")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT, labeller = label_wrap_gen(width=35), ncol = 3)+theme_bw(base_size = 10)+theme(panel.grid = element_blank())+ 
  scale_colour_manual(name=" ", values=c(LPJ.GUESS="#1b9e77", ITRDB="#d95f02"))


png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_tmax_cor_by_tmax.png")
GUESS.tmax.cor.bytmax+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_tmax_cor_by_map.png")
GUESS.tmax.cor.bymap+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_map_cor_by_tmax.png")
GUESS.map.cor.bytmax+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_map_cor_by_map.png")
GUESS.map.cor.bymap+theme(axis.text.x=element_text(hjust = 1, angle = 45))
dev.off()

#---------------Directly compare the boxplots of monthly correlation coefficients-----------
library(data.table)
col.tmax <- unique(ITRDB.cors.site.subset.pft$month)[unique(ITRDB.cors.site.subset.pft$month) %like% "tmax"]
col.tmean <- unique(ITRDB.cors.site.subset.pft$month)[unique(ITRDB.cors.site.subset.pft$month) %like% "tmean"]
col.tmin <- unique(ITRDB.cors.site.subset.pft$month)[unique(ITRDB.cors.site.subset.pft$month) %like%  "tmin"]
col.precip <- unique(ITRDB.cors.site.subset.pft$month)[unique(ITRDB.cors.site.subset.pft$month) %like%  "ppt"]
col.precip.tot <- unique(ITRDB.cors.site.subset.pft$month)[unique(ITRDB.cors.site.subset.pft$month) %like%  "ppt_total"]


ITRDB.cors.site.subset.pft$month2 <- ifelse(ITRDB.cors.site.subset.pft$month %in% col.tmax, paste0("tair_max_",as.numeric(substring( ITRDB.cors.site.subset.pft$month, 6))), 
                                            ifelse(ITRDB.cors.site.subset.pft$month %in% col.precip & !ITRDB.cors.site.subset.pft$month %in% col.precip.tot, paste0("precip_", as.numeric(substring( ITRDB.cors.site.subset.pft$month, 5))),
                                                  ifelse(ITRDB.cors.site.subset.pft$month %in% "ppt_total.wy", "precip_total_wtr_yr.mm",
                                                         ifelse(ITRDB.cors.site.subset.pft$month %in% "ppt_total", "precip_total.mm", NA)) ))


# get dataframes with ITRDB, and GUESS, and ITRDB & ED2:
# for LPJ GUESS
ITRDB.cors.lpj.pft <- ITRDB.cors.site.subset.pft %>% dplyr::select(Longitude, Latitude, LPJ.GUESS.PFT, month2, coef)
colnames(ITRDB.cors.lpj.pft) <- c("lon", "lat", "LPJ.GUESS.PFT", "month", "coef")
ITRDB.cors.lpj.pft$type <- "ITRDB"
GUESS.cors.lpj.pft <- GUESS.cors.compare %>% dplyr::select(lon, lat, LPJ.GUESS.PFT, month, coef)
GUESS.cors.lpj.pft$type <- "LPJ.GUESS"
ITRDB.GUESS <- rbind(ITRDB.cors.lpj.pft, GUESS.cors.lpj.pft)



col.tmax <- unique(ITRDB.GUESS$month)[unique(ITRDB.GUESS$month) %like% "tair_max"]
col.tmean <- unique(ITRDB.GUESS$month)[unique(ITRDB.GUESS$month) %like% "tair_mean"]
col.tmin <- unique(ITRDB.GUESS$month)[unique(ITRDB.GUESS$month) %like%  "tair_min"]
col.precip <- unique(ITRDB.GUESS$month)[unique(ITRDB.GUESS$month) %like%  "precip"]

# reorder factors:
col.tmax <- col.tmax[ c(4, 2,7,  3,  9, 10,  5, 12, 11,8,  6,  1)] # reorder
ITRDB.GUESS.tmax <- ITRDB.GUESS[ITRDB.GUESS$month %in% col.tmax,]
ITRDB.GUESS.tmax$month <- factor(ITRDB.GUESS.tmax$month,levels = col.tmax)

col.precip <- col.precip[ c(5, 8,4, 11, 13,  3, 10,  7,  6,   12,  2, 14, 1,  9)] # reorder
ITRDB.GUESS.precip <- ITRDB.GUESS[ITRDB.GUESS$month %in% col.precip,]
ITRDB.GUESS.precip$month <- factor(ITRDB.GUESS.precip$month,levels = col.precip)



tmax.itrdb.guess <- ggplot(ITRDB.GUESS.tmax, aes(month, coef, fill = type))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+scale_fill_manual(name=" ", values=c("LPJ.GUESS"="#1b9e77", "ITRDB"="#d95f02"))+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT, ncol = 4, labeller = label_wrap_gen(width=35))+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 0.5), panel.grid = element_blank())


precip.itrdb.guess <- ggplot(ITRDB.GUESS.precip, aes(month, coef, fill = type))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+scale_fill_manual(name=" ", values=c("LPJ.GUESS"="#1b9e77", "ITRDB"="#d95f02"))+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~LPJ.GUESS.PFT, ncol = 4,labeller = label_wrap_gen(width=35))+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), panel.grid = element_blank())


png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_tmax_cors.png")
tmax.itrdb.guess
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_precip_cors.png")
precip.itrdb.guess
dev.off()

# for ED2:
ITRDB.cors.ed.pft <- ITRDB.cors.site.subset.pft %>% dplyr::select(Longitude, Latitude, ED.PFT, month2, coef)
colnames(ITRDB.cors.ed.pft) <- c("lon", "lat", "ED.PFT", "month", "coef")
ITRDB.cors.ed.pft$type <- "ITRDB"
ED.cors.ED.pft <- ED2.cors.compare %>% dplyr::select(lon, lat, ED.PFT, month, coef)
ED.cors.ED.pft$type <- "ED2"
ITRDB.ED <- rbind(ITRDB.cors.ed.pft, ED.cors.ED.pft)



col.tmax <- unique(ITRDB.ED$month)[unique(ITRDB.ED$month) %like% "tair_max"]
col.tmean <- unique(ITRDB.ED$month)[unique(ITRDB.ED$month) %like% "tair_mean"]
col.tmin <- unique(ITRDB.ED$month)[unique(ITRDB.ED$month) %like%  "tair_min"]
col.precip <- unique(ITRDB.ED$month)[unique(ITRDB.ED$month) %like%  "precip"]

# reorder factors:
col.tmax <- col.tmax[ c(4, 2,7,  3,  9, 10,  5, 12, 11,8,  6,  1)] # reorder
ITRDB.ED.tmax <- ITRDB.ED[ITRDB.ED$month %in% col.tmax,]
ITRDB.ED.tmax$month <- factor(ITRDB.ED.tmax$month,levels = col.tmax)

col.precip <- col.precip[ c(5, 8,4, 11, 13,  3, 10,  7,  6,   12,  2, 14, 1,  9)] # reorder
ITRDB.ED.precip <- ITRDB.ED[ITRDB.ED$month %in% col.precip,]
ITRDB.ED.precip$month <- factor(ITRDB.ED.precip$month,levels = col.precip)



tmax.itrdb.ED <- ggplot(ITRDB.ED.tmax, aes(month, coef, fill = type))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+scale_fill_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT, ncol = 4, labeller = label_wrap_gen(width=35))+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 0.5), panel.grid = element_blank())


precip.itrdb.ED <- ggplot(ITRDB.ED.precip, aes(month, coef, fill = type))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+scale_fill_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~ED.PFT, ncol = 4,labeller = label_wrap_gen(width=35))+theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), panel.grid = element_blank())



png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_tmax_cors.png")
tmax.itrdb.ED
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_precip_cors.png")
precip.itrdb.ED
dev.off()


# next plot the same correlation coefficiencts, but by ageclass:

#----------Lets plot the basic model parameters for ITRDB, ED, and GUESS together----------
ITRDB.params <-read.csv("outputs/ITRDB_models/ITRDB_species_no_re/all_parameter_mean_CI.csv")
ED.params <-read.csv("outputs/gwbi_model/ED2_no_re/all_parameter_mean_CI.csv")
GUESS.params <-read.csv("outputs/gwbi_model/LPJ_GUESS_no_re/all_parameter_mean_CI.csv")
ED.params$PFT <- ED.params$species
GUESS.params$PFT <- GUESS.params$species


species.num.trans <- read.csv( file = "Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv")
ITRDB.params.pft <- merge(species.num.trans, ITRDB.params, by.x = "SPEC.CODE", by.y = "species")

ITRDB.pft.ED <- ITRDB.params.pft %>% select(SPEC.CODE, ED.PFT, variable, mean, Ci.low, Ci.high)
colnames(ITRDB.pft.ED) <- c("species", "PFT", "parameter", "mean", "Ci.low", "Ci.high")
ITRDB.pft.ED$type <- "ITRDB"

ED.params <- ED.params %>% select(species, PFT, variable, mean, Ci.low, Ci.high)
colnames(ED.params) <- c("species", "PFT", "parameter", "mean", "Ci.low", "Ci.high")
ED.params$type <- "ED2"

ED.ITRDB.summary <- rbind(ITRDB.pft.ED, ED.params)



INTERCEPTS <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "alpha" & !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = type), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Species intercept Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

MAPS <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta1" & !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = type), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Precipitation Sensitivity \n (Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)


JUNTMAX <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta2" & !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Jun Tmax Sensitivity \n (Beta2) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

PREVRWI_1 <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta3"& !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta3) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

PREVRWI_2 <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta4"& !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean,color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta4) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

AGE <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta5"& !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Age (Beta5) \n Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)


png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_alpha_no_re.png")
INTERCEPTS
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_MAP_no_re.png")
MAPS
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_JUNTMAX_no_re.png")
JUNTMAX
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_prevRWI1_no_re.png")
PREVRWI_1
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_prevRWI2_no_re.png")
PREVRWI_2
dev.off()


# do the same for GUESS:

ITRDB.pft.GUESS <- ITRDB.params.pft %>% select(SPEC.CODE, LPJ.GUESS.PFT, variable, mean, Ci.low, Ci.high)
colnames(ITRDB.pft.GUESS) <- c("species", "PFT", "parameter", "mean", "Ci.low", "Ci.high")
ITRDB.pft.GUESS$type <- "ITRDB"

GUESS.params.full <- merge(GUESS.params, guess.trans, by.x = "species", by.y ="gwbi.pft" )
GUESS.params.df <- GUESS.params.full %>% select(LPJ.short, LPJ.GUESS.PFT, variable, mean, Ci.low, Ci.high)
colnames(GUESS.params.df) <- c("species", "PFT", "parameter", "mean", "Ci.low", "Ci.high")
GUESS.params.df$type <- "LPJ.GUESS"

GUESS.ITRDB.summary <- rbind(ITRDB.pft.GUESS, GUESS.params.df)



INTERCEPTS <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "alpha"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = type), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Species intercept Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

MAPS <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta1" & !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = type), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Precipitation Sensitivity \n (Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))


JUNTMAX <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta2" & !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Jun Tmax Sensitivity \n (Beta2) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

PREVRWI_1 <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta3"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta3) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

PREVRWI_2 <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta4"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean,color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta4) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

AGE <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta5"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = type))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Age (Beta5) \n Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))




png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_alpha_no_re.png")
INTERCEPTS
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_MAP_no_re.png")
MAPS
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_JUNTMAX_no_re.png")
JUNTMAX
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_prevRWI1_no_re.png")
PREVRWI_1
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_prevRWI2_no_re.png")
PREVRWI_2
dev.off()


#----------Lets plot the time random effects parameters for ITRDB, ED, and GUESS together----------
ITRDB.params <-read.csv("outputs/ITRDB_models/ITRDB_species_time_re/all_parameter_mean_CI.csv")
ED.params <-read.csv("outputs/gwbi_model/ED2_time_re/all_parameter_mean_CI.csv")
GUESS.params <-read.csv("outputs/gwbi_model/LPJ_GUESS_time_re/all_parameter_mean_CI.csv")
ED.params$PFT <- ED.params$species
GUESS.params$PFT <- GUESS.params$species


species.num.trans <- read.csv( file = "Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv")
ITRDB.params.pft <- merge(species.num.trans, ITRDB.params, by.x = "SPEC.CODE", by.y = "species")

ITRDB.pft.ED <- ITRDB.params.pft %>% select(SPEC.CODE, ED.PFT, timeclass,variable, mean, Ci.low, Ci.high)
colnames(ITRDB.pft.ED) <- c("species", "PFT", "period","parameter", "mean", "Ci.low", "Ci.high")
ITRDB.pft.ED$type <- "ITRDB"

ED.params <- ED.params %>% select(species, PFT, timeclass, variable, mean, Ci.low, Ci.high)
colnames(ED.params) <- c("species", "PFT", "period","parameter", "mean", "Ci.low", "Ci.high")
ED.params$type <- "ED2"

ED.ITRDB.summary <- rbind(ITRDB.pft.ED, ED.params)
ED.ITRDB.summary$parameter2 <- ED.ITRDB.summary$parameter  
ED.ITRDB.summary$parameter <- substring(as.character(ED.ITRDB.summary$parameter2), 1,5)


INTERCEPTS <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "alpha" & !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color = period, fill = period))+geom_point(position = "dodge")+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = period), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed", position = "dodge")+#scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Species intercept Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

MAPS <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta1" & !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color =  period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = period), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Precipitation Sensitivity \n (Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)


JUNTMAX <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta2" & !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color= period), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Jun Tmax Sensitivity \n (Beta2) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

PREVRWI_1 <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta3"& !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color  = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta3) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

PREVRWI_2 <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta4"& !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean,color  = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta4) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)

# AGE <- ggplot(ED.ITRDB.summary[ED.ITRDB.summary$parameter %in% "beta5"& !ED.ITRDB.summary$PFT %in% "mean.gwbi" ,], aes(species, mean, color  = period))+geom_point()+
#   geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("ED2"="#7570b3", "ITRDB"="#d95f02"))+
#   ylab("Age (Beta5) \n Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 5)


png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_alpha_time_re.png")
INTERCEPTS
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_MAP_time_re.png")
MAPS
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_JUNTMAX_time_re.png")
JUNTMAX
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_prevRWI1_time_re.png")
PREVRWI_1
dev.off()

png(height = 4, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/ED_itrdb_prevRWI2_time_re.png")
PREVRWI_2
dev.off()


# do the same for GUESS:

ITRDB.pft.GUESS <- ITRDB.params.pft %>% select(SPEC.CODE, LPJ.GUESS.PFT, timeclass,variable, mean, Ci.low, Ci.high)
colnames(ITRDB.pft.GUESS) <- c("species", "PFT", "period","parameter", "mean", "Ci.low", "Ci.high")
ITRDB.pft.GUESS$type <- "ITRDB"

GUESS.params.full <- merge(GUESS.params, guess.trans, by.x = "species", by.y ="gwbi.pft" )
GUESS.params.df <- GUESS.params.full %>% select(LPJ.short, LPJ.GUESS.PFT, timeclass,variable, mean, Ci.low, Ci.high)
colnames(GUESS.params.df) <- c("species", "PFT", "period", "parameter", "mean", "Ci.low", "Ci.high")
GUESS.params.df$type <- "LPJ.GUESS"

GUESS.ITRDB.summary <- rbind(ITRDB.pft.GUESS, GUESS.params.df)
GUESS.ITRDB.summary$parameter2 <- GUESS.ITRDB.summary$parameter  
GUESS.ITRDB.summary$parameter <- substring(as.character(GUESS.ITRDB.summary$parameter2), 1,5)

a <- ifelse(unique(GUESS.ITRDB.summary$species) %in% c("BIBS", "BINE", "TeBE", "TeBS", "Total"), "red", "blue")

df.facet <- unique(GUESS.ITRDB.summary[,c("PFT", "species")])
df.facet <- df.facet[order(df.facet$PFT),]
a <- ifelse(df.facet$species %in% c("BIBS", "BINE", "TeBE", "TeBS", "Total"), "red", "blue")


INTERCEPTS <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "alpha"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = period), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Species intercept Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

MAPS <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta1" & !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = period), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Precipitation Sensitivity \n (Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+#theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = a))+
facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))#+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = a))


JUNTMAX <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta2" & !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color  = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Jun Tmax Sensitivity \n (Beta2) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

PREVRWI_1 <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta3"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = type, fill = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta3) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

PREVRWI_2 <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta4"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean,color = type, fill = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("PrevRWI_1 \n (Beta4) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))

AGE <- ggplot(GUESS.ITRDB.summary[GUESS.ITRDB.summary$parameter %in% "beta5"& !GUESS.ITRDB.summary$PFT %in% "Total" ,], aes(species, mean, color = type, fill = period))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+#scale_colour_manual(name=" ", values=c("LPJ.GUESS"="#7570b3", "ITRDB"="#d95f02"))+
  ylab("Age (Beta5) \n Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~PFT, scales = "free_x", ncol = 3, labeller = label_wrap_gen(width=35))




png(height = 8, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_alpha_time_re.png")
INTERCEPTS
dev.off()

png(height = 8, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_MAP_time_re.png")
MAPS
dev.off()

png(height = 8, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_JUNTMAX_time_re.png")
JUNTMAX
dev.off()

png(height = 8, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_prevRWI1_time_re.png")
PREVRWI_1
dev.off()

png(height = 8, width = 12, units = "in", res = 300, "outputs/itrdb_model_compare/GUESS_itrdb_prevRWI2_time_re.png")
PREVRWI_2
dev.off()



# compare autocorrelation in the itmeseries of MIP MET and PRISM

