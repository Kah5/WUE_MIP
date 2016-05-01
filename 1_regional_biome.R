library(ncdf4)
library(car)
library(raster)
library(reshape)
library(ggplot2)
library(sp)
library(rgdal)
library(fields)
library(maptools)
require(grid)
require(plyr)
require(maps)
library(gridExtra)
require(maptools)
require(ggplot2)
setwd("C:/Users/Kelly/Documents/MIP")

# ------------------------------------------------
# Setting up to look at initial biome 
# ------------------------------------------------
biome.dir <- "C:/Users/Kelly/Documents/MIP/WUE_MIP/phase_2/env_paleon/biome/"
inputfile <-"C:/Users/Kelly/Documents/MIP/WUE_MIP/phase_2/env_paleon/biome/biome_potential_vegtype_biome.nc"

#i love the raster package...you can just make it a raster
biome <- raster(inputfile)




mip_lon = c(-72.18,-68.73,-89.53,-94.58,-95.17,-82.83)
mip_lat = c(42.54,45.25,46.22,46.28,47.17,43.61)
mip_names = c("PHA","PHO","PUN","PBL","PDL","PMB")
mip_longnames = c("Harvard Forest", "Howland Forest", "UNDERC",
                   "Billy's Lake", "Demming Lake", "Minden Bog")

nat.earth <- readOGR(dsn='C:/Users/Kelly/Documents/BIOMASS/biomass_UW/IL_IN_covariates/data/ne_50m_admin_1_states_provinces_lakes/ne_50m_admin_1_states_provinces_lakes.shp', layer='ne_50m_admin_1_states_provinces_lakes')
#nat.earth.alb <- spTransform(x = nat.earth, CRSobj = CRS('+init=epsg:3175'))
inil<- c("Arkansas","Connecticut","Iowa", "Maine", "Massachusetts", "Missouri",
         "Nebraska","Delaware","Maryland", "New Hampshire", "New York", "North Dakota", "Ohio",
         "Pennsylvania", "Rhode Island", "South Dakota", "Vermont",
         "Kansas", "Kentucky", "West Virginia","Tennessee",
         "Alabama","North Carolina", "South Carolina","District of Columbia", "Virginia", "New Jersey","Minnesota", "Wisconsin", "Michigan", "Illinois", "Indiana")
#inil <- c("Illinois", "Indiana")
mip.domain= nat.earth[match(toupper(inil), toupper(nat.earth$name)),]



X11(width = 12)
pdf("regional_biome_map.pdf")
#my_col = rev(terrain.colors(n = 4))
my_col = c('forestgreen','darkolivegreen1','darkgreen','sienna1', "goldenrod")
 
plot(biome, legend = FALSE, col = my_col)
legend(x='bottomright', legend = c("Hardwood Forest",
                                     "Mixed Forest",
                                     "Conifer Forest", 
                                     "Savanna & Shrublands",
                                     "Grasslands"), fill = my_col)
plot(mip.domain,border = "grey",add = TRUE)
points(mip_lon, mip_lat, pch=19, cex=1)
text(mip_lon, mip_lat + .5,labels = mip_longnames)
title(main = "PalEON MIP Sites")

dev.off()


