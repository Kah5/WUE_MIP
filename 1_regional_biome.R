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


#biome.dir <- "C:/Users/Kelly/Documents/MIP/WUE_MIP/phase_2/env_paleon/biome/"
inputfile <-"C:/Users/Kelly/Documents/MIP/WUE_MIP/phase_2/env_paleon/biome/biome_potential_vegtype_pft_fraction.nc"
biome <- raster(inputfile)
b<- nc_open(inputfile)
pft<- ncvar_get(b, "pft")
pft_frac <- ncvar_get(b, "pft_frac")

#need to transpose the matrix to have it portrayed correctly

test1 <- raster(t(pft_frac[1:80,1:30,1]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test2 <- raster(t(pft_frac[1:80,1:30,2]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test3 <- raster(t(pft_frac[1:80,1:30,3]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test4 <- raster(t(pft_frac[1:80,1:30,4]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test5 <- raster(t(pft_frac[1:80,1:30,5]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test6 <- raster(t(pft_frac[1:80,1:30,6]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test7 <- raster(t(pft_frac[1:80,1:30,7]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test8 <- raster(t(pft_frac[1:80,1:30,8]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test9 <- raster(t(pft_frac[1:80,1:30,9]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)
test10 <- raster(t(pft_frac[1:80,1:30,10]), xmn= -100, xmx = -60, ymn= 35, ymx = 50)



# Convert to data.frame
r_df = as.data.frame(as(test1, 'SpatialPixelsDataFrame'))
print(str(r_df))
#now add columns for the other rasters
r_df$TBED = as.data.frame(as(test2, "SpatialPixelsDataFrame"))$layer
r_df$TNEV = as.data.frame(as(test3, "SpatialPixelsDataFrame"))$layer
r_df$TNED = as.data.frame(as(test4, "SpatialPixelsDataFrame"))$layer
r_df$SBEV = as.data.frame(as(test5, "SpatialPixelsDataFrame"))$layer
r_df$SBRD = as.data.frame(as(test6, "SpatialPixelsDataFrame"))$layer
r_df$SNEE = as.data.frame(as(test7, "SpatialPixelsDataFrame"))$layer
r_df$SNED = as.data.frame(as(test8, "SpatialPixelsDataFrame"))$layer
r_df$grass = as.data.frame(as(test9, "SpatialPixelsDataFrame"))$layer
r_df$Bare = as.data.frame(as(test10, "SpatialPixelsDataFrame"))$layer

  print(str(r_df))
  colnames(r_df) <- c( "Tree, Broadleaf Evergreen", "x", "y",
                       "Tree, Broadleaf Decidious",
                       "Tree, Needleleaf Evergreen",
                       "Tree, Needleleaf Decidious",
                       "Shrub, Broadleaf Evergreen",
                       "Shrub, Broadleaf Decidious",
                       "Shrub, Needleleaf Evergreen",
                       "Shrub, Needleleaf Decidious",
                       "C3 grass",
                       "Bare/non-vegetated")

  # Reshape the data for ggplot
  plotData = melt(r_df, id.vars = c('x','y'))

  library("PBSmapping")
  library("maps")
  library("data.table")
  #need to be able to cut off the polygons outside of the spatial domain,
  #we use pbsmapping and the state borders from the maps package
  
  statemap = map_data("state")
  setnames(statemap, c("X","Y","PID","POS","region","subregion"))
  statemap = clipPolys(statemap, xlim = c(-100,-60), ylim = c(35,50))
  #setnames(mip.domain, c("X","Y","PID","POS","region","subregion"))
  #mip.domain <- clipPolys(mip.domain, keepExtra = TRUE,xlim = c(-100,-60), ylim = c(35,50))
  
 # domain <- fortify(mip.domain)
  #d.join = join(domain, mip.domain@data)
  
  png("PFT_frac.png")
  ggplot(aes(x = x, y = y), data = plotData) +
      geom_tile(aes(fill = value)) +
      geom_polygon(data=statemap,aes(X,Y,group=PID),alpha=1,colour="grey", fill=NA, size=0.7)+
    coord_cartesian( xlim= c(-90,-55),ylim= c(45,50))+
    xlab("Long") +ylab("Lat")+
  facet_wrap(~ variable) +
      scale_fill_gradient("Fraction PFT",low = 'white', high = 'forestgreen') +
      coord_equal()+ 
    theme(strip.text.x = element_text(size = 6))
dev.off()
 
  
 
 