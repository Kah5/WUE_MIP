# Preliminary ITRDB analysese:
# Author: K.Heilman
# Overview:
# 1. load ITRDB data
# 2. load climate data
# 3. PCA of tree ring series
# 4. Species correlations of detrended data with multiple climate parameters
# 5. Site level correlations of detrended data with multiple climate parameters & do cluster analysis on the corelation output

library(raster)
library(sp)
library(tidyr)
library(reshape2)
library(dplyr)
library(ggplot2)

#---------------------- 1. load ITRDB data -----------------------------------------------------
library(raster)
library(sp)
library(tidyr)
# read in the ITRDB file
rwl.age.ll <- readRDS( "Data/ITRDB/rwl.ages.df.nona_spatial.rds")
rwl.age.ll.unique <- unique(rwl.age.ll[, c("Longitude", "Latitude", "studyCode", "SPEC.CODE")])

# get the LatLon for the grid cell:
toRaster <- "/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/paleon.unit.ll_01.tif"
paleon.ll <- raster("/Users/kah/Documents/WUE_MIP/WUE_MIP/Data/paleon.unit.ll_01.tif")
values(paleon.ll) <- 1:ncell(paleon.ll)
lat.lon <- raster::extract(paleon.ll, rwl.age.ll.unique[,c("Longitude", "Latitude")], cellnumber = TRUE, df = TRUE)
y <- data.frame(rasterToPoints(paleon.ll))

xy <- xyFromCell(paleon.ll, cell = lat.lon$cells)

lat.lon$x <- xy[,"x"]
lat.lon$y <- xy[,"y"]
lat.lon$Longitude <- rwl.age.ll.unique$Longitude
lat.lon$Latitude <- rwl.age.ll.unique$Latitude
lat.lon$Latitude <- rwl.age.ll.unique$Latitude

lat.lon.uni <- unique(lat.lon[, c("x", "y", "Longitude", "Latitude")])

# add paleon grid cells lat longs to the rwl.age.ll data.frame
rwl.age.ll.unique$Longitude_Pal <- lat.lon$Longitude 
rwl.age.ll.unique$Latitude_Pal <- lat.lon$Latitude

lat.lon.uni <- unique(lat.lon[, c("x", "y")])


#---------------------- 2. load climate data -----------------------------------------------------
# tmax & ppt to start with:

tmax.itrdb.df <- readRDS( "Data/ITRDB/PRISM/tmax/Tmax_1895_2016_extracted_ITRDB.rds")
ppt.itrdb.df <- readRDS( "Data/ITRDB/PRISM/ppt/ppt_1895_2016_extracted_ITRDB.rds")


#---------------------- 3. PCA of tree ring series-----------------------------------------------------
# What are the main axes of variablity in chronologies in itrdb here? Species? lat/lon? elevation?
# get the crns. that were developed from splines:

crn.df <- readRDS("Data/ITRDB/crn.df.paleon.rds")
crn.df$year.site <- rownames(crn.df)
crn.df <- crn.df %>% separate(year.site, c("site", "filetype", "year"))
crn.df$year <- as.numeric(crn.df$year)

crn.summary <- crn.df %>% group_by(site) %>% dplyr::summarise(yearmn = min(year, na.rm =TRUE),
                                                       yearmx = max(year, na.rm =TRUE))

crn.summary %>% filter(yearmn >= 1850 & yearmx >= 2000) %>% dplyr::summarise(n())
crn.summary %>% filter(yearmn >= 1850 & yearmx >= 1980) %>% dplyr::summarise(n())
crn.summary %>% filter(yearmn <= 1850 & yearmx >= 1980) %>% dplyr::summarise(n())
crn.summary %>% filter(yearmn <= 1850 & yearmx >= 1970) %>% dplyr::summarise(n())

modern.ish <- crn.summary %>% filter(yearmn <= 1895 & yearmx >= 1970) 

crn.wide <- crn.df %>% select(year, site, xxxstd) %>% filter(year >= 1895 & year <=1970) %>% spread(key = site, value = xxxstd, drop = TRUE)
crn.common <- crn.wide[,names(crn.wide) %in% c(unique(modern.ish$site), 'year')]
#crn.common.nona <- crn.common[!is.na(crn.common)]

# for some reason this results in a df that sitll has NA values! 
crn.sub <- crn.common [crn.common$year >= 1850 & crn.common$year <= 1970, ]
ggplot(crn.sub, aes(year, wv002))+geom_point()+geom_line()

TR <- prcomp(~ ., data = data.frame(crn.sub[!names(crn.sub) %in% "year"]), na.action = na.omit)
# covmat.chrons <- cov.wt(na.omit(data.frame(crn.sub[!names(crn.sub) %in% "year",])))
# 
# TR <- princomp(covmat = covmat.chrons)
plot(TR)
biplot(TR)

PCvals <- TR$x 
PCvals.rot <- data.frame(TR$rotation)
PCvals.rot$studyCode <- toupper(rownames(PCvals.rot))


uniquesites <- unique(rwl.age.ll.unique[, c("Longitude", "Latitude","Longitude_Pal", "Latitude_Pal", "SPEC.CODE",  "studyCode")])

full.pcs <- left_join(uniquesites, PCvals.rot, by ="studyCode", all.x = FALSE)

ggplot(full.pcs, aes(PC1, PC2, color = SPEC.CODE))+geom_point()
ggplot(full.pcs, aes(PC1, PC2, color = Latitude))+geom_point()
ggplot(full.pcs, aes(PC1, PC2, color = Longitude))+geom_point()

ggplot(full.pcs, aes(PC3, PC4, color = SPEC.CODE))+geom_point()
ggplot(full.pcs, aes(Longitude, PC1, color = SPEC.CODE))+geom_point()
ggplot(full.pcs, aes(Latitude, PC2, color = SPEC.CODE))+geom_point()

# PC1 is linked to Lon & PC2 is likely Lat

#---------------------- 4. Species correlations of detrended chronologies with multiple climate parameters------------------

uniquesites$x <- uniquesites$Longitude
uniquesites$y <- uniquesites$Latitude

tmax.summary <- tmax.itrdb.df %>% group_by(Longitude, Latitude, year) %>% dplyr::summarise(meanTmax = mean(Tmax))

ggplot()+geom_point(data = tmax.summary[tmax.summary$year %in% "1895",], aes(x =Longitude,y = Latitude, color = meanTmax))#+
  #geom_point(data = rwl.age.ll.unique, aes(Longitude, Latitude))

#test.merge <-  merge(tmax.summary, rwl.age.ll.unique, by.x = c("x", "y"), by.y = c("Longitude", "Latitude"))

tmax.itrdb <- left_join(tmax.itrdb.df, uniquesites, by = c("Longitude", "Latitude"))

#ggplot(tmax.itrdb[tmax.itrdb$year %in% "1895" & tmax.itrdb$month %in% "01",], aes(Longitude, Latitude))+geom_point()
tmax.sites.only <- tmax.itrdb[!is.na(tmax.itrdb$studyCode),]
tmax.sites.only <- tmax.sites.only[,1:11]
colnames(tmax.sites.only) <- c("x", "y", "Tmax", "Longitude", "Latitude", "year", "month", "Longitude_Pal", "Latitude_Pal", "SPEC.CODE", "studyCode")

saveRDS(tmax.sites.only, paste0("Data/ITRDB/PRISM/tmax/Tmax_1895_2016_extracted_ITRDB_sites_only.rds"))


# get data reformatted to have months in columns & merge with sites:

tmax.month <- tmax.sites.only %>% select("Longitude", "Latitude","Tmax", "year", "month", "studyCode", "SPEC.CODE") %>% spread(key = month, value = Tmax)
tmax.month$year <- as.numeric(tmax.month$year)


# join tmax.month with itrdb dataframe:
rwl.itrdb <- rwl.age.ll[,c("Longitude", "Latitude","SPEC.CODE", "studyCode", "ID", "year", "Age", "RWI", "ageclass", "RWI_1", "RWI_2", "RWI_2")]
rwl.itrdb.nona <- rwl.itrdb[!is.na(rwl.itrdb$RWI),]
rwl.itrdb.clim <- left_join(rwl.itrdb.nona, tmax.month, by = c("Longitude", "Latitude","SPEC.CODE", "studyCode", "year"))

rwl.itrdb.clim.nona <- rwl.itrdb.clim[!is.na(rwl.itrdb.clim$`01`),]

correlate.tmax <- function(x){
      test.nona <- rwl.itrdb.clim.nona[rwl.itrdb.clim.nona$studyCode %in% x, ]
      
      test.nona$RWI <- as.numeric(test.nona$RWI)
      corM <- cor(test.nona$RWI, test.nona[, c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")], use = "pairwise.complete")
      
      library(Hmisc) # You need to download it first.
      cor.mat <- rcorr( as.matrix(test.nona[, c("RWI","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")]), type="pearson") 
      cor.mat.df <- data.frame(month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                               coef = cor.mat$r[2:13,1], 
                 p = cor.mat$P[2:13,1])
      cat("*")
      cor.mat.df
}

names <- as.list(as.character(unique(rwl.itrdb.clim.nona$studyCode))) # get names to apply function over
system.time(tmax.cors <- lapply(names, correlate.tmax))
names(tmax.cors) <- unique(rwl.itrdb.clim.nona$studyCode)

tmax.cors.df <- do.call(rbind, tmax.cors) # takes a minute
tmax.cors.df$studyCode <- rep(names(tmax.cors), sapply(tmax.cors, nrow)) # add the site names

# basic tile plot
ggplot(tmax.cors.df, aes(month, studyCode, fill = coef))+geom_tile()+scale_fill_distiller(palette = "Spectral")

# -------------- do the same to get corrs for ppt:
# get only sites for ppt:
ppt.itrdb <- left_join(ppt.itrdb.df, uniquesites, by = c("Longitude", "Latitude"))

#ggplot(ppt.itrdb[ppt.itrdb$year %in% "1895" & ppt.itrdb$month %in% "01",], aes(Longitude, Latitude))+geom_point()
ppt.sites.only <- ppt.itrdb[!is.na(ppt.itrdb$studyCode),]
ppt.sites.only <- ppt.sites.only[,1:11]
colnames(ppt.sites.only) <- c("x", "y", "ppt", "Longitude", "Latitude", "year", "month", "Longitude_Pal", "Latitude_Pal", "SPEC.CODE", "studyCode")

saveRDS(ppt.sites.only, paste0("Data/ITRDB/PRISM/ppt/ppt_1895_2016_extracted_ITRDB_sites_only.rds"))

# get data reformatted to have months in columns & merge with sites:

ppt.month <- ppt.sites.only %>% select("Longitude", "Latitude","ppt", "year", "month", "studyCode", "SPEC.CODE") %>% spread(key = month, value = ppt)
ppt.month$year <- as.numeric(ppt.month$year)


# join ppt.month with itrdb dataframe:
rwl.itrdb <- rwl.age.ll[,c("Longitude", "Latitude","SPEC.CODE", "studyCode", "ID", "year", "Age", "RWI", "ageclass", "RWI_1", "RWI_2", "RWI_2")]
rwl.itrdb.nona <- rwl.itrdb[!is.na(rwl.itrdb$RWI),]
rwl.itrdb.clim <- left_join(rwl.itrdb.nona, ppt.month, by = c("Longitude", "Latitude","SPEC.CODE", "studyCode", "year"))

rwl.itrdb.clim.nona <- rwl.itrdb.clim[!is.na(rwl.itrdb.clim$`01`),]

correlate.ppt <- function(x){
  test.nona <- rwl.itrdb.clim.nona[rwl.itrdb.clim.nona$studyCode %in% x, ]
  
  test.nona$RWI <- as.numeric(test.nona$RWI)
  corM <- cor(test.nona$RWI, test.nona[, c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")], use = "pairwise.complete")
  
  library(Hmisc) # You need to download it first.
  cor.mat <- rcorr( as.matrix(test.nona[, c("RWI","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")]), type="pearson") 
  cor.mat.df <- data.frame(month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                           coef = cor.mat$r[2:13,1], 
                           p = cor.mat$P[2:13,1])
  cat("*")
  cor.mat.df
}

names <- as.list(as.character(unique(rwl.itrdb.clim.nona$studyCode))) # get names to apply function over
system.time(ppt.cors <- lapply(names, correlate.ppt))
names(ppt.cors) <- unique(rwl.itrdb.clim.nona$studyCode)

ppt.cors.df <- do.call(rbind, ppt.cors) # takes a minute
ppt.cors.df$studyCode <- rep(names(ppt.cors), sapply(ppt.cors, nrow)) # add the site names

# basic tile plot
ggplot(ppt.cors.df, aes(month, studyCode, fill = coef))+geom_tile()+scale_fill_distiller(palette = "Spectral")




# 5. Site level correlations of detrended data with multiple climate parameters & do cluster analysis on the corelation output
