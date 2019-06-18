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
library(cowplot)

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

crn.wide <- crn.df %>% dplyr::select(year, site, xxxstd) %>% filter(year >= 1895 & year <=1970) %>% spread(key = site, value = xxxstd, drop = TRUE)
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

tmax.month <- tmax.sites.only %>% dplyr::select("Longitude", "Latitude","Tmax", "year", "month", "studyCode", "SPEC.CODE") %>% spread(key = month, value = Tmax)
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
tmax.cors.df <- left_join(uniquesites, tmax.cors.df, by = "studyCode")
tmax.cors.df<- tmax.cors.df[!is.na(tmax.cors.df$coef),]


# read in taxa translation to generalize:
taxa.trans <- read.csv("Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv", stringsAsFactors = FALSE)

tmax.cors.df <- left_join(tmax.cors.df, taxa.trans, by = "SPEC.CODE")

# basic tile plot
ggplot(tmax.cors.df[tmax.cors.df$PALEON %in% "Pine",], aes(month, studyCode, fill = coef))+geom_tile()+scale_fill_distiller(palette = "Spectral")


# cluster the coeffeicent temperature responses:
tmax.clusters <- tmax.cors.df %>% dplyr::select("Longitude", "Latitude", "SPEC.CODE", "studyCode", "PALEON", "ED.PFT", "LPJ.GUESS.PFT", "LINKAGES", "month", "coef") %>% spread(key = month, value = coef)

k3 <- cluster::pam(tmax.clusters[,c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")], k = 3, diss = FALSE)
tmax.clusters$k3 <- as.character(k3$clustering)

k4 <- cluster::pam(tmax.clusters[,c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")], k = 4, diss = FALSE)
tmax.clusters$k4 <- as.character(k4$clustering)

k5 <- cluster::pam(tmax.clusters[,c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")], k = 5, diss = FALSE)
tmax.clusters$k5 <- as.character(k5$clustering)

ggplot(tmax.clusters, aes(Longitude, Latitude, color = k3))+geom_point()
ggplot(tmax.clusters, aes(Longitude, Latitude, color = k4))+geom_point()
ggplot(tmax.clusters, aes(Longitude, Latitude, color = k5))+geom_point()


# -------------- do the same to get corrs for ppt:
# get only sites for ppt:
ppt.itrdb <- left_join(ppt.itrdb.df, uniquesites, by = c("Longitude", "Latitude"))

#ggplot(ppt.itrdb[ppt.itrdb$year %in% "1895" & ppt.itrdb$month %in% "01",], aes(Longitude, Latitude))+geom_point()
ppt.sites.only <- ppt.itrdb[!is.na(ppt.itrdb$studyCode),]
ppt.sites.only <- ppt.sites.only[,1:11]
colnames(ppt.sites.only) <- c("x", "y", "ppt", "Longitude", "Latitude", "year", "month", "Longitude_Pal", "Latitude_Pal", "SPEC.CODE", "studyCode")

saveRDS(ppt.sites.only, paste0("Data/ITRDB/PRISM/ppt/ppt_1895_2016_extracted_ITRDB_sites_only.rds"))

# get data reformatted to have months in columns & merge with sites:


# create function to get the water year
wtr_yr <- function(df, start_month=9) {
  # Year offset
  offset = ifelse(as.numeric(df$month) >= start_month - 1, 1, 0)
  # Water year
  adj.year = as.numeric(df$year) + offset
  # Return the water year
  adj.year
}

# use wtr_year function to get water year as a column
ppt.sites.only$wtr.year <- wtr_yr(ppt.sites.only)

# get total precipitation for each water year:
long.prism.wy.MAP <- ppt.sites.only %>% group_by(studyCode, wtr.year) %>% dplyr::summarise(MAP.wy = sum(ppt, na.rm=TRUE) )
colnames(long.prism.wy.MAP) <- c("studyCode", "year", "MAP.wy")
ppt.sites.only$year <- as.numeric(ppt.sites.only$year)

ppt.sites.only <- left_join(ppt.sites.only, long.prism.wy.MAP, by = c("studyCode", "year"))

ppt.month <- ppt.sites.only %>% dplyr::select("Longitude", "Latitude","ppt", "year", "month", "studyCode", "SPEC.CODE") %>% spread(key = month, value = ppt)
ppt.month$year <- as.numeric(ppt.month$year)

ppt.month <- left_join(ppt.month, long.prism.wy.MAP, by = c("studyCode", "year"))


# join ppt.month with itrdb dataframe:
rwl.itrdb <- rwl.age.ll[,c("Longitude", "Latitude","SPEC.CODE", "studyCode", "ID", "year", "Age", "RWI", "ageclass", "RWI_1", "RWI_2", "RWI_2")]
rwl.itrdb.nona <- rwl.itrdb[!is.na(rwl.itrdb$RWI),]
rwl.itrdb.clim <- left_join(rwl.itrdb.nona, ppt.month, by = c("Longitude", "Latitude","SPEC.CODE", "studyCode", "year"))

rwl.itrdb.clim.nona <- rwl.itrdb.clim[!is.na(rwl.itrdb.clim$`01`),]

# get the total precip for this year (note that it should be for water year, but this is calendar year)
rwl.itrdb.clim.nona$total <- rowSums(rwl.itrdb.clim.nona[,c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")])


correlate.ppt <- function(x){
  test.nona <- rwl.itrdb.clim.nona[rwl.itrdb.clim.nona$studyCode %in% x, ]
  
  test.nona$RWI <- as.numeric(test.nona$RWI)
  corM <- cor(test.nona$RWI, test.nona[, c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "total", "MAP.wy")], use = "pairwise.complete")
  
  library(Hmisc) # You need to download it first.
  cor.mat <- rcorr( as.matrix(test.nona[, c("RWI","01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "total", "MAP.wy")]), type="pearson") 
  cor.mat.df <- data.frame(month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "total", "total.wy"),
                           coef = cor.mat$r[2:15,1], 
                           p = cor.mat$P[2:15,1])
  cat("*")
  cor.mat.df
}

names <- as.list(as.character(unique(rwl.itrdb.clim.nona$studyCode))) # get names to apply function over
system.time(ppt.cors <- lapply(names, correlate.ppt))
names(ppt.cors) <- unique(rwl.itrdb.clim.nona$studyCode)

ppt.cors.df <- do.call(rbind, ppt.cors) # takes a minute
ppt.cors.df$studyCode <- rep(names(ppt.cors), sapply(ppt.cors, nrow)) # add the site names
ppt.cors.df <- left_join(uniquesites,ppt.cors.df, by = "studyCode")
ppt.cors.df <- ppt.cors.df[!is.na(ppt.cors.df$coef),]


# basic tile plot
ggplot(ppt.cors.df, aes(month, studyCode, fill = coef))+geom_tile()+scale_fill_distiller(palette = "Spectral")


#---------------------------------Merge Tmax and precipitation----------------------------------

colnames(rwl.itrdb.clim.nona)[13:26] <- paste0("ppt_",colnames(rwl.itrdb.clim.nona)[13:26])
colnames(tmax.month)[6:17]<- paste0("tmax_", colnames(tmax.month)[6:17])
full.clim <- left_join(rwl.itrdb.clim.nona, tmax.month, by = c("Longitude", "Latitude", "year", "studyCode", "SPEC.CODE"))
saveRDS(full.clim, "Data/ITRDB/full.clim.prism.rds")

# ----------cluster the correlation coeffients & plot tileplots by those clusters:--------------
tmax.clusters <- tmax.cors.df %>% dplyr::select("Longitude", "Latitude", "SPEC.CODE", "studyCode", "PALEON", "ED.PFT", "LPJ.GUESS.PFT", "LINKAGES", "month", "coef") %>% spread(key = month, value = coef)
ppt.clusters <- ppt.cors.df %>% dplyr::select("Longitude", "Latitude", "SPEC.CODE", "studyCode", "month", "coef") %>% spread(key = month, value = coef)

# rename the temp and precip column names...This is clunky but it will do for now
colnames(ppt.clusters)[5:18] <- paste0("ppt_", colnames(ppt.clusters)[5:18])
colnames(tmax.clusters)[9:20] <- paste0("tmax_", colnames(tmax.clusters)[9:20])

# join ppt & tmax cluster dfs:
all.cors <- left_join(tmax.clusters, ppt.clusters, by = c("Longitude", "Latitude",  "SPEC.CODE", "studyCode"))
saveRDS(all.cors, "Data/ITRDB/rwl.itrdb.clim.correlations.rds")


saveRDS(rwl.itrdb.clim.nona, "Data/ITRDB/rwl.itrdb.clim.nona.rds")



k2 <- cluster::pam(all.cors[,c("tmax_01",       "tmax_02",       "tmax_03",       "tmax_04",      
                               "tmax_05",       "tmax_06",      "tmax_07",       "tmax_08",       "tmax_09",       "tmax_10",      
                               "tmax_11",       "tmax_12",       "ppt_01",        "ppt_02",        "ppt_03",        "ppt_04",       
                               "ppt_05",        "ppt_06",        "ppt_07",        "ppt_08",        "ppt_09",        "ppt_10",       
                               "ppt_11",        "ppt_12",        "ppt_total" ,"ppt_total.wy" )], k = 2, diss = FALSE)
all.cors$k2 <- as.character(k2$clustering)


k3 <- cluster::pam(all.cors[,c("tmax_01",       "tmax_02",       "tmax_03",       "tmax_04",      
                                    "tmax_05",       "tmax_06",      "tmax_07",       "tmax_08",       "tmax_09",       "tmax_10",      
                                    "tmax_11",       "tmax_12",       "ppt_01",        "ppt_02",        "ppt_03",        "ppt_04",       
                                    "ppt_05",        "ppt_06",        "ppt_07",        "ppt_08",        "ppt_09",        "ppt_10",       
                                     "ppt_11",        "ppt_12",        "ppt_total" ,"ppt_total.wy" )], k = 3, diss = FALSE)
all.cors$k3 <- as.character(k3$clustering)

k4 <- cluster::pam(all.cors[,c("tmax_01",       "tmax_02",       "tmax_03",       "tmax_04",      
                               "tmax_05",       "tmax_06",      "tmax_07",       "tmax_08",       "tmax_09",       "tmax_10",      
                               "tmax_11",       "tmax_12",       "ppt_01",        "ppt_02",        "ppt_03",        "ppt_04",       
                               "ppt_05",        "ppt_06",        "ppt_07",        "ppt_08",        "ppt_09",        "ppt_10",       
                               "ppt_11",        "ppt_12",        "ppt_total","ppt_total.wy"  )], k = 4, diss = FALSE)
all.cors$k4 <- as.character(k4$clustering)

k5 <- cluster::pam(all.cors[,c("tmax_01",       "tmax_02",       "tmax_03",       "tmax_04",      
                               "tmax_05",       "tmax_06",      "tmax_07",       "tmax_08",       "tmax_09",       "tmax_10",      
                               "tmax_11",       "tmax_12",       "ppt_01",        "ppt_02",        "ppt_03",        "ppt_04",       
                               "ppt_05",        "ppt_06",        "ppt_07",        "ppt_08",        "ppt_09",        "ppt_10",       
                               "ppt_11",        "ppt_12",        "ppt_total","ppt_total.wy"  )], k = 5, diss = FALSE)
all.cors$k5 <- as.character(k5$clustering)

k6 <- cluster::pam(all.cors[,c("tmax_01",       "tmax_02",       "tmax_03",       "tmax_04",      
                               "tmax_05",       "tmax_06",      "tmax_07",       "tmax_08",       "tmax_09",       "tmax_10",      
                               "tmax_11",       "tmax_12",       "ppt_01",        "ppt_02",        "ppt_03",        "ppt_04",       
                               "ppt_05",        "ppt_06",        "ppt_07",        "ppt_08",        "ppt_09",        "ppt_10",       
                               "ppt_11",        "ppt_12",        "ppt_total","ppt_total.wy"  )], k = 6, diss = FALSE)
all.cors$k6 <- as.character(k6$clustering)

summary(k2)
summary(k3)
summary(k4)
summary(k5)
summary(k6)

# visualise the different cluster k mediod correlations:
k2.df <- k2$medoids
k2.df.m <- melt(k2.df)
ggplot(k2.df.m, aes(Var2, Var1, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")

k3.df <- k3$medoids
k3.df.m <- melt(k3.df)
ggplot(k3.df.m, aes(Var2, Var1, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")

k4.df <- k4$medoids
k4.df.m <- melt(k4.df)
ggplot(k4.df.m, aes(Var2, Var1, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")

k5.df <- k5$medoids
k5.df.m <- melt(k5.df)
ggplot(k5.df.m, aes(Var2, Var1, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")

k6.df <- k6$medoids
k6.df.m <- melt(k6.df)
ggplot(k6.df.m, aes(Var2, Var1, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")


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

#-------------MAPS for differenct clusters:----------------
k2.map <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k2, size = ppt_total.wy))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k3.map <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k3, size = ppt_total.wy))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k4.map <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k4, size = ppt_total.wy))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k5.map <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k5, size = ppt_total.wy))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k6.map <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k6, size = ppt_total.wy))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

png(height = 10, width = 18, units = "in", res = 250, "outputs/ITRDB/map_water_yr_precip_cor_clusters.png")
cowplot::plot_grid(k2.map.tmax, k3.map.tmax, k4.map.tmax, k5.map.tmax, k6.map.tmax, ncol = 2, align = 'hv')
dev.off()

k2.map.tmax <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k2, size =tmax_06))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k3.map.tmax <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k3, size = tmax_06))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k4.map.tmax <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k4, size = tmax_06))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k5.map.tmax <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k5, size = tmax_06))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

k6.map.tmax <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+
  #geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="white", fill = "grey")+#geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_point(data = all.cors, aes(Longitude, Latitude, color = k6, size = tmax_06))+
  coord_cartesian(ylim = c(36, 49), xlim= c(-98,-66))+theme_bw()

png(height = 10, width = 18, units = "in", res = 250, "outputs/ITRDB/map_jun_tmax_cor_clusters.png")
cowplot::plot_grid(k2.map.tmax, k3.map.tmax, k4.map.tmax, k5.map.tmax, k6.map.tmax, ncol = 2, align = 'hv')
dev.off()


ggplot(all.cors, aes(PALEON, fill = PALEON))+geom_bar()+facet_wrap(~k2)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(all.cors, aes(PALEON, fill = PALEON))+geom_bar()+facet_wrap(~k3)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(all.cors, aes(PALEON, fill = PALEON))+geom_bar()+facet_wrap(~k4)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(all.cors, aes(PALEON, fill = PALEON))+geom_bar()+facet_wrap(~k5)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(all.cors, aes(PALEON, fill = PALEON))+geom_bar()+facet_wrap(~k6)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

k6.melt <- melt(all.cors, id.vars = c( "Longitude",     "Latitude" ,     "SPEC.CODE"  ,   "studyCode"  ,   "PALEON" ,       "ED.PFT" ,      
                                       "LPJ.GUESS.PFT", "LINKAGES", "k6", "k5", "k4", "k3", "k2") )

ggplot(k6.melt, aes(variable, PALEON, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")+facet_wrap(~k6)
ggplot(k6.melt, aes(variable, PALEON, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")+facet_wrap(~k2)
ggplot(k6.melt, aes(variable, PALEON, fill = value))+geom_tile()+scale_fill_distiller(palette = "Spectral")+facet_wrap(~k3)


png(height = 8, width = 13, units = "in", res = 300, "outputs/ITRDB/ITRDB_crn_cluster_k3_correlations.png")
ggplot(k6.melt, aes(variable, value , fill = PALEON))+geom_boxplot()+facet_wrap(~k3, ncol = 1)+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Correlation Coefficient")+xlab("Climate Variable")+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")
dev.off()


png(height = 8, width = 13, units = "in", res = 300, "outputs/ITRDB/ITRDB_crn_cluster_k2_correlations.png")
ggplot(k6.melt, aes(variable, value , fill = PALEON))+geom_boxplot()+facet_wrap(~k2, ncol = 1)+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Correlation Coefficient")+xlab("Climate Variable")+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")
dev.off()

png(height = 8, width = 13, units = "in", res = 300, "outputs/ITRDB/ITRDB_crn_cluster_k4_correlations.png")
ggplot(k6.melt, aes(variable, value , fill = PALEON))+geom_boxplot()+facet_wrap(~k4, ncol = 1)+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Correlation Coefficient")+xlab("Climate Variable")+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")
dev.off()



biplot.2 <- ggplot(all.cors, aes(tmax_06, ppt_total.wy, color = k2))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("orrelation with July Tmax")+theme(panel.grid = element_blank())
biplot.3 <- ggplot(all.cors, aes(tmax_06, ppt_total.wy, color = k3))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("correlation with July Tmax")+theme(panel.grid = element_blank())
biplot.4 <- ggplot(all.cors, aes(tmax_06, ppt_total.wy, color = k4))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("correlation with July Tmax")+theme(panel.grid = element_blank())
biplot.5 <- ggplot(all.cors, aes(tmax_06, ppt_total.wy, color = k5))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("correlation with July Tmax")+theme(panel.grid = element_blank())
biplot.6 <- ggplot(all.cors, aes(tmax_06, ppt_total.wy, color = k6))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("correlation with July Tmax")+theme(panel.grid = element_blank())


ggplot(all.cors, aes(tmax_06, ppt_06, color = k3))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("correlation with July Tmax")+theme(panel.grid = element_blank())

#biplot.spec <- 
  ggplot(all.cors, aes(tmax_06, ppt_total.wy, color = PALEON))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("correlation with July Tmax")+theme(panel.grid = element_blank())

  ggplot(all.cors, aes(tmax_06, ppt_total.wy, color = PALEON))+geom_point()+geom_hline(yintercept = 0, color = "grey")+geom_vline(xintercept = 0, color = "grey")+theme_bw()+ylab("correlation with water year precipitation")+xlab("correlation with July Tmax")+theme(panel.grid = element_blank())
  
  

png(height = 8, width = 13, units = "in", res = 300, "outputs/ITRDB/ITRDB_ppt_06Tmax_clusters.png")
plot_grid(biplot.2, biplot.3, biplot.4, biplot.5, biplot.6, ncol = 3, align = "hv")
dev.off()



TMAX.by.PaLEON <- ggplot(k6.melt[k6.melt$variable %in% c("tmax_01", "tmax_02", "tmax_03", "tmax_04","tmax_05", "tmax_06","tmax_07", "tmax_08",     
                                       "tmax_09",  "tmax_10" ,"tmax_11","tmax_12"),], aes(variable, value , fill = PALEON))+geom_boxplot()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Correlation Coefficient")+xlab("Climate Variable")+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+facet_wrap(~PALEON)


precip.by.PaLEON <- ggplot(k6.melt[k6.melt$variable %in% c("ppt_01","ppt_02",       "ppt_03",      "ppt_04",      
                                                           "ppt_05",       "ppt_06",       "ppt_07",       "ppt_08",       "ppt_09",       "ppt_10",       "ppt_11",       "ppt_12",      
                                                            "ppt_total",    "ppt_total.wy"),], aes(variable, value , fill = PALEON))+geom_boxplot()+theme_bw()+
  theme(axis.text = element_text(angle = 45, hjust = 1), panel.grid = element_blank())+ylab("Correlation Coefficient")+xlab("Climate Variable")+geom_hline(yintercept = 0, color = "grey", linetype = "dashed")+facet_wrap(~PALEON)

png(height = 8, width = 13, units = "in", res = 300, "outputs/ITRDB/ITRDB_ppt_corrs_by_paleon_taxa.png")
precip.by.PaLEON
dev.off()

png(height = 8, width = 13, units = "in", res = 300, "outputs/ITRDB/ITRDB_TMAX_corrs_by_paleon_taxa.png")
TMAX.by.PaLEON
dev.off()


# 5. Site level correlations of detrended data with multiple climate parameters & do cluster analysis on the corelation output

