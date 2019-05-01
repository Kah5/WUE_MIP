# script to find grid cells and ITRDB sites with similar mean annual climate spaces:

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

grid.met.summary <- all.ED %>% filter(Year >= 1895) %>% group_by(lon, lat) %>% summarise(MAP = mean(precip_total.mm, na.rm=TRUE),
                                                                MAP.wy = mean(precip_total_wtr_yr.mm, na.rm=TRUE), 
                                                                tair_max_1 = mean(tair_max_1- 273.15, na.rm=TRUE) ,
                                                                tair_max_2 = mean(tair_max_2- 273.15, na.rm=TRUE),
                                                                tair_max_3 = mean(tair_max_3- 273.15, na.rm=TRUE),
                                                                tair_max_4 = mean(tair_max_4- 273.15, na.rm=TRUE),
                                                                tair_max_5 = mean(tair_max_5- 273.15, na.rm=TRUE),
                                                                tair_max_6 = mean(tair_max_6- 273.15, na.rm=TRUE),
                                                                tair_max_7 = mean(tair_max_7- 273.15, na.rm=TRUE),
                                                                tair_max_8 = mean(tair_max_8- 273.15, na.rm=TRUE),
                                                                tair_max_9 = mean(tair_max_9- 273.15, na.rm=TRUE),
                                                                tair_max_10 = mean(tair_max_10- 273.15, na.rm=TRUE),
                                                                tair_max_11 = mean(tair_max_11- 273.15, na.rm=TRUE),
                                                                tair_max_12 = mean(tair_max_12- 273.15, na.rm=TRUE))





# get prism summary: 
rwl.itrdb.clim.nona <- readRDS( paste0(getwd(),"/Data/ITRDB/full.clim.prism.rds"))

all.prism.summary <- rwl.itrdb.clim.nona %>% group_by(Longitude, Latitude) %>% summarise(MAP = mean(ppt_total, na.rm=TRUE),
                                                                              MAP.wy = mean(ppt_MAP.wy, na.rm=TRUE), 
                                                                              tair_max_1 = mean(tmax_01, na.rm=TRUE),
                                                                              tair_max_2 = mean(tmax_02, na.rm=TRUE),
                                                                              tair_max_3 = mean(tmax_03, na.rm=TRUE),
                                                                              tair_max_4 = mean(tmax_04, na.rm=TRUE),
                                                                              tair_max_5 = mean(tmax_05, na.rm=TRUE),
                                                                              tair_max_6 = mean(tmax_06, na.rm=TRUE),
                                                                              tair_max_7 = mean(tmax_07, na.rm=TRUE),
                                                                              tair_max_8 = mean(tmax_08, na.rm=TRUE),
                                                                              tair_max_9 = mean(tmax_09, na.rm=TRUE),
                                                                              tair_max_10 = mean(tmax_10, na.rm=TRUE),
                                                                              tair_max_11 = mean(tmax_11, na.rm=TRUE),
                                                                              tair_max_12 = mean(tmax_12, na.rm=TRUE))


ggplot(all.prism.summary, aes(Longitude, Latitude, color = MAP.wy))+geom_point()
ggplot(all.prism.summary, aes(Longitude, Latitude, color = tair_max_6))+geom_point()

ggplot()+geom_point(data = grid.met.summary, aes(MAP.wy,tair_max_6), color = "blue")+
geom_point(data = all.prism.summary, aes(MAP.wy,tair_max_6), color = "red")+ylim(15, 35)+xlim(400, 1800)

ggplot(all.prism.summary, aes(MAP.wy,tair_max_6, color = Longitude))+geom_point()+ylim(15, 35)+xlim(400, 1800)

ggplot()+geom_histogram(data = grid.met.summary, aes(MAP.wy), fill = "red")+
  geom_histogram(data = all.prism.summary, aes(MAP.wy), fill = "blue")


ggplot()+geom_histogram(data = grid.met.summary, aes(tair_max_6), fill = "red")+
  geom_histogram(data = all.prism.summary, aes(tair_max_6), fill = "blue")

# need to subset and select ITRDB sites and model grid cells with comparable averages in recent years:
grid.met.summary$tmax_06_bins <- cut(grid.met.summary$tair_max_6, breaks=seq(15, 35, by = 1))

all.prism.summary$tmax_06_bins <- cut(all.prism.summary$tair_max_6, breaks=seq(15, 35, by = 1))

tmax.ordered.cuts <- data.frame(tmax_06_bins = levels(cut(grid.met.summary[order(grid.met.summary$tair_max_6),]$tair_max_6, breaks=seq(15, 35, by = 1))),
                           tmax06.mids=seq(15.5, 34.5, by = 1))

all.prism.summary <- left_join(all.prism.summary, tmax.ordered.cuts, by = "tmax_06_bins")
grid.met.summary <- left_join(grid.met.summary, tmax.ordered.cuts, by = "tmax_06_bins")



# get bins of the prism precipitation data
grid.met.summary$wtryr_bins <- cut(grid.met.summary$MAP.wy, breaks=seq(400, 1450, by = 50))

all.prism.summary$wtryr_bins <- cut(all.prism.summary$MAP.wy, breaks=seq(400, 1450, by = 50))

precip.ordered.cuts <- data.frame(wtryr_bins = levels(cut(grid.met.summary[order(grid.met.summary$MAP.wy),]$MAP.wy, breaks=seq(400, 1450, by = 50))),
                                wtr.mids=seq(425, 1425, by = 50))

all.prism.summary <- left_join(all.prism.summary, precip.ordered.cuts, by = "wtryr_bins")
grid.met.summary <- left_join(grid.met.summary, precip.ordered.cuts, by = "wtryr_bins")

ggplot()+geom_point(data= all.prism.summary, aes(wtr.mids, tmax06.mids), color = "red", alpha = 0.5)+
  geom_point(data= grid.met.summary, aes(wtr.mids, tmax06.mids), pch = 6,color = "blue", alpha = 0.5)


# get the list of sites where climate bins over lap (+/- 50mm MAP and 1 degree mean Tmax in june)
prism.short <- all.prism.summary %>% dplyr::select(Longitude, Latitude, wtryr_bins, wtr.mids, tmax_06_bins, tmax06.mids)
met.short <- grid.met.summary %>% dplyr::select(lon, lat, wtryr_bins, wtr.mids, tmax_06_bins, tmax06.mids)

common_envts <- merge(prism.short, met.short, by = c("wtryr_bins","wtr.mids", "tmax_06_bins", "tmax06.mids"))


saveRDS(common_envts, "outputs/data/ITRDB_MET_common_envts.rds")

#------------Read in pearson correlation coefficients and compare correlations across common envts------------------

# for ED2
ED2.cors.df <- readRDS( "outputs/gwbi_model/ED2_gwbi_correlation_coefs_by_pft.rds")
load("Data/PalEON_siteInfo_all.RData")
paleon$Site <- as.character(paleon$num)
ED2.cors.site <- left_join(paleon[,c("lon", "lat", "Site")], ED2.cors.df, by = "Site")

ED2.cors.site.subset <- merge(common_envts, ED2.cors.site, by = c("lon", "lat"))
ggplot(ED2.cors.site.subset[ED2.cors.site.subset$month %in% "tair_max_6",], aes(tmax06.mids, coef, color = PFT))+geom_point()+stat_smooth(method = "lm")

ggplot(ED2.cors.site.subset[ED2.cors.site.subset$month %in% "precip_total_wtr_yr.mm",], aes(tmax06.mids, coef, color = PFT))+geom_point()+stat_smooth(method = "lm")

# for GUESS:


# for ITRDB:
ITRDB.cors.df <- readRDS( "Data/ITRDB/rwl.itrdb.clim.correlations.rds" )

ITRDB.cors.df.slim <- ITRDB.cors.df %>% dplyr::select(Longitude:studyCode,tmax_01:ppt_total.wy) %>% group_by(Longitude, Latitude, SPEC.CODE, studyCode) %>% gather(key = month,value = coef, tmax_01:ppt_total.wy)
ITRDB.cors.site.subset <- merge(common_envts, ITRDB.cors.df.slim, by = c("Longitude", "Latitude"))

ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "tmax_06",], aes(tmax06.mids, coef, color = SPEC.CODE))+geom_point()+stat_smooth(method = "lm")
ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "ppt_total.wy",], aes(tmax06.mids, coef, color = SPEC.CODE))+geom_point()

ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "ppt_total.wy",], aes(wtr.mids, coef, color = SPEC.CODE))+geom_point()
ggplot(ITRDB.cors.site.subset[ITRDB.cors.site.subset$month %in% "tmax_06",], aes(wtr.mids, coef, color = SPEC.CODE))+geom_point()


# compare autocorrelation in the itmeseries of MIP MET and PRISM

