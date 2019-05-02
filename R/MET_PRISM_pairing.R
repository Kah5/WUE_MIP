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
ED.tmax.cor.bytmax
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_tmax_cor_by_map.png")
ED.tmax.cor.bymap
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_map_cor_by_tmax.png")
ED.map.cor.bytmax
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/ED_itrdb_map_cor_by_map.png")
ED.map.cor.bymap
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
GUESS.tmax.cor.bytmax
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_tmax_cor_by_map.png")
GUESS.tmax.cor.bymap
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_map_cor_by_tmax.png")
GUESS.map.cor.bytmax
dev.off()

png(height = 5, width = 8, units = "in", res = 300, 
    "outputs/itrdb_model_compare/guess_itrdb_map_cor_by_map.png")
GUESS.map.cor.bymap
dev.off()


# compare autocorrelation in the itmeseries of MIP MET and PRISM

