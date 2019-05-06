# some basic GWBI visualizations:
library(tidyr)
library(ggplot2)

ED2.gwbi.clim.nona <- readRDS("Data/ED2_gwbi_pft_clim.rds")

ggplot(ED2.gwbi.clim.nona, aes(Year, GWBI, color = Site))+geom_point()+facet_wrap(~PFT)

# read in dataframe with WUE
WUE.dens.dfs <- readRDS("outputs/data/ED2/dens_agbi_climate_ED2.rds")
WUE.dens.dfs$Site <- substring(WUE.dens.dfs$Site, 2)

ED2 <- left_join(WUE.dens.dfs, ED2.gwbi.clim.nona, by = c("Site", "Year"))


# remove the outliers:
ED2.rm <- ED2[ED2$IWUE <= 50, ]
ggplot(ED2.rm, aes(Tair.C, IWUE))+geom_point()

ED2.rm <- ED2.rm[!is.na(ED2.rm$PFT), ]

ggplot(ED2.rm, aes(IWUE, Rel.Dens))+geom_point(size = 0.5)
ED2.IWUE.GWBI.PFT <- ggplot(ED2.rm, aes(IWUE, GWBI, color = PFT))+geom_point(size = 0.5)+facet_wrap(~PFT)

png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/ED2_GWBI_IWUE_by_pft.png")
ED2.IWUE.GWBI.PFT
dev.off()

# plot out effects of IWUE, climate on GWBI:


ED2.precip.GWBI.PFT <- ggplot(ED2.rm, aes( precip_total.mm, GWBI,color = PFT))+geom_point(size = 0.5)+stat_smooth(color = "black")+facet_wrap(~PFT)

png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/ED2_GWBI_precip_by_pft.png")
ED2.precip.GWBI.PFT
dev.off()





ED2.tmax.GWBI.PFT <- ggplot(ED2.rm, aes( tair_max_6, GWBI,color = PFT))+geom_point(size = 0.5)+stat_smooth(color = "black")+facet_wrap(~PFT)

png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/ED2_GWBI_tmax_by_pft.png")
ED2.tmax.GWBI.PFT
dev.off()

# do places with lower precipitation have higher increases in WUE? & which species does this benefit?


all.met <- readRDS( paste0(getwd(),"/Data/MET/all.met.summary.rds"))
# merge climate and growth for ED2:
colnames(all.met)[3] <- "Year"


all.met.summary <- all.met %>% group_by(lon, lat) %>% summarise(Mean_MAP = mean(precip_total.mm, na.rm=TRUE),
                                                                Mean_MAP.wy = mean(precip_total_wtr_yr.mm, na.rm=TRUE), 
                                                                Mean_tair_max_1 = mean(tair_max_1, na.rm=TRUE),
                                                                Mean_tair_max_2 = mean(tair_max_2, na.rm=TRUE),
                                                                Mean_tair_max_3 = mean(tair_max_3, na.rm=TRUE),
                                                                Mean_tair_max_4 = mean(tair_max_4, na.rm=TRUE),
                                                                Mean_tair_max_5 = mean(tair_max_5, na.rm=TRUE),
                                                                Mean_tair_max_6 = mean(tair_max_6, na.rm=TRUE),
                                                                Mean_tair_max_7 = mean(tair_max_7, na.rm=TRUE),
                                                                Mean_tair_max_8 = mean(tair_max_8, na.rm=TRUE),
                                                                Mean_tair_max_9 = mean(tair_max_9, na.rm=TRUE),
                                                                Mean_tair_max_10 = mean(tair_max_10, na.rm=TRUE),
                                                                Mean_tair_max_11 = mean(tair_max_11, na.rm=TRUE),
                                                                Mean_tair_max_12 = mean(tair_max_12, na.rm=TRUE))

ggplot(all.met.summary, aes(lon, lat, fill = Mean_MAP.wy))+geom_raster()
ggplot(all.met.summary, aes(lon, lat, fill = Mean_tair_max_6))+geom_raster()


# join the site means to the sites:

ED2.rm.site <- left_join(all.met.summary, ED2.rm , by = c("lon", "lat"))

ggplot(ED2.rm.site[ED2.rm.site$Site %in% "1",], aes(Year, IWUE, color = Mean_tair_max_6))+geom_point()+geom_line()+stat_smooth()

# get the change in %WUE over between 850-1850 and 1950-present
mean.850.1850 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1849) %>% summarise(IWUE.850.1850 = mean(IWUE, na.rm=TRUE),
                                                                                                    GWBI.850.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                    MAP.wy.850.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                    Tmax_6.850.1850 = mean(tair_max_6, na.rm =TRUE))




mean.1690.1850 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1690 & Year <= 1849) %>% summarise(IWUE.1690.1850 = mean(IWUE, na.rm=TRUE),
                                                                                                       GWBI.1690.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1690.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1690.1850 = mean(tair_max_6, na.rm =TRUE))





mean.1850.2011 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1850) %>% summarise(IWUE.1850.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                      GWBI.1850.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                      MAP.wy.1850.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                      Tmax_6.1850.2011 = mean(tair_max_6, na.rm =TRUE))




mean.1950.2011 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1950) %>% summarise(IWUE.1950.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                       GWBI.1950.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1950.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1950.2011 = mean(tair_max_6, na.rm =TRUE))





ggplot(mean.850.1850, aes(MAP.wy.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(IWUE.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(Tmax_6.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(Tmax_6.850.1850, MAP.wy.850.1850, color = IWUE.850.1850))+geom_point()


time.periods <- left_join(mean.1950.2011, mean.850.1850, by = c("lat", "lon", "Site", "PFT"))

pct.change <- time.periods %>% group_by(lat, lon, Site, PFT) %>% summarise(IWUE.change = mean(((IWUE.1950.2011 - IWUE.850.1850)/IWUE.850.1850)*100, na.rm=TRUE),
                                                                           precip.change = mean(((MAP.wy.1950.2011 - MAP.wy.850.1850)/MAP.wy.850.1850)*100, na.rm=TRUE),
                                                                           tmax6_change = mean(((Tmax_6.1950.2011 - Tmax_6.850.1850)/Tmax_6.850.1850)*100, na.rm=TRUE),
                                                                           GWBI.change = mean(((GWBI.1950.2011 - GWBI.850.1850)/GWBI.850.1850)*100, na.rm=TRUE))

GWBI.WUE.change <- ggplot(pct.change, aes(IWUE.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
IWUE.precip.change <- ggplot(pct.change, aes(IWUE.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
IWUE.temp.change <- ggplot(pct.change, aes(IWUE.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

GWBI.temp.change <- ggplot(pct.change, aes( tmax6_change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.precip.change <- ggplot(pct.change, aes( precip.change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())



# need to do this alsow for the later part of 20th century:
# see if drier places have higher % change in WUE or gwbi