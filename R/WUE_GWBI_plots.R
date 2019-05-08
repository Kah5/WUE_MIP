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
ED2.rm <- ED2[ED2$IWUE <= 500, ]
#ED2.rm <- ED2
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
                                                                                                      WUE.et.850.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                      WUE.t.850.1850 = mean(WUEt, na.rm=TRUE),
                                                                                                    GWBI.850.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                    MAP.wy.850.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                    Tmax_6.850.1850 = mean(tair_max_6, na.rm =TRUE),
                                                                                                    GPP.850.1850 = mean (GPP, na.rm=TRUE),
                                                                                                    ET.850.1850 = mean (ET, na.rm=TRUE),
                                                                                                    Transp.850.1850 = mean (Transp, na.rm=TRUE),
                                                                                                    Evap.850.1850 = mean (Evap, na.rm=TRUE))




mean.1690.1850 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1690 & Year <= 1849) %>% summarise(IWUE.1690.1850 = mean(IWUE, na.rm=TRUE),
                                                                                                                      WUE.et.1690.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                                      WUE.t.1690.1850 = mean(WUEt, na.rm=TRUE),
                                                                                                                      GWBI.1690.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1690.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1690.1850 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1690.1850 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1690.1850 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1690.1850 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1690.1850 = mean (Evap, na.rm=TRUE))





mean.1850.2011 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1850) %>% summarise(IWUE.1850.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                       WUE.et.1850.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1850.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                      GWBI.1850.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                      MAP.wy.1850.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                      Tmax_6.1850.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                      GPP.1850.2011 = mean (GPP, na.rm=TRUE),
                                                                                                      ET.1850.2011 = mean (ET, na.rm=TRUE),
                                                                                                      Transp.1850.2011 = mean (Transp, na.rm=TRUE),
                                                                                                      Evap.1850.2011 = mean (Evap, na.rm=TRUE))




mean.1950.2011 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1950) %>% summarise(IWUE.1950.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                       WUE.et.1950.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1950.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                       GWBI.1950.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1950.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1950.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1950.2011 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1950.2011 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1950.2011 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1950.2011 = mean (Evap, na.rm=TRUE))





ggplot(mean.850.1850, aes(MAP.wy.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(IWUE.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(Tmax_6.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(Tmax_6.850.1850, MAP.wy.850.1850, color = IWUE.850.1850))+geom_point()


time.periods <- left_join(mean.1950.2011, mean.850.1850, by = c("lat", "lon", "Site", "PFT"))

pct.change <- time.periods %>% group_by(lat, lon, Site, PFT) %>% summarise(IWUE.change = mean(((IWUE.1950.2011 - IWUE.850.1850)/IWUE.850.1850)*100, na.rm=TRUE),
                                                                           WUEet.change = mean(((WUE.et.1950.2011 - WUE.et.850.1850)/WUE.et.850.1850)*100, na.rm=TRUE),
                                                                           WUEt.change = mean(((WUE.t.1950.2011 - WUE.t.850.1850)/WUE.t.850.1850)*100, na.rm=TRUE),
                                                                           precip.change = mean(((MAP.wy.1950.2011 - MAP.wy.850.1850)/MAP.wy.850.1850)*100, na.rm=TRUE),
                                                                           tmax6_change = mean((Tmax_6.1950.2011 - Tmax_6.850.1850), na.rm=TRUE),
                                                                           GWBI.change = mean(((GWBI.1950.2011 - GWBI.850.1850)/GWBI.850.1850)*100, na.rm=TRUE),
                                                                           GPP.change = mean(((GPP.1950.2011 - GPP.850.1850)/GPP.850.1850)*100, na.rm=TRUE),
                                                                           ET.change = mean(((ET.1950.2011 - ET.850.1850)/ET.850.1850)*100, na.rm=TRUE),
                                                                           Transp.change = mean(((Transp.1950.2011 - Transp.850.1850)/Transp.850.1850)*100, na.rm=TRUE), 
                                                                           Evap.change = mean(((Evap.1950.2011 - Evap.850.1850)/Evap.850.1850)*100, na.rm=TRUE))


GWBI.WUE.change <- ggplot(pct.change, aes(IWUE.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEet.change <- ggplot(pct.change, aes(WUEet.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEt.change <- ggplot(pct.change, aes(WUEt.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

IWUE.precip.change <- ggplot(pct.change, aes(IWUE.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.precip.change <- ggplot(pct.change, aes(WUEet.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.precip.change <- ggplot(pct.change, aes(WUEt.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

IWUE.temp.change <- ggplot(pct.change, aes(IWUE.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.temp.change <- ggplot(pct.change, aes(WUEet.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.temp.change <- ggplot(pct.change, aes(WUEt.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())


GWBI.temp.change <- ggplot(pct.change, aes( tmax6_change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "Change in Summer Maximum temperatures (degC)")+ylab("% change in GWBI")
GWBI.precip.change <- ggplot(pct.change, aes( precip.change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "% Change MAP")+ylab("% change in GWBI")



# need to do this alsow for the full 20th century:
# see if drier places have higher % change in WUE or gwbi- needto merge again with site means:
pct.change.site <- left_join(all.met.summary, pct.change , by = c("lon", "lat"))

pct.change.site <- pct.change.site[!is.na(pct.change.site$PFT), ]

# plot relationships between GWBI and climate with significance. 
p.vals.precip = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ precip.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.precip.m <- melt(p.vals.precip)
p.vals.precip.m$PFT <- rownames(p.vals.precip.m)
p.vals.precip.m$sig <- ifelse(p.vals.precip.m$value <= 0.05, "*", "N.S" )

GWBI.precip.change.prec <- ggplot(pct.change.site, aes( precip.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+geom_text(data = p.vals.precip.m , aes(label = paste("p = ", value, " ", sig), x = 3, y = -75), color = "black")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_precip_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.precip.change.prec
dev.off()

# WUE GWBI correlations
p.vals.IWUE = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ IWUE.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.IWUE.m <- melt(p.vals.IWUE)
p.vals.IWUE.m$PFT <- rownames(p.vals.IWUE.m)
p.vals.IWUE.m$sig <- ifelse(p.vals.IWUE.m$value <= 0.05, "*", "N.S" )
p.vals.IWUE.m$value <- ifelse(p.vals.IWUE.m$value <= 0.0001, "0.00001", p.vals.IWUE.m$value )


GWBI.WUE.change.prec <- ggplot(pct.change.site, aes( IWUE.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUE.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUE")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_wue_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.WUE.change.prec
dev.off()


# WUEet GWBI correlations
p.vals.IWUEet = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ WUEet.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.IWUEet.m <- melt(p.vals.IWUEet)
p.vals.IWUEet.m$PFT <- rownames(p.vals.IWUEet.m)
p.vals.IWUEet.m$sig <- ifelse(p.vals.IWUEet.m$value <= 0.05, "*", "N.S" )
p.vals.IWUEet.m$value <- ifelse(p.vals.IWUEet.m$value <= 0.0001, "0.00001", p.vals.IWUEet.m$value )


GWBI.WUEet.change.prec <- ggplot(pct.change.site, aes( WUEet.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUEet.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUEet")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEet_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.WUEet.change.prec
dev.off()

# WUEt GWBI correlations
p.vals.IWUEt = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ WUEt.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.IWUEt.m <- melt(p.vals.IWUEt)
p.vals.IWUEt.m$PFT <- rownames(p.vals.IWUEt.m)
p.vals.IWUEt.m$sig <- ifelse(p.vals.IWUEt.m$value <= 0.05, "*", "N.S" )
p.vals.IWUEt.m$value <- ifelse(p.vals.IWUEt.m$value <= 0.0001, "0.00001", p.vals.IWUEt.m$value )


GWBI.WUEt.change.prec <- ggplot(pct.change.site, aes( WUEt.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUEt.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUEt")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEt_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.WUEt.change.prec
dev.off()

# tmax GWBI correlations
p.vals.tmax6 = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ tmax6_change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.tmax6.m <- melt(p.vals.tmax6)
p.vals.tmax6.m$PFT <- rownames(p.vals.tmax6.m)
p.vals.tmax6.m$sig <- ifelse(p.vals.tmax6.m$value <= 0.05, "*", "N.S" )
p.vals.tmax6.m$value <- ifelse(p.vals.tmax6.m$value <= 0.0001, "0.00001", p.vals.tmax6.m$value )

GWBI.tmax.change.prec <- ggplot(pct.change.site, aes( tmax6_change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_tmax_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.tmax.change.prec
dev.off()



# plot the correlation of IWUE with climate variables:
# for IWUE:
precip.iwue.lm <- summary(lm(precip.change ~ IWUE.change, data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
IWUE.precip.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( precip.change, IWUE.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in IWUE")+geom_text( label = paste("p = ", round(precip.iwue.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")


tmax.iwue.lm <- summary(lm(tmax6_change ~ IWUE.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
IWUE.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( tmax6_change, IWUE.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(tmax.iwue.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in IWUE")

# for WUEet
precip.WUEet.lm <- summary(lm(precip.change ~ WUEet.change, data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEet.precip.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( precip.change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in WUEet")+geom_text( label = paste("p = ", round(precip.WUEet.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")


tmax.WUEet.lm <- summary(lm(tmax6_change ~ WUEet.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEet.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( tmax6_change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(tmax.WUEet.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in WUEet")

# for WUEt
precip.WUEt.lm <- summary(lm(precip.change ~ WUEt.change, data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEt.precip.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( precip.change, WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in WUEt")+geom_text( label = paste("p = ", round(precip.WUEt.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")


tmax.WUEt.lm <- summary(lm(tmax6_change ~ WUEt.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEt.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( tmax6_change, WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(tmax.WUEt.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in WUEt")


tmax.precip.lm <- summary(lm(tmax6_change ~ precip.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
#p.vals.tmax6.m$value <- ifelse(p.vals.tmax6.m$value <= 0.0001, "0.00001", p.vals.tmax6.m$value )

precip.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( precip.change, tmax6_change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", tmax.precip.lm$coefficients[2,4], sep=""),x=2, y=0.025, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in precip")


# save these plots to png
library(cowplot)
legend.MAP <- get_legend(precip.tmax6.change.prec) 

png(height = 8, width = 10, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_wue_precip_tmax_850_1850_compared1950_2011.png")
plot_grid(plot_grid(precip.tmax6.change.prec+theme(legend.position = "none"), 
          IWUE.tmax6.change.prec+theme(legend.position = "none"), 
          IWUE.precip.change.prec+theme(legend.position = "none"), 
          WUEet.precip.change.prec+theme(legend.position = "none"),
          WUEt.precip.change.prec+theme(legend.position = "none"),ncol = 3),
          legend.MAP,ncol = 2, rel_widths = c(1,0.25))
dev.off()




pct <- pct.change.site[pct.change.site$PFT %in% "conifer.late",]



# plotting different WUE ests vs. Evap
# generate basic lm for the relationships
IWUE.evap.lm <- summary(lm(IWUE.change ~ Evap.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEet.evap.lm <- summary(lm(WUEet.change ~ Evap.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEt.evap.lm <- summary(lm(WUEt.change ~ Evap.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))

IWUE.evap.change.prec <- ggplot(pct, aes( Evap.change,IWUE.change,  color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(IWUE.evap.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Evap")+ylab("% change in IWUE")
WUEet.evap.change.prec <- ggplot(pct, aes(  Evap.change, WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.evap.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Evap")+ylab("% change in WUEet")
WUEt.evap.change.prec <- ggplot(pct, aes(  Evap.change, WUEt.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.evap.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Evap")+ylab("% change in WUEt")

# plotting different WUE ests vs. Transp
# generate basic lm for the relationships
IWUE.Transp.lm <- summary(lm(IWUE.change ~ Transp.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEet.Transp.lm <- summary(lm(WUEet.change ~ Transp.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEt.Transp.lm <- summary(lm(WUEt.change ~ Transp.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))

IWUE.Transp.change.prec <- ggplot(pct, aes(  Transp.change, IWUE.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(IWUE.Transp.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Transp")+ylab("% change in IWUE")
WUEet.Transp.change.prec <- ggplot(pct, aes( Transp.change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.Transp.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Transp")+ylab("% change in WUEet")
WUEt.Transp.change.prec <- ggplot(pct, aes(  Transp.change, WUEt.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.Transp.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Transp")+ylab("% change in WUEt")

# plotting different WUE ests vs. ET
# generate basic lm for the relationships
IWUE.ET.lm <- summary(lm(IWUE.change ~ ET.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEet.ET.lm <- summary(lm(WUEet.change ~ ET.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEt.ET.lm <- summary(lm(WUEt.change ~ ET.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))


IWUE.ET.change.prec <- ggplot(pct, aes(ET.change,  IWUE.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(IWUE.ET.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in ET")+ylab("% change in IWUE")
WUEet.ET.change.prec <- ggplot(pct, aes(  ET.change, WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.ET.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in ET")+ylab("% change in WUEet")
WUEt.ET.change.prec <- ggplot(pct, aes(ET.change,  WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.ET.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in ET")+ylab("% change in WUEt")

# plotting different WUE ests vs. GPP
IWUE.GPP.lm <- summary(lm(IWUE.change ~ GPP.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEet.GPP.lm <- summary(lm(WUEet.change ~ GPP.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
WUEt.GPP.lm <- summary(lm(WUEt.change ~ GPP.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))


IWUE.GPP.change.prec <- ggplot(pct, aes( GPP.change,  IWUE.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(IWUE.GPP.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in GPP")+ylab("% change in IWUE")
WUEet.GPP.change.prec <- ggplot(pct, aes(  GPP.change, WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.GPP.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in GPP")+ylab("% change in WUEet")
WUEt.GPP.change.prec <- ggplot(pct, aes( GPP.change, WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.GPP.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in GPP")+ylab("% change in WUEt")


# now plot all the wue vs other params all together:
legend.map <- get_legend(IWUE.GPP.change.prec)

png(height = 10, width = 10, res = 300, units = "in", "outputs/preliminaryplots/ED2_all_WUE_vs_GPP_ET_850_1850_1950_2011.png")
plot_grid(plot_grid(IWUE.GPP.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEet.GPP.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEt.GPP.change.prec+theme(legend.position = "none")+ylim(0,20), 
          
          IWUE.ET.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEet.ET.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEt.ET.change.prec+theme(legend.position = "none")+ylim(0,20),
          
          IWUE.evap.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEet.evap.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEt.evap.change.prec+theme(legend.position = "none")+ylim(0,20),
          
          IWUE.Transp.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEet.Transp.change.prec+theme(legend.position = "none")+ylim(0,20),
          WUEt.Transp.change.prec+theme(legend.position = "none")+ylim(0,20),
          ncol = 3), legend.map, ncol = 2, rel_widths = c(1,0.25))
dev.off()


# plot boxplots of IWUE change:
pct.melt <- pct %>% dplyr::select(lon, lat, Mean_MAP, Mean_MAP.wy, IWUE.change, WUEet.change, WUEt.change, GPP.change, ET.change, Transp.change, Evap.change) %>%
            group_by(lon, lat, Mean_MAP, Mean_MAP.wy) %>% gather(key = "variable", value = "value", IWUE.change:Evap.change)

pct.melt$variable <- factor(pct.melt$variable, levels = c("IWUE.change", "WUEet.change", "WUEt.change", "ET.change", "Evap.change", "Transp.change","GPP.change"))
ggplot(pct.melt, aes(variable, value, fill = variable))+geom_boxplot()+theme_bw(base_size = 10)+theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ---------------Make 3 dimensional plots to visualize multiple respones:------------
# 1. changes in WUE vs temperature and precip, colored by mean MAP
# 2. changes in IWUE vs changes in WUEet vs changes in WUEt vs temperature & precip, colored by MAP:
# 3. changes in IWUE vs changes in WUEet vs changes in WUEt vs GPP, ET.
# 4. changes in IWUE vs changes in WUEet vs changes in WUEt vs GPP, Evap.
# 5. changes in IWUE vs changes in WUEet vs changes in WUEt vs GPP, transp.
# 6. changes in GWBI vs temperature and precip, colored by mean MAP
# 7. changes in GWBI in respones to temperature, precip, colored by change in WUE


#library(plot3D)
library(car)
library(rgl)

# 1. changes in WUE vs temperature and precip, colored by mean MAP
pct <- pct.change.site[pct.change.site$PFT %in% "conifer.late",]
#rgl::plot3d( pct$precip.change, pct$IWUE.change,  pct$tmax6_change,)



library(scatterplot3d)
library(RColorBrewer)

# get colors for labeling the points
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaPrecip <- pct$precip.change
deltatmax <- pct$tmax6_change
deltaWUE <- pct$IWUE.change
deltaGWBI <- pct$GWBI.change

# scatter plot
plot.angle <- 65

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_wue_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in IWUE", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
            col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                             col="black", cex=1, side=3, line=1)) 
dev.off()

# 2. changes in IWUE vs changes in WUEet vs changes in WUEt vs temperature & precip, colored by MAP:
# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaPrecip <- pct$precip.change
deltatmax <- pct$tmax6_change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 65

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEet_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaPrecip <- pct$precip.change
deltatmax <- pct$tmax6_change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 65

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEt_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# 3. changes in IWUE vs changes in WUEet vs changes in WUEt vs GPP, ET.
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaET <- pct$ET.change
deltaWUE <- pct$IWUE.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUE_GPP_ET_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaET, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in ET", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaET <- pct$ET.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEet_GPP_ET_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaET, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in ET", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaET <- pct$ET.change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEt_GPP_ET_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaET, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in GPP", zlab = "% Change in ET", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# 4. changes in IWUE vs changes in WUEet vs changes in WUEt vs GPP, Evap.
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaEvap <- pct$Evap.change
deltaWUE <- pct$IWUE.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUE_GPP_Evap_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaEvap, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Evaporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaEvap <- pct$Evap.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEet_GPP_Evaporation_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaEvap, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Evaporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaEvap <- pct$Evap.change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEt_GPP_Evap_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaEvap, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in GPP", zlab = "% Change in Evaporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# 5. changes in IWUE vs changes in WUEet vs changes in WUEt vs GPP, transp.

plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaTransp <- pct$Transp.change
deltaWUE <- pct$IWUE.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUE_GPP_Transp_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaTransp, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Transporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaTransp <- pct$Transp.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEet_GPP_Transporation_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaTransp, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Transporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaTransp <- pct$Transp.change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEt_GPP_Transp_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaTransp, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in GPP", zlab = "% Change in Transporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# 6. changes in GWBI vs temperature and precip, colored by mean MAP

# will need to loop through each unique pft and make these plots:
PFT.list <- unique(pct.change.site$PFT)
for(i in 1:length(PFT.list)){
  
      pct <- pct.change.site[pct.change.site$PFT %in% PFT.list[i],]
      plotvar <- pct$Mean_MAP.wy # pick a variable to plot
      nclr <- 8 # number of colors
      plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
      colornum <- cut(rank(plotvar), nclr, labels=FALSE)
      colcode <- plotclr[colornum] # assign color
      deltaPrecip <- pct$precip.change
      deltatmax <- pct$tmax6_change
      deltaWUE <- pct$IWUE.change
      deltaGWBI <- pct$GWBI.change
      plot.angle <- 65
      
      png(height = 5.5, width = 6, units = "in", res = 300, paste("outputs/preliminaryplots/ED2_delta_",PFT.list[i],"_GWBI_precip_tmax_3D_850_1850_compared1950_2011.png"))
      
      scatterplot3d(deltaGWBI, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
                    col.axis="gray", col.grid="gray", xlab = paste("% change in", PFT.list[i], "GWBI"), ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 
      
      par(mar=c(5, 4, 4, 2) + 0.1) 
      fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                          col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                           col="black", cex=1, side=3, line=1)) 
      dev.off()

}

# 7. changes in GWBI in respones to temperature, precip, colored by change in WUE:

# will need to loop through each unique pft and make these plots:
PFT.list <- unique(pct.change.site$PFT)
for(i in 1:length(PFT.list)){
  
  pct <- pct.change.site[pct.change.site$PFT %in% PFT.list[i],]
  plotvar <- pct$IWUE.change # pick a variable to plot
  nclr <- 8 # number of colors
  plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
  colornum <- cut(rank(plotvar), nclr, labels=FALSE)
  colcode <- plotclr[colornum] # assign color
  deltaPrecip <- pct$precip.change
  deltatmax <- pct$tmax6_change
  deltaWUE <- pct$IWUE.change
  deltaGWBI <- pct$GWBI.change
  plot.angle <- 65
  
  png(height = 5.5, width = 6, units = "in", res = 300, paste("outputs/preliminaryplots/ED2_delta_",PFT.list[i],"_GWBI_precip_tmax_col_by_WUE_3D_850_1850_compared1950_2011.png"))
  
  scatterplot3d(deltaGWBI, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
                col.axis="gray", col.grid="gray", xlab = paste("% change in", PFT.list[i], "GWBI"), ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 
  
  par(mar=c(5, 4, 4, 2) + 0.1) 
  fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                      col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="% change in IWUE",
                                                                       col="black", cex=1, side=3, line=1)) 
  dev.off()
  
}



#--------------------Run the same analyses for GUESS---------------------------------
GUESS.gwbi.clim.nona <- readRDS("Data/GUESS_gwbi_pft_clim.rds")

ggplot(GUESS.gwbi.clim.nona, aes(Year, GWBI, color = Site))+geom_point()+facet_wrap(~PFT)

# read in dataframe with WUE

WUE.dens.dfs <- readRDS("outputs/data/GUESS/GUESS.alldat.yrmeans.rds")
WUE.dens.dfs$Site <- substring(WUE.dens.dfs$Site, 2)

GUESS <- left_join(WUE.dens.dfs, GUESS.gwbi.clim.nona, by = c("Site", "Year"))

hist(GUESS$WUEet)
# remove the outliers:
# remove the outliers:
GUESS.rm <- GUESS[GUESS$WUEet <= 500, ]
#GUESS.rm <- GUESS
ggplot(GUESS.rm, aes(Tair.C, WUEet))+geom_point()

GUESS.rm <- GUESS.rm[!is.na(GUESS.rm$PFT), ]

ggplot(GUESS.rm, aes(WUEet, Rel.Dens))+geom_point(size = 0.5)
 GUESS.WUEet.GWBI.PFT <- ggplot(GUESS.rm, aes(WUEet, GWBI, color = PFT))+geom_point(size = 0.5)+facet_wrap(~PFT)
 
 png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/GUESS_GWBI_WUEet_by_pft.png")
 GUESS.WUEet.GWBI.PFT
 dev.off()

# plot out effects of WUEet, climate on GWBI:


GUESS.precip.GWBI.PFT <- ggplot(GUESS.rm, aes( precip_total.mm, GWBI,color = PFT))+geom_point(size = 0.5)+stat_smooth(color = "black")+facet_wrap(~PFT)

png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/GUESS_GWBI_precip_by_pft.png")
GUESS.precip.GWBI.PFT
dev.off()





GUESS.tmax.GWBI.PFT <- ggplot(GUESS.rm, aes( tair_max_6, GWBI,color = PFT))+geom_point(size = 0.5)+stat_smooth(color = "black")+facet_wrap(~PFT)

png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/GUESS_GWBI_tmax_by_pft.png")
GUESS.tmax.GWBI.PFT
dev.off()

# do places with lower precipitation have higher increases in WUE? & which species does this benefit?


all.met <- readRDS( paste0(getwd(),"/Data/MET/all.met.summary.rds"))
# merge climate and growth for GUESS:
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

GUESS.rm.site <- left_join(all.met.summary, GUESS.rm , by = c("lon", "lat"))

ggplot(GUESS.rm.site[GUESS.rm.site$Site %in% "1",], aes(Year, WUEet, color = Mean_tair_max_6))+geom_point()+geom_line()+stat_smooth()

# get the change in %WUE over between 850-1850 and 1950-present
mean.850.1850 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1849) %>% summarise(#WUEet.850.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                      WUE.et.850.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                      WUE.t.850.1850 = mean(WUEt, na.rm=TRUE),
                                                                                                      GWBI.850.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                      MAP.wy.850.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                      Tmax_6.850.1850 = mean(tair_max_6, na.rm =TRUE),
                                                                                                      GPP.850.1850 = mean (GPP, na.rm=TRUE),
                                                                                                      ET.850.1850 = mean (ET, na.rm=TRUE),
                                                                                                      Transp.850.1850 = mean (Transp, na.rm=TRUE),
                                                                                                      Evap.850.1850 = mean (Evap, na.rm=TRUE))




mean.1690.1850 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1690 & Year <= 1849) %>% summarise(#WUEet.1690.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                                      WUE.et.1690.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                                      WUE.t.1690.1850 = mean(WUEt, na.rm=TRUE),
                                                                                                                      GWBI.1690.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                                      MAP.wy.1690.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                                      Tmax_6.1690.1850 = mean(tair_max_6, na.rm =TRUE),
                                                                                                                      GPP.1690.1850 = mean (GPP, na.rm=TRUE),
                                                                                                                      ET.1690.1850 = mean (ET, na.rm=TRUE),
                                                                                                                      Transp.1690.1850 = mean (Transp, na.rm=TRUE),
                                                                                                                      Evap.1690.1850 = mean (Evap, na.rm=TRUE))





mean.1850.2011 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1850) %>% summarise(#WUEet.1850.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.et.1850.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1850.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                       GWBI.1850.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1850.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1850.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1850.2011 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1850.2011 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1850.2011 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1850.2011 = mean (Evap, na.rm=TRUE))




mean.1950.2011 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1950) %>% summarise(#WUEet.1950.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.et.1950.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1950.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                       GWBI.1950.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1950.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1950.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1950.2011 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1950.2011 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1950.2011 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1950.2011 = mean (Evap, na.rm=TRUE))





ggplot(mean.850.1850, aes(MAP.wy.850.1850, GWBI.850.1850, color = PFT))+geom_point()
#ggplot(mean.850.1850, aes(WUEet.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(Tmax_6.850.1850, GWBI.850.1850, color = PFT))+geom_point()
ggplot(mean.850.1850, aes(Tmax_6.850.1850, MAP.wy.850.1850, color = WUEet.850.1850))+geom_point()


time.periods <- left_join(mean.1950.2011, mean.850.1850, by = c("lat", "lon", "Site", "PFT"))

pct.change <- time.periods %>% group_by(lat, lon, Site, PFT) %>% summarise(#WUEet.change = mean(((WUEet.1950.2011 - WUEet.850.1850)/WUEet.850.1850)*100, na.rm=TRUE),
                                                                           WUEet.change = mean(((WUE.et.1950.2011 - WUE.et.850.1850)/WUE.et.850.1850)*100, na.rm=TRUE),
                                                                           WUEt.change = mean(((WUE.t.1950.2011 - WUE.t.850.1850)/WUE.t.850.1850)*100, na.rm=TRUE),
                                                                           precip.change = mean(((MAP.wy.1950.2011 - MAP.wy.850.1850)/MAP.wy.850.1850)*100, na.rm=TRUE),
                                                                           tmax6_change = mean((Tmax_6.1950.2011 - Tmax_6.850.1850), na.rm=TRUE),
                                                                           GWBI.change = mean(((GWBI.1950.2011 - GWBI.850.1850)/GWBI.850.1850)*100, na.rm=TRUE),
                                                                           GPP.change = mean(((GPP.1950.2011 - GPP.850.1850)/GPP.850.1850)*100, na.rm=TRUE),
                                                                           ET.change = mean(((ET.1950.2011 - ET.850.1850)/ET.850.1850)*100, na.rm=TRUE),
                                                                           Transp.change = mean(((Transp.1950.2011 - Transp.850.1850)/Transp.850.1850)*100, na.rm=TRUE), 
                                                                           Evap.change = mean(((Evap.1950.2011 - Evap.850.1850)/Evap.850.1850)*100, na.rm=TRUE))


#GWBI.WUE.change <- ggplot(pct.change, aes(WUEet.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEet.change <- ggplot(pct.change, aes(WUEet.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEt.change <- ggplot(pct.change, aes(WUEt.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

#WUEet.precip.change <- ggplot(pct.change, aes(WUEet.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.precip.change <- ggplot(pct.change, aes(WUEet.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.precip.change <- ggplot(pct.change, aes(WUEt.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

#WUEet.temp.change <- ggplot(pct.change, aes(WUEet.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.temp.change <- ggplot(pct.change, aes(WUEet.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.temp.change <- ggplot(pct.change, aes(WUEt.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())


GWBI.temp.change <- ggplot(pct.change, aes( tmax6_change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "Change in Summer Maximum temperatures (degC)")+ylab("% change in GWBI")
GWBI.precip.change <- ggplot(pct.change, aes( precip.change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "% Change MAP")+ylab("% change in GWBI")



# need to do this alsow for the full 20th century:
# see if drier places have higher % change in WUE or gwbi- needto merge again with site means:
pct.change.site <- left_join(all.met.summary, pct.change , by = c("lon", "lat"))

pct.change.site <- pct.change.site[!is.na(pct.change.site$PFT), ]

# plot relationships between GWBI and climate with significance. 
p.vals.precip = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ precip.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.precip.m <- melt(p.vals.precip)
p.vals.precip.m$PFT <- rownames(p.vals.precip.m)
p.vals.precip.m$sig <- ifelse(p.vals.precip.m$value <= 0.05, "*", "N.S" )

GWBI.precip.change.prec <- ggplot(pct.change.site, aes( precip.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+geom_text(data = p.vals.precip.m , aes(label = paste("p = ", value, " ", sig), x = 3, y = -75), color = "black")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_precip_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.precip.change.prec
dev.off()

# WUE GWBI correlations
# p.vals.WUEet = sapply(unique(pct.change.site$PFT), function(i) {
#   round(coef(summary(lm(GWBI.change ~ WUEet.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
# })
# 
# p.vals.WUEet.m <- melt(p.vals.WUEet)
# p.vals.WUEet.m$PFT <- rownames(p.vals.WUEet.m)
# p.vals.WUEet.m$sig <- ifelse(p.vals.WUEet.m$value <= 0.05, "*", "N.S" )
# p.vals.WUEet.m$value <- ifelse(p.vals.WUEet.m$value <= 0.0001, "0.00001", p.vals.WUEet.m$value )
# 
# 
# GWBI.WUE.change.prec <- ggplot(pct.change.site, aes( WUEet.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
#   geom_text(data = p.vals.WUEet.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
#   facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
#   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEet")+ylab("% change in GWBI")
# 
# png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_wue_gwbi_by_pft_850_1850_compared1950_2011.png")
# GWBI.WUE.change.prec
# dev.off()


# WUEet GWBI correlations
p.vals.WUEetet = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ WUEet.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.WUEetet.m <- melt(p.vals.WUEetet)
p.vals.WUEetet.m$PFT <- rownames(p.vals.WUEetet.m)
p.vals.WUEetet.m$sig <- ifelse(p.vals.WUEetet.m$value <= 0.05, "*", "N.S" )
p.vals.WUEetet.m$value <- ifelse(p.vals.WUEetet.m$value <= 0.0001, "0.00001", p.vals.WUEetet.m$value )


GWBI.WUEet.change.prec <- ggplot(pct.change.site, aes( WUEet.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.WUEetet.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEetet")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEet_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.WUEet.change.prec
dev.off()

# WUEt GWBI correlations
p.vals.WUEett = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ WUEt.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.WUEett.m <- melt(p.vals.WUEett)
p.vals.WUEett.m$PFT <- rownames(p.vals.WUEett.m)
p.vals.WUEett.m$sig <- ifelse(p.vals.WUEett.m$value <= 0.05, "*", "N.S" )
p.vals.WUEett.m$value <- ifelse(p.vals.WUEett.m$value <= 0.0001, "0.00001", p.vals.WUEett.m$value )


GWBI.WUEt.change.prec <- ggplot(pct.change.site, aes( WUEt.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.WUEett.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEett")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEt_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.WUEt.change.prec
dev.off()

# tmax GWBI correlations
p.vals.tmax6 = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ tmax6_change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.tmax6.m <- melt(p.vals.tmax6)
p.vals.tmax6.m$PFT <- rownames(p.vals.tmax6.m)
p.vals.tmax6.m$sig <- ifelse(p.vals.tmax6.m$value <= 0.05, "*", "N.S" )
p.vals.tmax6.m$value <- ifelse(p.vals.tmax6.m$value <= 0.0001, "0.00001", p.vals.tmax6.m$value )

GWBI.tmax.change.prec <- ggplot(pct.change.site, aes( tmax6_change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_tmax_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.tmax.change.prec
dev.off()



# plot the correlation of WUEet with climate variables:
# for WUEet:
# precip.iwue.lm <- summary(lm(precip.change ~ WUEet.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
# WUEet.precip.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( precip.change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
#   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in WUEet")+geom_text( label = paste("p = ", round(precip.iwue.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")


# tmax.iwue.lm <- summary(lm(tmax6_change ~ WUEet.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
# WUEet.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( tmax6_change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
#   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(tmax.iwue.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in WUEet")

# for WUEet
precip.WUEet.lm <- summary(lm(precip.change ~ WUEet.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEet.precip.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( precip.change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in WUEet")+geom_text( label = paste("p = ", round(precip.WUEet.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")


tmax.WUEet.lm <- summary(lm(tmax6_change ~ WUEet.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEet.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( tmax6_change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(tmax.WUEet.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in WUEet")

# for WUEt
precip.WUEt.lm <- summary(lm(precip.change ~ WUEt.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEt.precip.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( precip.change, WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in WUEt")+geom_text( label = paste("p = ", round(precip.WUEt.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")


tmax.WUEt.lm <- summary(lm(tmax6_change ~ WUEt.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEt.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( tmax6_change, WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(tmax.WUEt.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in WUEt")


tmax.precip.lm <- summary(lm(tmax6_change ~ precip.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
#p.vals.tmax6.m$value <- ifelse(p.vals.tmax6.m$value <= 0.0001, "0.00001", p.vals.tmax6.m$value )

precip.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( precip.change, tmax6_change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", tmax.precip.lm$coefficients[2,4], sep=""),x=2, y=0.025, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in precip")


# save these plots to png
library(cowplot)
legend.MAP <- get_legend(precip.tmax6.change.prec) 

png(height = 6, width = 10, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_wue_precip_tmax_850_1850_compared1950_2011.png")
plot_grid(plot_grid(precip.tmax6.change.prec+theme(legend.position = "none"), 
                    #WUEet.tmax6.change.prec+theme(legend.position = "none"), 
                    #WUEet.precip.change.prec+theme(legend.position = "none"), 
                    WUEet.precip.change.prec+theme(legend.position = "none"),
                    WUEt.precip.change.prec+theme(legend.position = "none"),ncol = 3),
          legend.MAP,ncol = 2, rel_widths = c(1,0.25))
dev.off()




pct <- pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]



# plotting different WUE ests vs. Evap
# generate basic lm for the relationships
#WUEet.evap.lm <- summary(lm(WUEet.change ~ Evap.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEet.evap.lm <- summary(lm(WUEet.change ~ Evap.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEt.evap.lm <- summary(lm(WUEt.change ~ Evap.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

#WUEet.evap.change.prec <- ggplot(pct, aes( Evap.change,WUEet.change,  color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
 # scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.evap.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Evap")+ylab("% change in WUEet")
WUEet.evap.change.prec <- ggplot(pct, aes(  Evap.change, WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.evap.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Evap")+ylab("% change in WUEet")
WUEt.evap.change.prec <- ggplot(pct, aes(  Evap.change, WUEt.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.evap.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in Evap")+ylab("% change in WUEt")

# plotting different WUE ests vs. Transp
# generate basic lm for the relationships
#WUEet.Transp.lm <- summary(lm(WUEet.change ~ Transp.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEet.Transp.lm <- summary(lm(WUEet.change ~ Transp.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEt.Transp.lm <- summary(lm(WUEt.change ~ Transp.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

#WUEet.Transp.change.prec <- ggplot(pct, aes(  Transp.change, WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
 # scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.Transp.lm$coefficients[2,4], 4), sep=""),x=0, y=0.025, color = "black")+xlab("% Change in Transp")+ylab("% change in WUEet")
WUEet.Transp.change.prec <- ggplot(pct, aes( Transp.change, WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.Transp.lm$coefficients[2,4], 4), sep=""),x=0, y=0.025, color = "black")+xlab("% Change in Transp")+ylab("% change in WUEet")
WUEt.Transp.change.prec <- ggplot(pct, aes(  Transp.change, WUEt.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.Transp.lm$coefficients[2,4], 4), sep=""),x=0, y=0.025, color = "black")+xlab("% Change in Transp")+ylab("% change in WUEt")

# plotting different WUE ests vs. ET
# generate basic lm for the relationships
#WUEet.ET.lm <- summary(lm(WUEet.change ~ ET.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEet.ET.lm <- summary(lm(WUEet.change ~ ET.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEt.ET.lm <- summary(lm(WUEt.change ~ ET.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))


#WUEet.ET.change.prec <- ggplot(pct, aes(ET.change,  WUEet.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
 # scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.ET.lm$coefficients[2,4], 4), sep=""),x=0, y=0.025, color = "black")+xlab("% Change in ET")+ylab("% change in WUEet")
WUEet.ET.change.prec <- ggplot(pct, aes(  ET.change, WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.ET.lm$coefficients[2,4], 4), sep=""),x=0, y=0.025, color = "black")+xlab("% Change in ET")+ylab("% change in WUEet")
WUEt.ET.change.prec <- ggplot(pct, aes(ET.change,  WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.ET.lm$coefficients[2,4], 4), sep=""),x=0, y=0.025, color = "black")+xlab("% Change in ET")+ylab("% change in WUEt")

# plotting different WUE ests vs. GPP
#WUEet.GPP.lm <- summary(lm(WUEet.change ~ GPP.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEet.GPP.lm <- summary(lm(WUEet.change ~ GPP.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))
WUEt.GPP.lm <- summary(lm(WUEt.change ~ GPP.change,data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))


#WUEet.GPP.change.prec <- ggplot(pct, aes( GPP.change,  WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
 # scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.GPP.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in GPP")+ylab("% change in WUEet")
WUEet.GPP.change.prec <- ggplot(pct, aes(  GPP.change, WUEet.change,color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEet.GPP.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in GPP")+ylab("% change in WUEet")
WUEt.GPP.change.prec <- ggplot(pct, aes( GPP.change, WUEt.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(WUEt.GPP.lm$coefficients[2,4], 4), sep=""),x=10, y=0.025, color = "black")+xlab("% Change in GPP")+ylab("% change in WUEt")


# now plot all the wue vs other params all together:
legend.map <- get_legend(WUEet.GPP.change.prec)

png(height = 10, width = 7, res = 300, units = "in", "outputs/preliminaryplots/GUESS_all_WUE_vs_GPP_ET_850_1850_1950_2011.png")
plot_grid(plot_grid(#WUEet.GPP.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEet.GPP.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEt.GPP.change.prec+theme(legend.position = "none")+ylim(0,20), 
                    
                    #WUEet.ET.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEet.ET.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEt.ET.change.prec+theme(legend.position = "none")+ylim(0,20),
                    
                    #WUEet.evap.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEet.evap.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEt.evap.change.prec+theme(legend.position = "none")+ylim(0,20),
                    
                    #WUEet.Transp.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEet.Transp.change.prec+theme(legend.position = "none")+ylim(0,20),
                    WUEt.Transp.change.prec+theme(legend.position = "none")+ylim(0,20),
                    ncol = 2), legend.map, ncol = 2, rel_widths = c(1,0.25))
dev.off()


# plot boxplots of WUEet change:
pct.melt <- pct %>% dplyr::select(lon, lat, Mean_MAP, Mean_MAP.wy, WUEet.change, WUEt.change, GPP.change, ET.change, Transp.change, Evap.change) %>%
  group_by(lon, lat, Mean_MAP, Mean_MAP.wy) %>% gather(key = "variable", value = "value", WUEet.change:Evap.change)

pct.melt$variable <- factor(pct.melt$variable, levels = c( "WUEet.change", "WUEt.change", "ET.change", "Evap.change", "Transp.change","GPP.change"))
ggplot(pct.melt, aes(variable, value, fill = variable))+geom_boxplot()+theme_bw(base_size = 10)+theme(axis.text.x = element_text(angle = 45, hjust = 1))



# ---------------Make 3 dimensional plots to visualize multiple respones:------------
# 1. changes in WUE vs temperature and precip, colored by mean MAP
# 2. changes in WUEet vs changes in WUEet vs changes in WUEt vs temperature & precip, colored by MAP:
# 3. changes in WUEet vs changes in WUEet vs changes in WUEt vs GPP, ET.
# 4. changes in WUEet vs changes in WUEet vs changes in WUEt vs GPP, Evap.
# 5. changes in WUEet vs changes in WUEet vs changes in WUEt vs GPP, transp.
# 6. changes in GWBI vs temperature and precip, colored by mean MAP
# 7. changes in GWBI in respones to temperature, precip, colored by change in WUE


#library(plot3D)
library(car)
library(rgl)

# 1. changes in WUE vs temperature and precip, colored by mean MAP
pct <- pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]
#rgl::plot3d( pct$precip.change, pct$WUEet.change,  pct$tmax6_change,)



library(scatterplot3d)
library(RColorBrewer)

# get colors for labeling the points
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaPrecip <- pct$precip.change
deltatmax <- pct$tmax6_change
deltaWUE <- pct$WUEet.change
deltaGWBI <- pct$GWBI.change

# scatter plot
plot.angle <- 65

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_wue_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# 2. changes in WUEet vs changes in WUEet vs changes in WUEt vs temperature & precip, colored by MAP:
# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaPrecip <- pct$precip.change
deltatmax <- pct$tmax6_change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 65

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEet_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaPrecip <- pct$precip.change
deltatmax <- pct$tmax6_change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 65

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEt_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# 3. changes in WUEet vs changes in WUEet vs changes in WUEt vs GPP, ET.
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaET <- pct$ET.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUE_GPP_ET_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaET, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in ET", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaET <- pct$ET.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEet_GPP_ET_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaET, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in ET", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaET <- pct$ET.change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEt_GPP_ET_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaET, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in GPP", zlab = "% Change in ET", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# 4. changes in WUEet vs changes in WUEet vs changes in WUEt vs GPP, Evap.
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaEvap <- pct$Evap.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUE_GPP_Evap_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaEvap, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Evaporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaEvap <- pct$Evap.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEet_GPP_Evaporation_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaEvap, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Evaporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaEvap <- pct$Evap.change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEt_GPP_Evap_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaEvap, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in GPP", zlab = "% Change in Evaporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# 5. changes in WUEet vs changes in WUEet vs changes in WUEt vs GPP, transp.

plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaTransp <- pct$Transp.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUE_GPP_Transp_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaTransp, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Transporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()

# same as 1, but WUE == WUEet
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaTransp <- pct$Transp.change
deltaWUE <- pct$WUEet.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEet_GPP_Transporation_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaTransp, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in GPP", zlab = "% Change in Transporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# same as 1, but WUE == WUEt
plotvar <- pct$Mean_MAP.wy # pick a variable to plot
nclr <- 8 # number of colors
plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
colornum <- cut(rank(plotvar), nclr, labels=FALSE)
colcode <- plotclr[colornum] # assign color
deltaGPP <- pct$GPP.change
deltaTransp <- pct$Transp.change
deltaWUE <- pct$WUEt.change


# scatter plot
plot.angle <- 55

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEt_GPP_Transp_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaGPP, deltaTransp, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEt", ylab = "% change in GPP", zlab = "% Change in Transporation", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()


# 6. changes in GWBI vs temperature and precip, colored by mean MAP

# will need to loop through each unique pft and make these plots:
PFT.list <- unique(pct.change.site$PFT)
for(i in 1:length(PFT.list)){
  
  pct <- pct.change.site[pct.change.site$PFT %in% PFT.list[i],]
  plotvar <- pct$Mean_MAP.wy # pick a variable to plot
  nclr <- 8 # number of colors
  plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
  colornum <- cut(rank(plotvar), nclr, labels=FALSE)
  colcode <- plotclr[colornum] # assign color
  deltaPrecip <- pct$precip.change
  deltatmax <- pct$tmax6_change
  deltaWUE <- pct$WUEet.change
  deltaGWBI <- pct$GWBI.change
  plot.angle <- 65
  
  png(height = 5.5, width = 6, units = "in", res = 300, paste("outputs/preliminaryplots/GUESS_delta_",PFT.list[i],"_GWBI_precip_tmax_3D_850_1850_compared1950_2011.png"))
  
  scatterplot3d(deltaGWBI, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
                col.axis="gray", col.grid="gray", xlab = paste("% change in", PFT.list[i], "GWBI"), ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 
  
  par(mar=c(5, 4, 4, 2) + 0.1) 
  fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                      col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                       col="black", cex=1, side=3, line=1)) 
  dev.off()
  
}

# 7. changes in GWBI in respones to temperature, precip, colored by change in WUE:

# will need to loop through each unique pft and make these plots:
PFT.list <- unique(pct.change.site$PFT)
for(i in 1:length(PFT.list)){
  
  pct <- pct.change.site[pct.change.site$PFT %in% PFT.list[i],]
  plotvar <- pct$WUEet.change # pick a variable to plot
  nclr <- 8 # number of colors
  plotclr <- brewer.pal(nclr,"RdYlBu") # get the colors
  colornum <- cut(rank(plotvar), nclr, labels=FALSE)
  colcode <- plotclr[colornum] # assign color
  deltaPrecip <- pct$precip.change
  deltatmax <- pct$tmax6_change
  deltaWUE <- pct$WUEet.change
  deltaGWBI <- pct$GWBI.change
  plot.angle <- 65
  
  png(height = 5.5, width = 6, units = "in", res = 300, paste("outputs/preliminaryplots/GUESS_delta_",PFT.list[i],"_GWBI_precip_tmax_col_by_WUE_3D_850_1850_compared1950_2011.png"))
  
  scatterplot3d(deltaGWBI, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
                col.axis="gray", col.grid="gray", xlab = paste("% change in", PFT.list[i], "GWBI"), ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 
  
  par(mar=c(5, 4, 4, 2) + 0.1) 
  fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                      col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="% change in WUEet",
                                                                       col="black", cex=1, side=3, line=1)) 
  dev.off()
  
}
