# some basic GWBI visualizations:
library(tidyr)
library(ggplot2)
library(dplyr)
library(data.table)
ED2.gwbi.clim.nona <- readRDS("Data/ED2_gwbi_pft_clim.rds")

# does growth increase overall?


#ggplot(ED2.gwbi.clim.nona, aes(Year, GWBI, color = Site))+geom_point()+facet_wrap(~PFT)

# read in dataframe with WUE
WUE.dens.dfs <- readRDS("outputs/data/ED2/dens_agbi_climate_ED2.rds")
WUE.dens.dfs$Site <- substring(WUE.dens.dfs$Site, 2)

ED2 <- left_join(WUE.dens.dfs, ED2.gwbi.clim.nona, by = c("Site", "Year"))

# read in Fcomp for ED2:
ED2.fcomp <- readRDS( "outputs/data/ED2/ED2_mean_yearly_fcomp.rds")
ED2.fcomp.yr <- ED2.fcomp %>% group_by(Year, Site) %>% gather(key = "PFT", value = "Fcomp",grass.c4:Araucaria)
#ED2.fcomp.yr <-  ED2.fcomp.yr %>% filter(PFT %in% unique(ED2.gwbi.clim.nona$PFT))
ED2.fcomp.yr$Site <- as.character(ED2.fcomp.yr$Site)

 #png(height = 26, width = 16, units = "in", res = 300, "outputs/preliminaryplots/ED2_GPP_vs_ET.png")
 #ggplot(na.omit(ED2), aes(GPP, ET, color = Year))+geom_point()+stat_smooth()+facet_wrap(~Site)#+xlim(0,0.0075)+ylim(0,0.000075)#
 #dev.off()

ED2.full <- left_join(ED2, ED2.fcomp.yr, by = c("Year", "Site", "PFT"))


# remove the outliers:
ED2.rm <- ED2.full[ED2.full$IWUE <= 500, ]
#ED2.rm <- ED2
#ggplot(ED2.rm, aes(Tair.C, IWUE))+geom_point()

ED2.rm <- ED2.rm[!is.na(ED2.rm$PFT), ]

#ggplot(ED2.rm, aes(IWUE, Rel.Dens))+geom_point(size = 0.5)
ED2.IWUE.GWBI.PFT <- ggplot(ED2.rm, aes(IWUE, GWBI, color = PFT))+geom_point(size = 0.5)+facet_wrap(~PFT)

#png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/ED2_GWBI_IWUE_by_pft.png")
#ED2.IWUE.GWBI.PFT
#dev.off()

# plot out effects of IWUE, climate on GWBI:


ED2.precip.GWBI.PFT <- ggplot(ED2.rm, aes( precip_total.mm, GWBI,color = PFT))+geom_point(size = 0.5)+stat_smooth(color = "black")+facet_wrap(~PFT)

#png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/ED2_GWBI_precip_by_pft.png")
#ED2.precip.GWBI.PFT
#dev.off()





ED2.tmax.GWBI.PFT <- ggplot(ED2.rm, aes( tair_max_6, GWBI,color = PFT))+geom_point(size = 0.5)+stat_smooth(color = "black")+facet_wrap(~PFT)

# png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/ED2_GWBI_tmax_by_pft.png")
# ED2.tmax.GWBI.PFT
# dev.off()

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

#ggplot(all.met.summary, aes(lon, lat, fill = Mean_MAP.wy))+geom_raster()
#ggplot(all.met.summary, aes(lon, lat, fill = Mean_tair_max_6))+geom_raster()


# join the site means to the sites:

ED2.rm.site <- left_join(all.met.summary, ED2.rm , by = c("lon", "lat"))

#ggplot(ED2.rm.site[ED2.rm.site$Site %in% "5",], aes(Year, WUEet, color = Mean_tair_max_6))+geom_point()+geom_line()+stat_smooth()

# for ED, remove WUE values that are over 
ED2.rm.site<- ED2.rm.site %>% filter(WUEet >= 0  & WUEet <=100)
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
                                                                                                    Evap.850.1850 = mean (Evap, na.rm=TRUE),
                                                                                                    Fcomp.850.1850 = mean(Fcomp, na.rm =TRUE),
                                                                                                    Dens.850.1850 = mean(Dens, na.rm =TRUE), 
                                                                                                    GS_agb.850.1850 =mean(GS_agb, na.rm = TRUE),
                                                                                                    AGB.850.1850 = mean(AGB, na.rm = TRUE), 
                                                                                                    LAI.850.1850 = mean(LAI, na.rm = TRUE))




mean.1690.1850 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1690 & Year <= 1849) %>% summarise(IWUE.1690.1850 = mean(IWUE, na.rm=TRUE),
                                                                                                                      WUE.et.1690.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                                      WUE.t.1690.1850 = mean(WUEt, na.rm=TRUE),
                                                                                                                      GWBI.1690.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1690.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1690.1850 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1690.1850 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1690.1850 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1690.1850 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1690.1850 = mean (Evap, na.rm=TRUE),
                                                                                                       Fcomp.1690.1850 = mean(Fcomp, na.rm =TRUE),
                                                                                                       Dens.1690.1850 = mean(Dens, na.rm =TRUE), 
                                                                                                       GS_agb.1690.1850 =mean(GS_agb, na.rm = TRUE),
                                                                                                       AGB.1690.1850 = mean(AGB, na.rm = TRUE), 
                                                                                                       LAI.1690.1850 = mean(LAI, na.rm = TRUE))





mean.1850.2011 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1850) %>% summarise(IWUE.1850.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                       WUE.et.1850.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1850.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                      GWBI.1850.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                      MAP.wy.1850.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                      Tmax_6.1850.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                      GPP.1850.2011 = mean (GPP, na.rm=TRUE),
                                                                                                      ET.1850.2011 = mean (ET, na.rm=TRUE),
                                                                                                      Transp.1850.2011 = mean (Transp, na.rm=TRUE),
                                                                                                      Evap.1850.2011 = mean (Evap, na.rm=TRUE),
                                                                                                      Fcomp.1850.2011 = mean(Fcomp, na.rm =TRUE),
                                                                                                      Dens.1850.2011 = mean(Dens, na.rm =TRUE), 
                                                                                                      GS_agb.1850.2011 =mean(GS_agb, na.rm = TRUE),
                                                                                                      AGB.1850.2011 = mean(AGB, na.rm = TRUE), 
                                                                                                      LAI.1850.2011 = mean(LAI, na.rm = TRUE))




mean.1950.2011 <- ED2.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1950) %>% summarise(IWUE.1950.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                       WUE.et.1950.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1950.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                       GWBI.1950.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1950.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1950.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1950.2011 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1950.2011 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1950.2011 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1950.2011 = mean (Evap, na.rm=TRUE),
                                                                                                       Fcomp.1950.2011 = mean(Fcomp, na.rm =TRUE),
                                                                                                       Dens.1950.2011 = mean(Dens, na.rm =TRUE), 
                                                                                                       GS_agb.1950.2011 =mean(GS_agb, na.rm = TRUE),
                                                                                                       AGB.1950.2011 = mean(AGB, na.rm = TRUE), 
                                                                                                       LAI.1950.2011 = mean(LAI, na.rm = TRUE))





#ggplot(mean.850.1850, aes(MAP.wy.850.1850, Dens.850.1850))+geom_point()
# ggplot(mean.850.1850, aes(IWUE.850.1850, GWBI.850.1850, color = PFT))+geom_point()
# ggplot(mean.850.1850, aes(Tmax_6.850.1850, GWBI.850.1850, color = PFT))+geom_point()
# ggplot(mean.850.1850, aes(Tmax_6.850.1850, MAP.wy.850.1850, color = IWUE.850.1850))+geom_point()


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
                                                                           Evap.change = mean(((Evap.1950.2011 - Evap.850.1850)/Evap.850.1850)*100, na.rm=TRUE),
                                                                           Fcomp.change = mean(((Fcomp.1950.2011 - Fcomp.850.1850)/Fcomp.850.1850)*100, na.rm=TRUE),
                                                                           Dens.change = mean(((Dens.1950.2011 - Dens.850.1850)/Dens.850.1850)*100, na.rm=TRUE),
                                                                           GS_agb.change = mean(((GS_agb.1950.2011 - GS_agb.850.1850)/GS_agb.850.1850)*100, na.rm=TRUE),
                                                                           AGB.change = mean(((AGB.1950.2011 - AGB.850.1850)/AGB.850.1850)*100, na.rm=TRUE),
                                                                           LAI.change = mean(((LAI.1950.2011 - LAI.850.1850)/LAI.850.1850)*100, na.rm=TRUE),
                                                                           AGB.orig = mean(AGB.850.1850, na.rm = TRUE))

ggplot(pct.change, aes(PFT, GWBI.change, fill = PFT))+geom_boxplot()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")

# save the pct change df so we can make plots of them all together:
saveRDS(pct.change, "outputs/itrdb_model_compare/ED2_pct_change_vars.rds")

saveRDS(mean.1950.2011, "outputs/itrdb_model_compare/ED2_mean_1950_2011_vars.rds")
saveRDS(mean.850.1850, "outputs/itrdb_model_compare/ED2_mean_850_1850_vars.rds")
saveRDS(mean.1690.1850, "outputs/itrdb_model_compare/ED2_mean_1690.1850_vars.rds")


GWBI.WUE.change <- ggplot(pct.change, aes(IWUE.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEet.change <- ggplot(pct.change, aes(WUEet.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEt.change <- ggplot(pct.change, aes(WUEt.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

Fcomp.WUE.change <- ggplot(pct.change, aes(IWUE.change, Fcomp.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
Fcomp.WUEet.change <- ggplot(pct.change, aes(WUEet.change, Fcomp.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
Fcomp.WUEt.change <- ggplot(pct.change, aes(WUEt.change, Fcomp.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())


IWUE.precip.change <- ggplot(pct.change, aes(IWUE.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.precip.change <- ggplot(pct.change, aes(WUEet.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.precip.change <- ggplot(pct.change, aes(WUEt.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

IWUE.temp.change <- ggplot(pct.change, aes(IWUE.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.temp.change <- ggplot(pct.change, aes(WUEet.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.temp.change <- ggplot(pct.change, aes(WUEt.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())


GWBI.temp.change <- ggplot(pct.change, aes( tmax6_change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "Change in Summer Maximum temperatures (degC)")+ylab("% change in GWBI")
GWBI.precip.change <- ggplot(pct.change, aes( precip.change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "% Change MAP")+ylab("% change in GWBI")

ggplot(pct.change, aes(IWUE.change, AGB.change))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())


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



# plot relationships between Fcomp and climate with significance.
pct.change.site.fcomp <- pct.change.site[!pct.change.site$PFT %in% "mean.gwbi",]

p.vals.precip = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ precip.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.precip.m <- melt(p.vals.precip)
p.vals.precip.m$PFT <- rownames(p.vals.precip.m)
p.vals.precip.m$sig <- ifelse(p.vals.precip.m$value <= 0.05, "*", "N.S" )

Fcomp.precip.change.prec <- ggplot(pct.change.site.fcomp, aes( precip.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+geom_text(data = p.vals.precip.m , aes(label = paste("p = ", value, " ", sig), x = 3, y = -75), color = "black")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_precip_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.precip.change.prec
dev.off()

# WUE Fcomp correlations
p.vals.IWUE = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ IWUE.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.IWUE.m <- melt(p.vals.IWUE)
p.vals.IWUE.m$PFT <- rownames(p.vals.IWUE.m)
p.vals.IWUE.m$sig <- ifelse(p.vals.IWUE.m$value <= 0.05, "*", "N.S" )
p.vals.IWUE.m$value <- ifelse(p.vals.IWUE.m$value <= 0.0001, "0.00001", p.vals.IWUE.m$value )


Fcomp.WUE.change.prec <- ggplot(pct.change.site.fcomp, aes( IWUE.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUE.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUE")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_wue_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.WUE.change.prec
dev.off()


# WUEet Fcomp correlations
p.vals.IWUEet = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ WUEet.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.IWUEet.m <- melt(p.vals.IWUEet)
p.vals.IWUEet.m$PFT <- rownames(p.vals.IWUEet.m)
p.vals.IWUEet.m$sig <- ifelse(p.vals.IWUEet.m$value <= 0.05, "*", "N.S" )
p.vals.IWUEet.m$value <- ifelse(p.vals.IWUEet.m$value <= 0.0001, "0.00001", p.vals.IWUEet.m$value )


Fcomp.WUEet.change.prec <- ggplot(pct.change.site.fcomp, aes( WUEet.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUEet.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUEet")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEet_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.WUEet.change.prec
dev.off()

# WUEt Fcomp correlations
p.vals.IWUEt = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ WUEt.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.IWUEt.m <- melt(p.vals.IWUEt)
p.vals.IWUEt.m$PFT <- rownames(p.vals.IWUEt.m)
p.vals.IWUEt.m$sig <- ifelse(p.vals.IWUEt.m$value <= 0.05, "*", "N.S" )
p.vals.IWUEt.m$value <- ifelse(p.vals.IWUEt.m$value <= 0.0001, "0.00001", p.vals.IWUEt.m$value )


Fcomp.WUEt.change.prec <- ggplot(pct.change.site.fcomp, aes( WUEt.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUEt.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUEt")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_WUEt_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.WUEt.change.prec
dev.off()

# tmax Fcomp correlations
p.vals.tmax6 = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ tmax6_change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.tmax6.m <- melt(p.vals.tmax6)
p.vals.tmax6.m$PFT <- rownames(p.vals.tmax6.m)
p.vals.tmax6.m$sig <- ifelse(p.vals.tmax6.m$value <= 0.05, "*", "N.S" )
p.vals.tmax6.m$value <- ifelse(p.vals.tmax6.m$value <= 0.0001, "0.00001", p.vals.tmax6.m$value )

Fcomp.tmax.change.prec <- ggplot(pct.change.site.fcomp, aes( tmax6_change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_tmax_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.tmax.change.prec
dev.off()

# correlation of WUE, tmax, and precip changes  with Density changes:

dens.tmax6 <-  ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( tmax6_change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in Density")
dens.tmax6.lm <- summary(lm(Dens.change ~ tmax6_change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))

dens.precip <-  ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( precip.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Precipitation")+ylab("% change in Density")
dens.precip.lm <- summary(lm(Dens.change ~ precip.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


dens.IWUE <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( IWUE.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUE")+ylab("% change in Density")
dens.IWUE.lm <- summary(lm(Dens.change ~ IWUE.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


dens.WUEet <-  ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( WUEet.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEet")+ylab("% change in Density")
dens.WUEet.lm <- summary(lm(Dens.change ~ WUEet.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


dens.WUEt <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( WUEt.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEt")+ylab("% change in Density")
dens.WUEt.lm <- summary(lm(Dens.change ~ WUEt.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


dens.AGB <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( AGB.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in AGB")+ylab("% change in Density")
dens.AGB.lm <- summary(lm(Dens.change ~ AGB.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


dens.LAI <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( AGB.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in LAI")+ylab("% change in Density")
dens.LAI.lm <- summary(lm(Dens.change ~ LAI.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


dens.fcomp <- ggplot(pct.change.site.fcomp, aes( Dens.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+facet_wrap(~PFT)+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Density")+ylab("% change in Fcomp")


legend.precip <- get_legend(dens.WUEt) 
png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_density_changes_by_climate_wue_850_1850_compared1950_2011.png")
plot_grid(dens.tmax6+theme(legend.position = "none"), 
          dens.precip+theme(legend.position = "none"), 
          dens.IWUE+theme(legend.position = "none"), 
          dens.WUEet+theme(legend.position = "none"), 
          dens.WUEt+theme(legend.position = "none"), 
          legend.precip, ncol = 2)
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_density_changes_by_Fcomp_850_1850_compared1950_2011.png")
dens.fcomp
dev.off()
 # correlation of WUE, tmax, and precip changes  with GS agb changes:
 
AGB.tmax6 <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( tmax6_change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in AGB")
AGB.tmax6.lm <- summary(lm(AGB.change ~ tmax6_change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))

 
AGB.precip <-  ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( precip.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Precipitation")+ylab("% change in AGB")
AGB.precip.lm <- summary(lm(AGB.change ~ precip.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))

 
AGB.IWUE <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( IWUE.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUE")+ylab("% change in AGB")
AGB.IWUE.lm <- summary(lm(AGB.change ~ IWUE.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


AGB.WUEet <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( WUEet.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEet")+ylab("% change in AGB")
AGB.WUEet.lm <- summary(lm(AGB.change ~ WUEet.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))

 
AGB.WUEt <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( WUEt.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
   #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
   theme_bw()+theme(panel.grid = element_blank())+
   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEt")+ylab("% change in AGB")
AGB.WUEt.lm <- summary(lm(AGB.change ~ WUEt.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))

 
AGB.ABG <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( AGB.orig, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Mean AGB 850-1850")+ylab("% change in AGB")
AGB.AGB.lm <- summary(lm(AGB.change ~ AGB.orig, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


AGB.dens <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes(  Dens.change,AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Density")+ylab("% change in AGB")
AGB.dens.lm <- summary(lm(AGB.change ~ Dens.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


AGB.LAI <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( LAI.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in LAI")+ylab("% change in AGB")
AGB.LAI.lm <- summary(lm(AGB.change ~ LAI.change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


agb.fcomp <- ggplot(pct.change.site.fcomp, aes( AGB.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+facet_wrap(~PFT)+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in AGB")+ylab("% change in Fcomp")


ggplot(pct.change.site.fcomp, aes( Fcomp.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+
     theme_bw()+theme(panel.grid = element_blank())+
     scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Mean AGB 850-1850")+ylab("% change in AGB")

ggplot(pct.change.site.fcomp, aes( Dens.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Mean AGB 850-1850")+ylab("% change in AGB")

dev.off()

png(height = 12, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_AGB_changes_by_climate_wue_850_1850_compared1950_2011.png")
plot_grid(AGB.tmax6+theme(legend.position = "none"), 
          AGB.precip+theme(legend.position = "none"), 
          AGB.IWUE+theme(legend.position = "none"), 
          AGB.WUEet+theme(legend.position = "none"), 
          AGB.WUEt+theme(legend.position = "none"), 
          AGB.ABG + theme(legend.position = "none"),
          legend.precip, ncol = 2)
dev.off()
 
png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_agb_changes_by_Fcomp_850_1850_compared1950_2011.png")
dens.fcomp
dev.off()

# correlation of WUE, tmax, and precip changes  with GS LAI changes:

LAI.tmax6 <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( tmax6_change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in LAI")
LAI.tmax6.lm <- summary(lm(LAI.change ~ tmax6_change,pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))

LAI.precip <-  ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( precip.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Precipitation")+ylab("% change in LAI")
LAI.precip.lm <- summary(lm(LAI.change ~ precip.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


LAI.IWUE <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( IWUE.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUE")+ylab("% change in LAI")
LAI.IWUE.lm <- summary(lm(LAI.change ~ IWUE.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


LAI.WUEet <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( WUEet.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEet")+ylab("% change in LAI")
LAI.WUEet.lm <- summary(lm(LAI.change ~ WUEet.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


LAI.WUEt <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( WUEt.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEt")+ylab("% change in LAI")
LAI.WUEt.lm <- summary(lm(LAI.change ~ WUEt.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


LAI.dens <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes(  Dens.change,LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Density")+ylab("% change in LAI")
LAI.dens.lm <- summary(lm(LAI.change ~ Dens.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))


LAI.AGB <- ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( AGB.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
    theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in AGB")+ylab("% change in LAI")
LAI.AGB.lm <- summary(lm(LAI.change ~ AGB.change, pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",]))

LAI.fcomp <- ggplot(pct.change.site.fcomp, aes( LAI.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+facet_wrap(~PFT)+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in LAI")+ylab("% change in Fcomp")


png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_LAI_changes_by_climate_wue_850_1850_compared1950_2011.png")
plot_grid(LAI.tmax6+theme(legend.position = "none") , 
          LAI.precip+theme(legend.position = "none"), 
          LAI.IWUE+theme(legend.position = "none"), 
          LAI.WUEet+theme(legend.position = "none"), 
          LAI.WUEt+theme(legend.position = "none"), 
          legend.precip, ncol = 2)
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_LAI_changes_by_Fcomp_850_1850_compared1950_2011.png")
LAI.fcomp
dev.off()

png(height = 12, width = 12, units = "in", res = 300, "outputs/preliminaryplots/ED2_LAI_AGB_Dens_changes_by_climate_850_1850_compared1950_2011.png")

plot_grid(dens.WUEet + annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(dens.WUEet.lm$coefficients[2,4], 5), "\n",
                                                                            ifelse(round(dens.WUEet.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(dens.WUEet.lm$r.squared, 3)), " "))), 
          
          dens.tmax6+ annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(dens.tmax6.lm$coefficients[2,4], 5), "\n",
                                                                           ifelse(round(dens.tmax6.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(dens.tmax6.lm$r.squared, 3)), " "))), 
          
          dens.precip+ annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(dens.tmax6.lm$coefficients[2,4], 5), "\n",
                                                                            ifelse(round(dens.tmax6.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(dens.tmax6.lm$r.squared, 3)), " "))), 
          
          LAI.WUEet+ annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(LAI.WUEet.lm$coefficients[2,4], 5), "\n",
                              ifelse(round(LAI.WUEet.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(LAI.WUEet.lm$r.squared, 3)), " "))), 
          
          LAI.tmax6 + annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(LAI.tmax6.lm$coefficients[2,4], 5),  "\n",
                               ifelse(round(LAI.tmax6.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(LAI.tmax6.lm$r.squared, 3)), " "))),
          
          LAI.precip+ annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(LAI.precip.lm$coefficients[2,4], 5),  "\n",
                                ifelse(round(LAI.precip.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(LAI.precip.lm$r.squared, 3)), " "))),
          
          AGB.WUEet+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(AGB.WUEet.lm$coefficients[2,4], 5),  "\n",
                                                                          ifelse(round(AGB.WUEet.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(AGB.WUEet.lm$r.squared, 3)), " "))),
          
          AGB.tmax6+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(AGB.tmax6.lm$coefficients[2,4], 5),  "\n",
                                                                          ifelse(round(AGB.tmax6.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(AGB.tmax6.lm$r.squared, 3)), " "))),
          
          AGB.precip+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 55, label = paste("p = ", round(AGB.precip.lm$coefficients[2,4], 5),  "\n",
                                                                           ifelse(round(AGB.precip.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(AGB.precip.lm$r.squared, 3)), " "))),
          
          AGB.dens+ylim(-16, 60)+ annotate(geom = "text", x = 5, y = 55, label = paste("p = ", round(AGB.dens.lm$coefficients[2,4], 5),  "\n",
                                                                         ifelse(round(AGB.precip.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(AGB.precip.lm$r.squared, 3)), " "))),
          
          LAI.AGB+ylim(-16, 60)+ annotate(geom = "text", x = 5, y = 55, label = paste("p = ", round(LAI.AGB.lm$coefficients[2,4], 5),  "\n",
                                                                        ifelse(round(LAI.AGB.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(LAI.AGB.lm$r.squared, 3)), " "))),
          
          LAI.dens+ylim(-16, 60)+ annotate(geom = "text", x = 5, y = 55, label = paste("p = ", round(LAI.dens.lm$coefficients[2,4], 5),  "\n",
                                                                         ifelse(round(LAI.dens.lm$coefficients[2,4], 5) >= 0, paste("Rsq = ", round(LAI.dens.lm$r.squared, 3)), " "))),
          
          ncol = 3)

dev.off()

ggplot(pct.change.site.fcomp[pct.change.site.fcomp$PFT %in% "conifer.late",], aes( AGB.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in AGB")+ylab("% change in LAI")

 
# assessment of which species are driving changes in Density in ED:
ED.dens.pft <- readRDS("outputs/data/ED2/ED2_density_by_PFT.rds")
ED.dens.all <- readRDS("outputs/data/ED2/dens_agbi_climate_ED2.rds")
colnames(ED.dens.pft) <- c("Year", "Site", "PFTDens", "PFT")
ED.dens.pfts <- merge(ED.dens.all, ED.dens.pft, by = c("Year", "Site"), all.y = TRUE)
pct.change.site.fcomp$Site <- paste0("X",pct.change.site.fcomp$Site)

ED.dens.pfts.change <- merge(ED.dens.pfts, pct.change.site.fcomp, by = c("Site", "PFT"))
ggplot(ED.dens.pfts, aes(Dens, PFTDens, color = PFT))+geom_point()+facet_wrap(~PFT)

# get the grid cells increaseing in density & plot trajectory by PFT over the 20th century:
ggplot(ED.dens.pfts[ED.dens.pfts$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid"),], aes(Dens, PFTDens, color = PFT))+geom_point()+facet_wrap(~PFT)

# plot the PFT density by the % change in overall density
all.change.dens.1750.2011<- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & ED.dens.pfts.change$Year >=1750,], aes(Year, PFTDens, color = Dens.change))+
  geom_point()+facet_wrap(~PFT)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "% change in Density")
ED.dens.pfts.change$dens.change.facet <- ifelse(ED.dens.pfts.change$Dens.change >=25, ">=25%", "<=25%")

change.dens.1750.2011 <- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & ED.dens.pfts.change$Year >=1750,], aes(Year, PFTDens, color = Dens.change))+
  geom_point()+facet_grid(~PFT+dens.change.facet)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "% change in Density")


ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & ED.dens.pfts.change$Year >=1750,], aes(dens.change.facet, PFTDens, fill = PFT))+geom_boxplot()
  
greaterthan25changedens<- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & ED.dens.pfts.change$Year >=1750 & ED.dens.pfts.change$dens.change.facet %in% ">=25%",], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)
#ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & ED.dens.pfts.change$Year >=1750 & ED.dens.pfts.change$Dens.change >= 15,], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)
#ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & ED.dens.pfts.change$Year >=1750 & ED.dens.pfts.change$Dens.change <= 10,], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)
lessthan0changedens <- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & ED.dens.pfts.change$Year >=1750 & ED.dens.pfts.change$Dens.change <= 0,], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_Dens_changes_by_site_declining_dens_850_1850_compared1950_2011.png")
lessthan0changedens
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_Dens_changes_by_site_25pctincreasing_dens_850_1850_compared1950_2011.png")
greaterthan25changedens
dev.off()


#ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late") & ED.dens.pfts.change$Year >=1750,], aes(LAI, Dens, color = precip.mm))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")
png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_Dens_LAI_byPrecip_1750.png")
DENS.LAI.ED <- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late") & ED.dens.pfts.change$Year >=1750,], aes(LAI, Dens, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("ED2 Tree Density")+xlab("ED2 LAI")
DENS.LAI.ED
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_Dens_Precip_byprecip_1750.png")
DENS.MAP.ED <- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late") & ED.dens.pfts.change$Year >=1750,], aes(precip.mm, Dens, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("ED2 Tree Density")+xlab("Mean Annual Precipitation (mm)")
DENS.MAP.ED
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_Dens_AGB_byprecip_1750.png")
DENS.AGB.ED <- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late") & ED.dens.pfts.change$Year >=1750,], aes(AGB, Dens, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("ED2 Tree Density")+xlab("ED2 AGB")
DENS.AGB.ED
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_AGB_LAI_byLAI_1750.png")
AGB.LAI.ED <- ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late") & ED.dens.pfts.change$Year >=1750,], aes(AGB, LAI, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("ED2 AGB")+xlab("ED2 LAI")
AGB.LAI.ED
dev.off()

emerg.leg <- get_legend(AGB.LAI.ED)


# save all emergent relationships to one png
png(height = 10, width = 6*3, units = "in", res = 300, "outputs/preliminaryplots/ED2_AGB_DENS_LAI_emergent.png")
plot_grid(DENS.AGB.ED +theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          DENS.LAI.ED+theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          AGB.LAI.ED+theme_bw(base_size = 18)+ theme(legend.position = "none"),
  emerg.leg, rel_widths = c(1,1,1,0.25),
  ncol = 4, align = "hv")
dev.off()



ED.dens.pfts.change$year.facet <- ifelse(ED.dens.pfts.change$Year >= 1950, ">1950", 
                                         ifelse(ED.dens.pfts.change$Year >= 1850 & ED.dens.pfts.change$Year <= 1950, "1850-1950", 
                                                "pre-1850"))
png(height = 10, width = 8, units = "in", res = 300, "outputs/preliminaryplots/ED2_Dens_AGB_byPrecip_timeperiod_facet.png")
ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late"),], aes(AGB, Dens, color = precip))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")+facet_wrap(~year.facet)
dev.off()

png(height = 10, width = 8, units = "in", res = 300, "outputs/preliminaryplots/ED2_Dens_LAI_byPrecip_timeperiod_facet.png")
ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late"),], aes(LAI, Dens, color = precip))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")+facet_wrap(~year.facet)
dev.off()
#ggplot(ED.dens.pfts.change[ED.dens.pfts.change$PFT %in% c("conifer.late") & ED.dens.pfts.change$Year >=1750,], aes(LAI, Dens, color = precip.mm))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")+facet_wrap(~Site)


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

ET.MAP.change.prec <- ggplot(pct, aes( precip.change, ET.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% change in Precipitation")+ylab("% change in ET")


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


# plot the change in Fcomp and gwbi for each PFT
fcomp.gwbi.melt <- pct.change.site %>%  dplyr::select(lon, lat, PFT, Mean_MAP, Mean_MAP.wy, GWBI.change, Fcomp.change) %>%
  group_by(lon, lat, Mean_MAP, Mean_MAP.wy, PFT) %>% gather(key = "variable", value = "value", GWBI.change:Fcomp.change)

#pct.melt$variable <- factor(pct.melt$variable, levels = c("IWUE.change", "WUEet.change", "WUEt.change", "ET.change", "Evap.change", "Transp.change","GPP.change"))
ggplot(fcomp.gwbi.melt, aes(variable, value, fill = PFT))+geom_boxplot()+theme_bw(base_size = 10)+theme(axis.text.x = element_text(angle = 45, hjust = 1))


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


# fit linear models to see what explains the delta WUE best, changes in GPP, or changes in ET
summary(lm(IWUE.change ~ GPP.change + ET.change + Transp.change + Evap.change + Mean_MAP.wy + tmax6_change, data = pct))
summary(lm(WUEet.change ~ GPP.change + ET.change + Transp.change + Evap.change + Mean_MAP.wy+ tmax6_change, data = pct))
summary(lm(WUEt.change ~ GPP.change + ET.change + Transp.change + Evap.change + Mean_MAP.wy+ tmax6_change, data = pct))

hist(pct.change$GPP.change - pct.change$ET.change) # GPP increases more than ET at most sites
hist(pct.change$GPP.change - pct.change$Transp.change) # GPP increases more than TRansp at most sites
hist(pct.change$GPP.change - pct.change$Evap.change) # GPP increases more than Evap at ~ half the sites
hist(pct.change$Transp.change - pct.change$Evap.change) # Evap increases more than Transp at ~ half the sites


summary(pct.change$GPP.change - pct.change$ET.change) 
hist(pct.change$GPP.change - pct.change$ET.change) 

ED2.pct.change <- pct.change

# add better maps as background
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

library(rnaturalearth)
#  is all downloaded from <a href=>http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

# lakes
ne_lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical')
sp::plot(ne_lakes, col = 'blue')
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

# ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
#   geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
#   geom_raster(data = pct, aes(lon, lat, fill = IWUE.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

delta.WUEet.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = WUEet.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_WUEet_change_map.png")
delta.WUEet.change
dev.off()

deltaWUEt.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = WUEt.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_WUEt_change_map.png")
delta.WUEt.change
dev.off()

delta.precip.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = precip.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_precip_change_map.png")
delta.precip.change
dev.off()

delta.GPP.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = GPP.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_GPP_change_map.png")
delta.GPP.change 
dev.off()

delta.Evap.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = Evap.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_Evap_change_map.png")
delta.Evap.change
dev.off()

delta.ET.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = ET.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_ET_change_map.png")
delta.ET.change
dev.off()

delta.transp.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = Transp.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_Transp_change_map.png")
delta.Transp.change
dev.off()

delta.fcomp.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct.change[!pct.change$PFT %in% c("BeIBS.gwbi", "TrIBE.gwbi"),], aes(lon, lat, fill = Fcomp.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))+facet_wrap(~PFT, ncol = 2)

png(height = 10, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_Fcomp_PFT_change_map.png")
delta.fcomp.change 
dev.off()


png(height = 20, width = 25, units = "in", res= 300,"outputs/preliminaryplots/ED2_all_changes_map.png")
plot_grid(delta.WUEet.change, 
          deltaWUEt.change,
          delta.GPP.change,
          delta.transp.change, 
          delta.precip.change, 
          delta.Evap.change, 
          delta.ET.change,
          ncol = 23)
dev.off()


ggplot(pct.change, aes(GPP.change, Transp.change))+geom_point()
ggplot(pct.change, aes(GPP.change, Evap.change))+geom_point()
ggplot(pct.change, aes(ET.change, Fcomp.change, color = PFT))+geom_point()+facet_wrap(~PFT)+geom_smooth(method = "lm")
ggplot(pct.change, aes(GPP.change, ET.change))+geom_point()
ggplot(pct.change, aes(GPP.change, Fcomp.change, color = PFT))+geom_point()
ggplot(pct.change, aes(GPP.change, GWBI.change, color = PFT))+geom_point()+facet_wrap(~PFT)+geom_smooth()
ggplot(pct.change, aes(ET.change, GWBI.change, color = PFT))+geom_point()+facet_wrap(~PFT)+geom_smooth()

ggplot(pct.change, aes(GPP.change, ET.change))+geom_point()


#--------------------Run the same analyses for GUESS---------------------------------
GUESS.gwbi.clim.nona <- readRDS("Data/GUESS_gwbi_pft_clim.rds")

ggplot(GUESS.gwbi.clim.nona, aes(Year, GWBI, color = Site))+geom_point()+facet_wrap(~PFT)

# read in dataframe with WUE

WUE.dens.dfs <- readRDS("outputs/data/GUESS/GUESS.alldat.yrmeans.rds")
WUE.dens.dfs$Site <- substring(WUE.dens.dfs$Site, 2)

GUESS.df <- left_join(WUE.dens.dfs, GUESS.gwbi.clim.nona, by = c("Site", "Year"))

summary(GUESS.df$WUEet)

# read in PFT-level LAI and AGB, get totals, and save PFT level dfs:
GUESS.LAI.PFT <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.LAI_PFT.rds")
GUESS.AGB.PFT <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.AGB.rds")
GUESS.Dens.PFT <- readRDS("Data/LPJ-GUESS/LPJ-GUESS.Dens.rds")

guess.pft.lab <- c("BNE", "BINE", "BNS", "BIBS", "TeBS", "TelBS", "TeBE",
                   "TrBE", "TrlBE", "TrBR", "C3G", "C4G", "Total")
load("Data/PalEON_siteInfo_all.RData")
yrlyvar <- (0:1160) + 850
dimnames(GUESS.LAI.PFT) <-  list(yrlyvar, paleon$num, guess.pft.lab)
dimnames(GUESS.AGB.PFT) <-  list(yrlyvar, paleon$num, guess.pft.lab)
dimnames(GUESS.Dens.PFT) <-  list(yrlyvar, paleon$num, guess.pft.lab)

GUESS.AGB.PFT.m <- melt(GUESS.AGB.PFT)
GUESS.LAI.PFT.m <- melt(GUESS.LAI.PFT)
GUESS.Dens.PFT.m <- melt(GUESS.Dens.PFT)

colnames(GUESS.LAI.PFT.m) <- c("Year", "Site", "PFT", "LAI")
colnames(GUESS.AGB.PFT.m) <- c("Year", "Site", "PFT", "AGB")
colnames(GUESS.Dens.PFT.m) <- c("Year", "Site", "PFT", "Dens")

ggplot(GUESS.AGB.PFT.m[GUESS.AGB.PFT.m$Site %in% "1",], aes(Year, AGB, color = PFT))+geom_point()
ggplot(GUESS.LAI.PFT.m[GUESS.LAI.PFT.m$Site %in% "1",], aes(Year,LAI, color = PFT))+geom_point()
ggplot(GUESS.Dens.PFT.m[GUESS.Dens.PFT.m$Site %in% "7",], aes(Year,Dens, color = PFT))+geom_point()

LAI.tot <- GUESS.LAI.PFT.m %>% filter(PFT %in% "Total") %>% dplyr::select(Year, Site, LAI)
AGB.tot <- GUESS.AGB.PFT.m %>% filter(PFT %in% "Total")%>% dplyr::select(Year, Site, AGB)
Dens.tot <- GUESS.Dens.PFT.m %>% filter(PFT %in% "Total")%>% dplyr::select(Year, Site, Dens)

GUESS.df1 <- merge(GUESS.df, Dens.tot, by = c("Site", "Year"))
GUESS.df2 <- merge(GUESS.df1, LAI.tot, by = c("Site", "Year"))
GUESS <- merge(GUESS.df2, AGB.tot, by = c("Site", "Year"))

# read in Fcomp for ED2:
GUESS.fcomp <- readRDS( "Data/GUESS.Fcomp.pft.rds" )
GUESS.fcomp$PFT <- paste0(GUESS.fcomp$PFT, ".gwbi")
#ED2.fcomp.yr <-  ED2.fcomp.yr %>% filter(PFT %in% unique(ED2.gwbi.clim.nona$PFT))
GUESS.fcomp$Site <- as.character(GUESS.fcomp$Site)

GUESS.full <- left_join(GUESS, GUESS.fcomp, by = c("Year", "Site", "PFT"))


# remove the outliers:
GUESS.rm <- GUESS.full[GUESS.full$WUEet <= 500 & GUESS.full$WUEet > 0, ]

# remove the outliers:
# remove the outliers:
#GUESS.rm <- GUESS[GUESS$WUEet <= 500, ]
#GUESS.rm <- GUESS
ggplot(GUESS.rm, aes(Tair.C, WUEet))+geom_point()

GUESS.rm <- GUESS.rm[!is.na(GUESS.rm$PFT), ]

#ggplot(GUESS.rm, aes(WUEet, Rel.Dens))+geom_point(size = 0.5)
 #GUESS.WUEet.GWBI.PFT <- ggplot(GUESS.rm, aes(WUEet, GWBI, color = PFT))+geom_point(size = 0.5)+facet_wrap(~PFT)
 
 # png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/GUESS_GWBI_WUEet_by_pft.png")
 # GUESS.WUEet.GWBI.PFT
 # dev.off()

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

#ggplot(all.met.summary, aes(lon, lat, fill = Mean_MAP.wy))+geom_raster()
#ggplot(all.met.summary, aes(lon, lat, fill = Mean_tair_max_6))+geom_raster()


# join the site means to the sites:

GUESS.rm.site <- left_join(all.met.summary, GUESS.rm , by = c("lon", "lat"))

#ggplot(GUESS.rm.site[GUESS.rm.site$Site %in% "1",], aes(Year, WUEet, color = Mean_tair_max_6))+geom_point()+geom_line()+stat_smooth()

# get the change in %WUE over between 850-1850 and 1950-present
# get the change in %WUE over between 850-1850 and 1950-present
mean.850.1850 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1849) %>% summarise(#IWUE.850.1850 = mean(IWUE, na.rm=TRUE),
                                                                                                      WUE.et.850.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                      WUE.t.850.1850 = mean(WUEt, na.rm=TRUE),
                                                                                                      GWBI.850.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                      MAP.wy.850.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                      Tmax_6.850.1850 = mean(tair_max_6, na.rm =TRUE),
                                                                                                      GPP.850.1850 = mean (GPP, na.rm=TRUE),
                                                                                                      ET.850.1850 = mean (ET, na.rm=TRUE),
                                                                                                      Transp.850.1850 = mean (Transp, na.rm=TRUE),
                                                                                                      Evap.850.1850 = mean (Evap, na.rm=TRUE),
                                                                                                      Fcomp.850.1850 = mean(Fcomp, na.rm =TRUE),
                                                                                                      Dens.850.1850 = mean(Dens, na.rm =TRUE), 
                                                                                                      #GS_agb.850.1850 =mean(GS_agb, na.rm = TRUE),
                                                                                                      AGB.850.1850 = mean(AGB, na.rm = TRUE), 
                                                                                                      LAI.850.1850 = mean(LAI, na.rm = TRUE))




mean.1690.1850 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1690 & Year <= 1849) %>% summarise(#IWUE.1690.1850 = mean(IWUE, na.rm=TRUE),
                                                                                                                      WUE.et.1690.1850 = mean(WUEet, na.rm=TRUE),
                                                                                                                      WUE.t.1690.1850 = mean(WUEt, na.rm=TRUE),
                                                                                                                      GWBI.1690.1850 = mean(GWBI, na.rm=TRUE),
                                                                                                                      MAP.wy.1690.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                                      Tmax_6.1690.1850 = mean(tair_max_6, na.rm =TRUE),
                                                                                                                      GPP.1690.1850 = mean (GPP, na.rm=TRUE),
                                                                                                                      ET.1690.1850 = mean (ET, na.rm=TRUE),
                                                                                                                      Transp.1690.1850 = mean (Transp, na.rm=TRUE),
                                                                                                                      Evap.1690.1850 = mean (Evap, na.rm=TRUE),
                                                                                                                      Fcomp.1690.1850 = mean(Fcomp, na.rm =TRUE),
                                                                                                                      Dens.1690.1850 = mean(Dens, na.rm =TRUE), 
                                                                                                                      #GS_agb.1690.1850 =mean(GS_agb, na.rm = TRUE),
                                                                                                                      AGB.1690.1850 = mean(AGB, na.rm = TRUE), 
                                                                                                                      LAI.1690.1850 = mean(LAI, na.rm = TRUE))





mean.1850.2011 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1850) %>% summarise(#IWUE.1850.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                       WUE.et.1850.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1850.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                       GWBI.1850.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1850.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1850.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1850.2011 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1850.2011 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1850.2011 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1850.2011 = mean (Evap, na.rm=TRUE),
                                                                                                       Fcomp.1850.2011 = mean(Fcomp, na.rm =TRUE),
                                                                                                       Dens.1850.2011 = mean(Dens, na.rm =TRUE), 
                                                                                                       #GS_agb.1850.2011 =mean(GS_agb, na.rm = TRUE),
                                                                                                       AGB.1850.2011 = mean(AGB, na.rm = TRUE), 
                                                                                                       LAI.1850.2011 = mean(LAI, na.rm = TRUE))




mean.1950.2011 <- GUESS.rm.site %>% group_by(lat, lon, Site, PFT) %>% filter(Year >= 1950) %>% summarise(#IWUE.1950.2011 = mean(IWUE, na.rm=TRUE),
                                                                                                       WUE.et.1950.2011 = mean(WUEet, na.rm=TRUE),
                                                                                                       WUE.t.1950.2011 = mean(WUEt, na.rm=TRUE),
                                                                                                       GWBI.1950.2011 = mean(GWBI, na.rm=TRUE),
                                                                                                       MAP.wy.1950.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
                                                                                                       Tmax_6.1950.2011 = mean(tair_max_6, na.rm =TRUE),
                                                                                                       GPP.1950.2011 = mean (GPP, na.rm=TRUE),
                                                                                                       ET.1950.2011 = mean (ET, na.rm=TRUE),
                                                                                                       Transp.1950.2011 = mean (Transp, na.rm=TRUE),
                                                                                                       Evap.1950.2011 = mean (Evap, na.rm=TRUE),
                                                                                                       Fcomp.1950.2011 = mean(Fcomp, na.rm =TRUE),
                                                                                                       Dens.1950.2011 = mean(Dens, na.rm =TRUE), 
                                                                                                       #GS_agb.1950.2011 =mean(GS_agb, na.rm = TRUE),
                                                                                                       AGB.1950.2011 = mean(AGB, na.rm = TRUE), 
                                                                                                       LAI.1950.2011 = mean(LAI, na.rm = TRUE))





#ggplot(mean.850.1850, aes(MAP.wy.850.1850, Dens.850.1850))+geom_point()
# ggplot(mean.850.1850, aes(IWUE.850.1850, GWBI.850.1850, color = PFT))+geom_point()
# ggplot(mean.850.1850, aes(Tmax_6.850.1850, GWBI.850.1850, color = PFT))+geom_point()
# ggplot(mean.850.1850, aes(Tmax_6.850.1850, MAP.wy.850.1850, color = IWUE.850.1850))+geom_point()


time.periods <- left_join(mean.1950.2011, mean.850.1850, by = c("lat", "lon", "Site", "PFT"))

pct.change <- time.periods %>% group_by(lat, lon, Site, PFT) %>% summarise(#IWUE.change = mean(((IWUE.1950.2011 - IWUE.850.1850)/IWUE.850.1850)*100, na.rm=TRUE),
                                                                           WUEet.change = mean(((WUE.et.1950.2011 - WUE.et.850.1850)/WUE.et.850.1850)*100, na.rm=TRUE),
                                                                           WUEt.change = mean(((WUE.t.1950.2011 - WUE.t.850.1850)/WUE.t.850.1850)*100, na.rm=TRUE),
                                                                           precip.change = mean(((MAP.wy.1950.2011 - MAP.wy.850.1850)/MAP.wy.850.1850)*100, na.rm=TRUE),
                                                                           tmax6_change = mean((Tmax_6.1950.2011 - Tmax_6.850.1850), na.rm=TRUE),
                                                                           GWBI.change = mean(((GWBI.1950.2011 - GWBI.850.1850)/GWBI.850.1850)*100, na.rm=TRUE),
                                                                           GPP.change = mean(((GPP.1950.2011 - GPP.850.1850)/GPP.850.1850)*100, na.rm=TRUE),
                                                                           ET.change = mean(((ET.1950.2011 - ET.850.1850)/ET.850.1850)*100, na.rm=TRUE),
                                                                           Transp.change = mean(((Transp.1950.2011 - Transp.850.1850)/Transp.850.1850)*100, na.rm=TRUE), 
                                                                           Evap.change = mean(((Evap.1950.2011 - Evap.850.1850)/Evap.850.1850)*100, na.rm=TRUE),
                                                                           Fcomp.change = mean(((Fcomp.1950.2011 - Fcomp.850.1850)/Fcomp.850.1850)*100, na.rm=TRUE),
                                                                           Dens.change = mean(((Dens.1950.2011 - Dens.850.1850)/Dens.850.1850)*100, na.rm=TRUE),
                                                                           #GS_agb.change = mean(((GS_agb.1950.2011 - GS_agb.850.1850)/GS_agb.850.1850)*100, na.rm=TRUE),
                                                                           AGB.change = mean(((AGB.1950.2011 - AGB.850.1850)/AGB.850.1850)*100, na.rm=TRUE),
                                                                           LAI.change = mean(((LAI.1950.2011 - LAI.850.1850)/LAI.850.1850)*100, na.rm=TRUE),
                                                                           AGB.orig = mean(AGB.850.1850, na.rm = TRUE))


saveRDS(pct.change, "outputs/itrdb_model_compare/GUESS_pct_change_vars.rds")

saveRDS(mean.1950.2011, "outputs/itrdb_model_compare/GUESS_mean_1950_2011_vars.rds")
saveRDS(mean.850.1850, "outputs/itrdb_model_compare/GUESS_mean_850_1850_vars.rds")
saveRDS(mean.1690.1850, "outputs/itrdb_model_compare/GUESS_mean_1690.1850_vars.rds")



#GWBI.WUE.change <- ggplot(pct.change, aes(WUEet.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEet.change <- ggplot(pct.change, aes(WUEet.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
GWBI.WUEt.change <- ggplot(pct.change, aes(WUEt.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

#WUEet.precip.change <- ggplot(pct.change, aes(WUEet.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.precip.change <- ggplot(pct.change, aes(WUEet.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.precip.change <- ggplot(pct.change, aes(WUEt.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

#WUEet.temp.change <- ggplot(pct.change, aes(WUEet.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.et.temp.change <- ggplot(pct.change, aes(WUEet.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
WUE.t.temp.change <- ggplot(pct.change, aes(WUEt.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

#Fcomp.WUE.change <- ggplot(pct.change, aes(IWUE.change, Fcomp.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
Fcomp.WUEet.change <- ggplot(pct.change, aes(WUEet.change, Fcomp.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
Fcomp.WUEt.change <- ggplot(pct.change, aes(WUEt.change, Fcomp.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())


GWBI.temp.change <- ggplot(pct.change, aes( tmax6_change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "Change in Summer Maximum temperatures (degC)")+ylab("% change in GWBI")
GWBI.precip.change <- ggplot(pct.change, aes( precip.change, GWBI.change,color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+xlab( "% Change MAP")+ylab("% change in GWBI")

#ggplot(pct.change, aes( tmax6_change,Dens.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
#ggplot(pct.change, aes( precip.change, Dens.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
#ggplot(pct.change, aes( precip.change, LAI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())


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
p.vals.WUEt = sapply(unique(pct.change.site$PFT), function(i) {
  round(coef(summary(lm(GWBI.change ~ WUEt.change, data=pct.change.site[pct.change.site$PFT==i, ])))[2,4], 4)
})

p.vals.WUEt.m <- melt(p.vals.WUEt)
p.vals.WUEt.m$PFT <- rownames(p.vals.WUEt.m)
p.vals.WUEt.m$sig <- ifelse(p.vals.WUEt.m$value <= 0.05, "*", "N.S" )
p.vals.WUEt.m$value <- ifelse(p.vals.WUEt.m$value <= 0.0001, "0.00001", p.vals.WUEt.m$value )


GWBI.WUEt.change.prec <- ggplot(pct.change.site, aes( WUEt.change, GWBI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.WUEt.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEett")+ylab("% change in GWBI")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEt_gwbi_by_pft_850_1850_compared1950_2011.png")
GWBI.WUEt.change.prec
dev.off()

# plot relationships between Fcomp and climate with significance.
pct.change.site.fcomp <- pct.change.site[!pct.change.site$PFT %in% c("Total.gwbi", "BeIBS.gwbi", "TrIBE.gwbi"),]

p.vals.precip = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ precip.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.precip.m <- melt(p.vals.precip)
p.vals.precip.m$PFT <- rownames(p.vals.precip.m)
p.vals.precip.m$sig <- ifelse(p.vals.precip.m$value <= 0.05, "*", "N.S" )

Fcomp.precip.change.prec <- ggplot(pct.change.site.fcomp, aes( precip.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+geom_text(data = p.vals.precip.m , aes(label = paste("p = ", value, " ", sig), x = 3, y = -75), color = "black")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_precip_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.precip.change.prec
dev.off()

# # WUE Fcomp correlations
# p.vals.IWUE = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
#   round(coef(summary(lm(Fcomp.change ~ IWUE.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
# })
# 
# p.vals.IWUE.m <- melt(p.vals.IWUE)
# p.vals.IWUE.m$PFT <- rownames(p.vals.IWUE.m)
# p.vals.IWUE.m$sig <- ifelse(p.vals.IWUE.m$value <= 0.05, "*", "N.S" )
# p.vals.IWUE.m$value <- ifelse(p.vals.IWUE.m$value <= 0.0001, "0.00001", p.vals.IWUE.m$value )
# 
# 
# Fcomp.WUE.change.prec <- ggplot(pct.change.site.fcomp, aes( IWUE.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
#   geom_text(data = p.vals.IWUE.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
#   facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
#   scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUE")+ylab("% change in Fcomp")
# 
# png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_wue_Fcomp_by_pft_850_1850_compared1950_2011.png")
# Fcomp.WUE.change.prec
# dev.off()


# WUEet Fcomp correlations
p.vals.IWUEet = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ WUEet.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.IWUEet.m <- melt(p.vals.IWUEet)
p.vals.IWUEet.m$PFT <- rownames(p.vals.IWUEet.m)
p.vals.IWUEet.m$sig <- ifelse(p.vals.IWUEet.m$value <= 0.05, "*", "N.S" )
p.vals.IWUEet.m$value <- ifelse(p.vals.IWUEet.m$value <= 0.0001, "0.00001", p.vals.IWUEet.m$value )


Fcomp.WUEet.change.prec <- ggplot(pct.change.site.fcomp, aes( WUEet.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUEet.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUEet")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEet_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.WUEet.change.prec
dev.off()

# WUEt Fcomp correlations
p.vals.IWUEt = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ WUEt.change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.IWUEt.m <- melt(p.vals.IWUEt)
p.vals.IWUEt.m$PFT <- rownames(p.vals.IWUEt.m)
p.vals.IWUEt.m$sig <- ifelse(p.vals.IWUEt.m$value <= 0.05, "*", "N.S" )
p.vals.IWUEt.m$value <- ifelse(p.vals.IWUEt.m$value <= 0.0001, "0.00001", p.vals.IWUEt.m$value )


Fcomp.WUEt.change.prec <- ggplot(pct.change.site.fcomp, aes( WUEt.change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.IWUEt.m , aes(label = paste("p =", value, sig), x = 10, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in IWUEt")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_WUEt_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.WUEt.change.prec
dev.off()

# tmax Fcomp correlations
p.vals.tmax6 = sapply(unique(pct.change.site.fcomp$PFT), function(i) {
  round(coef(summary(lm(Fcomp.change ~ tmax6_change, data=pct.change.site.fcomp[pct.change.site.fcomp$PFT==i, ])))[2,4], 4)
})

p.vals.tmax6.m <- melt(p.vals.tmax6)
p.vals.tmax6.m$PFT <- rownames(p.vals.tmax6.m)
p.vals.tmax6.m$sig <- ifelse(p.vals.tmax6.m$value <= 0.05, "*", "N.S" )
p.vals.tmax6.m$value <- ifelse(p.vals.tmax6.m$value <= 0.0001, "0.00001", p.vals.tmax6.m$value )

Fcomp.tmax.change.prec <- ggplot(pct.change.site.fcomp, aes( tmax6_change, Fcomp.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in Fcomp")

png(height = 4, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_tmax_Fcomp_by_pft_850_1850_compared1950_2011.png")
Fcomp.tmax.change.prec
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


# correlation of WUE, tmax, and precip changes  with Density changes:

dens.tmax6 <-  ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( tmax6_change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in Density")
dens.tmax6.lm <- summary(lm(Dens.change ~ tmax6_change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

dens.precip <-  ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( precip.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Precipitation")+ylab("% change in Density")
dens.precip.lm <- summary(lm(Dens.change ~ precip.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))



dens.WUEet <-  ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( WUEet.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEet")+ylab("% change in Density")
dens.WUEet.lm <- summary(lm(Dens.change ~ WUEet.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

dens.WUEt <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( WUEt.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEt")+ylab("% change in Density")
dens.WUEt.lm <- summary(lm(Dens.change ~ WUEt.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

legend.precip <- get_legend(dens.WUEt) 
png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_density_changes_by_climate_wue_850_1850_compared1950_2011.png")
plot_grid(dens.tmax6+theme(legend.position = "none"), 
          dens.precip+theme(legend.position = "none"), 
          #dens.IWUE+theme(legend.position = "none"), 
          dens.WUEet+theme(legend.position = "none"), 
          dens.WUEt+theme(legend.position = "none"), 
          legend.precip, ncol = 2)
dev.off()

# correlation of WUE, tmax, and precip changes  with GS agb changes:

AGB.tmax6 <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( tmax6_change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in AGB")
AGB.tmax6.lm <- summary(lm(AGB.change ~ tmax6_change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))


AGB.precip <-  ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( precip.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Precipitation")+ylab("% change in AGB")
AGB.precip.lm <- summary(lm(AGB.change ~ precip.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))



AGB.WUEet <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( WUEet.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEet")+ylab("% change in AGB")
AGB.WUEet.lm <- summary(lm(AGB.change ~ WUEet.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))


AGB.WUEt <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( WUEt.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEt")+ylab("% change in AGB")
AGB.WUEt.lm <- summary(lm(AGB.change ~ WUEt.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

AGB.ABG <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( AGB.orig, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Mean AGB 850-1850")+ylab("% change in AGB")
AGB.AGB.lm <- summary(lm(AGB.change ~ AGB.orig, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

AGB.dens <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes(Dens.change , AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% change in Density")+ylab("% change in AGB")
AGB.dens.lm <- summary(lm(AGB.change ~ Dens.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))


ggplot(pct.change.site, aes( Fcomp.change, AGB.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Mean AGB 850-1850")+ylab("% change in AGB")
dev.off()

png(height = 12, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_AGB_changes_by_climate_wue_850_1850_compared1950_2011.png")
plot_grid(AGB.tmax6+theme(legend.position = "none"), 
          AGB.precip+theme(legend.position = "none"), 
          #AGB.IWUE+theme(legend.position = "none"), 
          AGB.WUEet+theme(legend.position = "none"), 
          AGB.WUEt+theme(legend.position = "none"), 
          AGB.ABG + theme(legend.position = "none"),
          legend.precip, ncol = 2)
dev.off()


# correlation of WUE, tmax, and precip changes  with GS LAI changes:

LAI.tmax6 <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( tmax6_change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("Change in Temperature DegC")+ylab("% change in LAI")
LAI.tmax6.lm <- summary(lm(LAI.change ~ tmax6_change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

LAI.precip <-  ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( precip.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in Precipitation")+ylab("% change in LAI")
LAI.precip.lm <- summary(lm(LAI.change ~ precip.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

LAI.WUEet <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( WUEet.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEet")+ylab("% change in LAI")
LAI.WUEet.lm <- summary(lm(LAI.change ~ WUEet.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

LAI.WUEt <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( WUEt.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in WUEt")+ylab("% change in LAI")
LAI.WUEt.lm <- summary(lm(LAI.change ~ WUEt.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

LAI.dens <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes(Dens.change , LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% change in Density")+ylab("% change in LAI")
LAI.dens.lm <- summary(lm(LAI.change ~ Dens.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))

LAI.AGB <- ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes(AGB.change , LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% change in AGB")+ylab("% change in LAI")
LAI.AGB.lm <- summary(lm(LAI.change ~ AGB.change, data = pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]))


png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_LAI_changes_by_climate_wue_850_1850_compared1950_2011.png")
plot_grid(LAI.tmax6+theme(legend.position = "none"), 
          LAI.precip+theme(legend.position = "none"), 
          #LAI.IWUE+theme(legend.position = "none"), 
          LAI.WUEet+theme(legend.position = "none"), 
          LAI.WUEt+theme(legend.position = "none"), 
          legend.precip, ncol = 2)
dev.off()

png(height = 12, width = 12, units = "in", res = 300, "outputs/preliminaryplots/GUESS_LAI_AGB_Dens_changes_by_climate_850_1850_compared1950_2011.png")

plot_grid(dens.WUEet+ylim(-16, 60)+annotate(geom = "text", x = 5, y = 45, label = paste("p = ", round(dens.WUEet.lm$coefficients[2,4], 5), "\n",
                                                                            ifelse(round(dens.WUEet.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(dens.WUEet.lm$r.squared, 3)), " "))), 
          
          dens.tmax6+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 45, label = paste("p = ", round(dens.tmax6.lm$coefficients[2,4], 5), "\n",
                                                                           ifelse(round(dens.tmax6.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(dens.tmax6.lm$r.squared, 3)), " "))), 
          
          dens.precip+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 45, label = paste("p = ", round(dens.tmax6.lm$coefficients[2,4], 5), "\n",
                                                                            ifelse(round(dens.tmax6.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(dens.tmax6.lm$r.squared, 3)), " "))), 
          
          LAI.WUEet+ylim(-16, 60)+annotate(geom = "text", x = 5, y = 45, label = paste("p = ", round(LAI.WUEet.lm$coefficients[2,4], 5), "\n",
                                                                          ifelse(round(LAI.WUEet.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(LAI.WUEet.lm$r.squared, 3)), " "))), 
          
          LAI.tmax6 +ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 45, label = paste("p = ", round(LAI.tmax6.lm$coefficients[2,4], 5),  "\n",
                                                                           ifelse(round(LAI.tmax6.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(LAI.tmax6.lm$r.squared, 3)), " "))),
          
          LAI.precip+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 45, label = paste("p = ", round(LAI.precip.lm$coefficients[2,4], 5),  "\n",
                                                                           ifelse(round(LAI.precip.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(LAI.precip.lm$r.squared, 3)), " "))),
          
          AGB.WUEet+ylim(-16, 60)+ annotate(geom = "text", x = 5, y = 45, label = paste("p = ", round(AGB.WUEet.lm$coefficients[2,4], 5),  "\n",
                                                                                        ifelse(round(AGB.WUEet.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(AGB.WUEet.lm$r.squared, 3)), " "))),
          
          AGB.tmax6+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 45, label = paste("p = ", round(AGB.tmax6.lm$coefficients[2,4], 5),  "\n",
                                                                                        ifelse(round(AGB.tmax6.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(AGB.tmax6.lm$r.squared, 3)), " "))),
          
          AGB.precip+ylim(-16, 60)+ annotate(geom = "text", x = 1, y = 45, label = paste("p = ", round(AGB.precip.lm$coefficients[2,4], 5),  "\n",
                                                                                         ifelse(round(AGB.precip.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(AGB.precip.lm$r.squared, 3)), " "))),
          
          AGB.dens+ylim(-16, 60)+ annotate(geom = "text", x = 5, y = 45, label = paste("p = ", round(AGB.dens.lm$coefficients[2,4], 5),  "\n",
                                                                                       ifelse(round(AGB.precip.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(AGB.precip.lm$r.squared, 3)), " "))),
          
          LAI.AGB+ylim(-16, 60)+ annotate(geom = "text", x = 5, y = 45, label = paste("p = ", round(LAI.AGB.lm$coefficients[2,4], 5),  "\n",
                                                                                      ifelse(round(LAI.AGB.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(LAI.AGB.lm$r.squared, 3)), " "))),
          
          LAI.dens+ylim(-16, 60)+ annotate(geom = "text", x = 5, y = 45, label = paste("p = ", round(LAI.dens.lm$coefficients[2,4], 5),  "\n",
                                                                                       ifelse(round(LAI.dens.lm$coefficients[2,4], 5) <= 0.05, paste("Rsq = ", round(LAI.dens.lm$r.squared, 3)), " "))),
          
          ncol = 3)

dev.off()

ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( AGB.change, LAI.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in AGB")+ylab("% change in LAI")

ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( AGB.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in AGB")+ylab("% change in Density")

ggplot(pct.change.site[pct.change.site$PFT %in% "Total.gwbi",], aes( LAI.change, Dens.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  #geom_text(data = p.vals.tmax6.m , aes(label = paste("p =", value, sig), x = 1, y = -75), color = "black")+
  theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in LAI")+ylab("% change in Density")


# assessment of which species are driving changes in Density in ED:
# get all PFTs but Total Density
GUESS.pfts.density <- GUESS.Dens.PFT.m  %>% filter(PFT %in% c("BNE", "BINE", "BNS", "BIBS", "TeBS", "TelBS", "TeBE",
                                        "TrBE", "TrlBE", "TrBR", "C3G", "C4G"))# obj from above 
colnames(GUESS.pfts.density) <- c("Year", "Site", "PFT", "PFTDens")

# merge together all the total AGB, LAI, and Density 
GUESS.wue <- merge(WUE.dens.dfs, Dens.tot, by = c("Site", "Year"))
GUESS.wue2 <-  merge(GUESS.wue, AGB.tot, by = c("Site", "Year"))
GUESS.yr <-  merge(GUESS.wue2, LAI.tot, by = c("Site", "Year"))

# merge yearly total table with Density by PFT table:
GUESS.dens.pfts.m <- merge(GUESS.Dens.PFT.m, GUESS.yr, by = c("Site", "Year"))
ggplot(GUESS.dens.pfts.m, aes(Dens, PFTDens, color = PFT))+geom_point()+facet_wrap(~PFT)

# get the grid cells increaseing in density & plot trajectory by PFT over the 20th century:
pct.change.unique <- pct.change %>%dplyr::select(lat, lon, Site, Dens.change, precip.change, AGB.change, LAI.change)
pct.change.unique <- unique(pct.change.unique)
GUESS.dens.pfts.change <- merge(GUESS.dens.pfts.m, pct.change.unique, by = c("Site"))


# plot the PFT density by the % change in overall density
all.change.dens.1750.2011 <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BNE", "BINE", "TeBS", "TelBS", "TeBE", "BelBS", "BIBS") & GUESS.dens.pfts.change$Year >=1750,], aes(Year, PFTDens, color = Dens.change))+
  geom_point()+facet_wrap(~PFT)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "% change in Density")

GUESS.dens.pfts.change$dens.change.facet <- ifelse(GUESS.dens.pfts.change$Dens.change >=15, ">=15%", "<=15%")

change.dens.1750.2011 <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BNE", "BINE", "TeBS", "TelBS", "TeBE", "BelBS", "BIBS") & GUESS.dens.pfts.change$Year >=1750,], aes(Year, PFTDens, color = Dens.change))+
  geom_point()+facet_grid(~PFT+dens.change.facet)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "% change in Density")


ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BNE", "BINE", "TeBS", "TelBS", "TeBE", "BelBS", "BIBS") & GUESS.dens.pfts.change$Year >=1750,], aes(dens.change.facet, PFTDens, fill = PFT))+geom_boxplot()

greaterthan25changedens<- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BNE", "BINE", "TeBS", "TelBS", "TeBE", "BelBS", "BIBS") & GUESS.dens.pfts.change$Year >=1750 & GUESS.dens.pfts.change$dens.change.facet %in% ">=15%",], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)
#ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & GUESS.dens.pfts.change$Year >=1750 & GUESS.dens.pfts.change$Dens.change >= 15,], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)
#ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & GUESS.dens.pfts.change$Year >=1750 & GUESS.dens.pfts.change$Dens.change <= 10,], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)
lessthan0changedens <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BNE", "BINE", "TeBS", "TelBS", "TeBE", "BelBS", "BIBS") & GUESS.dens.pfts.change$Year >=1750 & GUESS.dens.pfts.change$Dens.change <= 0,], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_changes_by_site_declining_dens_850_1850_compared1950_2011.png")
lessthan0changedens
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_changes_by_site_15pctincreasing_dens_850_1850_compared1950_2011.png")
greaterthan25changedens
dev.off()


#ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("conifer.late") & GUESS.dens.pfts.change$Year >=1750,], aes(LAI, Dens, color = precip.mm))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")
png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_LAI_byPrecip_1750.png")
ggplot(na.omit(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BINE") & GUESS.dens.pfts.change$Year >=1750,]), aes(LAI, Dens, color = precip.mm))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_Precip_byLAI_1750.png")
ggplot(na.omit(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BINE") & GUESS.dens.pfts.change$Year >=1750,]), aes(precip.mm, Dens, color = LAI))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "LAI")
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_AGB_byLAI_1750.png")
ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BINE") & GUESS.dens.pfts.change$Year >=1750,], aes(AGB, Dens, color = LAI))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "LAI")
dev.off()


GUESS.dens.pfts.change$year.facet <- ifelse(GUESS.dens.pfts.change$Year >= 1950, ">1950", 
                                         ifelse(GUESS.dens.pfts.change$Year >= 1850 & GUESS.dens.pfts.change$Year <= 1950, "1850-1950", 
                                                "pre-1850"))
png(height = 10, width = 8, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_AGB_byPrecip_timeperiod_facet.png")
ggplot(na.omit(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BINE"),]), aes(AGB, Dens, color = precip))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")+facet_wrap(~year.facet)
dev.off()

png(height = 10, width = 8, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_LAI_byPrecip_timeperiod_facet.png")
ggplot(na.omit(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BINE"),]), aes(LAI, Dens, color = precip))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")+facet_wrap(~year.facet)
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

png(height = 6, width = 7, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_wue_precip_tmax_850_1850_compared1950_2011.png")
plot_grid(plot_grid(precip.tmax6.change.prec+theme(legend.position = "none"), 
                    WUEet.tmax6.change.prec+theme(legend.position = "none"), 
                    WUEet.precip.change.prec+theme(legend.position = "none"), 
                    WUEet.precip.change.prec+theme(legend.position = "none"),
                    WUEt.precip.change.prec+theme(legend.position = "none"),ncol = 3),
          legend.MAP,ncol = 2, rel_widths = c(1,0.25))
dev.off()




pct <- pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]

summary(lm(WUEet.change ~ Mean_MAP.wy + tmax6_change + precip.change, data = pct))
summary(lm(WUEet.change ~ Mean_MAP.wy + tmax6_change + precip.change, data = pct))
summary(lm(WUEet.change ~ Mean_MAP.wy + tmax6_change + precip.change + GPP.change + ET.change, data = pct))

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

# get an idea of what might be driving this:
summary(lm(WUEet.change ~ GPP.change + ET.change + Evap.change + Transp.change + precip.change, data = pct))
summary(lm(WUEt.change ~ GPP.change + ET.change + Evap.change + Transp.change + precip.change, data = pct))

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

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_delta_wueet_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaWUE, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in WUEet", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

# dims <- par("usr")
# x <- dims[1]+ 0.9*diff(dims[1:2])
# y <- dims[3]+ 0.08*diff(dims[3:4])
# text(x,y,"% change in Precip",srt=70)
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

pct <- pct.change.site[pct.change.site$PFT %in% "Total.gwbi",]

# fit linear models to see what explains the delta WUE best, changes in GPP, or changes in ET
#summary(lm(IWUE.change ~ GPP.change + ET.change + Transp.change + Evap.change + Mean_MAP.wy + tmax6_change, data = pct))
summary(lm(WUEet.change ~ GPP.change + ET.change + Transp.change + Evap.change + Mean_MAP.wy+ tmax6_change, data = pct))
summary(lm(WUEt.change ~ GPP.change + ET.change + Transp.change + Evap.change + Mean_MAP.wy+ tmax6_change, data = pct))

hist(pct.change$GPP.change - pct.change$ET.change) # GPP increases more than ET at most sites
hist(pct.change$GPP.change - pct.change$Transp.change) # GPP increases more than TRansp at most sites
hist(pct.change$GPP.change - pct.change$Evap.change) # GPP increases more than Evap at ~ half the sites
hist(pct.change$Transp.change - pct.change$Evap.change) # Evap increases more than Transp at ~ half the sites

summary(pct.change$GPP.change - pct.change$ET.change) 

LPJ.GUESS.pct.change <- pct.change

# add better maps as background

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

library(rnaturalearth)
#  is all downloaded from <a href=>http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

# lakes
ne_lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical')
sp::plot(ne_lakes, col = 'blue')
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

# ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
#   geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
#   geom_raster(data = pct, aes(lon, lat, fill = IWUE.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

delta.WUEet.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = WUEet.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_WUEet_change_map.png")
delta.WUEet.change
dev.off()

deltaWUEt.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = WUEt.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_WUEt_change_map.png")
delta.WUEt.change
dev.off()

delta.precip.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = precip.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_precip_change_map.png")
delta.precip.change
dev.off()

delta.GPP.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = GPP.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_GPP_change_map.png")
delta.GPP.change 
dev.off()

delta.Evap.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = Evap.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_Evap_change_map.png")
delta.Evap.change
dev.off()

delta.ET.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = ET.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_ET_change_map.png")
delta.ET.change
dev.off()

delta.transp.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct, aes(lon, lat, fill = Transp.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_Transp_change_map.png")
delta.Transp.change
dev.off()

delta.fcomp.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = pct.change[!pct.change$PFT %in% c("BeIBS.gwbi", "TrIBE.gwbi"),], aes(lon, lat, fill = Fcomp.change))+scale_fill_gradientn(colours = rev(heat.colors(10)))+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))+facet_wrap(~PFT, ncol = 2)

png(height = 10, width = 7, units = "in", res= 300,"outputs/preliminaryplots/GUESS_Fcomp_PFT_change_map.png")
delta.fcomp.change 
dev.off()


png(height = 20, width = 25, units = "in", res= 300,"outputs/preliminaryplots/GUESS_all_changes_map.png")
plot_grid(delta.WUEet.change, 
          deltaWUEt.change,
          delta.GPP.change,
          delta.transp.change, 
          delta.precip.change, 
          delta.Evap.change, 
          delta.ET.change,
          ncol = 23)
dev.off()


ggplot(pct.change, aes(GPP.change, Transp.change))+geom_point()+stat_smooth(method = "lm")
ggplot(pct.change, aes(GPP.change, Evap.change))+geom_point()+stat_smooth(method = "lm")
ggplot(pct.change, aes(GPP.change, ET.change))+geom_point()+stat_smooth(method = "lm")
ggplot(pct.change, aes(GPP.change, Fcomp.change, color = PFT))+geom_point()+stat_smooth(method = "lm")
ggplot(pct.change, aes(GPP.change, GWBI.change, color = PFT))+geom_point()+facet_wrap(~PFT)+stat_smooth(method = "lm")



#-----------------------Plot deltaGPP-deltaET to see where GPP or ET is driving increased WUE--------------
GUESS.850.1850 <- GUESS.rm.site %>%  group_by(lat, lon, Site, PFT) %>% filter(Year >= 1849) %>% summarise(#WUEet.850.1850 = mean(WUEet, na.rm=TRUE),
  WUE.et.850.1850 = mean(WUEet, na.rm=TRUE),
  WUE.t.850.1850 = mean(WUEt, na.rm=TRUE),
  GWBI.850.1850 = mean(GWBI, na.rm=TRUE),
  MAP.wy.850.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
  Tmax_6.850.1850 = mean(tair_max_6, na.rm =TRUE),
  GPP.850.1850 = mean (GPP*1000, na.rm=TRUE),
  ET.850.1850 = mean (ET, na.rm=TRUE),
  Transp.850.1850 = mean (Transp, na.rm=TRUE),
  Evap.850.1850 = mean (Evap, na.rm=TRUE),
  Fcomp.850.1850 = mean(Fcomp, na.rm = TRUE))

GUESS.1950.2011<- GUESS.rm.site %>%  group_by(lat, lon, Site, PFT) %>% filter(Year >= 1950) %>% summarise(#WUEet.850.1850 = mean(WUEet, na.rm=TRUE),
  WUE.et.1950.2011 = mean(WUEet, na.rm=TRUE),
  WUE.t.1950.2011 = mean(WUEt, na.rm=TRUE),
  GWBI.1950.2011 = mean(GWBI, na.rm=TRUE),
  MAP.wy.1950.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
  Tmax_6.1950.2011 = mean(tair_max_6, na.rm =TRUE),
  GPP.1950.2011 = mean (GPP*1000, na.rm=TRUE),
  ET.1950.2011 = mean (ET, na.rm=TRUE),
  Transp.1950.2011 = mean (Transp, na.rm=TRUE),
  Evap.1950.2011 = mean (Evap, na.rm=TRUE),
  Fcomp.1950.2011 = mean(Fcomp, na.rm = TRUE))

guess.time.periods <- left_join(GUESS.1950.2011, GUESS.850.1850, by = c("lat", "lon", "Site", "PFT"))

guess.mean.diff.change <- guess.time.periods %>% group_by(lat, lon, Site, PFT) %>% summarise(#WUEet.change = mean(((WUEet.1950.2011 - WUEet.850.1850)/WUEet.850.1850)*100, na.rm=TRUE),
  WUEet.change = mean(((WUE.et.1950.2011 - WUE.et.850.1850)), na.rm=TRUE),
  WUEt.change = mean(((WUE.t.1950.2011 - WUE.t.850.1850)), na.rm=TRUE),
  precip.change = mean(((MAP.wy.1950.2011 - MAP.wy.850.1850)), na.rm=TRUE),
  tmax6_change = mean((Tmax_6.1950.2011 - Tmax_6.850.1850), na.rm=TRUE),
  GWBI.change = mean(((GWBI.1950.2011 - GWBI.850.1850)), na.rm=TRUE),
  GPP.change = mean(((GPP.1950.2011 - GPP.850.1850)), na.rm=TRUE),
  ET.change = mean(((ET.1950.2011 - ET.850.1850)), na.rm=TRUE),
  Transp.change = mean(((Transp.1950.2011 - Transp.850.1850)), na.rm=TRUE), 
  Evap.change = mean(((Evap.1950.2011 - Evap.850.1850)), na.rm=TRUE),
  Fcomp.change = mean(((Fcomp.1950.2011 - Fcomp.850.1850)), na.rm=TRUE),
  GPP.ET.diff = (GPP.1950.2011 - GPP.850.1850) - (ET.1950.2011 - ET.850.1850),
  GPP.ET.ratio = mean((GPP.1950.2011 - GPP.850.1850) / (ET.1950.2011 - ET.850.1850)),
  model = "LPJ-GUESS")



ED2.850.1850 <- ED2.rm.site %>%  group_by(lat, lon, Site, PFT) %>% filter(Year >= 1849) %>% summarise(#WUEet.850.1850 = mean(WUEet, na.rm=TRUE),
  WUE.et.850.1850 = mean(WUEet, na.rm=TRUE),
  WUE.t.850.1850 = mean(WUEt, na.rm=TRUE),
  GWBI.850.1850 = mean(GWBI, na.rm=TRUE),
  MAP.wy.850.1850 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
  Tmax_6.850.1850 = mean(tair_max_6, na.rm =TRUE),
  GPP.850.1850 = mean (GPP*1000, na.rm=TRUE),
  ET.850.1850 = mean (ET, na.rm=TRUE),
  Transp.850.1850 = mean (Transp, na.rm=TRUE),
  Evap.850.1850 = mean (Evap, na.rm=TRUE),
  Fcomp.850.1850 = mean(Fcomp, na.rm = TRUE))

ED2.1950.2011 <- ED2.rm.site %>%  group_by(lat, lon, Site, PFT) %>% filter(Year >= 1950) %>% summarise(#WUEet.850.1850 = mean(WUEet, na.rm=TRUE),
  WUE.et.1950.2011 = mean(WUEet, na.rm=TRUE),
  WUE.t.1950.2011 = mean(WUEt, na.rm=TRUE),
  GWBI.1950.2011 = mean(GWBI, na.rm=TRUE),
  MAP.wy.1950.2011 = mean (precip_total_wtr_yr.mm, na.rm = TRUE), 
  Tmax_6.1950.2011 = mean(tair_max_6, na.rm =TRUE),
  GPP.1950.2011 = mean (GPP*1000, na.rm=TRUE), # GPP in gC/m2/s
  ET.1950.2011 = mean (ET, na.rm=TRUE),  # ET in kgC/m2/s
  Transp.1950.2011 = mean (Transp, na.rm=TRUE),
  Evap.1950.2011 = mean (Evap, na.rm=TRUE),
  Fcomp.1950.2011 = mean(Fcomp, na.rm = TRUE))

ED2.time.periods <- left_join(ED2.1950.2011, ED2.850.1850, by = c("lat", "lon", "Site", "PFT"))

ED2.mean.diff.change <- ED2.time.periods %>% group_by(lat, lon, Site, PFT) %>% summarise(#WUEet.change = mean(((WUEet.1950.2011 - WUEet.850.1850)/WUEet.850.1850)*100, na.rm=TRUE),
  WUEet.change = mean(((WUE.et.1950.2011 - WUE.et.850.1850)), na.rm=TRUE),
  WUEt.change = mean(((WUE.t.1950.2011 - WUE.t.850.1850)), na.rm=TRUE),
  precip.change = mean(((MAP.wy.1950.2011 - MAP.wy.850.1850)), na.rm=TRUE),
  tmax6_change = mean((Tmax_6.1950.2011 - Tmax_6.850.1850), na.rm=TRUE),
  GWBI.change = mean(((GWBI.1950.2011 - GWBI.850.1850)), na.rm=TRUE),
  GPP.change = mean(((GPP.1950.2011 - GPP.850.1850)), na.rm=TRUE),
  ET.change = mean(((ET.1950.2011 - ET.850.1850)), na.rm=TRUE),
  Transp.change = mean(((Transp.1950.2011 - Transp.850.1850)), na.rm=TRUE), 
  Evap.change = mean(((Evap.1950.2011 - Evap.850.1850)), na.rm=TRUE),
  Fcomp.change = mean(((Fcomp.1950.2011 - Fcomp.850.1850)), na.rm=TRUE),
  GPP.ET.diff = mean((GPP.1950.2011 - GPP.850.1850) - (ET.1950.2011 - ET.850.1850)),
  GPP.ET.ratio = mean((GPP.1950.2011 - GPP.850.1850) / (ET.1950.2011 - ET.850.1850)),
  model = "ED2")


mean.diff.change <- rbind(guess.mean.diff.change, ED2.mean.diff.change)

ggplot(data = mean.diff.change)+geom_histogram(data = mean.diff.change, aes(ET.change), fill = "blue", alpha = 0.5)+
  geom_histogram(data = mean.diff.change, aes(GPP.change), fill = "red", alpha = 0.5)+geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")+
  theme_bw(base_size = 20)+facet_wrap(~model)


ggplot(data = mean.diff.change)+geom_histogram(data = mean.diff.change, aes(GPP.ET.diff), fill = "blue", alpha = 0.5)+
  facet_wrap(~model)+geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")



ggplot(data = mean.diff.change, aes(lon, lat, fill = GPP.ET.diff))+geom_raster()+facet_wrap(~model,ncol = 1)+scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                                                                                                                          high="red", space ="Lab" )


mean.diff.change.summmary <- mean.diff.change %>% filter(PFT %in% c("mean.gwbi", "Total.gwbi")) %>% group_by(model) %>% summarise(mean = mean(GWBI.change),
                                                                                                                                  Ci.low = quantile(GWBI.change, 0.025),
                                                                                                                                  Ci.high = quantile(GWBI.change, 0.975))
ggplot(data = mean.diff.change.summmary , aes( x = model,y=mean, fill = model))+geom_bar(stat = "identity")+
  geom_errorbar(aes(x = model, ymin = Ci.low, ymax = Ci.high), width = 0.1)+ylab("Mean change in Stem growth \n between past and modern")
                                                                                                                                  

delta.GPP.ET.change <- ggplot()+geom_polygon( data = mapdata, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+
  geom_polygon( data = ca.data, aes(group = group,x=long, y =lat),colour="darkgrey", fill = NA)+geom_polygon(data=lakes.subset, aes(x = long, y = lat, group = group), fill = '#a6bddb')+ 
  geom_raster(data = mean.diff.change, aes(lon, lat, fill = GPP.ET.diff))+scale_fill_gradient2(midpoint=0, low="blue", mid="white",
                                                                                               high="red", space ="Lab" )+theme_bw()+coord_cartesian(ylim = c(35, 49), xlim= c(-100,-61))+facet_wrap(~model, ncol = 1)

png(height = 4, width = 7, units = "in", res= 300,"outputs/preliminaryplots/ED2_GUESS_GPP_minus_ET_change_map.png")
delta.GPP.ET.change
dev.off()

ggplot(data = mean.diff.change)+geom_histogram(data = mean.diff.change, aes(GPP.ET.ratio), fill = "blue", alpha = 0.5)+
  facet_wrap(~model)+geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")+xlim(-10,10)


mean.diff.change.1pft <- mean.diff.change %>%dplyr::select(-PFT, -Fcomp.change)

GPP.ET.diff.unique <- unique(mean.diff.change.1pft)
library(ggridges)

GPP.ET.differences <- ggplot(GPP.ET.diff.unique, aes(x=GPP.ET.diff, y=model, fill = model, height = ..density..)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0), bw = 0.0000009,
    point_shape = '|', point_size = 1, point_alpha = 1, stat = "density",
    color = "cornsilk4")+geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed")+
  scale_fill_manual(name=" ", values=c("LPJ-GUESS"="#d95f02", "ED2"="#1b9e77"))+
  coord_flip()+ylab("")+xlab(expression(Delta~GPP ~ - ~Delta~ET))+
  theme_bw(base_size = 20)+theme(panel.grid = element_blank(), legend.position = c(0.75,0.2))

png(height = 5, width = 5, units = "in", res = 300, "outputs/preliminaryplots/change_GPP_change_mean_ET_mods.png")
GPP.ET.differences
dev.off()



GPP.ET.diff.unique$GPP.ET.class <- ifelse(GPP.ET.diff.unique$GPP.ET.diff >=0, "GPP", "ET")

ggplot(data = GPP.ET.diff.unique, aes(GPP.ET.class, precip.change, fill = model))+geom_boxplot()
ggplot(data = GPP.ET.diff.unique, aes(GPP.ET.class, tmax6_change, fill = model))+geom_boxplot()

ggplot()+geom_histogram(data = guess.mean.diff.change, aes(ET.change), fill = "blue", alpha = 0.5)+
  geom_histogram(data = guess.mean.diff.change, aes(GPP.change), fill = "red", alpha = 0.5)+geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")

ED2.time.periods$model <- "ED2"
guess.time.periods$model <- "LPJ-GUESS"
time.periods.both <- rbind(ED2.time.periods, guess.time.periods)


time.periods.both $model <- as.factor(time.periods.both$model)

# reformat to plot with geom_density ridges:
time.periods.gpp.et.mean <- time.periods.both %>%dplyr::select(model, lon, lat, Site,ET.1950.2011, ET.850.1850, GPP.1950.2011, GPP.850.1850)%>%group_by(model, lon, lat, Site) %>%
  gather(key = "timeperiod", value = "ET.GPP", ET.1950.2011:GPP.850.1850 )
time.periods.both <- time.periods.gpp.et.mean %>% separate(timeperiod, c("variable", "timestart", "timeend")) %>% unite(period, c(timestart, timeend), sep = "-")

ggplot()+geom_histogram(data = time.periods.both, aes(ET.GPP, fill = period), alpha = 0.5)+facet_grid(~model+variable)


ET.means<- ggplot(time.periods.both[time.periods.both$variable %in% "ET",], aes(x=ET.GPP, y=model, fill = period, adjust = 10, height = ..density..)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),alpha = 0.75,
    point_shape = '|', point_size = 1, point_alpha = 1, stat = "density", #bw = 4,
    color = "cornsilk4")+geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed")+
  scale_fill_manual(name=" ", values=c("1950-2011"="#b2182b", "850-1850"="#2166ac"))+
  coord_flip()+ylab("")+xlab("Mean ET (kg/m2/s)")+
  theme_bw(base_size = 20)+theme(panel.grid = element_blank(), legend.position = c(0.75,0.75))


GPP.means <- ggplot(time.periods.both[time.periods.both$variable %in% "GPP",], aes(x=ET.GPP, y=model, fill = period, adjust = 10, height = ..density..)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),alpha = 0.75,
    point_shape = '|', point_size = 1, point_alpha = 1, stat = "density", #bw = 4,
    color = "cornsilk4")+geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed")+
  scale_fill_manual(name=" ", values=c("1950-2011"="#b2182b", "850-1850"="#2166ac"))+
  coord_flip()+ylab("")+xlab("Mean GPP (g/m2/s)")+
  theme_bw(base_size = 20)+theme(panel.grid = element_blank(), legend.position = c(0.75,0.75))

png(height = 15, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GPP.ET.means_GPP.ET.change.png")
plot_grid(GPP.means, ET.means, GPP.ET.differences, ncol = 1, align = "hv", labels = "AUTO")
dev.off()

hist(abs(guess.mean.diff.change$GPP.change)/abs(guess.mean.diff.change$ET.change))
summary(abs(ED2.mean.diff.change$GPP.change)/abs(ED2.mean.diff.change$ET.change))
summary(abs(guess.mean.diff.change$GPP.change)/abs(guess.mean.diff.change$ET.change))

hist(abs(guess.mean.diff.change$GPP.change)-abs(guess.mean.diff.change$ET.change))
hist(ED2.mean.diff.change$GPP.change+ED2.mean.diff.change$ET.change)


# what we want to compare is the mean change in GPP vs ET 

ED2.uni <- ED2.pct.change %>% dplyr::select(-PFT, -Fcomp.change)
ED2.unique <- unique(ED2.uni)
ED2.unique$GPP.ET.diff <- ED2.unique$GPP.change - ED2.unique$ET.change
ED2.unique$model <- "ED2"

GUESS.uni <- LPJ.GUESS.pct.change %>% dplyr::select(-PFT, -Fcomp.change)
GUESS.unique <- unique(GUESS.uni)
GUESS.unique$GPP.ET.diff <- GUESS.unique$GPP.change - GUESS.unique$ET.change
GUESS.unique$model <- "LPJ-GUESS"

model.unique <- rbind(ED2.unique, GUESS.unique)
ggplot()+geom_density(data = model.unique, aes(GPP.ET.diff, fill = model), binwidth = 5, alpha = 0.5)+theme_bw()+
  geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")+coord_flip()

ggplot()+geom_density(data = model.unique, aes(ET.change, fill = model), binwidth = 5, alpha = 0.5)+theme_bw()+
  geom_vline(aes(xintercept = 0), color = "grey", linetype = "dashed")+coord_flip()


GPP.ET.mod <- ggplot(model.unique, aes(x=GPP.ET.diff, y=model, fill = model,adjust = 10, height = ..density..)) +
  geom_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 1, point_alpha = 1, stat = "density", bw = 4,
  color = "cornsilk4")+geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed")+
  scale_fill_manual(name=" ", values=c("LPJ-GUESS"="#d95f02", "ED2"="#1b9e77"))+
  coord_flip()+ylab("")+xlab(expression("%"~Delta~GPP ~ - ~"%"~Delta~ET))+
  theme_bw(base_size = 20)+theme(panel.grid = element_blank(), legend.position = c(0.75,0.2))

png(height = 5, width = 5, units = "in", res = 300, "outputs/preliminaryplots/change_GPP_change_ET_mods.png")
GPP.ET.mod
dev.off()




# assessment of which species are driving changes in Density in ED:
head(GUESS.dens.pfts.m)
pct.change.site.fcomp$Site <- paste0("X",pct.change.site.fcomp$Site)
GUESS.dens.pfts.m$Site <- paste0("X",GUESS.dens.pfts.m$Site)
GUESS.dens.pfts.m$PFT <- paste0(GUESS.dens.pfts.m$PFT , ".gwbi")
GUESS.dens.pfts.change <- merge(GUESS.dens.pfts.m, pct.change.site.fcomp, by = c("Site", "PFT"))

#ggplot(ED.dens.pfts, aes(Dens, PFTDens, color = PFT))+geom_point()+facet_wrap(~PFT)

# get the grid cells increaseing in density & plot trajectory by PFT over the 20th century:
#ggplot(ED.dens.pfts[ED.dens.pfts$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid"),], aes(Dens, PFTDens, color = PFT))+geom_point()+facet_wrap(~PFT)

# plot the PFT density by the % change in overall density
all.change.dens.1750.2011<- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi", "BINE.gwbi", "BNE.gwbi",  "TeBS.gwbi", "TeBE.gwbi") & GUESS.dens.pfts.change$Year >=1750,], aes(Year, Dens.x, color = Dens.change))+
  geom_point()+facet_wrap(~PFT)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "% change in Density")
GUESS.dens.pfts.change$dens.change.facet <- ifelse(GUESS.dens.pfts.change$Dens.change >=25, ">=25%", "<=25%")

change.dens.1750.2011 <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi", "BINE.gwbi", "BNE.gwbi",  "TeBS.gwbi", "TeBE.gwbi") & GUESS.dens.pfts.change$Year >=1750,], aes(Year, Dens.x, color = Dens.change))+
  geom_point()+facet_grid(~PFT+dens.change.facet)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "% change in Density")


ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("conifer.late", "pine.north", "temp.decid.early","temp.decid.late", "temp.decid.mid") & GUESS.dens.pfts.change$Year >=1750,], aes(dens.change.facet, PFTDens, fill = PFT))+geom_boxplot()

greaterthan25changedens<- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi", "BINE.gwbi", "BNE.gwbi",  "TeBS.gwbi", "TeBE.gwbi") & GUESS.dens.pfts.change$Year >=1750 & GUESS.dens.pfts.change$dens.change.facet %in% ">=25%",], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)
lessthan0changedens <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi", "BINE.gwbi", "BNE.gwbi",  "TeBS.gwbi", "TeBE.gwbi") & GUESS.dens.pfts.change$Year >=1750 & GUESS.dens.pfts.change$Dens.change <= 0,], aes(Year, PFTDens, color = PFT))+geom_point()+facet_wrap(~Site)

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_changes_by_site_declining_dens_850_1850_compared1950_2011.png")
lessthan0changedens
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_changes_by_site_25pctincreasing_dens_850_1850_compared1950_2011.png")
greaterthan25changedens
dev.off()


#ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("conifer.late") & GUESS.dens.pfts.change$Year >=1750,], aes(LAI, Dens, color = precip.mm))+geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "MAP (mm)")
png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_LAI_byPrecip_1750.png")
DENS.LAI.GUESS <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi") & GUESS.dens.pfts.change$Year >=1750,], aes(LAI, Dens.y, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("GUESS Tree Density")+xlab("GUESS LAI")
DENS.LAI.GUESS
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_Precip_byprecip_1750.png")
DENS.MAP.GUESS <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi") & GUESS.dens.pfts.change$Year >=1750,], aes(precip.mm, Dens.y, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("GUESS Tree Density")+xlab("Mean Annual Precipitation (mm)")
DENS.MAP.GUESS
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_Dens_AGB_byprecip_1750.png")
DENS.AGB.GUESS <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi") & GUESS.dens.pfts.change$Year >=1750,], aes(AGB, Dens.y, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("GUESS Tree Density")+xlab("GUESS AGB")
DENS.AGB.GUESS
dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/preliminaryplots/GUESS_AGB_LAI_byLAI_1750.png")
AGB.LAI.GUESS <- ggplot(GUESS.dens.pfts.change[GUESS.dens.pfts.change$PFT %in% c("BIBS.gwbi") & GUESS.dens.pfts.change$Year >=1750,], aes(AGB, LAI, color = precip.mm))+
  geom_point(size = 0.25)+scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Mean \n Annual \n precipitation \n (mm)")+theme_bw()+ylab("GUESS AGB")+xlab("GUESS LAI")
AGB.LAI.GUESS
dev.off()

emerg.leg <- get_legend(AGB.LAI.GUESS+theme_bw(base_size = 18))


# save all emergent relationships to one png
png(height = 10, width = 6*3, units = "in", res = 300, "outputs/preliminaryplots/GUESS_AGB_DENS_LAI_emergent.png")
plot_grid(DENS.AGB.GUESS +theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          DENS.LAI.GUESS+theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          AGB.LAI.GUESS+theme_bw(base_size = 18)+ theme(legend.position = "none"),
          emerg.leg, rel_widths = c(1,1,1,0.25),
          ncol = 4, align = "hv")
dev.off()


# plot all ED2 and lpj-guess together
png(height = 10, width = 6*3, units = "in", res = 300, "outputs/preliminaryplots/GUESS_ED_AGB_DENS_LAI_emergent.png")
plot_grid(
plot_grid(DENS.AGB.ED +theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          DENS.LAI.ED+theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          AGB.LAI.ED+theme_bw(base_size = 18)+ theme(legend.position = "none"),
          
          DENS.AGB.GUESS +theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          DENS.LAI.GUESS+theme_bw(base_size = 18)+ theme(legend.position = "none"), 
          AGB.LAI.GUESS+theme_bw(base_size = 18)+ theme(legend.position = "none"),
          
          ncol = 3, align = "hv", labels = "AUTO"),
emerg.leg, rel_widths = c(1,0.15)
)
dev.off()
