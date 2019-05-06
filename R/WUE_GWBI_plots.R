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
                                                                           tmax6_change = mean((Tmax_6.1950.2011 - Tmax_6.850.1850), na.rm=TRUE),
                                                                           GWBI.change = mean(((GWBI.1950.2011 - GWBI.850.1850)/GWBI.850.1850)*100, na.rm=TRUE))

GWBI.WUE.change <- ggplot(pct.change, aes(IWUE.change, GWBI.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
IWUE.precip.change <- ggplot(pct.change, aes(IWUE.change, precip.change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())
IWUE.temp.change <- ggplot(pct.change, aes(IWUE.change, tmax6_change, color = PFT))+geom_point()+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT)+theme_bw()+theme(panel.grid = element_blank())

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
precip.iwue.lm <- summary(lm(precip.change ~ IWUE.change, data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
IWUE.precip.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( precip.change, IWUE.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+xlab("% Change in precipitation")+ylab("% change in IWUE")+geom_text( label = paste("p = ", round(precip.iwue.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")


tmax.iwue.lm <- summary(lm(tmax6_change ~ IWUE.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
IWUE.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( tmax6_change, IWUE.change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", round(tmax.iwue.lm$coefficients[2,4], 4), sep=""),x=1, y=0.25, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in IWUE")


tmax.precip.lm <- summary(lm(tmax6_change ~ precip.change,data = pct.change.site[pct.change.site$PFT %in% "conifer.late",]))
#p.vals.tmax6.m$value <- ifelse(p.vals.tmax6.m$value <= 0.0001, "0.00001", p.vals.tmax6.m$value )

precip.tmax6.change.prec <- ggplot(pct.change.site[pct.change.site$PFT %in% "conifer.late",], aes( precip.change, tmax6_change, color = Mean_MAP.wy))+geom_point(size = 0.5)+stat_smooth(method = "lm", color = "black")+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(panel.grid = element_blank())+
  scale_colour_gradientn(colours = rev(colorRamps::blue2red(10)), name = "Site MAP")+geom_text( label = paste("p = ", tmax.precip.lm$coefficients[2,4], sep=""),x=2, y=0.025, color = "black")+xlab("Change in Summer Tmax (DegC)")+ylab("% change in precip")


# save these plots to png
library(cowplot)
legend.MAP <- get_legend(precip.tmax6.change.prec) 

png(height = 4, width = 10, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_wue_precip_tmax_850_1850_compared1950_2011.png")
plot_grid(precip.tmax6.change.prec+theme(legend.position = "none"), 
          IWUE.tmax6.change.prec+theme(legend.position = "none"), 
          IWUE.precip.change.prec+theme(legend.position = "none"), 
          legend.MAP,ncol = 4, rel_widths = c(1,1,1,0.25))
dev.off()



# ---------------Make 3 dimensional plots to visualize multiple respones:------------
# 1. changes in WUE vs temperature and precip, colored by mean MAP
# 2. changes in GWBI vs temperature and precip, colored by mean MAP
# 3. changes in GWBI in respones to temperature, precip, colored by change in WUE

#library(plot3D)
library(car)
library(rgl)

# 1. changes in WUE vs temperature and precip, colored by mean MAP
pct <- pct.change.site[pct.change.site$PFT %in% "conifer.late",]
rgl::plot3d( pct$precip.change, pct$IWUE.change,  pct$tmax6_change,)



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

# 2. changes in GWBI vs temperature and precip, colored by mean MAP

# will need to loop through each unique pft and make these plots:

plot.angle <- 65

png(height = 5.5, width = 6, units = "in", res = 300, "outputs/preliminaryplots/ED2_delta_wue_precip_tmax_3D_850_1850_compared1950_2011.png")

scatterplot3d(deltaGWBI, deltaPrecip, deltatmax, type="h", angle=plot.angle, color=colcode, pch=20, cex.symbols=1,
              col.axis="gray", col.grid="gray", xlab = "% change in IWUE", ylab = "% change in Precip", zlab = "Change in Tmax (DegC)", mar=c(5, 3, 5, 7)+0.1) 

par(mar=c(5, 4, 4, 2) + 0.1) 
fields::image.plot( legend.only=TRUE, zlim= c(min(plotvar), max(plotvar)), nlevel=8, 
                    col=brewer.pal(nclr,"RdYlBu"), legend.args=list( text="   Site MAP (mm)",
                                                                     col="black", cex=1, side=3, line=1)) 
dev.off()
