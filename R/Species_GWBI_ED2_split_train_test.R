library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(caTools)


# -------------------Join the species specific growth from ED2, LINKAGES, GUESS to met driver data--------------

ED2.fcomp.pft <- readRDS("outputs/data/ED2/ED2_mean_yearly_fcomp.rds")
ggplot(ED2.fcomp.pft[ED2.fcomp.pft$Site %in% "1",], aes(Year, pine.north))+geom_line()


ED2.gwbi <- readRDS("Data/ED2/ED2.GWBI.rds")
yrlyvar <- (0:1160) + 850

# make plots for ED2:
timevec <- 1:13932
month <- rep(1:12, 1161)
yearsince  <- rep(0:1160, each =12)
year <- yearsince + 850

dimnames(ED2.gwbi) <- list(timevec, paleon$num)

ED2.gwbi.m <- melt(ED2.gwbi)
head(ED2.gwbi.m) # year, site, GWBI
colnames(ED2.gwbi.m) <- c("time", "Site",  "GWBI")
ED2.gwbi.m$Year <- year 
ED2.gwbi.m$Month <- month 
ED2.gwbi.m$num <- as.numeric(ED2.gwbi.m$Site)

ED2.gwbi.by.year <- ED2.gwbi.m %>% group_by(Site,num,Year) %>% summarise(total.gwbi = sum(GWBI, na.rm=TRUE), 
                                                                          mean.gwbi = mean(GWBI, na.rm=TRUE))
# total and mean are basically the same:
ggplot(ED2.gwbi.by.year, aes(total.gwbi, mean.gwbi))+geom_point()

ED2.gwbi.fcomp <- left_join(ED2.gwbi.by.year, ED2.fcomp.pft, by = c("Year", "Site"))


ED2.gwbi.fcomp.weighted <-  ED2.gwbi.fcomp
ED2.gwbi.fcomp.weighted[,6:length(ED2.gwbi.fcomp.weighted)] <- ED2.gwbi.fcomp.weighted$total.gwbi*ED2.gwbi.fcomp.weighted[,6:length(ED2.gwbi.fcomp.weighted)]

# map out each of PFT's changes separately:
ggplot(ED2.gwbi.fcomp.weighted, aes(total.gwbi, pine.north))+geom_point()
ggplot(ED2.gwbi.fcomp.weighted, aes(total.gwbi, temp.decid.early))+geom_point()
ggplot(ED2.gwbi.fcomp.weighted, aes(total.gwbi, temp.decid.mid))+geom_point()
ggplot(ED2.gwbi.fcomp.weighted[ED2.gwbi.fcomp.weighted$Site %in% "1",], aes(Year, temp.decid.early))+geom_line()

ED2.gwbi.pft <- ED2.gwbi.fcomp.weighted
load("Data/PalEON_siteInfo_all.RData")
ED2.gwbi.pft$num <- ED2.gwbi.pft$Site

ED2.gwbi.pft.ll <- left_join( paleon[,c("num", "lon", "lat")], ED2.gwbi.pft, by = c("num"))


# We already collated met drivers:
# tair.summaries.df<- readRDS(paste0(getwd(),"/Data/MET/tair.summary.rds"))
# precipf.summaries.df<- readRDS(paste0(getwd(),"/Data/MET/precipf.summary.rds"))
# 
# # need to reformat temperature data to get monthly
# tair.mo.mean <- tair.summaries.df %>% select (lon, lat, year, month, tair_mean) %>% 
#                 group_by(lon, lat, year) %>% spread(month, tair_mean)
# 
# tair.mo.min <- tair.summaries.df %>% select (lon, lat, year, month, tair_min) %>% 
#   group_by(lon, lat, year) %>% spread(month, tair_min)
# 
# tair.mo.max <- tair.summaries.df %>% select (lon, lat, year, month, tair_max) %>% 
#   group_by(lon, lat, year) %>% spread(month, tair_max)
# 
# precipf.mo.sum <- precipf.summaries.df %>% select (lon, lat, year, month, precipf_sum) %>% 
#   group_by(lon, lat, year) %>% spread(month, precipf_sum)
# 
# # rename columns & join together:
# colnames(precipf.mo.sum)[4:length(colnames(precipf.mo.sum))] <- paste0("precip_", colnames(precipf.mo.sum)[4:length(colnames(precipf.mo.sum))])
# colnames(tair.mo.mean)[4:length(colnames(tair.mo.mean))] <- paste0("tair_mean_", colnames(tair.mo.mean)[4:length(colnames(tair.mo.mean))])
# colnames(tair.mo.max)[4:length(colnames(tair.mo.max))] <- paste0("tair_max_", colnames(tair.mo.max)[4:length(colnames(tair.mo.max))])
# colnames(tair.mo.min)[4:length(colnames(tair.mo.min))] <- paste0("tair_min_", colnames(tair.mo.min)[4:length(colnames(tair.mo.min))])
# 
# precipf.mo.sum$precip_total <- rowSums(precipf.mo.sum[4:15])
# 
# 
# # get data reformatted to have months in columns & merge with sites:
# 
# 
# # create function to get the water year
# wtr_yr <- function(df, start_month=9) {
#   # Year offset
#   offset = ifelse(as.numeric(df$month) >= start_month - 1, 1, 0)
#   # Water year
#   adj.year = as.numeric(df$year) + offset
#   # Return the water year
#   adj.year
# }
# 
# # use wtr_year function to get water year as a column
# precipf.summaries.df$wtr.year <- wtr_yr(precipf.summaries.df)
# 
# precipf.mo.sum.wy <- precipf.summaries.df %>% select (lon, lat, wtr.year, month, precipf_sum) %>% 
#   group_by(lon, lat, wtr.year) %>% spread(month, precipf_sum)
# 
# precipf.mo.sum.wy$precip_total_wtr_yr <- rowSums(precipf.mo.sum.wy[4:15], na.rm=TRUE)
# colnames(precipf.mo.sum.wy)[3] <- "year"
# # left_join climate together:
# 
# precip.full <- left_join(precipf.mo.sum, precipf.mo.sum.wy[,c("lon","lat", "year", "precip_total_wtr_yr")], by =c("lon", "lat", "year"))
# precip.tmean <- left_join(precip.full, tair.mo.mean, by =c("lon","lat", "year") )
# precip.tmean.tmax <- left_join(precip.tmean, tair.mo.max, by =c("lon","lat", "year") )
# all.met <- left_join(precip.tmean.tmax, tair.mo.min, by =c("lon","lat", "year") )
# convert2mm <- 60*60*24
# all.met$precip_total_wtr_yr.mm <- all.met$precip_total_wtr_yr*convert2mm # convert to mm
# all.met$precip_total.mm <- all.met$precip_total*convert2mm
# 
# # save all the met:
# saveRDS(all.met, paste0(getwd(),"/Data/MET/all.met.summary.rds"))

all.met <- readRDS( paste0(getwd(),"/Data/MET/all.met.summary.rds"))
# merge climate and growth for ED2:
colnames(all.met)[3] <- "Year"
ED2.all <- left_join(ED2.gwbi.pft.ll, all.met, by = c("lon", "lat", "Year"))

saveRDS(ED2.all, paste0(getwd(),"/outputs/data/ED2/ED2.gwbi.pft.all.met.rds"))

ggplot(ED2.all, aes( tair_mean_6, precip_total.mm, color = temp.decid.mid))+geom_point()

# now need to filter out grid cells/times where fcomp does not include the taxa of choice:

ED2.all <- readRDS( paste0(getwd(),"/outputs/data/ED2/ED2.gwbi.pft.all.met.rds"))

ED2.df <- ED2.all

# if fcomp == 0, then set gwbi to NA (there is a more elegant way of doing this but, its okay)
ED2.df[, 8:24][ED2.df[, 8:24] == 0] <- NA



# need to relativize tree growth by mean for each site (& species)
ggplot(ED2.df, aes( tair_mean_6, precip_total.mm, color = temp.decid.early))+geom_point()

# -----------------------------get gwbi-1 and gwbi-2:----------------------------------
# calculate lagged gwbi:
ED2.df.slim <- ED2.df %>% dplyr::select(num:Araucaria) %>% group_by(num, lon, lat, Year, Site) %>% gather(key = PFT, value = GWBI, mean.gwbi:Araucaria)

min.totals <- ED2.df.slim %>% group_by(Site, PFT) %>% summarise(min.gwbi = min(GWBI, na.rm = TRUE), 
                                                          mean.gwbi = mean(GWBI, na.rm = TRUE))
GWBI.ED2.mins <- merge(ED2.df.slim, min.totals, by = c("Site", "PFT"))
rel.ED2.gwbi <- GWBI.ED2.mins #%>% group_by(lon, lat, Site, num,Year, PFT) %>% dplyr::summarise(rel.gwbi = GWBI - (min.gwbi-0.15),
                                   
#                                      rel.gwbi.raw = GWBI - (min.gwbi), 
 #                                                                        mean.diff = GWBI - mean.gwbi)
rel.ED2.gwbi$rel.gwbi.raw <- rel.ED2.gwbi$GWBI - rel.ED2.gwbi$min.gwbi
rel.ED2.gwbi$rel.gwbi <- rel.ED2.gwbi$GWBI - (rel.ED2.gwbi$min.gwbi-0.015)
rel.ED2.gwbi$mean.diff <- rel.ED2.gwbi$GWBI - rel.ED2.gwbi$mean.gwbi
hist(as.numeric(rel.ED2.gwbi$rel.gwbi.raw), breaks = 100)
hist(as.numeric(rel.ED2.gwbi$mean.diff), breaks = 100)
hist(as.numeric(rel.ED2.gwbi$GWBI), breaks = 100)


ggplot(rel.ED2.gwbi[rel.ED2.gwbi$Site %in% "4",], aes( Year , rel.gwbi, color = PFT))+geom_line()

head(rel.ED2.gwbi)
ED2.df.slim <- rel.ED2.gwbi[,c("num", "lon", "lat", "Year", "Site", "PFT", "rel.gwbi")]
colnames(ED2.df.slim)[7] <- "GWBI" 
ED2.df.slim <- ED2.df.slim[!is.na(ED2.df.slim$Year),]

# get previous years growth for ED2.df
PFT.groups <- as.list(unique(ED2.df.slim$PFT))
# PFT.group <- "temp.decid.late"
# get_prev_gwbi(PFT.group)

get_prev_gwbi <- function(PFT.group){
  cat(PFT.group)
      x <- ED2.df.slim[ED2.df.slim$PFT %in% PFT.group,]
      
      uni.Sites <- unique(x$Site)
      ED2.df.sort <- x[with(x, order(Site, Year, PFT)),]
      rownames(ED2.df.sort) <- NULL
      ED2.df.sort.wide <- ED2.df.sort[,c("Year", "Site", "PFT", "GWBI")] %>% group_by(Year) %>% spread(key = "Site", value = "GWBI")
      
      tail(ED2.df.sort) #279802, 279803, 279804, 279805, 279806, 279807, 279808, 279809, 279810, 279811, 279812, 279813, 279814
      ED2.df.sort_1.wide <- ED2.df.sort.wide[1:(length(ED2.df.sort.wide$Year)-1),]
      ED2.df.sort_1.wide$Year <- ED2.df.sort.wide[2:(length(ED2.df.sort.wide$Year)),]$Year
      
      ED2.df.sort_2.wide <- ED2.df.sort.wide[1:(length(ED2.df.sort.wide$Year)-2),]
      ED2.df.sort_2.wide$Year <- ED2.df.sort.wide[3:(length(ED2.df.sort.wide$Year)),]$Year
      
      ED2.df.sort_3.wide <- ED2.df.sort.wide[1:(length(ED2.df.sort.wide$Year)-3),]
      ED2.df.sort_3.wide$Year <- ED2.df.sort.wide[4:(length(ED2.df.sort.wide$Year)),]$Year
      
      ED2.df.sort_4.wide <- ED2.df.sort.wide[1:(length(ED2.df.sort.wide$Year)-4),]
      ED2.df.sort_4.wide$Year <- ED2.df.sort.wide[5:(length(ED2.df.sort.wide$Year)),]$Year
      
      ED2.df.sort_5.wide <- ED2.df.sort.wide[1:(length(ED2.df.sort.wide$Year)-5),]
      ED2.df.sort_5.wide$Year <- ED2.df.sort.wide[6:(length(ED2.df.sort.wide$Year)),]$Year
      
      ED2.df.sort.norm <- melt(ED2.df.sort.wide, id.vars = c("Year", "PFT"))
      colnames(ED2.df.sort.norm) <- c("Year","PFT", "Site", "GWBI")
      ED2.df.sort_1 <- melt(ED2.df.sort_1.wide, id.vars = c("Year", "PFT"))
      colnames(ED2.df.sort_1) <- c("Year","PFT", "Site", "GWBI_1")
      ED2.df.sort_2 <- melt(ED2.df.sort_2.wide, id.vars = c("Year", "PFT"))
      colnames(ED2.df.sort_2) <- c("Year","PFT","Site", "GWBI_2")
      
      
      ED2.df.sort.norm1 <-  left_join(ED2.df.sort.norm, ED2.df.sort_1, by = c("Year", "Site", "PFT"))
      ED2.df.sort_lag <- left_join(ED2.df.sort.norm1, ED2.df.sort_2, by = c("Year", "Site", "PFT"))
      ED2.df.sort_lag
}

list.of.prev.gwbi <- lapply(PFT.groups, get_prev_gwbi)
prev.gwbi <- do.call(rbind, list.of.prev.gwbi)

head(prev.gwbi)

# -------------------join with climate data again---------------
climate.only <- ED2.df %>% dplyr::select(num:Year, precip_1:precip_total.mm)
climate.only$Site <- as.character(climate.only$num)

gwbi.clim <- left_join(prev.gwbi, climate.only, by =c("Site", "Year"))
ggplot(gwbi.clim, aes(tair_mean_6, GWBI))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~PFT)

#---------------- do some preliminary analyses:-----------------

ED2.gwbi <- gwbi.clim #[,c("lon", "lat","PFT", "Site", "Year",  "GWBI", "GWBI_1", "GWBI_2")]
ED2.gwbi.clim.nona <- ED2.gwbi[!is.na(ED2.gwbi$GWBI),]

# omit pfts that have no tree data:
ED2.gwbi.clim.nona <- ED2.gwbi.clim.nona[ED2.gwbi.clim.nona$PFT %in% c("conifer.late", "pine.north", "temp.decid.early", "temp.decid.mid","temp.decid.late","mean.gwbi"),]
#ED2.gwbi.clim <- left_join(ED2.gwbi.nona, tmax.month, by = c("Longitude", "Latitude","SPEC.CODE", "studyCode", "year"))


library(Hmisc) # You need to download it first.

correlate.tmax <- function(x){
  test.nona <- ED2.gwbi.clim.nona[ED2.gwbi.clim.nona$Site %in% x, ]
  cat(x)
  test.nona$GWBI <- as.numeric(test.nona$GWBI)
  #corM <- cor(test.nona$GWBI, test.nona[, c("tair_max_1", "tair_max_2", "tair_max_3", "tair_max_4", "tair_max_5", "tair_max_6", "tair_max_7", "tair_max_8", "tair_max_9", "tair_max_10", "tair_max_11", "tair_max_12")], use = "pairwise.complete")
  PFTS <- unique(test.nona$PFT)
  cor.mat.site <- lapply(PFTS, function(a){
  
         test.PFT <-  test.nona[test.nona$PFT %in% a,]
         if(length(test.PFT$Year) <= 4){
           cor.mat.df <- data.frame(PFT = a,
                                    month = colnames(test.PFT)[10:length(test.PFT)],
                                    coef = NA, 
                                    p = NA)
         }else{
         cor.mat <- rcorr( as.matrix(test.PFT[, c("GWBI", colnames(test.PFT)[10:length(test.PFT)])]), type="pearson") 
         cor.mat.df <- data.frame(PFT = a,
                                  month = colnames(test.PFT)[10:length(test.PFT)] ,
                                 coef = cor.mat$r[2:53,1], 
                                 p = cor.mat$P[2:53,1])
         }
         cat(a)
         cor.mat.df
  })
  cor.mat.site.df <- do.call(rbind, cor.mat.site)
  cor.mat.site.df
}

names <- as.list(as.character(unique(ED2.gwbi.clim.nona$Site))) # get names to apply function over
#names[1:2]
#system.time(correlate.tmax(names[[1]]))
system.time(tmax.cors <- lapply(names, correlate.tmax))
names(tmax.cors) <- unique(ED2.gwbi.clim.nona$Site)

tmax.cors.df <- do.call(rbind, tmax.cors) # takes a minute
tmax.cors.df$Site <- rep(names(tmax.cors), sapply(tmax.cors, nrow)) # add the site names
#tmax.cors.df <- left_join(uniquesites, tmax.cors.df, by = "Site")
tmax.cors.df<- tmax.cors.df[!is.na(tmax.cors.df$coef),]

ggplot(tmax.cors.df, aes(month, coef))+geom_boxplot()+facet_wrap(~PFT)

# save the correlation coefficients for each site:
saveRDS(tmax.cors.df, "outputs/gwbi_model/ED2_gwbi_correlation_coefs_by_pft.rds")

# make separate ggplots for tmean, tmax, and tmin, precip, 


library(data.table)
col.tmax <- unique(tmax.cors.df$month)[unique(tmax.cors.df$month) %like% "tair_max"]
col.tmean <- unique(tmax.cors.df$month)[unique(tmax.cors.df$month) %like% "tair_mean"]
col.tmin <- unique(tmax.cors.df$month)[unique(tmax.cors.df$month) %like% "tair_min"]
col.precip <- unique(tmax.cors.df$month)[unique(tmax.cors.df$month) %like% "precip"]



tmax <- ggplot(tmax.cors.df[tmax.cors.df$month %in% col.tmax,], aes(month, coef, fill = PFT))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

tmean <- ggplot(tmax.cors.df[tmax.cors.df$month %in% col.tmean,], aes(month, coef, fill = PFT))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

tmin <- ggplot(tmax.cors.df[tmax.cors.df$month %in% col.tmin,], aes(month, coef, fill = PFT))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

precip <- ggplot(tmax.cors.df[tmax.cors.df$month %in% col.precip,], aes(month, coef, fill = PFT))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())


png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_tmax_responses_rel.png")
tmax
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_tmean_responses_rel.png")
tmean
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_tmin_responses_rel.png")
tmin
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_precip_responses_rel.png")
precip
dev.off()




# cluster the coeffeicent temperature responses:
ED2.grid <- unique(ED2.gwbi.clim.nona[,c("lon", "lat", "Site")])
tmax.cors.df.ll <- left_join(tmax.cors.df, ED2.grid, by = "Site")



tmax.clusters <- tmax.cors.df.ll %>% dplyr::select("lon", "lat", "PFT", "Site", "month", "coef") %>% spread(key = month, value = coef)

k3 <- cluster::pam(tmax.clusters[,col.tmax], k = 3, diss = FALSE)
tmax.clusters$k3 <- as.character(k3$clustering)

k4 <- cluster::pam(tmax.clusters[,col.tmax], k = 4, diss = FALSE)
tmax.clusters$k4 <- as.character(k4$clustering)

k5 <- cluster::pam(tmax.clusters[,col.tmax], k = 5, diss = FALSE)
tmax.clusters$k5 <- as.character(k5$clustering)

ggplot(tmax.clusters, aes(lon, lat, color = k3))+geom_point()+facet_wrap(~PFT)
ggplot(tmax.clusters, aes(lon, lat, color = k4))+geom_point()+facet_wrap(~PFT)
ggplot(tmax.clusters, aes(lon, lat, color = precip_total_wtr_yr))+geom_point()+facet_wrap(~PFT)



#---------------- now plot correlations by time period:-----------------------
correlate.tmax.time.period <- function(x, min.yr, max.yr){
  test.nona <- ED2.gwbi.clim.nona[ED2.gwbi.clim.nona$Site %in% x, ]
  cat(x)
  test.nona$GWBI <- as.numeric(test.nona$GWBI)
  #corM <- cor(test.nona$GWBI, test.nona[, c("tair_max_1", "tair_max_2", "tair_max_3", "tair_max_4", "tair_max_5", "tair_max_6", "tair_max_7", "tair_max_8", "tair_max_9", "tair_max_10", "tair_max_11", "tair_max_12")], use = "pairwise.complete")
  PFTS <- unique(test.nona$PFT)
  dataframe.cor <- test.nona[test.nona$Year >= min.yr & test.nona$Year <= max.yr, ]
  
  cor.mat.site <- lapply(PFTS, function(a){
    
    test.PFT <-  dataframe.cor[dataframe.cor$PFT %in% a,]
    if(length(test.PFT$Year) <= 4){
      cor.mat.df <- data.frame(PFT = a,
                               month = colnames(test.PFT)[10:length(test.PFT)],
                               coef = NA, 
                               p = NA)
    }else{
      cor.mat <- rcorr( as.matrix(test.PFT[, c("GWBI", colnames(test.PFT)[10:length(test.PFT)])]), type="pearson") 
      cor.mat.df <- data.frame(PFT = a,
                               month = colnames(test.PFT)[10:length(test.PFT)] ,
                               coef = cor.mat$r[2:53,1], 
                               p = cor.mat$P[2:53,1])
    }
    cat(a)
    cor.mat.df
  })
  cor.mat.site.df <- do.call(rbind, cor.mat.site)
  cor.mat.site.df
}

names <- as.list(as.character(unique(ED2.gwbi.clim.nona$Site))) # get names to apply function over

# For 1900-2000
system.time(tmax.cors.1900.2011 <- lapply(names, correlate.tmax.time.period, min.yr = 1900, max.yr = 2011))
names(tmax.cors.1900.2011) <- unique(ED2.gwbi.clim.nona$Site)

tmax.cors.df.1900.2011 <- do.call(rbind,tmax.cors.1900.2011) # takes a minute
tmax.cors.df.1900.2011$Site <- rep(names(tmax.cors.1900.2011), sapply(tmax.cors.1900.2011, nrow)) # add the site names
tmax.cors.df.1900.2011$Timeperiod <- "1900-2011"

# for 1800- 1899
system.time(tmax.cors.1800.1899 <- lapply(names, correlate.tmax.time.period, min.yr = 1800, max.yr = 1899))
names(tmax.cors.1800.1899) <- unique(ED2.gwbi.clim.nona$Site)

tmax.cors.df.1800.1899 <- do.call(rbind,tmax.cors.1800.1899) # takes a minute
tmax.cors.df.1800.1899$Site <- rep(names(tmax.cors.1800.1899), sapply(tmax.cors.1800.1899, nrow)) # add the site names
tmax.cors.df.1800.1899$Timeperiod <- "1800-1899"

# for 850 to 1799
system.time(tmax.cors.850.1799 <- lapply(names, correlate.tmax.time.period, min.yr = 850, max.yr = 1799))
names(tmax.cors.850.1799) <- unique(ED2.gwbi.clim.nona$Site)

tmax.cors.df.850.1799 <- do.call(rbind,tmax.cors.850.1799) # takes a minute
tmax.cors.df.850.1799$Site <- rep(names(tmax.cors.850.1799), sapply(tmax.cors.850.1799, nrow)) # add the site names
tmax.cors.df.850.1799$Timeperiod <- "850-1799"

tmax.recent <- rbind(tmax.cors.df.1800.1899, tmax.cors.df.1900.2011)
tmax.recent <- tmax.recent[!is.na(tmax.recent$coef),]

tmax <- ggplot(tmax.recent[tmax.recent$month %in% col.tmax,], aes(month, coef, fill = Timeperiod))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

tmean <- ggplot(tmax.recent[tmax.recent$month %in% col.tmean,], aes(month, coef, fill = Timeperiod))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

tmin <- ggplot(tmax.recent[tmax.recent$month %in% col.tmin,], aes(month, coef, fill = Timeperiod))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())

precip <- ggplot(tmax.recent[tmax.recent$month %in% col.precip,], aes(month, coef, fill = Timeperiod))+geom_boxplot(outlier.size = 0.05, outlier.color = "grey")+
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+facet_wrap(~PFT, ncol = 4)+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid = element_blank())


png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_tmax_responses_by_time.png")
tmax
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_tmean_responses_by_time.png")
tmean
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_tmin_responses_by_time.png")
tmin
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/ED2_species_precip_responses_by_time.png")
precip
dev.off()



# -----------------finally split ED2 test & training:-------------------

# split training and testing data:


ED2.gwbi.clim.nona$Precip.scaled = as.vector(scale(ED2.gwbi.clim.nona$precip_total_wtr_yr.mm, center = TRUE, scale = TRUE))
ED2.gwbi.clim.nona.Precip.scaled = scale(ED2.gwbi.clim.nona$precip_total_wtr_yr.mm, center = TRUE, scale = TRUE)

ED2.gwbi.clim.nona$Temp.jun.scaled = as.vector(scale(ED2.gwbi.clim.nona$tair_max_6, center = TRUE, scale = TRUE))
ED2.gwbi.clim.nona.jun.scaled = scale(ED2.gwbi.clim.nona$tair_max_6, center = TRUE, scale = TRUE)

saveRDS(ED2.gwbi.clim.nona, "Data/ED2_gwbi_pft_clim.rds")
#splits <- unlist(strsplit(unique(ED.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = 1:length(unique(ED2.gwbi.clim.nona$Site)),
                                Site = unique(ED2.gwbi.clim.nona$Site))

covert_spec_codes <- data.frame(spec = 1:length(unique(ED2.gwbi.clim.nona$PFT)),
                                PFT = unique(ED2.gwbi.clim.nona$PFT))


ED2.gwbi.clim.nona <- left_join(ED2.gwbi.clim.nona, covert_site_codes, by = "Site")
ED2.gwbi.clim.nona <- left_join(ED2.gwbi.clim.nona, covert_spec_codes, by = "PFT")


# clean up the data and split testing and training:
rwl.full <- ED2.gwbi.clim.nona[!is.na(ED2.gwbi.clim.nona$GWBI_1) & !is.na(ED2.gwbi.clim.nona$GWBI_2)  ,]
rwl.full$GWBI <- as.numeric(rwl.full$GWBI)
rwl.full$GWBI_1 <- as.numeric(rwl.full$GWBI_1)
rwl.full$GWBI_2 <- as.numeric(rwl.full$GWBI_2)
#rwl.full$Age <- as.numeric(rwl.full$Age)

# also get rid of 0 values??
rwl.full <- rwl.full[!rwl.full$GWBI == 0, ]


# develop function to split testing and training datasets by species:
split.test.train.spec <- function( spec ){
  
  spec.full <- rwl.full[rwl.full$PFT %in% spec,]
  
  spec.full$spec <- ifelse(spec.full$PFT %in% spec, 1, 2)
  
  covert_site_codes.spec <- data.frame(site_num.spec = 1:length(unique(spec.full$Site)),
                                       Site = unique(spec.full$Site))
  
  spec.df <- left_join(spec.full, covert_site_codes.spec, by = "Site")
  
  msk <- caTools::sample.split( spec.df, SplitRatio = 3/4, group = NULL )
  
  train.spec <- spec.df[msk,]
  test.spec <- spec.df[!msk,]
  
  
  saveRDS(test.spec, paste0("outputs/gwbi_model/train_test_data/train_ED2", spec, "_nimble.rds"))
  saveRDS(test.spec, paste0("outputs/gwbi_model/train_test_data/test_ED2", spec, "_nimble.rds"))
  
  cat(spec)
}


spec.list  <- as.character( unique(rwl.full$PFT))


for(i in 1:length(spec.list)){
  split.test.train.spec(spec.list[i])
}



# do the same for period 1800-2011:
rwl.recent <- rwl.full[rwl.full$Year >= 1800 & rwl.full$Year <= 2011,]

split.test.train.spec.recent <- function( spec ){
  
  spec.full <- rwl.recent[rwl.recent$PFT %in% spec,]
  
  spec.full$spec <- ifelse(spec.full$PFT %in% spec, 1, 2)
  
  covert_site_codes.spec <- data.frame(site_num.spec = 1:length(unique(spec.full$Site)),
                                       Site = unique(spec.full$Site))
  
  spec.df <- left_join(spec.full, covert_site_codes.spec, by = "Site")
  
  msk <- caTools::sample.split( spec.df, SplitRatio = 3/4, group = NULL )
  
  train.spec <- spec.df[msk,]
  test.spec <- spec.df[!msk,]
  
  
  saveRDS(test.spec, paste0("outputs/gwbi_model/train_test_data/train_ED2_recent", spec, "_nimble.rds"))
  saveRDS(test.spec, paste0("outputs/gwbi_model/train_test_data/test_ED2_recent", spec, "_nimble.rds"))
  
  cat(spec)
}


spec.list  <- as.character( unique(rwl.recent$PFT))


for(i in 1:length(spec.list)){
  split.test.train.spec.recent(spec.list[i])
}

