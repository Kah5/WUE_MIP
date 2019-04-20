library(reshape2)
library(ggplot2)
library(tidyr)
library(dplyr)
library(caTools)


# -------------------Join the species specific growth from ED2, LINKAGES, GUESS to met driver data--------------

GUESS.gwbi.pft <- readRDS("Data/GUESS.gwbi.pft.wide.rds")
GUESS.gwbi.pft.m <- melt(GUESS.gwbi.pft, id.vars = c("Year", "Site"))
ggplot(GUESS.gwbi.pft.m[GUESS.gwbi.pft.m$Site %in% "2",], aes(Year, value, color = variable))+geom_line()
load("Data/PalEON_siteInfo_all.RData")
GUESS.gwbi.pft$num <- GUESS.gwbi.pft$Site

GUESS.gwbi.pft.ll <- left_join( paleon[,c("num", "lon", "lat")], GUESS.gwbi.pft, by = c("num"))


# read in climate drivers for GUESS:
tair.summaries.df<- readRDS(paste0(getwd(),"/Data/MET/tair.summary.rds"))
precipf.summaries.df<- readRDS(paste0(getwd(),"/Data/MET/precipf.summary.rds"))

# need to reformat temperature data to get monthly
tair.mo.mean <- tair.summaries.df %>% select (lon, lat, year, month, tair_mean) %>% 
                group_by(lon, lat, year) %>% spread(month, tair_mean)

tair.mo.min <- tair.summaries.df %>% select (lon, lat, year, month, tair_min) %>% 
  group_by(lon, lat, year) %>% spread(month, tair_min)

tair.mo.max <- tair.summaries.df %>% select (lon, lat, year, month, tair_max) %>% 
  group_by(lon, lat, year) %>% spread(month, tair_max)

precipf.mo.sum <- precipf.summaries.df %>% select (lon, lat, year, month, precipf_sum) %>% 
  group_by(lon, lat, year) %>% spread(month, precipf_sum)

# rename columns & join together:
colnames(precipf.mo.sum)[4:length(colnames(precipf.mo.sum))] <- paste0("precip_", colnames(precipf.mo.sum)[4:length(colnames(precipf.mo.sum))])
colnames(tair.mo.mean)[4:length(colnames(tair.mo.mean))] <- paste0("tair_mean_", colnames(tair.mo.mean)[4:length(colnames(tair.mo.mean))])
colnames(tair.mo.max)[4:length(colnames(tair.mo.max))] <- paste0("tair_max_", colnames(tair.mo.max)[4:length(colnames(tair.mo.max))])
colnames(tair.mo.min)[4:length(colnames(tair.mo.min))] <- paste0("tair_min_", colnames(tair.mo.min)[4:length(colnames(tair.mo.min))])

precipf.mo.sum$precip_total <- rowSums(precipf.mo.sum[4:15])


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
precipf.summaries.df$wtr.year <- wtr_yr(precipf.summaries.df)

precipf.mo.sum.wy <- precipf.summaries.df %>% select (lon, lat, wtr.year, month, precipf_sum) %>% 
  group_by(lon, lat, wtr.year) %>% spread(month, precipf_sum)

precipf.mo.sum.wy$precip_total_wtr_yr <- rowSums(precipf.mo.sum.wy[4:15], na.rm=TRUE)
colnames(precipf.mo.sum.wy)[3] <- "year"
# left_join climate together:

precip.full <- left_join(precipf.mo.sum, precipf.mo.sum.wy[,c("lon","lat", "year", "precip_total_wtr_yr")], by =c("lon", "lat", "year"))
precip.tmean <- left_join(precip.full, tair.mo.mean, by =c("lon","lat", "year") )
precip.tmean.tmax <- left_join(precip.tmean, tair.mo.max, by =c("lon","lat", "year") )
all.met <- left_join(precip.tmean.tmax, tair.mo.min, by =c("lon","lat", "year") )
convert2mm <- 60*60*24
all.met$precip_total_wtr_yr.mm <- all.met$precip_total_wtr_yr*convert2mm # convert to mm
all.met$precip_total.mm <- all.met$precip_total*convert2mm

# save all the met:
saveRDS(all.met, paste0(getwd(),"/Data/MET/all.met.summary.rds"))


# merge climate and growth for GUESS:
colnames(all.met)[3] <- "Year"
GUESS.all <- left_join(GUESS.gwbi.pft.ll, all.met, by = c("lon", "lat", "Year"))

saveRDS(GUESS.all, paste0(getwd(),"/outputs/data/GUESS/GUESS.gwbi.pft.all.met.rds"))

ggplot(GUESS.all, aes( tair_mean_6, precip_total.mm, color = BNS.gwbi))+geom_point()

# now need to filter out grid cells/times where fcomp does not include the taxa of choice:


GUESS.fcomp.pft <- readRDS("Data/GUESS.Fcomp.pft.rds")
GUESS.fcomp.pft.spread <- GUESS.fcomp.pft %>% group_by(Year, Site) %>% spread(PFT, Fcomp)

colnames(GUESS.fcomp.pft.spread)[3:length(GUESS.fcomp.pft.spread)] <- paste0(colnames(GUESS.fcomp.pft.spread)[3:length(GUESS.fcomp.pft.spread)], ".fcomp")

# merge with gwbi:
GUESS.df  <- left_join(GUESS.all, GUESS.fcomp.pft.spread, by = c("Year", "Site"))

# if fcomp == 0, then set gwbi to NA (there is a more elegant way of doing this but, its okay)
GUESS.df[GUESS.df$BNE.fcomp == 0,]$BNE.gwbi <- NA
GUESS.df[GUESS.df$BINE.fcomp == 0,]$BINE.gwbi <- NA
GUESS.df[GUESS.df$BNS.fcomp == 0,]$BNS.gwbi <- NA
GUESS.df[GUESS.df$BIBS.fcomp == 0,]$BIBS.gwbi <- NA
GUESS.df[GUESS.df$TeBS.fcomp == 0,]$TeBS.gwbi <- NA
GUESS.df[GUESS.df$TelBS.fcomp == 0,]$TelBS.gwbi <- NA
GUESS.df[GUESS.df$TeBE.fcomp == 0,]$TeBE.gwbi <- NA
GUESS.df[GUESS.df$TrBE.fcomp == 0,]$TrBE.gwbi <- NA

GUESS.df[GUESS.df$TrlBE.fcomp == 0,]$TrlBE.gwbi <- NA
GUESS.df[GUESS.df$TrBR.fcomp == 0,]$TrBR.gwbi <- NA
GUESS.df[GUESS.df$C3G.fcomp == 0,]$C3G.gwbi <- NA
GUESS.df[GUESS.df$C4G.fcomp == 0,]$C4G.gwbi <- NA


ggplot(GUESS.df, aes( tair_mean_6, precip_total.mm, color = BIBS.gwbi))+geom_point()

# -----------------------------get gwbi-1 and gwbi-2:----------------------------------
# calculate lagged gwbi:
GUESS.df.slim <- GUESS.df %>% select(num:Total.gwbi) %>% group_by(num, lon, lat, Year, Site) %>% gather(key = PFT, value = GWBI,BNE.gwbi:Total.gwbi)

# get previous years growth for GUESS.df
PFT.groups <- as.list(unique(GUESS.df.slim$PFT))
PFT.group<- "BNE.gwbi"
get_prev_gwbi <- function(PFT.group){
  cat(PFT.group)
      x <- GUESS.df.slim[GUESS.df.slim$PFT %in% PFT.group,]
      
      uni.Sites <- unique(x$Site)
      GUESS.df.sort <- x[with(x, order(Site, Year, PFT)),]
      
      GUESS.df.sort.wide <- GUESS.df.sort[,c("Year", "Site","PFT", "GWBI")] %>% spread(key = "Site", value = "GWBI")
      GUESS.df.sort_1.wide <- GUESS.df.sort.wide[1:(length(GUESS.df.sort.wide$Year)-1),]
      GUESS.df.sort_1.wide$Year <- GUESS.df.sort.wide[2:(length(GUESS.df.sort.wide$Year)),]$Year
      
      GUESS.df.sort_2.wide <- GUESS.df.sort.wide[1:(length(GUESS.df.sort.wide$Year)-2),]
      GUESS.df.sort_2.wide$Year <- GUESS.df.sort.wide[3:(length(GUESS.df.sort.wide$Year)),]$Year
      
      GUESS.df.sort_3.wide <- GUESS.df.sort.wide[1:(length(GUESS.df.sort.wide$Year)-3),]
      GUESS.df.sort_3.wide$Year <- GUESS.df.sort.wide[4:(length(GUESS.df.sort.wide$Year)),]$Year
      
      GUESS.df.sort_4.wide <- GUESS.df.sort.wide[1:(length(GUESS.df.sort.wide$Year)-4),]
      GUESS.df.sort_4.wide$Year <- GUESS.df.sort.wide[5:(length(GUESS.df.sort.wide$Year)),]$Year
      
      GUESS.df.sort_5.wide <- GUESS.df.sort.wide[1:(length(GUESS.df.sort.wide$Year)-5),]
      GUESS.df.sort_5.wide$Year <- GUESS.df.sort.wide[6:(length(GUESS.df.sort.wide$Year)),]$Year
      
      GUESS.df.sort.norm <- melt(GUESS.df.sort.wide, id.vars = c("Year", "PFT"))
      colnames(GUESS.df.sort.norm) <- c("Year","PFT", "Site", "GWBI")
      GUESS.df.sort_1 <- melt(GUESS.df.sort_1.wide, id.vars = c("Year", "PFT"))
      colnames(GUESS.df.sort_1) <- c("Year","PFT", "Site", "GWBI_1")
      GUESS.df.sort_2 <- melt(GUESS.df.sort_2.wide, id.vars = c("Year", "PFT"))
      colnames(GUESS.df.sort_2) <- c("Year","PFT","Site", "GWBI_2")
      
      
      GUESS.df.sort.norm1 <-  left_join(GUESS.df.sort.norm, GUESS.df.sort_1, by = c("Year", "Site", "PFT"))
      GUESS.df.sort_lag <- left_join(GUESS.df.sort.norm1, GUESS.df.sort_2, by = c("Year", "Site", "PFT"))
      GUESS.df.sort_lag
}

list.of.prev.gwbi <- lapply(PFT.groups, get_prev_gwbi)
prev.gwbi <- do.call(rbind, list.of.prev.gwbi)

head(prev.gwbi)

# -------------------join with climate data again---------------
climate.only <- GUESS.df %>% select(num:Year, precip_1:precip_total.mm)
climate.only$Site <- as.character(climate.only$num)

gwbi.clim <- left_join(prev.gwbi, climate.only, by =c("Site", "Year"))
ggplot(gwbi.clim, aes(tair_mean_6, GWBI))+geom_point()+stat_smooth(method = "lm")+facet_wrap(~PFT)

#---------------- do some preliminary analyses:-----------------

guess.gwbi <- gwbi.clim #[,c("lon", "lat","PFT", "Site", "Year",  "GWBI", "GWBI_1", "GWBI_2")]
guess.gwbi.clim.nona <- guess.gwbi[!is.na(guess.gwbi$GWBI),]

#guess.gwbi.clim <- left_join(guess.gwbi.nona, tmax.month, by = c("Longitude", "Latitude","SPEC.CODE", "studyCode", "year"))

x <- names[[1]]
library(Hmisc) # You need to download it first.

correlate.tmax <- function(x){
  test.nona <- guess.gwbi.clim.nona[guess.gwbi.clim.nona$Site %in% x, ]
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

names <- as.list(as.character(unique(guess.gwbi.clim.nona$Site))) # get names to apply function over
#names[1:2]
#system.time(correlate.tmax(names[[1]]))
system.time(tmax.cors <- lapply(names, correlate.tmax))
names(tmax.cors) <- unique(guess.gwbi.clim.nona$Site)

tmax.cors.df <- do.call(rbind, tmax.cors) # takes a minute
tmax.cors.df$Site <- rep(names(tmax.cors), sapply(tmax.cors, nrow)) # add the site names
#tmax.cors.df <- left_join(uniquesites, tmax.cors.df, by = "Site")
tmax.cors.df<- tmax.cors.df[!is.na(tmax.cors.df$coef),]

ggplot(tmax.cors.df, aes(month, coef))+geom_boxplot()+facet_wrap(~PFT)


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


png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/GUESS_species_tmax_responses.png")
tmax
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/GUESS_species_tmean_responses.png")
tmean
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/GUESS_species_tmin_responses.png")
tmin
dev.off()

png(height = 6, width = 10, units = "in", res = 300, "outputs/gwbi_model/GUESS_species_precip_responses.png")
precip
dev.off()

