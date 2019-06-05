library(rjags)
library(ggplot2)
library(caTools)
library(ggridges)
library(tidyr)
library(reshape2)
library(dplyr)

set.seed(22)

# ---------------read in model agbi, dens summaries for ED2
all.df.yr <- readRDS("outputs/data/ED2/dens_agbi_climate_ED2.rds")
#dens.agbi <- readRDS( "outputs/data/ED2/ED2.agbi.dens.site.rds")


sec2yr <- 1*60*60*24*365.25
JJAmeans.ED <- readRDS("outputs/data/ED2/ED2.alldat.jjameans.rds")
JJAmeans.ED$Tair.C.jja <- JJAmeans.ED$Tair - 273.15
JJAmeans.ED$precip.mm.jja <- JJAmeans.ED$precip*(sec2yr*3/12)
colnames(JJAmeans.ED)

ED.all <- left_join(all.df.yr, JJAmeans.ED[,c("Year", "Site", "Tair.C.jja", "precip.mm.jja")], by = c("Year", "Site"))

min.totals <- ED.all %>% group_by(Site) %>% summarise(min.gwbi = min(GS_gwbi),
                                                      mean.gwbi = mean(GS_gwbi))
GWBI.ED.mins <- merge(ED.all, min.totals, by = "Site")
rel.ed.gwbi <- GWBI.ED.mins %>% group_by(Site, Year) %>% summarise(rel.gwbi = GS_gwbi - (min.gwbi-0.15),
                                                                   rel.gwbi.raw = GS_gwbi - (min.gwbi),
                                                                   mean.diff = GS_gwbi - mean.gwbi)

ED.all <- merge(ED.all, rel.ed.gwbi)

ED.gwbi.spec <- readRDS( paste0("outputs/gwbi_model/train_test_data/train_ED2_recentmean.gwbi_nimble.rds"))
ED.gwbi.test <- readRDS( paste0("outputs/gwbi_model/train_test_data/test_ED2_recentmean.gwbi_nimble.rds"))
ED.gwbi.spec2 <- rbind(ED.gwbi.spec, ED.gwbi.test)
ED.gwbi <- ED.gwbi.spec2 %>% select(Site, Year, GWBI, GWBI_1, GWBI_2)
ED.gwbi$Site <- paste0("X",ED.gwbi$Site)

ED.join <- left_join(ED.all, ED.gwbi, by = c("Site", "Year"))
ED.all <- ED.join
# ------------read in model agbi, dens summaries for LPJ-GUESS
all.df.yr.GUESS <- readRDS("outputs/data/GUESS/GUESS.alldat.yrmeans.rds")



sec2yr <- 1*60*60*24*365.25
JJAmeans.GUESS <- readRDS("outputs/data/GUESS/GUESS.alldat.jjameans.rds")
JJAmeans.GUESS$Tair.C.jja <- JJAmeans.GUESS$Tair - 273.15
JJAmeans.GUESS$precip.mm.jja <- JJAmeans.GUESS$precip*(sec2yr*3/12)
colnames(JJAmeans.GUESS)

GUESS.all <- left_join(all.df.yr.GUESS, JJAmeans.GUESS[,c("Year", "Site", "Tair.C.jja", "precip.mm.jja")], by = c("Year", "Site"))

test.guess.ed <- left_join(GUESS.all, ED.all, by = c("Year", "Site"))

# for GUESS, we also have density, and gwbi at PFT scale, so lets read them in here:
GWBI.GUESS <- readRDS("Data/GUESS.gwbi.pft.wide.rds")
GWBI.GUESS$Site <- paste0("X", GWBI.GUESS$Site)
head(GWBI.GUESS)

# get the minimum for each site so we can relativize by the min:
min.totals <- GWBI.GUESS %>% group_by(Site) %>% summarise(min.gwbi = min(Total.gwbi), 
                                                          mean.gwbi = mean(Total.gwbi))
GWBI.GUESS.mins <- merge(GWBI.GUESS, min.totals, by = "Site")
rel.guess.gwbi <- GWBI.GUESS.mins %>% group_by(Site, Year) %>% summarise(rel.gwbi = Total.gwbi - (min.gwbi-0.15),
                                                                         rel.gwbi.raw = Total.gwbi - (min.gwbi), 
                                                                         mean.diff = Total.gwbi - mean.gwbi)

ggplot(rel.guess.gwbi, aes(Year, mean.diff, color = Site))+geom_point()+theme(legend.position = "none")

GWBI.GUESS <- merge(GWBI.GUESS, rel.guess.gwbi)

Dens.GUESS <- readRDS("Data/GUESS.Dens.pft.wide.rds")
Dens.GUESS$Site <- paste0("X", Dens.GUESS$Site)

GUESS.all.y <- left_join(GUESS.all, GWBI.GUESS, by =c("Year", "Site"))
GUESS.all <- left_join(GUESS.all.y, Dens.GUESS, by =c("Year", "Site"))

# make a DF paralell to ED2:
colnames(ED.all)
colnames(GUESS.all)

GUESS.totals <- GUESS.all[,c("Year", "Site", "Rel.Dens", "WUEet", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Total.Dens", "Total.gwbi","mean.diff", "Tair.C.jja", "precip.mm.jja")]

GUESS.totals$Model <- "GUESS"
colnames(GUESS.totals) <- c("Year", "Site", "Rel.Dens",  "WUEet", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                            "Dens", "gwbi","rel.gwbi", "Tair.C.jja", "precip.mm.jja", "Model")


# now subset ED.all by because we dont have all the WUE

ED.totals <- ED.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEet", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                       "Dens", "GS_gwbi", "mean.diff", "Tair.C.jja", "precip.mm.jja",  "GWBI", "GWBI_1", "GWBI_2")]
ED.totals$Model <- "ED2"
colnames(ED.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEet","WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi",  "rel.gwbi","Tair.C.jja", "precip.mm.jja",   "GWBI", "GWBI_1", "GWBI_2", "Model")




# combine together (in case we want to model together):
ED.GUESS <- rbind(ED.totals, GUESS.totals)

# some prelimiary plots to visualize the data:
ggplot(ED.GUESS, aes( precip.mm, rel.gwbi,  color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( Tair.C.jja, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( CO2, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( precip.mm.jja, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()



# read in tree ring data 


#---------------------read in TR data and relativise to compare----------------------
full.ghcn <- read.csv("/Users/kah/Documents/TreeRings/outputs/data/rwi_age_dbh_ghcn.df")
summary(full.ghcn)

# get all records that have all RWI and don't have negative diams or NA diams
#full.ghcn <- full.ghcn[!is.na(full.ghcn$RWI) & full.ghcn$DBH > 0 & !is.na(full.ghcn$DBH),]

rwi.ghcn <- read.csv("/Users/kah/Documents/TreeRings/outputs/full_ghcn_all_months_rwi.csv")
rwi.prism <- read.csv("/Users/kah/Documents/TreeRings/outputs/full_prism_all_months_rwi.csv")

# get rel.rwi:
min.totals <- full.ghcn %>% group_by(site) %>% summarise(min.gwbi = min(RWI, na.rm = TRUE), 
                                                         mean.gwbi = mean(RWI, na.rm = TRUE))
RWI.mins <- merge(full.ghcn, min.totals, by = "site")
rel.TR.gwbi <- RWI.mins %>% group_by(site,ID, year) %>% summarise( mean.diff = RWI - mean.gwbi)

ggplot(rel.TR.gwbi , aes(year, mean.diff, color = site))+geom_point()+theme(legend.position = "none")

full.ghcn.rel <- merge( full.ghcn, rel.TR.gwbi, by = c("ID", "site","year"))
# calculate JJA VPD for each year
rwi.prism$jja.VPDmax <- rowMeans(rwi.prism[,c("Month_vpdmax_6", "Month_vpdmax_7", "Month_vpdmax_8")])
rwi.prism$jja.VPDmin <- rowMeans(rwi.prism[,c("Month_vpdmin_6", "Month_vpdmin_7", "Month_vpdmin_8")])

# calculate JJA BAL for each year
rwi.prism$jja.BAL <- rowMeans(rwi.prism[,c("Month_BAL_6", "Month_BAL_7", "Month_BAL_8")])

# calculate total precip for each year
rwi.ghcn$MAP.prism <- rowSums(rwi.prism[,c("Month_pcp_1","Month_pcp_2", "Month_pcp_3","Month_pcp_4","Month_pcp_5","Month_pcp_6", "Month_pcp_7", "Month_pcp_8", "Month_pcp_9", "Month_pcp_10", "Month_pcp_11", "Month_pcp_12")])

rwi.ghcn$MAP.ghcn <- rowSums(rwi.ghcn[,c("Month_pcp_1","Month_pcp_2", "Month_pcp_3","Month_pcp_4","Month_pcp_5","Month_pcp_6", "Month_pcp_7", "Month_pcp_8", "Month_pcp_9", "Month_pcp_10", "Month_pcp_11", "Month_pcp_12")])

# make sure the prism pcp, temp values are specified in columns, so when we merge df, it won't be a huge deal:
colnames(rwi.prism)[12:59] <- paste0("prism_",colnames(rwi.prism)[12:59])
rwi.prism.sub <- rwi.prism[,!colnames(rwi.prism) %in% c("DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")]
rwi.ghcn.sub <- rwi.ghcn[,!colnames(rwi.ghcn) %in% c("DBH", "dbhclass", "ageclass", "SpecCode", "RWI", "RWI_1", "RWI_2", "RWI_3")]
# merge rwi.ghcn and rwi.prism
full.clim <- merge(rwi.ghcn.sub, rwi.prism.sub, by = c("year", "site", "ID"))

# merge to full.ghcn
full.ghcn <- merge(full.ghcn.rel, full.clim, by = c("ID", "year", "site" ))

full.ghcn$site_age <- paste0(full.ghcn$site, "-", full.ghcn$ageclass)
full.ghcn$site_age.code <- as.numeric(as.factor(full.ghcn$site_age))



full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))


# standardise predictor variables to have mean 0 and sd = 1
DI.scaled = scale(full.ghcn$JJA.pdsi, center= TRUE, scale=TRUE)
DBH.scaled = scale(full.ghcn$DBH, center= TRUE, scale=TRUE)
full.ghcn$T.scaled = as.vector(scale(full.ghcn$JUNTmax, center= TRUE, scale=TRUE))
full.ghcn$DI.scaled = as.vector(scale(full.ghcn$JJA.pdsi, center = TRUE, scale = TRUE))
full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP1.scaled = as.vector(scale(full.ghcn$SP01_6, center = TRUE, scale = TRUE))

full.ghcn$jja.VPDmax.scaled <- as.vector(scale(full.ghcn$jja.VPDmax, center = TRUE, scale = TRUE))
full.ghcn$jja.BAL.scaled <- as.vector(scale(full.ghcn$jja.BAL, center = TRUE, scale = TRUE))
full.ghcn$MAP.scaled = as.vector(scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE))

full.ghcn.MAP.scaled <- scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE)
full.ghcn.T.scaled <- scale(full.ghcn$JUNTmax, center= TRUE, scale=TRUE)

SP1.scaled <- scale(full.ghcn$SP01_6, center = TRUE, scale = TRUE)
SP6.scaled <- scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE)



# need to define site level structures, if not already defined:

if(! "structure" %in% colnames(full.ghcn)){
  
  structure <- data.frame(site = c("AVO","BON","COR",  "ENG",  "GLA",  "GLL1", "GLL2", "GLL3","GLL4", "HIC",  "MOU",  "PLE",  "PVC",  "STC",  "TOW", "UNC" ),
                          structure = c("Forest", "Savanna", "Forest", "Forest", "Savanna", "Forest", "Savanna", "Savanna", "Forest", "Savanna", "Forest","Savanna", "Savanna", "Savanna", "Forest", "Savanna"))
  full.ghcn <- merge(full.ghcn, structure, by = "site")
}



#struct.cohort and struct.cohort.code (if it doesnt alread exist)

if(! "struct.cohort" %in% colnames(full.ghcn)){
  
  full.ghcn$struct.cohort <- paste0(full.ghcn$ageclass,"-", full.ghcn$structure)
  full.ghcn$struct.cohort.code <- ifelse(full.ghcn$struct.cohort %in% "Past-Forest", 1,
                                         ifelse(full.ghcn$struct.cohort %in% "Modern-Forest", 2, 
                                                ifelse(full.ghcn$struct.cohort %in% "Past-Savanna", 3,
                                                       ifelse(full.ghcn$struct.cohort %in% "Modern-Savanna", 4,"NA" ))))
  
}


# get lagged relative RWI for tree ring data:
full.ghcn.sort <- full.ghcn[with(full.ghcn, order(ID, year)),]

full.ghcn.sort.wide <- full.ghcn.sort[,c("year", "ID", "RWI")] %>% spread(key = "ID", value = "RWI")
full.ghcn.sort_1.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-1),]
full.ghcn.sort_1.wide$year <- full.ghcn.sort.wide[2:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_2.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-2),]
full.ghcn.sort_2.wide$year <- full.ghcn.sort.wide[3:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_3.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-3),]
full.ghcn.sort_3.wide$year <- full.ghcn.sort.wide[4:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_4.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-4),]
full.ghcn.sort_4.wide$year <- full.ghcn.sort.wide[5:(length(full.ghcn.sort.wide$year)),]$year

full.ghcn.sort_5.wide <- full.ghcn.sort.wide[1:(length(full.ghcn.sort.wide$year)-5),]
full.ghcn.sort_5.wide$year <- full.ghcn.sort.wide[6:(length(full.ghcn.sort.wide$year)),]$year


full.ghcn.sort_1 <- melt(full.ghcn.sort_1.wide, id.vars = c("year"))
colnames(full.ghcn.sort_1) <- c("year", "ID", "RWI_1")

full.ghcn.sort_2 <- melt(full.ghcn.sort_2.wide, id.vars = c("year"))
colnames(full.ghcn.sort_2) <- c("year", "ID", "RWI_2")

full.ghcn.sort_3 <- melt(full.ghcn.sort_3.wide, id.vars = c("year"))
colnames(full.ghcn.sort_3) <- c("year","ID", "RWI_3")

full.ghcn.sort_4 <- melt(full.ghcn.sort_4.wide, id.vars = c("year"))
colnames(full.ghcn.sort_4) <- c("year", "ID","RWI_4")

full.ghcn.sort_5 <- melt(full.ghcn.sort_5.wide, id.vars = c("year"))
colnames(full.ghcn.sort_5) <- c("year", "ID", "RWI_5")

full.ghcn.sort_lag <- left_join(full.ghcn.sort_1, full.ghcn.sort_2, by = c("year", "ID"))
full.ghcn.sort_lag34 <- left_join(full.ghcn.sort_3, full.ghcn.sort_4, by = c("year", "ID"))
full.ghcn.sort_lag <- left_join(full.ghcn.sort_lag, full.ghcn.sort_lag34, by = c("year", "ID"))
full.ghcn.sort_lag <- left_join(full.ghcn.sort_lag, full.ghcn.sort_5, by = c("year", "ID"))

full.ghcn.sort.omit <- full.ghcn.sort %>% select(-RWI_1, -RWI_2, -RWI_3)

full.ghcn.sort_lag <- left_join(full.ghcn.sort.omit, full.ghcn.sort_lag, by = c("year", "ID"))



# omit NA values for RWI - 1:
ghcn.clean <- full.ghcn.sort_lag[!is.na(full.ghcn.sort_lag$RWI) & !is.na(full.ghcn.sort_lag$RWI_1) & !is.na(full.ghcn.sort_lag$RWI_2) & !is.na(full.ghcn.sort_lag$RWI_3) & !is.na(full.ghcn.sort_lag$RWI_4)  & !is.na(full.ghcn.sort_lag$RWI_5) & !is.na(full.ghcn.sort_lag$DBH),]
ghcn.clean <- ghcn.clean[,c("site", "ID", "year", "RWI", "MAP.prism","MAP.scaled","JUNTavg","T.scaled", "DBH","mean.diff","RWI_1", "RWI_2",         
                            "RWI_3", "RWI_4","RWI_5" , "ageclass")]
full.ghcn <- ghcn.clean

# create classes for modern, past and pre-industrial:
full.ghcn$period <- ifelse(full.ghcn$year<= 1850, "pre-industrial", 
                    ifelse(full.ghcn$year <= 1950 & full.ghcn$year > 1850,"industrial-past",
                           ifelse(full.ghcn$year >= 1950, "modern-industrial", "NA")))

full.ghcn$period_cd <- ifelse(full.ghcn$year<= 1850, "3", 
                       ifelse(full.ghcn$year <= 1950 & full.ghcn$year > 1850,"2",
                              ifelse(full.ghcn$year >= 1950, "1", "NA")))


#splits <- unlist(strsplit(unique(GUESS.sort_lag$Site), "X"))
convert_site_codes_rwi <- data.frame(site = unique(full.ghcn$site),
                                site_code = 1:length(unique(full.ghcn$site)))


full.ghcn <- left_join(full.ghcn, convert_site_codes_rwi, by = "site")
# split training and testing datasets:
msk <- caTools::sample.split( full.ghcn, SplitRatio = 3/4, group = NULL )

train.RWI.full <- ghcn.clean[msk,]
test.RWI.full <- ghcn.clean[!msk,]

# create a dataste the elimates yrs 1900-1950 for the modern and 1950-present for past:

mod.post <- full.ghcn[full.ghcn$ageclass %in% "Modern" & full.ghcn$year >= 1950,]
past.pre <- full.ghcn[full.ghcn$ageclass %in% "Past" & full.ghcn$year < 1950,]

sub.ghcn <- rbind(mod.post, past.pre)

msk <- caTools::sample.split( sub.ghcn, SplitRatio = 3/4, group = NULL )

train.RWI <- full.ghcn[msk,]
test.RWI <- full.ghcn[!msk,]


# find the ED2 and GUESS model sites with the closest climate spaces
unique.clims.rwi <- full.ghcn.sort_lag %>% select(site, Year, prism_Month_tmax_6, PCP) %>% distinct() %>% group_by(site) %>% summarise(mean_tmax6 = mean(prism_Month_tmax_6),
                                                                                                                                       sd_tmax6 = sd(prism_Month_tmax_6),
                                                                                                                                       mean_precip = mean(PCP),
                                                                                                                                       precip_sd = sd(PCP))#%>% group_by(site) #%>% 



unique.clims.EM <- ED.totals %>% filter(Year >= 1895) %>% select(Site, Year, Tair.C.jja, precip.mm) %>% distinct() %>% group_by(Site) %>% summarise(mean_tmax6 = mean(Tair.C.jja),
                                                                                                                                       sd_tmax6 = sd(Tair.C.jja),
                                                                                                                                       mean_precip = mean(precip.mm),
                                                                                                                                       precip_sd = sd(precip.mm))#%>% group_by(site) #%>% 




all.ED <- readRDS(paste0(getwd(),"/outputs/data/ED2/ED2.gwbi.pft.all.met.rds"))

grid.met.summary <- all.ED %>% filter (Year >=1895) %>%select(lon, lat, Site, Year, tair_max_6, precip_total_wtr_yr.mm) %>% group_by(lon, lat) %>% 
                          summarise(mean_tmax6 = mean(tair_max_6-273.15),
                         sd_tmax6 = sd(tair_max_6-273.15),
                        mean_precip = mean(precip_total_wtr_yr.mm),
                         precip_sd = sd(precip_total_wtr_yr.mm))

# update below:

unique.clims.EM$tmax_06_bins <- cut(unique.clims.EM$mean_tmax6, breaks=seq(14, 35, by = 2))

unique.clims.rwi$tmax_06_bins <- cut(unique.clims.rwi$mean_tmax6, breaks=seq(14, 35, by = 2))

tmax.ordered.cuts <- data.frame(tmax_06_bins = levels(cut(unique.clims.EM[order(unique.clims.EM$mean_tmax6),]$mean_tmax6, breaks=seq(14, 35, by = 2))),
                                tmax06.mids=seq(15, 34, by = 2))

all.prism.summary <- left_join(unique.clims.rwi, tmax.ordered.cuts, by = "tmax_06_bins")
grid.met.summary <- left_join(unique.clims.EM, tmax.ordered.cuts, by = "tmax_06_bins")



# get bins of the prism precipitation data
grid.met.summary$wtryr_bins <- cut(unique.clims.EM$mean_precip, breaks=seq(400, 1450, by = 200))

all.prism.summary $wtryr_bins <- cut(unique.clims.rwi$mean_precip, breaks=seq(400, 1450, by = 200))


precip.ordered.cuts <- data.frame(wtryr_bins = levels(cut(unique.clims.rwi[order(unique.clims.rwi$mean_precip),]$mean_precip, breaks=seq(400, 1450, by = 200))),
                                  wtr.mids=seq(500, 1400, by = 200))

all.prism.summary <- left_join(all.prism.summary, precip.ordered.cuts, by = "wtryr_bins")
grid.met.summary <- left_join(grid.met.summary, precip.ordered.cuts, by = "wtryr_bins")

tmax.mids.plot <- ggplot()+geom_point(data= all.prism.summary, aes(wtr.mids, tmax06.mids), color = "red", alpha = 0.15)+
  geom_point(data= grid.met.summary, aes(wtr.mids, tmax06.mids), pch = 6,color = "blue", alpha = 0.5)
tmax.mids.plot 

# get the list of sites where climate bins over lap (+/- 50mm MAP and 1 degree mean Tmax in june)
prism.short <- all.prism.summary #%>% dplyr::select(Longitude, Latitude, wtryr_bins, wtr.mids, tmax_06_bins, tmax06.mids)
met.short <- grid.met.summary #%>% dplyr::select(lon, lat, wtryr_bins, wtr.mids, tmax_06_bins, tmax06.mids)

common_envts <- merge(prism.short, met.short, by = c("wtryr_bins","wtr.mids", "tmax_06_bins", "tmax06.mids"))

saveRDS(common_envts, "outputs/data/QUERCUS_ecological_MET_common_envts.rds")


#----------------------------------------
# Separate Testing and Training Datasets:

# 1. initial data cleaning & checking
# 2. create dummy variables for CO2 classes: "modern", "past", "pre-industrial"
# 3. calculate agbi lagged
# 4. remove NA values for lagged agbi
# 5. scale climate parameters
# 6. Split into testing and training data:

# select the 16 sites with most similar met driver climates:
ED <- ED.totals[!is.na(ED.totals$rel.gwbi) & !is.na(ED.totals$precip.mm) & ED.totals$Site %in% common_envts$Site ,]
GUESS <- GUESS.totals[!is.na(GUESS.totals$rel.gwbi) & !is.na(GUESS.totals$precip.mm) & GUESS.totals$Site %in% common_envts$Site,]

# create classes for modern, past and pre-industrial:
ED$period <- ifelse(ED$Year <= 1850, "pre-industrial", 
                    ifelse(ED$Year <= 1950 & ED$Year > 1850,"industrial-past",
                           ifelse(ED$Year >= 1950, "modern-industrial", "NA")))

ED$period_cd <- ifelse(ED$Year <= 1850, "3", 
                       ifelse(ED$Year <= 1950 & ED$Year > 1850,"2",
                              ifelse(ED$Year >= 1950, "1", "NA")))

# create classes for modern, past and pre-industrial:
GUESS$period <- ifelse(GUESS$Year <= 1850, "pre-industrial", 
                       ifelse(GUESS$Year <= 1950 & GUESS$Year > 1850,"industrial-past",
                              ifelse(GUESS$Year >= 1950, "modern-industrial", "NA")))

GUESS$period_cd <- ifelse(GUESS$Year <= 1850, "3", 
                          ifelse(GUESS$Year <= 1950 & GUESS$Year > 1850,"2",
                                 ifelse(GUESS$Year >= 1950, "1", "NA")))

# calculate lagged agbi:

# get previous years growth for ED
uni.Sites <- unique(ED$Site)
ED.sort <- ED[with(ED, order(Site, Year)),]

ED.sort.wide <- ED.sort[,c("Year", "Site", "rel.gwbi")] %>% spread(key = "Site", value = "rel.gwbi")
ED.sort_1.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-1),]
ED.sort_1.wide$Year <- ED.sort.wide[2:(length(ED.sort.wide$Year)),]$Year

ED.sort_2.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-2),]
ED.sort_2.wide$Year <- ED.sort.wide[3:(length(ED.sort.wide$Year)),]$Year

ED.sort_3.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-3),]
ED.sort_3.wide$Year <- ED.sort.wide[4:(length(ED.sort.wide$Year)),]$Year

ED.sort_4.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-4),]
ED.sort_4.wide$Year <- ED.sort.wide[5:(length(ED.sort.wide$Year)),]$Year

ED.sort_5.wide <- ED.sort.wide[1:(length(ED.sort.wide$Year)-5),]
ED.sort_5.wide$Year <- ED.sort.wide[6:(length(ED.sort.wide$Year)),]$Year


ED.sort_1 <- melt(ED.sort_1.wide, id.vars = c("Year"))
colnames(ED.sort_1) <- c("Year", "Site", "rel.gwbi_1")
ED.sort_2 <- melt(ED.sort_2.wide, id.vars = c("Year"))
colnames(ED.sort_2) <- c("Year", "Site", "rel.gwbi_2")

ED.sort_3 <- melt(ED.sort_3.wide, id.vars = c("Year"))
colnames(ED.sort_3) <- c("Year", "Site", "rel.gwbi_3")

ED.sort_4 <- melt(ED.sort_4.wide, id.vars = c("Year"))
colnames(ED.sort_4) <- c("Year", "Site", "rel.gwbi_4")

ED.sort_5 <- melt(ED.sort_5.wide, id.vars = c("Year"))
colnames(ED.sort_5) <- c("Year", "Site", "rel.gwbi_5")

ED.sort_lag <- left_join(ED.sort_1, ED.sort_2, by = c("Year", "Site"))
ED.sort_lag34 <- left_join(ED.sort_3, ED.sort_4, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_lag34, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_5, by = c("Year", "Site"))

ED.sort_lag <- left_join(ED.sort, ED.sort_lag, by = c("Year", "Site"))

# remove NAs from agbi_1 and agbi_2:

ED.sort_lag <- ED.sort_lag[!is.na(ED.sort_lag$rel.gwbi_1) & !is.na(ED.sort_lag$rel.gwbi_2) & !is.na(ED.sort_lag$rel.gwbi_3) & !is.na(ED.sort_lag$rel.gwbi_4) & !is.na(ED.sort_lag$rel.gwbi_5),]

# scale the climate parameters:

ED.sort_lag$Precip.scaled = as.vector(scale(ED.sort_lag$precip.mm, center = TRUE, scale = TRUE))
ED.sort_lag.Precip.scaled = scale(ED.sort_lag$precip.mm, center = TRUE, scale = TRUE)

ED.sort_lag$Temp.jja.scaled = as.vector(scale(ED.sort_lag$Tair.C.jja, center = TRUE, scale = TRUE))
ED.sort_lag.Temp.jja.scaled = scale(ED.sort_lag$Tair.C.jja, center = TRUE, scale = TRUE)

ED.sort_lag$Temp.scaled = as.vector(scale(ED.sort_lag$Tair.C, center = TRUE, scale = TRUE))
ED.sort_lag.Temp.scaled = scale(ED.sort_lag$Tair.C, center = TRUE, scale = TRUE)

# scale the CO2 parameters:
ED.sort_lag$CO2.scaled = as.vector(scale(ED.sort_lag$CO2, center = TRUE, scale = TRUE))
ED.sort_lag.CO2.scaled = scale(ED.sort_lag$CO2, center = TRUE, scale = TRUE)

#splits <- unlist(strsplit(unique(ED.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = unique(as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2))),
                                site_code = 1:length(unique(as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2)))))

ED.sort_lag$site_num <- as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2))
ED.sort_lag <- left_join(ED.sort_lag, covert_site_codes, by = "site_num")


# get previous years growth for GUESS:
# get previous years growth
uni.Sites <- unique(GUESS$Site)
GUESS.sort <- GUESS[with(GUESS, order(Site, Year)),]

GUESS.sort.wide <- GUESS.sort[,c("Year", "Site", "rel.gwbi")] %>% spread(key = "Site", value = "rel.gwbi")
GUESS.sort_1.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-1),]
GUESS.sort_1.wide$Year <- GUESS.sort.wide[2:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_2.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-2),]
GUESS.sort_2.wide$Year <- GUESS.sort.wide[3:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_3.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-3),]
GUESS.sort_3.wide$Year <- GUESS.sort.wide[4:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_4.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-4),]
GUESS.sort_4.wide$Year <- GUESS.sort.wide[5:(length(GUESS.sort.wide$Year)),]$Year

GUESS.sort_5.wide <- GUESS.sort.wide[1:(length(GUESS.sort.wide$Year)-5),]
GUESS.sort_5.wide$Year <- GUESS.sort.wide[6:(length(GUESS.sort.wide$Year)),]$Year


GUESS.sort_1 <- melt(GUESS.sort_1.wide, id.vars = c("Year"))
colnames(GUESS.sort_1) <- c("Year", "Site", "rel.gwbi_1")
GUESS.sort_2 <- melt(GUESS.sort_2.wide, id.vars = c("Year"))
colnames(GUESS.sort_2) <- c("Year", "Site", "rel.gwbi_2")

GUESS.sort_3 <- melt(GUESS.sort_3.wide, id.vars = c("Year"))
colnames(GUESS.sort_3) <- c("Year", "Site", "rel.gwbi_3")

GUESS.sort_4 <- melt(GUESS.sort_4.wide, id.vars = c("Year"))
colnames(GUESS.sort_4) <- c("Year", "Site", "rel.gwbi_4")

GUESS.sort_5 <- melt(GUESS.sort_5.wide, id.vars = c("Year"))
colnames(GUESS.sort_5) <- c("Year", "Site", "rel.gwbi_5")

GUESS.sort_lag <- left_join(GUESS.sort_1, GUESS.sort_2, by = c("Year", "Site"))
GUESS.sort_lag34 <- left_join(GUESS.sort_3, GUESS.sort_4, by = c("Year", "Site"))
GUESS.sort_lag <- left_join(GUESS.sort_lag, GUESS.sort_lag34, by = c("Year", "Site"))
GUESS.sort_lag <- left_join(GUESS.sort_lag, GUESS.sort_5, by = c("Year", "Site"))

GUESS.sort_lag <- left_join(GUESS.sort, GUESS.sort_lag, by = c("Year", "Site"))

# remove NAs from agbi_1 and agbi_2:

GUESS.sort_lag <- GUESS.sort_lag[!is.na(GUESS.sort_lag$rel.gwbi_1) & !is.na(GUESS.sort_lag$rel.gwbi_2) & !is.na(GUESS.sort_lag$rel.gwbi_3) & !is.na(GUESS.sort_lag$rel.gwbi_4) & !is.na(GUESS.sort_lag$rel.gwbi_5),]

# scale the climate parameters:

GUESS.sort_lag$Precip.scaled = as.vector(scale(GUESS.sort_lag$precip.mm, center = TRUE, scale = TRUE))
GUESS.sort_lag.Precip.scaled = scale(GUESS.sort_lag$precip.mm, center = TRUE, scale = TRUE)

GUESS.sort_lag$Temp.jja.scaled = as.vector(scale(GUESS.sort_lag$Tair.C.jja, center = TRUE, scale = TRUE))
GUESS.sort_lag.Temp.jja.scaled = scale(GUESS.sort_lag$Tair.C.jja, center = TRUE, scale = TRUE)

GUESS.sort_lag$Temp.scaled = as.vector(scale(GUESS.sort_lag$Tair.C, center = TRUE, scale = TRUE))
GUESS.sort_lag.Temp.scaled = scale(GUESS.sort_lag$Tair.C, center = TRUE, scale = TRUE)

# scale the CO2 parameters:
GUESS.sort_lag$CO2.scaled = as.vector(scale(GUESS.sort_lag$CO2, center = TRUE, scale = TRUE))
GUESS.sort_lag.CO2.scaled = scale(GUESS.sort_lag$CO2, center = TRUE, scale = TRUE)

#splits <- unlist(strsplit(unique(GUESS.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = unique(as.numeric(sapply(strsplit(GUESS.sort_lag$Site,"X"), `[`, 2))),
                                site_code = 1:length(unique(GUESS.sort_lag$Site)))

GUESS.sort_lag$site_num <- as.numeric(sapply(strsplit(GUESS.sort_lag$Site,"X"), `[`, 2))
GUESS.sort_lag <- left_join(GUESS.sort_lag, covert_site_codes, by = "site_num")

# relativise iWUE:
mean.WUEet.GUESS <- GUESS.sort_lag %>% group_by(Site) %>% summarise(mean.iWUE = mean(WUEet, na.rm =TRUE))

mean.WUEet.ED <- ED.sort_lag %>% group_by(Site) %>% summarise(mean.iWUE = mean(WUEet, na.rm =TRUE))

mean.WUEet.GUESS <- left_join(GUESS.sort_lag, mean.WUEet.GUESS, by = "Site")
mean.WUEet.ED <- left_join(ED.sort_lag, mean.WUEet.ED, by = "Site")

mean.WUEet.GUESS$rel.WUEet <- mean.WUEet.GUESS$WUEet - mean.WUEet.GUESS$mean.iWUE
mean.WUEet.ED$rel.WUEet <- mean.WUEet.ED$WUEet - mean.WUEet.ED$mean.iWUE
ggplot(mean.WUEet.ED[!is.infinite(mean.WUEet.ED$WUEet),], aes(Year, rel.WUEet, color = Site))+geom_point()+theme(legend.position = "none")
ggplot(mean.WUEet.GUESS, aes(Year, rel.WUEet, color = Site))+geom_point()+theme(legend.position = "none")

GUESS.sort_lag <- left_join(GUESS.sort_lag, mean.WUEet.GUESS[,c("Site", "Year", "rel.WUEet")], by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, mean.WUEet.ED[,c("Site", "Year", "rel.WUEet")], by = c("Year", "Site"))


# combine together (in case we want to model together):
full.df <- rbind(GUESS.sort_lag, ED.sort_lag)


# split training and testing datasets:
# split training and testing datasets:
msk <- caTools::sample.split( ED.sort_lag, SplitRatio = 3/4, group = NULL )

train.full <- ED.sort_lag [msk,]
test.full <- ED.sort_lag [!msk,]

train.mod <- train.full[train.full$period %in% c("modern-industrial", "industrial-past"),]
test.mod <- test.full[test.full$period %in% c("modern-industrial", "industrial-past"),]

train.ED <- train.mod[train.mod$Model %in% "ED2",]
test.ED <- test.mod[test.mod$Model %in% "ED2",]

train.full.ED <- train.full
test.full.ED <- test.full
#------------------------------
# split training and testing for GUESS:
# 
msk2 <- caTools::sample.split( GUESS.sort_lag, SplitRatio = 3/4, group = NULL )

train.GUESS.full <- GUESS.sort_lag[msk2,]
test.GUESS.full <- GUESS.sort_lag[!msk2,]

train.GUESS  <- train.GUESS.full[train.GUESS.full$period %in% c("modern-industrial", "industrial-past"),]
test.GUESS  <- test.GUESS.full[test.GUESS.full$period %in% c("modern-industrial", "industrial-past"),]

train.full.GUESS <- train.GUESS.full
test.full.GUESS <- test.GUESS.full



#  check that predictors make sense 
#--------------------------------------------
library(mgcv)
mod <- lm(rel.gwbi ~ Precip.scaled + Temp.jja.scaled + CO2 + rel.gwbi_1 + rel.gwbi_2 +rel.gwbi_3 +rel.gwbi_4+rel.gwbi_5 + Site, data = train.ED)
summary(mod)

mod <- lm(GWBI ~ Precip.scaled + Temp.jja.scaled + CO2 + GWBI_1 + GWBI_2 + Site, data = train.ED)
summary(mod)
#plot(mod)
preds <- predict(mod, test.ED)
plot(test.ED$gwbi, preds)
abline(a = 0, b = 1, col= "red")

mod <- lm(rel.gwbi ~ Precip.scaled + Temp.jja.scaled + CO2 + rel.gwbi_1 + rel.gwbi_2 +rel.gwbi_3 +rel.gwbi_4+rel.gwbi_5 + Site, data = train.GUESS)
summary(mod)
#plot(mod)
preds <- predict(mod, test.GUESS)
plot(test.GUESS$gwbi, preds)
abline(a = 0, b = 1, col= "red")



# model growth for ED2:

# basic lm with no re explais ~ 50 percent of variance

# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
ED_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] #+ beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
# beta5[c] ~ dnorm(mu_beta5, inv_beta5)
# beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-1, 1)
mu_beta1 ~ dunif(-1, 1)
mu_beta2 ~ dunif(-1, 1)
mu_beta3 ~ dunif(-1, 1)
mu_beta4 ~ dunif(-1, 1)
# mu_beta5 ~ dunif(-1, 1)
# mu_beta6 ~ dunif(-1, 1)

inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)
inv_beta4   ~ dgamma(0.001, 0.001)
sigma_beta4 <- 1/sqrt(inv_beta4)
# inv_beta5   ~ dgamma(0.001, 0.001)
# sigma_beta5 <- 1/sqrt(inv_beta5)
# inv_beta6   ~ dgamma(0.001, 0.001)
# sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is gwbbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] #+ beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}


# # Probe
# for(i in 1:nprobe){
# #process model
# Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi
# 
# #function g()
# gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 
# 
# }

beta1.diff <- beta1[2]-beta1[1]
beta2.diff <- beta2[2]-beta2[1]
beta3.diff <- beta3[2]-beta3[1]
beta4.diff <- beta3[2]-beta4[1]

}"






reg.EDel.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                                 data = list(Y = train.ED$rel.gwbi, n=length(train.ED$rel.gwbi), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, agbi_1 = train.ED$rel.gwbi_1,agbi_2 = train.ED$rel.gwbi_2, agbi_3 = train.ED$rel.gwbi_3, agbi_4 = train.ED$rel.gwbi_4,
                                             period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = as.numeric(train.ED$site_code), np=length(test.ED$period_cd), 
                                             sites.p = test.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, agbi_1.p = test.ED$rel.gwbi_1, agbi_2.p = test.ED$rel.gwbi_2, agbi_3.p = test.ED$rel.gwbi_3, agbi_4.p = test.ED$rel.gwbi_4,
                                             period.p = as.numeric(test.ED$period_cd),
                                             
                                             #nprobe=length(probe.ED$struct.cohort.code), 
                                             #sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, agbi_1.probe = probe.ED$rel.gwbi_1, agbi_2.probe = probe.ED$rel.gwbi_2, agbi_3.probe = probe.ED$rel.gwbi_3, agbi_4.probe = probe.ED$rel.gwbi_4,
                                             #period.probe = as.numeric(probe.ED$struct.cohort.code)),
                                             n.chains = 3, n.adapt = 100))


update(reg.EDel.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.ED.period <- coda.samples(reg.EDel.by_period, 
                               variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6" ), 
                               n.chains = 3, n.iter=10000, thin = 1)

samp.ED.ypred <- coda.samples(reg.EDel.by_period, 
                              variable.names=c("Ypred" ), 
                              n.chains = 3, n.iter=10000, thin = 1)

# samp.ED.yprobe <- coda.samples(reg.EDel.by_period, 
#                                variable.names=c("Yprobe" ), 
#                                n.chains = 3, n.iter=5000, thin = 1)


saveRDS(samp.ED.period, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_parameter_samps.rds")
saveRDS(samp.ED.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_Ypred_samps.rds")
#saveRDS(samp.ED.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_Yprobe_samps.rds")
#samp.ED.yprobe <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_Yprobe_samps.rds")

saveRDS(test.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_testdata.rds")
saveRDS(train.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_traindata.rds")

# Extract the samples for each parameter

samps       <- samp.ED.period[[1]]
Yp.samps    <- samp.ED.ypred [[1]]
Yprobe.sampes <- samp.ED.yprobe [[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site_num))]
beta1.samps  <- samps[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]
beta3.samps  <- samps[,(length(unique(test.ED$site_num))+5):(length(unique(test.ED$site_num))+6)]
beta4.samps  <- samps[,(length(unique(test.ED$site_num))+7):(length(unique(test.ED$site_num))+8)]
beta5.samps  <- samps[,(length(unique(test.ED$site_num))+9):(length(unique(test.ED$site_num))+10)]
beta6.samps  <- samps[,(length(unique(test.ED$site_num))+11):(length(unique(test.ED$site_num))+12)]


# check for convergence:


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$rel.gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$rel.gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/pred_vs_obs_ED2.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# from here we want to get sensitivities
# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(train.ED$period)
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(train.ED$period)

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(train.ED$period)
#colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(train.ED$period)
#
#colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from guess model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
ED.site.clim <- ED.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                             MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(ED.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")


b1.dot <-ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(-0.15, 0.25)+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <-ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <-ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <-ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <-ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 0.4,height = 0))+
  geom_point( size = 0.5)+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()

#-------------------------------------
# run the same model for LPJ-GUESS
#-------------------------------------


reg.GUESSel.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                                 data = list(Y = train.GUESS$rel.gwbi, n=length(train.GUESS$rel.gwbi), Precip.scaled = train.GUESS$Precip.scaled, Temp.jja.scaled = train.GUESS$Temp.jja.scaled, agbi_1 = train.GUESS$rel.gwbi_1,agbi_2 = train.GUESS$rel.gwbi_2, agbi_3 = train.GUESS$rel.gwbi_3, agbi_4 = train.GUESS$rel.gwbi_4,
                                             period = as.numeric(train.GUESS$period_cd), S = unique(train.GUESS$site_code),  C = unique(train.GUESS$period_cd), sites = as.numeric(train.GUESS$site_code), np=length(test.GUESS$period_cd), 
                                             sites.p = test.GUESS$site_code, Precip.scaled.p = test.GUESS$Precip.scaled, Temp.jja.scaled.p = test.GUESS$Temp.jja.scaled, agbi_1.p = test.GUESS$rel.gwbi_1, agbi_2.p = test.GUESS$rel.gwbi_2, agbi_3.p = test.GUESS$rel.gwbi_3, agbi_4.p = test.GUESS$rel.gwbi_4,
                                             period.p = as.numeric(test.GUESS$period_cd),
                                             
                                             #nprobe=length(probe.GUESS$struct.cohort.code), 
                                             #sites.probe = probe.GUESS$site_num, Precip.scaled.probe = probe.GUESS$DI.scaled, Temp.jja.scaled.probe = probe.GUESS$T.scaled, agbi_1.probe = probe.GUESS$rel.gwbi_1, agbi_2.probe = probe.GUESS$rel.gwbi_2, agbi_3.probe = probe.GUESS$rel.gwbi_3, agbi_4.probe = probe.GUESS$rel.gwbi_4,
                                             #period.probe = as.numeric(probe.GUESS$struct.cohort.code)),
                                             n.chains = 3, n.adapt = 100))


update(reg.GUESSel.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.GUESS.period <- coda.samples(reg.GUESSel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.GUESS.period <- coda.samples(reg.GUESSel.by_period, 
                               variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6" ), 
                               n.chains = 3, n.iter=10000, thin = 1)

samp.GUESS.ypred <- coda.samples(reg.GUESSel.by_period, 
                              variable.names=c("Ypred" ), 
                              n.chains = 3, n.iter=10000, thin = 1)

# samp.GUESS.yprobe <- coda.samples(reg.GUESSel.by_period, 
#                                variable.names=c("Yprobe" ), 
#                                n.chains = 3, n.iter=5000, thin = 1)


saveRDS(samp.GUESS.period, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_parameter_samps.rds")
saveRDS(samp.GUESS.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_Ypred_samps.rds")
#saveRDS(samp.GUESS.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_Yprobe_samps.rds")
#samp.GUESS.yprobe <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_Yprobe_samps.rds")

saveRDS(test.GUESS, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_traindata.rds")

# Extract the samples for each parameter

samps       <- samp.GUESS.period[[1]]
Yp.samps    <- samp.GUESS.ypred [[1]]
Yprobe.sampes <- samp.GUESS.yprobe [[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site_num))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]
beta3.samps  <- samps[,(length(unique(test.GUESS$site_num))+5):(length(unique(test.GUESS$site_num))+6)]
beta4.samps  <- samps[,(length(unique(test.GUESS$site_num))+7):(length(unique(test.GUESS$site_num))+8)]
beta5.samps  <- samps[,(length(unique(test.GUESS$site_num))+9):(length(unique(test.GUESS$site_num))+10)]
beta6.samps  <- samps[,(length(unique(test.GUESS$site_num))+11):(length(unique(test.GUESS$site_num))+12)]


# check for convergence:


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.GUESS$rel.gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.GUESS$rel.gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/pred_vs_obs_GUESS.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# from here we want to get sensitivities
# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.GUESS$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.GUESS$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.GUESS$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(train.GUESS$period)
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(train.GUESS$period)

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(train.GUESS$period)
#colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(train.GUESS$period)
#
#colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from guess model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
GUESS.site.clim <- GUESS.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                             MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(GUESS.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")


b1.dot <-ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(-0.15, 0.25)+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <-ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <-ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <-ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <-ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 0.4,height = 0))+
  geom_point( size = 0.5)+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/GUESS_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()


#-------------------------------------
# run the same model for tree ring data
#-------------------------------------


reg.RWIel.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                                    data = list(Y = log(train.RWI$RWI), n=length(train.RWI$RWI), Precip.scaled = train.RWI$MAP.scaled, Temp.jja.scaled = train.RWI$T.scaled, agbi_1 = train.RWI$RWI_1, agbi_2 = train.RWI$RWI_2, agbi_3 = train.RWI$RWI_3, agbi_4 = train.RWI$RWI_4,
                                                period = as.numeric(train.RWI$period_cd), S = unique(train.RWI$site_code),  C = unique(train.RWI$period_cd), sites = as.numeric(train.RWI$site_code), np=length(test.RWI$period_cd), 
                                                sites.p = test.RWI$site_code, Precip.scaled.p = test.RWI$MAP.scaled, Temp.jja.scaled.p = test.RWI$T.scaled, agbi_1.p = test.RWI$RWI_1, agbi_2.p = test.RWI$RWI_2, agbi_3.p = test.RWI$RWI_3, agbi_4.p = test.RWI$RWI_4,
                                                period.p = as.numeric(test.RWI$period_cd),
                                                
                                                #nprobe=length(probe.RWI$struct.cohort.code), 
                                                #sites.probe = probe.RWI$site_num, Precip.scaled.probe = probe.RWI$DI.scaled, Temp.jja.scaled.probe = probe.RWI$T.scaled, agbi_1.probe = probe.RWI$rel.gwbi_1, agbi_2.probe = probe.RWI$rel.gwbi_2, agbi_3.probe = probe.RWI$rel.gwbi_3, agbi_4.probe = probe.RWI$rel.gwbi_4,
                                                #period.probe = as.numeric(probe.RWI$struct.cohort.code)),
                                                n.chains = 3, n.adapt = 100))


update(reg.RWIel.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.RWI.period <- coda.samples(reg.RWIel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)

samp.RWI.period <- coda.samples(reg.RWIel.by_period, 
                                  variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6" ), 
                                  n.chains = 3, n.iter=15000, thin = 15)

samp.beta.diffs <- coda.samples(reg.RWIel.by_period, 
                               variable.names=c("beta1.diff", "beta2.diff", "beta3.diff", "beta4.diff" ), 
                               n.chains = 3, n.iter=15000, thin = 15)

samp.RWI.ypred <- coda.samples(reg.RWIel.by_period, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=10000, thin = 15)

# samp.RWI.yprobe <- coda.samples(reg.RWIel.by_period, 
#                                variable.names=c("Yprobe" ), 
#                                n.chains = 3, n.iter=5000, thin = 1)


saveRDS(samp.RWI.period, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/RWI_parameter_samps.rds")
saveRDS(samp.RWI.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/RWI_Ypred_samps.rds")
#saveRDS(samp.RWI.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/RWI_Yprobe_samps.rds")
#samp.RWI.yprobe <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/RWI_Yprobe_samps.rds")

saveRDS(test.RWI, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/RWI_testdata.rds")
saveRDS(train.RWI, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/RWI_traindata.rds")

# Extract the samples for each parameter

samps       <- samp.RWI.period[[1]]
Yp.samps    <- samp.RWI.ypred [[1]]
Yprobe.samps <- samp.RWI.yprobe [[1]]
alpha.samps <- samps[,1:length(unique(test.RWI$site_code))]
beta1.samps  <- samps[,(length(unique(test.RWI$site_code))+1):(length(unique(test.RWI$site_code))+2)]
beta2.samps  <- samps[,(length(unique(test.RWI$site_code))+3):(length(unique(test.RWI$site_code))+4)]
beta3.samps  <- samps[,(length(unique(test.RWI$site_code))+5):(length(unique(test.RWI$site_code))+6)]
beta4.samps  <- samps[,(length(unique(test.RWI$site_code))+7):(length(unique(test.RWI$site_code))+8)]
# beta5.samps  <- samps[,(length(unique(test.RWI$site_code))+9):(length(unique(test.RWI$site_code))+10)]
# beta6.samps  <- samps[,(length(unique(test.RWI$site_code))+11):(length(unique(test.RWI$site_code))+12)]


# check for convergence:
traceplot(samp.RWI.period[[1]])
acfplot(samp.RWI.period[[1]])

# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- log(test.RWI$RWI)

pred.obs <- summary(lm(colMeans(Yp.samps) ~ log(test.RWI$RWI)))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+
  geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)+
  ylim(0,10)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/pred_vs_obs_RWI.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.RWI$RWI)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.RWI$RWI)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# from here we want to get sensitivities
# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.RWI$site)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.RWI$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.RWI$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(train.RWI$period)
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(train.RWI$period)

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

# b5 <- data.frame(beta5.samps)
# colnames(b5) <-unique(train.RWI$period)
# #colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
# b5$num <- rownames(b5)
# b5.m <- melt(b5, id.vars=c("num"))
# b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")
# 
# b6 <- data.frame(beta6.samps)
# colnames(b6) <-unique(train.RWI$period)
# #
# #colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
# b6$num <- rownames(b6)
# b6.m <- melt(b6, id.vars=c("num"))
# b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from guess model:
#a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
#                                                            Ci.low = quantile(value, 0.025), 
#                                                            Ci.high = quantile(value, 0.975))
# #b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
# 
# b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
#                                                            Ci.low = quantile(value, 0.025), 
#                                                            Ci.high = quantile(value, 0.975))
# #b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
RWI.site.clim <- full.ghcn %>% group_by(site) %>% summarise(MAP = mean(MAP.prism, na.rm=TRUE), 
                                                                   MAT = mean(JUNTavg, na.rm=TRUE))

a1.sum$site <- a1.sum$variable
a1.clim <- merge(RWI.site.clim, a1.sum, by = "site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <-ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(-0.15, 0.25)+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <-ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <-ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <-ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

# b5.dot <-ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
#   geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")
# 
# 
# b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 0.25,height = 0))+
#   geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/RWI_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, #b5.dot, b6.dot, 
                   ncol = 1)
dev.off()



# calculate the difference between parameters in each mcmc:
beta.diffs <- data.frame(samp.beta.diffs[[1]])
beta.diffs.m <- melt(beta.diffs)

avg.diffs <- beta.diffs.m %>% group_by(variable) %>% summarise(mean = mean(value),
                                                  Ci.low = quantile(value, 0.025),
                                                  Ci.high = quantile(value, 0.975))

ggplot(avg.diffs, aes(variable, mean))+geom_bar(stat= "identity")+geom_errorbar(aes(ymin = Ci.low, ymax = Ci.high), width = 0.1)+theme_bw()




#-----------------------------------------------------------------------------------
# Running the climate match model for total GWBI for ED2:
#-----------------------------------------------------------------------------------
# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
ED_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] #+ beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
# beta5[c] ~ dnorm(mu_beta5, inv_beta5)
# beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-1, 1)
mu_beta1 ~ dunif(-1, 1)
mu_beta2 ~ dunif(-1, 1)
mu_beta3 ~ dunif(-1, 1)
mu_beta4 ~ dunif(-1, 1)
# mu_beta5 ~ dunif(-1, 1)
# mu_beta6 ~ dunif(-1, 1)

inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)
inv_beta4   ~ dgamma(0.001, 0.001)
sigma_beta4 <- 1/sqrt(inv_beta4)
# inv_beta5   ~ dgamma(0.001, 0.001)
# sigma_beta5 <- 1/sqrt(inv_beta5)
# inv_beta6   ~ dgamma(0.001, 0.001)
# sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is gwbbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] #+ beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}


# # Probe
# for(i in 1:nprobe){
# #process model
# Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi
# 
# #function g()
# gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 
# 
# }

beta1.diff <- beta1[2]-beta1[1]
beta2.diff <- beta2[2]-beta2[1]
beta3.diff <- beta3[2]-beta3[1]
beta4.diff <- beta3[2]-beta4[1]

}"






reg.EDel.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                                 data = list(Y = log(train.ED$GWBI), n=length(train.ED$GWBI), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, agbi_1 = train.ED$GWBI_1, agbi_2 = train.ED$GWBI_2, 
                                             period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = as.numeric(train.ED$site_code), np=length(test.ED$period_cd), 
                                             sites.p = test.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, agbi_1.p = test.ED$GWBI_1, agbi_2.p = test.ED$GWBI_2, 
                                             period.p = as.numeric(test.ED$period_cd),
                                             
                                             #nprobe=length(probe.ED$struct.cohort.code), 
                                             #sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, agbi_1.probe = probe.ED$rel.gwbi_1, agbi_2.probe = probe.ED$rel.gwbi_2, agbi_3.probe = probe.ED$rel.gwbi_3, agbi_4.probe = probe.ED$rel.gwbi_4,
                                             #period.probe = as.numeric(probe.ED$struct.cohort.code)),
                                             n.chains = 3, n.adapt = 100))


update(reg.EDel.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.ED.period <- coda.samples(reg.EDel.by_period, 
                               variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6" ), 
                               n.chains = 3, n.iter=10000, thin = 1)

samp.ED.ypred <- coda.samples(reg.EDel.by_period, 
                              variable.names=c("Ypred" ), 
                              n.chains = 3, n.iter=10000, thin = 1)

# samp.ED.yprobe <- coda.samples(reg.EDel.by_period, 
#                                variable.names=c("Yprobe" ), 
#                                n.chains = 3, n.iter=5000, thin = 1)


saveRDS(samp.ED.period, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_parameter_samps.rds")
saveRDS(samp.ED.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_Ypred_samps.rds")
#saveRDS(samp.ED.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_Yprobe_samps.rds")
#samp.ED.yprobe <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_Yprobe_samps.rds")

saveRDS(test.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_testdata.rds")
saveRDS(train.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_traindata.rds")

# Extract the samples for each parameter

samps       <- samp.ED.period[[1]]
Yp.samps    <- samp.ED.ypred [[1]]
Yprobe.sampes <- samp.ED.yprobe [[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site_num))]
beta1.samps  <- samps[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]
beta3.samps  <- samps[,(length(unique(test.ED$site_num))+5):(length(unique(test.ED$site_num))+6)]
beta4.samps  <- samps[,(length(unique(test.ED$site_num))+7):(length(unique(test.ED$site_num))+8)]
beta5.samps  <- samps[,(length(unique(test.ED$site_num))+9):(length(unique(test.ED$site_num))+10)]
beta6.samps  <- samps[,(length(unique(test.ED$site_num))+11):(length(unique(test.ED$site_num))+12)]


# check for convergence:


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$rel.gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$rel.gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/pred_vs_obs_ED2.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# from here we want to get sensitivities
# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(train.ED$period)
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(train.ED$period)
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(train.ED$period)

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(train.ED$period)
#colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(train.ED$period)
#
#colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from guess model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b3.sum <- b3.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))


b4.sum <- b4.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b5.sum <- b5.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

b6.sum <- b6.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
#b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
ED.site.clim <- ED.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                             MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(ED.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")


b1.dot <-ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(-0.15, 0.25)+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <-ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <-ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <-ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <-ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 0.4,height = 0))+
  geom_point( size = 0.5)+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")+xlim(-0.15, 0.25) + geom_vline(xintercept = 0, linetype = "dashed")

png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/Climate_match/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()
