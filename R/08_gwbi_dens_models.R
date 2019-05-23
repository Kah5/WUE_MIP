# Script to run mixed effects models on agbi & density:
library(rjags)
library(ggplot2)
library(caTools)
library(ggridges)
library(tidyr)
library(reshape2)
library(dplyr)


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

GUESS.totals <- GUESS.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Total.Dens", "Total.gwbi","mean.diff", "Tair.C.jja", "precip.mm.jja")]

GUESS.totals$Model <- "GUESS"
colnames(GUESS.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi","rel.gwbi", "Tair.C.jja", "precip.mm.jja", "Model")


# now subset ED.all by because we dont have all the WUE

ED.totals <- ED.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Dens", "GS_gwbi", "mean.diff", "Tair.C.jja", "precip.mm.jja")]
ED.totals$Model <- "ED2"
colnames(ED.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi",  "rel.gwbi","Tair.C.jja", "precip.mm.jja", "Model")


# ------------read in model agbi, dens summaries for LPJ-LINK
all.df.yr.LINK <- readRDS("outputs/data/LINKAGES/LINK.alldat.yrmeans.rds")




test.LINK.ed <- left_join(LINK.all, ED.all, by = c("Year", "Site"))


# make a DF paralell to ED2:
colnames(ED.all)
colnames(LINK.all)

LINK.totals <- LINK.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                             "Total.Dens", "Total.gwbi","mean.diff", "Tair.C.jja", "precip.mm.jja")]

LINK.totals$Model <- "LINK"
colnames(LINK.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                            "Dens", "gwbi","rel.gwbi", "Tair.C.jja", "precip.mm.jja", "Model")


# now subset ED.all by because we dont have all the WUE

ED.totals <- ED.all[,c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                       "Dens", "GS_gwbi", "mean.diff", "Tair.C.jja", "precip.mm.jja")]
ED.totals$Model <- "ED2"
colnames(ED.totals) <- c("Year", "Site", "Rel.Dens", "IWUE", "WUEt", "CO2", "Tair", "Tair.C", "precip", "precip.mm",
                         "Dens", "gwbi",  "rel.gwbi","Tair.C.jja", "precip.mm.jja", "Model")



# combine together (in case we want to model together):
ED.GUESS <- rbind(ED.totals, GUESS.totals)

# some prelimiary plots to visualize the data:
ggplot(ED.GUESS, aes( precip.mm, rel.gwbi,  color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( Tair.C.jja, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( CO2, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()
ggplot(ED.GUESS, aes( precip.mm.jja, rel.gwbi, color = Model))+geom_point()+stat_smooth()+theme(legend.position = "none")+theme_bw()



#----------------------------------------
# Separate Testing and Training Datasets:

# 1. initial data cleaning & checking
# 2. create dummy variables for CO2 classes: "modern", "past", "pre-industrial"
# 3. calculate agbi lagged
# 4. remove NA values for lagged agbi
# 5. scale climate parameters
# 6. Split into testing and training data:

# remove NAS: 
ED <- ED.totals[!is.na(ED.totals$rel.gwbi) & !is.na(ED.totals$precip.mm) & !is.infinite(ED.totals$IWUE),]
GUESS <- GUESS.totals[!is.na(GUESS.totals$rel.gwbi) & !is.na(GUESS.totals$precip.mm),]

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
mean.IWUE.GUESS <- GUESS.sort_lag %>% group_by(Site) %>% summarise(mean.iWUE = mean(IWUE, na.rm =TRUE))

mean.IWUE.ED <- ED.sort_lag %>% group_by(Site) %>% summarise(mean.iWUE = mean(IWUE, na.rm =TRUE))

mean.IWUE.GUESS <- left_join(GUESS.sort_lag, mean.IWUE.GUESS, by = "Site")
mean.IWUE.ED <- left_join(ED.sort_lag, mean.IWUE.ED, by = "Site")

mean.IWUE.GUESS$rel.IWUE <- mean.IWUE.GUESS$IWUE - mean.IWUE.GUESS$mean.iWUE
mean.IWUE.ED$rel.IWUE <- mean.IWUE.ED$IWUE - mean.IWUE.ED$mean.iWUE
ggplot(mean.IWUE.ED[!is.infinite(mean.IWUE.ED$IWUE),], aes(Year, rel.IWUE, color = Site))+geom_point()+theme(legend.position = "none")
ggplot(mean.IWUE.GUESS, aes(Year, rel.IWUE, color = Site))+geom_point()+theme(legend.position = "none")

GUESS.sort_lag <- left_join(GUESS.sort_lag, mean.IWUE.GUESS[,c("Site", "Year", "rel.IWUE")], by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, mean.IWUE.ED[,c("Site", "Year", "rel.IWUE")], by = c("Year", "Site"))


# combine together (in case we want to model together):
full.df <- rbind(GUESS.sort_lag, ED.sort_lag)


# split training and testing datasets:
# split training and testing datasets:
msk <- caTools::sample.split( ED.sort_lag, SplitRatio = 3/4, group = NULL )

train.full <- full.df[msk,]
test.full <- full.df[!msk,]

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

#unique(train.ED$site_code)

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
full.ghcn$T.scaled = as.vector(scale(full.ghcn$JUNTavg, center= TRUE, scale=TRUE))
full.ghcn$DI.scaled = as.vector(scale(full.ghcn$JJA.pdsi, center = TRUE, scale = TRUE))
full.ghcn$DBH.scaled = as.vector(scale(full.ghcn$DBH, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP6.scaled = as.vector(scale(full.ghcn$SP06_6, center = TRUE, scale = TRUE))
full.ghcn$SP1.scaled = as.vector(scale(full.ghcn$SP01_6, center = TRUE, scale = TRUE))

full.ghcn$jja.VPDmax.scaled <- as.vector(scale(full.ghcn$jja.VPDmax, center = TRUE, scale = TRUE))
full.ghcn$jja.BAL.scaled <- as.vector(scale(full.ghcn$jja.BAL, center = TRUE, scale = TRUE))
full.ghcn$MAP.scaled = as.vector(scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE))

full.ghcn.MAP.scaled <- scale(full.ghcn$MAP.prism, center = TRUE, scale = TRUE)
full.ghcn.T.scaled <- scale(full.ghcn$JUNTavg, center= TRUE, scale=TRUE)

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

full.ghcn.sort.wide <- full.ghcn.sort[,c("year", "ID", "mean.diff")] %>% spread(key = "ID", value = "mean.diff")
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
colnames(full.ghcn.sort_1) <- c("year", "ID", "rel.diff_1")

full.ghcn.sort_2 <- melt(full.ghcn.sort_2.wide, id.vars = c("year"))
colnames(full.ghcn.sort_2) <- c("year", "ID", "rel.diff_2")

full.ghcn.sort_3 <- melt(full.ghcn.sort_3.wide, id.vars = c("year"))
colnames(full.ghcn.sort_3) <- c("year","ID", "rel.diff_3")

full.ghcn.sort_4 <- melt(full.ghcn.sort_4.wide, id.vars = c("year"))
colnames(full.ghcn.sort_4) <- c("year", "ID","rel.diff_4")

full.ghcn.sort_5 <- melt(full.ghcn.sort_5.wide, id.vars = c("year"))
colnames(full.ghcn.sort_5) <- c("year", "ID", "rel.diff_5")

full.ghcn.sort_lag <- left_join(full.ghcn.sort_1, full.ghcn.sort_2, by = c("year", "ID"))
full.ghcn.sort_lag34 <- left_join(full.ghcn.sort_3, full.ghcn.sort_4, by = c("year", "ID"))
full.ghcn.sort_lag <- left_join(full.ghcn.sort_lag, full.ghcn.sort_lag34, by = c("year", "ID"))
full.ghcn.sort_lag <- left_join(full.ghcn.sort_lag, full.ghcn.sort_5, by = c("year", "ID"))

full.ghcn.sort_lag <- left_join(full.ghcn.sort, full.ghcn.sort_lag, by = c("year", "ID"))



# omit NA values for RWI - 1:
ghcn.clean <- full.ghcn.sort_lag[!is.na(full.ghcn.sort_lag$mean.diff) & !is.na(full.ghcn.sort_lag$rel.diff_1) & !is.na(full.ghcn.sort_lag$rel.diff_2) & !is.na(full.ghcn.sort_lag$rel.diff_3) & !is.na(full.ghcn.sort_lag$rel.diff_4)  & !is.na(full.ghcn.sort_lag$rel.diff_5) & !is.na(full.ghcn.sort_lag$DBH),]
ghcn.clean <- ghcn.clean[,c("site", "ID", "year", "RWI", "MAP.prism","MAP.scaled","JUNTavg","T.scaled", "DBH","mean.diff","rel.diff_1", "rel.diff_2",         
                            "rel.diff_3", "rel.diff_4","rel.diff_5" , "ageclass")]
full.ghcn <- ghcn.clean
# split training and testing datasets:
msk <- caTools::sample.split( ghcn.clean, SplitRatio = 3/4, group = NULL )

train.RWI.full <- ghcn.clean[msk,]
test.RWI.full <- ghcn.clean[!msk,]

# create a dataste the elimates yrs 1900-1950 for the modern and 1950-present for past:

mod.post <- full.ghcn[full.ghcn$ageclass %in% "Modern" & full.ghcn$year >= 1950,]
past.pre <- full.ghcn[full.ghcn$ageclass %in% "Past" & full.ghcn$year < 1950,]

sub.ghcn <- rbind(mod.post, past.pre)

msk <- caTools::sample.split( sub.ghcn, SplitRatio = 3/4, group = NULL )

train.RWI <- ghcn.clean[msk,]
test.RWI <- ghcn.clean[!msk,]



#------------------------------------------------------------------------------------
# create probe data to get posterior predictive estimates from ED and GUESS
# generate probe dataset:
DIprobe <- round(seq(range(train.full$Precip.scaled)[1], range(train.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.full$Temp.jja.scaled)[1], range(train.full$Temp.jja.scaled)[2], by = 2), 3)
RWI1probe <- round(seq(range(train.full$rel.gwbi_1)[1], range(train.full$rel.gwbi_1)[2], by = 2), 2)
RWI2probe <- round(seq(range(train.full$rel.gwbi_2)[1], range(train.full$rel.gwbi_2)[2], by = 2), 2)
RWI3probe <- round(seq(range(train.full$rel.gwbi_3)[1], range(train.full$rel.gwbi_3)[2], by = 2), 2)
RWI4probe <- round(seq(range(train.full$rel.gwbi_4)[1], range(train.full$rel.gwbi_4)[2], by = 2), 2)

# expand into full probe
probe.ED <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                     rel.gwbi_1 = 1, rel.gwbi_2= 1, rel.gwbi_3= 1,rel.gwbi_4= 1,
                     site_num = 1:221,struct.cohort.code= 1:2)


# -------------------------------------------
# Develop bayesian mixed models for ED
#--------------------------------------------
library(mgcv)
mod <- lm(rel.gwbi ~ Precip.scaled + Temp.jja.scaled + CO2 + rel.gwbi_1 + rel.gwbi_2 +rel.gwbi_3 +rel.gwbi_4+rel.gwbi_5 + Site, data = train.ED)
summary(mod)
#plot(mod)
preds <- predict(mod, test.ED)
plot(test.ED$gwbi, preds)
abline(a = 0, b = 1, col= "red")

# basic lm with no re explais ~ 50 percent of variance

# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
ED_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] + beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)
beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)
mu_beta6 ~ dunif(-2, 2)

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
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)
inv_beta6   ~ dgamma(0.001, 0.001)
sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is gwbbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}


# Probe
for(i in 1:nprobe){
 #process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

 #function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 

}

}"






reg.EDel.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                               data = list(Y = train.ED$rel.gwbi, n=length(train.ED$rel.gwbi), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, agbi_1 = train.ED$rel.gwbi_1,agbi_2 = train.ED$rel.gwbi_2, agbi_3 = train.ED$rel.gwbi_3, agbi_4 = train.ED$rel.gwbi_4,
                                           period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = as.numeric(train.ED$site_code), np=length(test.ED$period_cd), 
                                           sites.p = test.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, agbi_1.p = test.ED$rel.gwbi_1, agbi_2.p = test.ED$rel.gwbi_2, agbi_3.p = test.ED$rel.gwbi_3, agbi_4.p = test.ED$rel.gwbi_4,
                                           period.p = as.numeric(test.ED$period_cd),
                                           
                                           nprobe=length(probe.ED$struct.cohort.code), 
                                           sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, agbi_1.probe = probe.ED$rel.gwbi_1, agbi_2.probe = probe.ED$rel.gwbi_2, agbi_3.probe = probe.ED$rel.gwbi_3, agbi_4.probe = probe.ED$rel.gwbi_4,
                                           period.probe = as.numeric(probe.ED$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.EDel.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.EDel.by_period, 
 #                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
  #                          n.chains = 3, n.iter=2000, thin = 10)
samp.ED.period <- coda.samples(reg.EDel.by_period, 
                              variable.names=c("alpha", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6" ), 
                              n.chains = 3, n.iter=5000, thin = 1)

samp.ED.ypred <- coda.samples(reg.EDel.by_period, 
                               variable.names=c("Ypred" ), 
                               n.chains = 3, n.iter=5000, thin = 1)

samp.ED.yprobe <- coda.samples(reg.EDel.by_period, 
                              variable.names=c("Yprobe" ), 
                              n.chains = 3, n.iter=5000, thin = 1)


saveRDS(samp.ED.period, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_parameter_samps.rds")
saveRDS(samp.ED.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_Ypred_samps.rds")
saveRDS(samp.ED.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_Yprobe_samps.rds")
samp.ED.yprobe <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/ED_Yprobe_samps.rds")

saveRDS(test.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_testdata.rds")
saveRDS(train.ED, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_traindata.rds")

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
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/basic_reg/pred_vs_obs.png")
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

png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()


#-------------- plot posterior predictions:--------------------
# now look at predictions:

Yprobe.samps <- data.frame(Yprobe.sampes) 
Yp.m <- melt(Yprobe.samps)
#Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
 #                                                              ci.hi = quantile(value,0.975),
  #                                                             ci.lo = quantile(value,0.025))

colnames(Yprobe.samps) <- 1:length(Yprobe.samps)
probe.m <- melt(Yprobe.samps)
colnames(probe.m) <- c("num", "gwbi_pred")

probe.ED$num <- 1:length(probe.ED[,1])
#colnames(test.GUESS) <- c("Drought", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "num")

# summarize by cohort class only:
probe.ED$num <- as.factor(as.character(probe.ED$num))
full.p <- probe.ED

probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(probtest) <- c("num", "gwbi_pred", "MAP_scaled", "JJA.T.scaled", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "site_num", "period")
saveRDS(probtest, "outputs/gwbi_model/ED_probtest.rds")


cohort.summary.pr <- probtest %>% group_by(period, MAP_scaled) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                   gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                   gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- probtest %>% group_by(period, JJA.T.scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                             gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                             gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)

ggplot(cohort.summary.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

ggplot(data = cohort.summary.tm, aes(JJA.T.scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

# compare drought sensitivity at low temperatures and high temperatures:
cohort.summary.tm.pr <- probtest %>% group_by(period, JJA.T.scaled, MAP_scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm.pr$period <- as.factor(cohort.summary.tm.pr$period)
ggplot(data = cohort.summary.tm.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~JJA.T.scaled)

#------------------------------------------------------------------------------
# Run the model for GUESS:
#-------------------------------------------------------------------------------



library(mgcv)
mod <- lm(rel.gwbi ~ Precip.scaled + Temp.jja.scaled  + rel.gwbi_1 + rel.gwbi_2 +rel.gwbi_3 +rel.gwbi_4 + rel.gwbi_5 + Site, data = train.GUESS)
summary(mod)

# basic lm with no re explais ~ 50 percent of variance

# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
GUESS_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] + beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)
beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)
mu_beta6 ~ dunif(-2, 2)

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
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)
inv_beta6   ~ dgamma(0.001, 0.001)
sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}

# Probe
for(i in 1:nprobe){
#process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

#function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 

}


}"



DIprobe <- round(seq(range(train.GUESS.full$Precip.scaled)[1], range(train.GUESS.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.GUESS.full$Temp.jja.scaled)[1], range(train.GUESS.full$Temp.jja.scaled)[2], by = 2), 3)
GUESS1probe <- round(seq(range(train.GUESS.full$rel.gwbi_1)[1], range(train.GUESS.full$rel.gwbi_1)[2], by = 2), 2)
GUESS2probe <- round(seq(range(train.GUESS.full$rel.gwbi_2)[1], range(train.GUESS.full$rel.gwbi_2)[2], by = 2), 2)
GUESS3probe <- round(seq(range(train.GUESS.full$rel.gwbi_3)[1], range(train.GUESS.full$rel.gwbi_3)[2], by = 2), 2)
GUESS4probe <- round(seq(range(train.GUESS.full$rel.gwbi_4)[1], range(train.GUESS.full$rel.gwbi_4)[2], by = 2), 2)

# expand into full probe
probe.GUESS <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                        rel.gwbi_1 = 1, rel.gwbi_2= 1, rel.gwbi_3= 1,rel.gwbi_4= 1,
                        site_num = 1:length(unique(train.GUESS$Site)),struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(GUESS_re_site_time_period), 
                                data = list(Y = train.GUESS$rel.gwbi, n=length(train.GUESS$rel.gwbi), Precip.scaled = train.GUESS$Precip.scaled, Temp.jja.scaled = train.GUESS$Temp.jja.scaled, agbi_1 = train.GUESS$rel.gwbi_1,agbi_2 = train.GUESS$rel.gwbi_2, agbi_3 = train.GUESS$rel.gwbi_3, agbi_4 = train.GUESS$rel.gwbi_4,
                                            period = as.numeric(train.GUESS$period_cd), S = unique(train.GUESS$site_num),  C = unique(train.GUESS$period_cd), sites = train.GUESS$site_num, np=length(test.GUESS$period_cd), 
                                            sites.p = test.GUESS$site_num, Precip.scaled.p = test.GUESS$Precip.scaled, Temp.jja.scaled.p = test.GUESS$Temp.jja.scaled, agbi_1.p = test.GUESS$rel.gwbi_1, agbi_2.p = test.GUESS$rel.gwbi_2, agbi_3.p = test.GUESS$rel.gwbi_3, agbi_4.p = test.GUESS$rel.gwbi_4,
                                            period.p = as.numeric(test.GUESS$period_cd),
                                           
                                            nprobe=length(probe.GUESS$struct.cohort.code), 
                                            sites.probe = probe.GUESS$site_num, Precip.scaled.probe = probe.GUESS$DI.scaled, Temp.jja.scaled.probe = probe.GUESS$T.scaled, agbi_1.probe = probe.GUESS$rel.gwbi_1, agbi_2.probe = probe.GUESS$rel.gwbi_2, agbi_3.probe = probe.GUESS$rel.gwbi_3, agbi_4.probe = probe.GUESS$rel.gwbi_4,
                                            period.probe = as.numeric(probe.GUESS$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.GUESS.period <- coda.samples(reg.GUESSel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
samp.GUESS.period <- coda.samples(reg.model.by_period, 
                               variable.names=c("alpha", "beta1", "beta2","beta3","beta4","beta5","beta6" ), 
                               n.chains = 3, n.iter=5000, thin = 1)

samp.GUESS.ypred <- coda.samples(reg.model.by_period, 
                              variable.names=c("Ypred" ), 
                              n.chains = 3, n.iter=5000, thin = 1)

samp.GUESS.yprobe <- coda.samples(reg.model.by_period, 
                                 variable.names=c("Yprobe" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.GUESS.period, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_parameter_samps.rds")
saveRDS(samp.GUESS.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_Ypred_samps.rds")
saveRDS(samp.GUESS.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_Yprobe_samps.rds")
samp.GUESS.yprobe<- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_Yprobe_samps.rds")

saveRDS(test.GUESS, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_traindata.rds")


#Extract the samples for each parameter

samps       <- samp.GUESS.period[[1]]
Yp.samps    <- samp.GUESS.ypred [[1]]
Yprobe.samps <- samp.GUESS.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site_num))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]
beta3.samps  <- samps[,(length(unique(test.GUESS$site_num))+5):(length(unique(test.GUESS$site_num))+6)]
beta4.samps  <- samps[,(length(unique(test.GUESS$site_num))+7):(length(unique(test.GUESS$site_num))+8)]
beta5.samps  <- samps[,(length(unique(test.GUESS$site_num))+9):(length(unique(test.GUESS$site_num))+10)]
beta6.samps  <- samps[,(length(unique(test.GUESS$site_num))+11):(length(unique(test.GUESS$site_num))+12)]

#sigma.samps <- samps[,(length(unique(test.GUESS$site_num))+6):(length(test.GUESS$site_num)+length(unique(test.GUESS$site_num))+12)]



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
#png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/basic_reg/pred_vs_obs.png")
p.o.plot
#dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

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

#>>>>>>>> plot dot plots from GUESS model:
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


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <- ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <- ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <- ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()


#-------------- plot posterior predictions:--------------------
# now look at predictions:

Yprobe.samps <- data.frame(Yprobe.samps) 
Yp.m <- melt(Yprobe.samps)
#Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
#                                                              ci.hi = quantile(value,0.975),
#                                                             ci.lo = quantile(value,0.025))

colnames(Yprobe.samps) <- 1:length(Yprobe.samps)
probe.m <- melt(Yprobe.samps)
colnames(probe.m) <- c("num", "gwbi_pred")

probe.GUESS$num <- 1:length(probe.GUESS[,1])
#colnames(test.GUESS) <- c("Drought", "DBH","Tmax", "GUESS_1","GUESS_2","struct.cohort", "num")

# summarize by cohort class only:
probe.GUESS$num <- as.factor(as.character(probe.GUESS$num))
full.p <- probe.GUESS

probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(probtest) <- c("num", "gwbi_pred", "MAP_scaled", "JJA.T.scaled", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "site_num", "period")

saveRDS(probtest, "outputs/gwbi_model/GUESS_probtest.rds")
cohort.summary.pr <- probtest %>% group_by(period, MAP_scaled) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                    gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                    gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- probtest %>% group_by(period, JJA.T.scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)

ggplot(cohort.summary.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

ggplot(data = cohort.summary.tm, aes(JJA.T.scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

# compare drought sensitivity at low temperatures and high temperatures:
cohort.summary.tm.pr <- probtest %>% group_by(period, JJA.T.scaled, MAP_scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                              gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                              gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm.pr$period <- as.factor(cohort.summary.tm.pr$period)
ggplot(data = cohort.summary.tm.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~JJA.T.scaled)

# -------------- read in tree ring data and model tree ring data ---------------------------
colnames(train.RWI.full) <- c("site"  ,     "ID" ,        "year" ,      "RWI"  ,      "MAP.prism" , "Precip.scaled" ,"JUNTmax" ,  
                           "Temp.jja.scaled",  "DBH",  "rel.gwbi" , "rel.gwbi_1" ,"rel.gwbi_2", "rel.gwbi_3", "rel.gwbi_4",
                           "rel.gwbi_5", "ageclass")
train.rwi.full <- train.RWI.full

colnames(test.RWI.full) <- c("site"  ,     "ID" ,        "year" ,      "RWI"  ,      "MAP.prism" , "Precip.scaled" ,"JUNTmax" ,  
                          "Temp.jja.scaled",  "DBH",  "rel.gwbi" , "rel.gwbi_1" ,"rel.gwbi_2", "rel.gwbi_3", "rel.gwbi_4",
                          "rel.gwbi_5", "ageclass")
test.rwi.full <- test.RWI.full


# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
rwi_re_site_time_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*agbi_1[i] + beta4[period[i]]*agbi_2[i] + beta5[period[i]]*agbi_3[i] + beta6[period[i]]*agbi_4[i] # use Drought index as a scaled variable 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)
beta4[c] ~ dnorm(mu_beta4, inv_beta4)
beta5[c] ~ dnorm(mu_beta5, inv_beta5)
beta6[c] ~ dnorm(mu_beta6, inv_beta6)
}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)
mu_beta4 ~ dunif(-2, 2)
mu_beta5 ~ dunif(-2, 2)
mu_beta6 ~ dunif(-2, 2)

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
inv_beta5   ~ dgamma(0.001, 0.001)
sigma_beta5 <- 1/sqrt(inv_beta5)
inv_beta6   ~ dgamma(0.001, 0.001)
sigma_beta6 <- 1/sqrt(inv_beta6)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}

# Probe
for(i in 1:nprobe){
#process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

#function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*agbi_1.probe[i] + beta4[period.probe[i]]*agbi_2.probe[i] + beta5[period.probe[i]]*agbi_3.probe[i] + beta6[period.probe[i]]*agbi_4.probe[i]# use Drought index as a scaled variable 

}


}"



DIprobe <- round(seq(range(train.rwi.full$Precip.scaled)[1], range(train.rwi.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.rwi.full$Temp.jja.scaled)[1], range(train.rwi.full$Temp.jja.scaled)[2], by = 2), 3)
RWI1probe <- round(seq(range(train.rwi.full$rel.gwbi_1)[1], range(train.rwi.full$rel.gwbi_1)[2], by = 2), 2)
RWI2probe <- round(seq(range(train.rwi.full$rel.gwbi_2)[1], range(train.rwi.full$rel.gwbi_2)[2], by = 2), 2)
RWI3probe <- round(seq(range(train.rwi.full$rel.gwbi_3)[1], range(train.rwi.full$rel.gwbi_3)[2], by = 2), 2)
RWI4probe <- round(seq(range(train.rwi.full$rel.gwbi_4)[1], range(train.rwi.full$rel.gwbi_4)[2], by = 2), 2)

# expand into full probe
probe.RWI <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                           rel.gwbi_1 = 1, rel.gwbi_2= 1, rel.gwbi_3= 1,rel.gwbi_4= 1,
                           site_num = 1:16,struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(rwi_re_site_time_period), 
                                  data = list(Y = train.RWI.full$rel.gwbi, n=length(train.RWI.full$rel.gwbi), Precip.scaled = train.RWI.full$Precip.scaled, Temp.jja.scaled = train.RWI.full$Temp.jja.scaled, agbi_1 = train.RWI.full$rel.gwbi_1,agbi_2 = train.RWI.full$rel.gwbi_2, agbi_3 = train.RWI.full$rel.gwbi_3, agbi_4 = train.RWI.full$rel.gwbi_4,
                                              period = as.numeric(train.RWI.full$ageclass), S = unique(train.RWI.full$site),  C = unique(train.RWI.full$ageclass), sites = train.RWI.full$site, np=length(test.RWI.full$ageclass), 
                                              sites.p = test.RWI.full$site, Precip.scaled.p = test.RWI.full$Precip.scaled, Temp.jja.scaled.p = test.RWI.full$Temp.jja.scaled, agbi_1.p = test.RWI.full$rel.gwbi_1, agbi_2.p = test.RWI.full$rel.gwbi_2, agbi_3.p = test.RWI.full$rel.gwbi_3, agbi_4.p = test.RWI.full$rel.gwbi_4,
                                              period.p = as.numeric(test.RWI.full$ageclass),
                                              
                                              nprobe=length(probe.RWI$struct.cohort.code), 
                                              sites.probe = probe.RWI$site, Precip.scaled.probe = probe.RWI$DI.scaled, Temp.jja.scaled.probe = probe.RWI$T.scaled, agbi_1.probe = probe.RWI$rel.gwbi_1, agbi_2.probe = probe.RWI$rel.gwbi_2, agbi_3.probe = probe.RWI$rel.gwbi_3, agbi_4.probe = probe.RWI$rel.gwbi_4,
                                              period.probe = as.numeric(probe.RWI$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

samp.rwi.period <- coda.samples(reg.model.by_period, 
                                  variable.names=c("alpha", "beta1", "beta2","beta3","beta4","beta5","beta6" ), 
                                  n.chains = 3, n.iter=10000, thin = 10)

samp.rwi.ypred <- coda.samples(reg.model.by_period, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

samp.rwi.yprobe <- coda.samples(reg.model.by_period, 
                                  variable.names=c("Yprobe" ), 
                                  n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.rwi.period, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_parameter_samps.rds")
saveRDS(samp.rwi.ypred, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_Ypred_samps.rds")
saveRDS(samp.rwi.yprobe, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_Yprobe_samps.rds")
samp.rwi.yprobe<- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_Yprobe_samps.rds")

saveRDS(test.RWI, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_testdata.rds")
saveRDS(train.RWI, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_traindata.rds")

summary(samp.rwi.period)
#plot(samp.rwi.period)

gelman.diag(samp.rwi.period)
acfplot(samp.rwi.period)

#Extract the samples for each parameter

samps       <- samp.rwi.period[[1]]
Yp.samps    <- samp.rwi.ypred [[1]]
Yprobe.samps <- samp.rwi.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.RWI$site))]
beta1.samps  <- samps[,(length(unique(test.RWI$site))+1):(length(unique(test.RWI$site))+2)]
beta2.samps  <- samps[,(length(unique(test.RWI$site))+3):(length(unique(test.RWI$site))+4)]
beta3.samps  <- samps[,(length(unique(test.RWI$site))+5):(length(unique(test.RWI$site))+6)]
beta4.samps  <- samps[,(length(unique(test.RWI$site))+7):(length(unique(test.RWI$site))+8)]
beta5.samps  <- samps[,(length(unique(test.RWI$site))+9):(length(unique(test.RWI$site))+10)]
beta6.samps  <- samps[,(length(unique(test.RWI$site))+11):(length(unique(test.RWI$site))+12)]

#sigma.samps <- samps[,(length(unique(test.GUESS$site))+6):(length(test.GUESS$site)+length(unique(test.GUESS$site))+12)]



# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.RWI$rel.gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.RWI$rel.gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_pred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.RWI$rel.gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.RWI$rel.gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.RWI$site)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <- unique(levels(train.RWI$ageclass)) # past = 2, modern = 1 here
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(levels(train.RWI$ageclass)) 
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temperature slope")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(levels(train.RWI$ageclass)) 
#colnames(b3) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b3$num <- rownames(b3)
b3.m <- melt(b3, id.vars=c("num"))
b3.mplots <- ggplot(b3.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-1 Index slope")


b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(levels(train.RWI$ageclass)) 

#colnames(b4) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b4$num <- rownames(b4)
b4.m <- melt(b4, id.vars=c("num"))
b4.mplots <- ggplot(b4.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-2 Index slope")

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(levels(train.RWI$ageclass)) 
#colnames(b5) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b5$num <- rownames(b5)
b5.m <- melt(b5, id.vars=c("num"))
b5.mplots <- ggplot(b5.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-3 Index slope")

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(levels(train.RWI$ageclass)) 
#
#colnames(b6) <- c(unique(train.dry$struct.cohort)[order(unique(train.dry[,c("struct.cohort", "struct.cohort.code")])[,2])])
b6$num <- rownames(b6)
b6.m <- melt(b6, id.vars=c("num"))
b6.mplots <- ggplot(b6.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("gwbi-4 Index slope")

#>>>>>>>> plot dot plots from RWI model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.05), 
                                                           Ci.high = quantile(value, 0.95))
#b
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.05), 
                                                           Ci.high = quantile(value, 0.95))
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
RWI.site.clim <- sub.ghcn %>% group_by(site) %>% summarise(MAP = mean(MAP.prism, na.rm=TRUE), 
                                                                   MAT = mean(JUNTmax, na.rm=TRUE))

a1.sum$Site <- paste0( a1.sum$variable)

a1.clim <- merge(RWI.site.clim, a1.sum, by.x = "site", by.y = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+xlim(-0.1, 0.25)+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)") + geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated JJA temperauture sensitivity")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b3.dot <- ggplot(data.frame(b3.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -1 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b4.dot <- ggplot(data.frame(b4.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -2 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b5.dot <- ggplot(data.frame(b5.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -3 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")

b6.dot <- ggplot(data.frame(b6.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi -4 parameter")#+xlim(0.06, 0.1) + geom_vline(xintercept = 0, linetype = "dashed")


# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/RWI_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, b3.dot, b4.dot, b5.dot, b6.dot, ncol = 1)
dev.off()


#-------------- plot posterior predictions:--------------------
# now look at predictions:

Yprobe.samps <- data.frame(Yprobe.samps) 
Yp.m <- melt(Yprobe.samps)
#Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
#                                                              ci.hi = quantile(value,0.975),
#                                                             ci.lo = quantile(value,0.025))

colnames(Yprobe.samps) <- 1:length(Yprobe.samps)
probe.m <- melt(Yprobe.samps)
colnames(probe.m) <- c("num", "gwbi_pred")

probe.rwi$num <- 1:length(probe.rwi[,1])
#colnames(test.RWI) <- c("Drought", "DBH","Tmax", "RWI_1","RWI_2","struct.cohort", "num")

# summarize by cohort class only:
probe.rwi$num <- as.factor(as.character(probe.rwi$num))
full.p <- probe.rwi

probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(probtest) <- c("num", "gwbi_pred", "MAP_scaled", "JJA.T.scaled", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "site_num", "period")

saveRDS(probtest, "outputs/gwbi_model/RWI_probtest.rds")
cohort.summary.pr <- probtest %>% group_by(period, MAP_scaled) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                    gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                    gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- probtest %>% group_by(period, JJA.T.scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)

ggplot(cohort.summary.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

ggplot(data = cohort.summary.tm, aes(JJA.T.scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)

# compare drought sensitivity at low temperatures and high temperatures:
cohort.summary.tm.pr <- probtest %>% group_by(period, JJA.T.scaled, MAP_scaled) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                              gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                              gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm.pr$period <- as.factor(cohort.summary.tm.pr$period)
ggplot(data = cohort.summary.tm.pr, aes(MAP_scaled, gwbi, color = period))+geom_line()+geom_ribbon(data = cohort.summary.tm.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = period), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~JJA.T.scaled)




# ------------- read in both GUESS and ED and plot posteriors together to compare ---------
library(DMwR)
GUESS.probe <- readRDS("outputs/gwbi_model/GUESS_probtest.rds")

GUESS.probe$model <- "LPJ-GUESS"
GUESS.probe$Precip <- as.numeric(round(unscale(vals = GUESS.probe$MAP_scaled, norm.data = GUESS.sort_lag.Precip.scaled))) 
GUESS.probe$Temp <- as.numeric(round(unscale(vals = GUESS.probe$JJA.T.scaled, norm.data = GUESS.sort_lag.Temp.jja.scaled))) 



  
ED.probe <- readRDS("outputs/gwbi_model/ED_probtest.rds")
ED.probe$model <- "ED2"
ED.probe$Precip <- as.numeric(round(unscale(vals = ED.probe$MAP_scaled, norm.data = ED.sort_lag.Precip.scaled))) 
ED.probe$Temp <- as.numeric(round(unscale(vals = ED.probe$JJA.T.scaled, norm.data = ED.sort_lag.Temp.jja.scaled))) 



RWI.probe <- readRDS("outputs/gwbi_model/RWI_probtest.rds")
RWI.probe$model <- "Tree Rings"
RWI.probe$Precip <- as.numeric(round(unscale(vals = RWI.probe$MAP_scaled, norm.data = full.ghcn.MAP.scaled))) 
RWI.probe$Temp <- as.numeric(round(unscale(vals = RWI.probe$JJA.T.scaled, norm.data = full.ghcn.T.scaled))) 
RWI.probe$Temp <-  ((RWI.probe$Temp - 32) * (5 / 9)) 
 


#model.probe <- bind_rows(GUESS.probe, ED.probe)

model.probe.mod <- bind_rows(GUESS.probe, ED.probe)
model.probe <- bind_rows(model.probe.mod, RWI.probe)

# note 2 == Past and 1 == Modern:

cohort.summary.pr <- model.probe %>% group_by(period, Precip, model) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                    gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                    gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- model.probe %>% group_by(period, Temp, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                               gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                               gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)
cohort.summary.tm$ageclass <- ifelse(cohort.summary.tm$period == "1", "Modern", "Past")
cohort.summary.pr$ageclass <- ifelse(cohort.summary.pr$period == "1", "Modern", "Past")

# compare sensitivities of the models
ggplot(cohort.summary.pr, aes(Precip, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()

ggplot(cohort.summary.tm, aes(Temp, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()


precip.overall.sens <- ggplot(cohort.summary.pr, aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank())

temp.overall.sens <- ggplot(cohort.summary.tm, aes(Temp, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+labs(x = expression('Summer Temperature ('*~degree*C*')'),y = "Relative Growth")+theme(panel.grid = element_blank())

legend <- get_legend(precip.overall.sens)

png(height = 10, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/marginal_P_T_effects.png")
plot_grid(plot_grid( precip.overall.sens+theme(legend.position = "none"), 
                     temp.overall.sens+theme(legend.position = "none"), 
                      ncol = 1, align = "hv"),legend, ncol = 2, rel_widths = c(1,0.25))
dev.off()


# find the grid cells that are closest to the tree ring sites:


# read in the spatial points data for the Tree ring data:
TREERING_PALEON_GRID <- read.csv("/Users/kah/Documents/TreeRings/data/KH_Treering_sites_PALEON_model_grid.csv - KH_Treering_sites_PALEON_model_grid.csv-2.csv")

# find the closest grid cell:
load("Data/PalEON_siteInfo_all.RData")
TR.locs <- TREERING_PALEON_GRID[TREERING_PALEON_GRID$Site.code %in% c( "AVO",  "BON",  "COR",  "ENG",  "GLA","GLL",  "GLL1", "GLL2", "GLL3", "GLL4", "HIC",  "MOU",  "PLE",  "PVC", 
                                                            "STC",  "TOW",  "UNC"  ),]
TR.sites <- merge(paleon, TREERING_PALEON_GRID, by.x = "latlon", by.y = "latlon_PALEON")
ggplot(paleon, aes(lon, lat), fill = "forestgreen")+geom_raster()+coord_cartesian()+geom_point(data = TR.locs, aes(x = longitude, y = latitude, color = Site.code))

library(geosphere)

# create distance matrix
mat <- distm( TR.locs[,c('longitude','latitude')],paleon[,c('lon','lat')], fun=distVincentyEllipsoid)

# assign the name to the point in list1 based on shortest distance in the matrix
TR.locs$lat_PALEON_closest <- paleon$lat[max.col(-mat)]
TR.locs$lon_PALEON_closest <- paleon$lon[max.col(-mat)]
TR.locs$paleon_gridlatlon <- paleon$latlon[max.col(-mat)]

ggplot()+geom_raster(data = paleon, aes(x = lon, y =lat))+coord_cartesian()+geom_point(data = TR.locs, aes(x = longitude, y = latitude, color = Site.code))+geom_raster(data = TR.locs, aes(x = lon_PALEON_closest, y =lat_PALEON_closest), fill = "red")

paleon.sites <- paleon[paleon$latlon %in% TR.locs$paleon_gridlatlon,]

model.site.num <- paleon.sites$num
model.probe.subset <- model.probe.mod %>% filter(site_num %in% paleon.sites$num)
model.TR.sites <- bind_rows(model.probe.subset, RWI.probe)


# plot out only the grid cells where we have TR sites:
# note 2 == Past and 1 == Modern:

cohort.sites.pr <- model.TR.sites %>% group_by(period, Precip, model) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                              gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                              gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.sites.tm <- model.TR.sites %>% group_by(period, Temp, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                         gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                         gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.sites.pr$period <- as.factor(cohort.sites.pr$period)
cohort.sites.tm$period <- as.factor(cohort.sites.tm$period)
cohort.sites.tm$ageclass <- ifelse(cohort.sites.tm$period == "1", "Modern", "Past")
cohort.sites.pr$ageclass <- ifelse(cohort.sites.pr$period == "1", "Modern", "Past")

# compare sensitivities of the models
ggplot(cohort.sites.pr, aes(Precip, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.sites.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()

ggplot(cohort.sites.tm, aes(Temp, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.sites.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()


ggplot(cohort.sites.pr, aes(Precip, gwbi, color = ageclass))+geom_line()+geom_ribbon(data = cohort.sites.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 3)+theme_bw(base_size = 12)+ylab("Relativized Woody Growth")+xlab("Annual Precipitation (mm)")

ggplot(cohort.sites.tm, aes(Temp, gwbi, color = ageclass))+geom_line()+geom_ribbon(data = cohort.sites.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 3)+theme_bw(base_size = 12)+ylab("Relativized Woody Growth")+xlab(expression("Temperature " ( degree*C)))

# plot out pretty plot for just grid cells closest to 
precip.sites.sens <- ggplot(cohort.sites.pr, aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())

temp.sites.sens <- ggplot(cohort.sites.tm, aes(Temp, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+labs(x = expression('Summer Temperature ('*~degree*C*')'),y = "Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())

legend <- get_legend(precip.sites.sens)

png(height = 9, width = 14.5, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/marginal_P_T_effects_TR_sites_only.png")
plot_grid( precip.sites.sens+theme(legend.position = "none"), 
                     temp.sites.sens+theme(legend.position = "none"), 
                     ncol = 1, align = "hv")
dev.off()



# now lets look at posteriors for different conditions:
cohort.sites.tm.pr <- model.TR.sites %>% group_by(period, Temp, Precip, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                  gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                  gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.sites.tm.pr$period <- as.factor(cohort.sites.tm.pr$period)
cohort.sites.tm.pr$ageclass <- ifelse(cohort.sites.tm.pr$period == "1", "Modern", "Past")
cohort.sites.tm.pr$Precip <- as.numeric(cohort.sites.tm.pr$Precip)

cool <- ggplot(cohort.sites.tm.pr[cohort.sites.tm.pr$Temp < 15,], aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp < 15,],aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())
warm <- ggplot(cohort.sites.tm.pr[cohort.sites.tm.pr$Temp > 21 & cohort.sites.tm.pr$Temp < 24,], aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp > 21 & cohort.sites.tm.pr$Temp < 24,],aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())

title1 <- ggdraw()+draw_label(expression('COOL SUMMERS (>15'*~degree*C*')'), size = 42, fontface = "bold")
title2 <- ggdraw()+draw_label(expression('HOT SUMMERS (>25'*~degree*C*')'), size = 42, fontface = "bold")

legend <- get_legend(cool)

png(height = 12.5, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/P_effects_low_high_temp_TR_sites_only.png")
plot_grid( title1, 
                     cool+theme(legend.position = "none"), 
                     title2,
                     warm+theme(legend.position = "none"), 
                      ncol=1, rel_heights=c(0.1,1,0.1, 1), align = "hv")  # rel_heights values control title margins
dev.off()

png(height = 9, width = 14.5, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/P_effects_low_high_temp_TR_sites_only_no_labels.png")
plot_grid(  
                     cool+ylim(-1,2.5)+theme(legend.position = "none"), 
                     
                     warm+ylim(-1,2.5)+theme(legend.position = "none"), 
                     ncol=1, align = "hv") # rel_heights values control title margins
dev.off()

model.TR.sites.grouped <- model.TR.sites
model.TR.sites.grouped$Temp_class <- ifelse(model.TR.sites.grouped$Temp <=15, "Low", 
                                            ifelse(model.TR.sites.grouped$Temp > 15 & model.TR.sites.grouped$Temp <= 20, "Med",
                                                   ifelse(model.TR.sites.grouped$Temp > 20,"High",NA)))


model.TR.sites.grouped$Dry_class <- ifelse(model.TR.sites.grouped$Precip <= 300, "Low", 
                                            ifelse(model.TR.sites.grouped$Precip > 300 & model.TR.sites.grouped$Precip <= 1110, "Med",
                                                   ifelse(model.TR.sites.grouped$Precip > 1111,"High",NA)))

cohort.sites.groups <- model.TR.sites.grouped %>% group_by(period, Temp_class, Dry_class, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                             gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                             gwbi.high = quantile(gwbi_pred, 0.975)) 



cohort.sites.groups$period <- as.factor(cohort.sites.groups$period)
cohort.sites.groups$ageclass <- ifelse(cohort.sites.groups$period == "1", "Modern", "Past")
#cohort.sites.groups$Precip <- as.numeric(cohort.sites.groups$Precip)


ggplot(cohort.sites.groups[cohort.sites.groups$Temp_class %in% c("Low", "High") & cohort.sites.groups$Dry_class %in% c("Low", "High"),], aes(x = Temp_class, y = gwbi, color = ageclass))+geom_point(size = 5)+geom_errorbar( aes(ymin = gwbi.low, ymax = gwbi.high, size = 0.5,width = 0.5))+facet_grid(model ~ Dry_class)#+facet_wrap(~model)


# read in climate data for future projections to plot onto figures:
rcp <- 85
climate <- "pr"
setwd('/Users/kah/Documents/bimodality/data/cc85pr70/')
#spec.table <- read.csv('/Users/kah/Documents/bimodality/data/midwest_pls_full_density_pr_alb1.7-5.csv')
coordinates(TR.locs) <- ~ lon_PALEON_closest+ lat_PALEON_closest 
proj4string(TR.locs) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '
#tree.ll <- spTransform(TR.locs, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

month <- sprintf("%02d", 1:12)
month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
               'Jun', "Jul", "Aug", "Sep")
filenames <- list.files(pattern=paste0("cc",rcp,climate,"70",".*\\.tif$", sep = ""))
s <- stack(filenames)
t <- crop(s, extent(TR.locs))#make all into a raster
#s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
#crop to the extent of tree ring data
y <- data.frame(rasterToPoints(t)) #covert to dataframe

colnames(y) <- c("x", "y", month.abb)
y$gridNumber <- cellFromXY(s, y[, 1:2])
#write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/ccsm4_2.6_precip.csv' ))

full <- y
full$total<- rowSums(full[,3:14], na.rm=TRUE)
full$SI <- rowSums(abs(full[,3:14]-(full[,16]/12)))/full[,16]
  
# now extract full 
ggplot(full, aes(x,y, fill = total))+geom_raster()

full <- full[,c("x","y", "total")]
coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 
proj4string(full) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '
#full.rast <- raster(full)

TR.locs.df <- data.frame(TR.locs)
TR.locs.df$ccesm_85_70_total_pr <- raster::extract(avgs, TR.locs.df[,c("lon_PALEON_closest", "lat_PALEON_closest")])

# now pull summer temperatures:
rcp <- 85
climate <- "tn"
setwd('/Users/kah/Documents/bimodality/data/cc85tn70/')

month <- sprintf("%02d", 1:12)
month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
               'Jun', "Jul", "Aug", "Sep")
filenames <- list.files(pattern=paste0("cc",rcp,climate,"70",".*\\.tif$", sep = ""))
s <- stack(filenames)
t <- crop(s, extent(TR.locs))#make all into a raster

#crop to the extent of tree ring data
y <- data.frame(rasterToPoints(t)) #covert to dataframe

colnames(y) <- c("x", "y", month.abb)
y$gridNumber <- cellFromXY(s, y[, 1:2])
#write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/ccsm4_2.6_precip.csv' ))

full <- y
full$JJA_tmean <- (rowMeans(full[,c("Jun", "Jul", "Aug")], na.rm=TRUE)/10)
full$SI <- (rowSums(abs(full[,3:14]-(full[,16]/12)))/full[,16])/10

# now extract full 
ggplot(full, aes(x,y, fill = JJA_tmean))+geom_raster()

full <- full[,c("x","y", "JJA_tmean")]
coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 
proj4string(full) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '
#full.rast <- raster(full)


TR.locs.df$ccesm_85_70_JJA_temp <- data.frame(raster::extract(avgs, TR.locs.df[,c("lon_PALEON_closest", "lat_PALEON_closest")]))

# now add the ranges of TR future climate to plots:


future.clims <- unique(TR.locs.df[,c("lat_PALEON_closest", "lon_PALEON_closest", "ccesm_85_70_JJA_temp", "ccesm_85_70_total_pr")])
precip.overall.sens.fut <- ggplot(cohort.summary.pr, aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank())
future.clims <- data.frame(future.clims)

TR.pr <- max(future.clims$ccesm_85_70_total_pr)
TR.temp <- max(future.clims$ccesm_85_70_JJA_temp)

future.clims.summary <- data.frame(climate = c("Precip", "Temp"),
                                   max = c(TR.pr, 32),
                                   min = c(500, TR.temp),
                                   ymax = c(2.5,2.5),
                                   ymin = c(-1.5,-1.5))

future.clims.sum <- data.frame(Temp = c(TR.temp, 32),
                               Precip = c(500, TR.pr),
                               ymin = c(-1.5,-1.5),
                               ymax = c(2.5,2.5))

temp.overall.sens.fut <- ggplot() +geom_rect(data = future.clims.summary[future.clims.summary$climate %in% "Temp",] , aes(xmin = min, xmax = max,ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.1)+
  geom_line(data = cohort.summary.tm, aes(Temp, gwbi, color = ageclass))+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.tm,aes(x = Temp,ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.4, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+labs(x = expression('Summer Temperature ('*~degree*C*')'),y = "Relative Growth")+theme(panel.grid = element_blank())#+geom_rect(data = future.clims.summary[future.clims.summary$climate %in% "Temp",] , aes(xmin = min, xmax = max,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.1)


precip.overall.sens.fut <- ggplot()+geom_rect(data = future.clims.summary[future.clims.summary$climate %in% "Precip",] , aes(xmin = min, xmax = max,ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.1)+geom_line(data = cohort.summary.pr, aes(Precip, gwbi, color = ageclass))+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.pr,aes(x = Precip,ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.4, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank())

legend <- get_legend(precip.overall.sens)
setwd("/Users/kah/Documents/WUE_MIP/WUE_MIP/")

png(height = 9, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/marginal_P_T_effects_with_future_ranges.png")
plot_grid( precip.overall.sens.fut+theme(legend.position = "none"), 
                     temp.overall.sens.fut+theme(legend.position = "none"), 
                     ncol = 1, align = "hv")
dev.off()


# make the cool and warm plots with shading:

cool.fut <- ggplot()+geom_rect(data = future.clims.summary[future.clims.summary$climate %in% "Precip",] , aes(xmin = min, xmax = max,ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.1, show.legend = TRUE)+geom_line(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp < 15,], aes(Precip, gwbi, color = ageclass))+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp < 15,],aes(x = Precip, ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank(), legend.position = "bottom")
warm.fut <- ggplot()+geom_rect(data = future.clims.summary[future.clims.summary$climate %in% "Precip",] , aes(xmin = min, xmax = max,ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.1)+geom_line(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp > 21 & cohort.sites.tm.pr$Temp < 24,], aes(Precip, gwbi, color = ageclass))+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp > 21 & cohort.sites.tm.pr$Temp < 24,],aes(x = Precip, ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())

legend.fut <- get_legend(cool.fut)

png(height = 9, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/hot_cool_Precip_effects_with_future_ranges.png")
plot_grid( cool.fut + theme(legend.position = "none"), 
           warm.fut + theme(legend.position = "none"), 
           ncol = 1, align = "hv")
dev.off()

png(height = 2, width = 4, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/legend_fut.png")
plot_grid(legend.fut)
dev.off()
#------------------------Bring in all the coefficient estimates and put in one big graph-----------



samp.rwi.period <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_parameter_samps.rds")
samp.GUESS.period <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_parameter_samps.rds")
samp.ED.period <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/ED_parameter_samps.rds")

test.RWI <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_testdata.rds")
test.GUESS <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_testdata.rds")
test.ED <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/ED_testdata.rds")

train.RWI <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/rwi_traindata.rds")
train.GUESS <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/GUESS_traindata.rds")
train.ED <- readRDS( "outputs/gwbi_model/Lag4_cohort_re_clim/ED_traindata.rds")

samp.rwi.period <- samp.rwi.period[[1]]
samp.GUESS.period <- samp.GUESS.period[[1]]
samp.ED.period <- samp.ED.period[[1]]

# get parameter estimates from TR model:
alpha.samps <- samp.rwi.period[,1:length(unique(test.RWI$site))]
beta1.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+1):(length(unique(test.RWI$site))+2)]
beta2.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+3):(length(unique(test.RWI$site))+4)]
beta3.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+5):(length(unique(test.RWI$site))+6)]
beta4.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+7):(length(unique(test.RWI$site))+8)]
beta5.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+9):(length(unique(test.RWI$site))+10)]
beta6.samps  <- samp.rwi.period[,(length(unique(test.RWI$site))+11):(length(unique(test.RWI$site))+12)]

a <- data.frame(alpha.samps)
colnames(a) <- unique(train.RWI$site)
a$num <- rownames(a)
a.m.TR <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.TR, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <- unique(levels(train.RWI$ageclass)) # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.TR <- melt(b1, id.vars=c("num"))
b1.m.TR$model <- "Tree Rings"

b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(levels(train.RWI$ageclass)) 
b2$num <- rownames(b2)
b2.m.TR <- melt(b2, id.vars=c("num"))
b2.m.TR$model <- "Tree Rings"

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(levels(train.RWI$ageclass)) 
b3$num <- rownames(b3)
b3.m.TR<- melt(b3, id.vars=c("num"))
b3.m.TR$model <- "Tree Rings"

b4 <- data.frame(beta4.samps)
colnames(b4) <-unique(levels(train.RWI$ageclass)) 
b4$num <- rownames(b4)
b4.m.TR <- melt(b4, id.vars=c("num"))
b4.m.TR$model <- "Tree Rings"

b5 <- data.frame(beta5.samps)
colnames(b5) <-unique(levels(train.RWI$ageclass)) 
b5$num <- rownames(b5)
b5.m.TR <- melt(b5, id.vars=c("num"))
b5.m.TR$model <- "Tree Rings"

b6 <- data.frame(beta6.samps)
colnames(b6) <-unique(levels(train.RWI$ageclass)) 
b6$num <- rownames(b6)
b6.m.TR <- melt(b6, id.vars=c("num"))
b6.m.TR$model <- "Tree Rings"

# get parameter estimates from ED model:
alpha.sampsED <- samp.ED.period[,1:length(unique(test.ED$site_num))]
beta1.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]
beta3.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+5):(length(unique(test.ED$site_num))+6)]
beta4.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+7):(length(unique(test.ED$site_num))+8)]
beta5.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+9):(length(unique(test.ED$site_num))+10)]
beta6.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+11):(length(unique(test.ED$site_num))+12)]

a <- data.frame(alpha.sampsED)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m.ED <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.ED, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.ED$model <- "ED2"

b1 <- data.frame(beta1.sampsED)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.ED <- melt(b1, id.vars=c("num"))
b1.m.ED$model <- "ED2"

b2 <- data.frame(beta2.sampsED)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.ED <- melt(b2, id.vars=c("num"))
b2.m.ED$model <- "ED2"

b3 <- data.frame(beta3.sampsED)
colnames(b3) <-c("Modern", "Past")
b3$num <- rownames(b3)
b3.m.ED<- melt(b3, id.vars=c("num"))
b3.m.ED$model <- "ED2"

b4 <- data.frame(beta4.sampsED)
colnames(b4) <-c("Modern", "Past")
b4$num <- rownames(b4)
b4.m.ED <- melt(b4, id.vars=c("num"))
b4.m.ED$model <- "ED2"

b5 <- data.frame(beta5.sampsED)
colnames(b5) <-c("Modern", "Past")
b5$num <- rownames(b5)
b5.m.ED <- melt(b5, id.vars=c("num"))
b5.m.ED$model <- "ED2"

b6 <- data.frame(beta6.sampsED)
colnames(b6) <-c("Modern", "Past") 
b6$num <- rownames(b6)
b6.m.ED <- melt(b6, id.vars=c("num"))
b6.m.ED$model <- "ED2"


# get parameter estimates from LPJ-GUESS model:
alpha.sampsGUESS <- samp.GUESS.period[,1:length(unique(test.GUESS$site_num))]
beta1.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]
beta3.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+5):(length(unique(test.GUESS$site_num))+6)]
beta4.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+7):(length(unique(test.GUESS$site_num))+8)]
beta5.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+9):(length(unique(test.GUESS$site_num))+10)]
beta6.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+11):(length(unique(test.GUESS$site_num))+12)]

a <- data.frame(alpha.sampsGUESS)
colnames(a) <- unique(train.GUESS$site_num)
a$num <- rownames(a)
a.m.GUESS <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.GUESS, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.GUESS$model <- "LPJ-GUESS"

b1 <- data.frame(beta1.sampsGUESS)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.GUESS <- melt(b1, id.vars=c("num"))
b1.m.GUESS$model <- "LPJ-GUESS"

b2 <- data.frame(beta2.sampsGUESS)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.GUESS <- melt(b2, id.vars=c("num"))
b2.m.GUESS$model <- "LPJ-GUESS"

b3 <- data.frame(beta3.sampsGUESS)
colnames(b3) <-c("Modern", "Past")
b3$num <- rownames(b3)
b3.m.GUESS<- melt(b3, id.vars=c("num"))
b3.m.GUESS$model <- "LPJ-GUESS"

b4 <- data.frame(beta4.sampsGUESS)
colnames(b4) <-c("Modern", "Past")
b4$num <- rownames(b4)
b4.m.GUESS <- melt(b4, id.vars=c("num"))
b4.m.GUESS$model <- "LPJ-GUESS"

b5 <- data.frame(beta5.sampsGUESS)
colnames(b5) <-c("Modern", "Past")
b5$num <- rownames(b5)
b5.m.GUESS <- melt(b5, id.vars=c("num"))
b5.m.GUESS$model <- "LPJ-GUESS"

b6 <- data.frame(beta6.sampsGUESS)
colnames(b6) <-c("Modern", "Past")
b6$num <- rownames(b6)
b6.m.GUESS <- melt(b6, id.vars=c("num"))
b6.m.GUESS$model <- "LPJ-GUESS"


# now combine all the beta1s together and make a modern past dotplot:
a.m.TR$model <- "Tree Rings"
a1.m <- bind_rows(a.m.TR, a.m.ED, a.m.GUESS)
b1.m <- bind_rows(b1.m.TR, b1.m.ED, b1.m.GUESS)
b2.m <- bind_rows(b2.m.TR, b2.m.ED, b2.m.GUESS)
b3.m <- bind_rows(b3.m.TR, b3.m.ED, b3.m.GUESS)
b4.m <- bind_rows(b4.m.TR, b4.m.ED, b4.m.GUESS)
b5.m <- bind_rows(b5.m.TR, b5.m.ED, b5.m.GUESS)
b6.m <- bind_rows(b6.m.TR, b6.m.ED, b6.m.GUESS)


b1.sum <- b1.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

b1.sum$variable <- factor(b1.sum$variable, levels = c( "Past",  "Modern"))

b2.sum <- b2.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past",  "Modern"))


b3.sum <- b3.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past",  "Modern"))


b4.sum <- b4.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b4.sum$variable <- factor(b4.sum$variable, levels = c( "Past",  "Modern"))

b5.sum <- b5.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))
b5.sum$variable <- factor(b5.sum$variable, levels = c( "Past",  "Modern"))

b6.sum <- b6.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

b6.sum$variable <- factor(b6.sum$variable, levels = c( "Past",  "Modern"))


# now plot dotplots:

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0.5, alpha = 0.8), size = 5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size = 5)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Precipitation \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.25)

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Temperature \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.25)

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_point(position=position_dodge(width=0.5), size =  10)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Precipitation \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.25)

b2.dot <- ggplot(data.frame(b2.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Temperature \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(-0.155,0)

b3.dot <- ggplot(data.frame(b3.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -1 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

b4.dot <- ggplot(data.frame(b4.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -2 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

b5.dot <- ggplot(data.frame(b5.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -3 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

b6.dot <- ggplot(data.frame(b6.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Lag -4 effect") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)

legend <- get_legend(b1.dot)

png(height = 14, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/all_params_dotplot.png")
plot_grid( b1.dot+theme(legend.position = "none"), 
                     b2.dot+theme(legend.position = "none"), 
                     b3.dot+theme(legend.position = "none"), 
                     b4.dot+theme(legend.position = "none"), 
                     b5.dot+theme(legend.position = "none"),
                     b6.dot+theme(legend.position = "none"), ncol = 2, align = "hv", rel_heights = c(1,1,1,1,1,1))
dev.off()

png(height = 20, width = 7, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/all_params_dotplot_vertical.png")
plot_grid( b1.dot+theme(legend.position = "none", plot.margin = unit(c(-1, 0, 0, 0), "cm")), 
           b2.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b3.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b4.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b5.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")),
           b6.dot+theme(legend.position = "none"), ncol = 1, align = "hv",axis = "tb", rel_heights = c(1,1,1,1,1,1))
dev.off()

png(height = 8, width = 20, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/all_params_dotplot_horizontal.png")
plot_grid( b1.dot+theme(legend.position = "none", plot.margin = unit(c(-1, 0, 0, 0), "cm")), 
           b2.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b3.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b4.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")), 
           b5.dot+theme(legend.position = "none",plot.margin = unit(c(0, 0, 0, 0), "cm")),
           b6.dot+theme(legend.position = "none"), ncol = 3, align = "hv",axis = "tb", rel_heights = c(1,1,1,1,1,1))
dev.off()


# calculate mean differences between overall predicted tree growth under the same climate:
precip.range <- c(-2.4571, 0.1661, 3.014)
tmax.range <- c(-2.4571, 0.1661, 3.014)
rel_gwbi_1<- c(0.05)
rel_gwbi_2<- c(0.05)
rel_gwbi_3<- c(0.05)
rel_gwbi_4<- c(0.05)
period_cd <- unique(test.ED$period_cd)
model <- c("ED2", "LPJ-GUESS", "Tree Rings")
meanMAP.sim.all <- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model)
colnames(meanMAP.sim.all) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model")

head(samp.ED.period)
head(samp.GUESS.period)
head(samp.rwi.period)

int.mcmc <- as.mcmc(samp.ED.period)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

meanMAP.sim <- meanMAP.sim.all[meanMAP.sim.all$model %in% "ED2",]
meanMAP.sim<- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model = "ED2", site.num = 1:220)
colnames(meanMAP.sim) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model", "site.num")
meanMAP.sim$site.num <- as.character(meanMAP.sim$site.num)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$tmax)), nrow = nrow(int.mcmc.dat))
#meanMAP.sim.all <- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model)
#colnames(meanMAP.sim.all) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model")

#test <- a1.m %>% group_by(num, variable) %>% spread(value, model)

# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:

for(i in 1:length(meanMAP.sim$tmax)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("alpha.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,]$Precip+    
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"tmax"] + 
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_1"]  + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_2"] +
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_3"] + 
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"period"], ".")] * (meanMAP.sim[i,"gwbi_4"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

full.pred.ED <- full.pred

# make predictions for GUESS:

int.mcmc <- as.mcmc(samp.GUESS.period)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

meanMAP.sim <- meanMAP.sim.all[meanMAP.sim.all$model %in% "LPJ-GUESS",]
meanMAP.sim<- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model = "LPJ-GUESS", site.num = 1:220)
colnames(meanMAP.sim) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model", "site.num")
meanMAP.sim$site.num <- as.character(meanMAP.sim$site.num)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$tmax)), nrow = nrow(int.mcmc.dat))

# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:

for(i in 1:length(meanMAP.sim$tmax)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("alpha.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,]$Precip+    
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"tmax"] + 
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_1"]  + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_2"] +
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_3"] + 
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"period"], ".")] * (meanMAP.sim[i,"gwbi_4"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

full.pred.GUESS <- full.pred


# make predictions for RWI:


int.mcmc <- as.mcmc(samp.rwi.period)
int.mcmc.mat <- as.matrix(int.mcmc)
int.mcmc.dat <- data.frame(int.mcmc.mat)

meanMAP.sim <- meanMAP.sim.all[meanMAP.sim.all$model %in% "Tree Rings",]
meanMAP.sim<- expand.grid(precip.range, tmax.range, rel_gwbi_1, rel_gwbi_2, rel_gwbi_3, rel_gwbi_4, period_cd, model = "Tree Rings", site.num = 1:16)
colnames(meanMAP.sim) <- c("Precip", "tmax", "gwbi_1", "gwbi_2", "gwbi_3", "gwbi_4", "period", "model", "site.num")
meanMAP.sim$site.num <- as.character(meanMAP.sim$site.num)

int.1 <- matrix(rep(NA, nrow(int.mcmc.dat)*length(meanMAP.sim$tmax)), nrow = nrow(int.mcmc.dat))

# use betas to generate pp given a value for site, structure, dbh, rwi1, rwi2, and varying T and MAP:

for(i in 1:length(meanMAP.sim$tmax)){
  # for struct.cohort == 1
  int.1[,i] <- int.mcmc.dat[,paste0("alpha.", meanMAP.sim[i,"site.num"], ".")]+
    int.mcmc.dat[,paste0("beta1.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,]$Precip+    
    int.mcmc.dat[,paste0("beta2.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"tmax"] + 
    int.mcmc.dat[,paste0("beta3.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_1"]  + 
    int.mcmc.dat[,paste0("beta4.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_2"] +
    int.mcmc.dat[,paste0("beta5.", meanMAP.sim[i,"period"], ".")]*meanMAP.sim[i,"gwbi_3"] + 
    int.mcmc.dat[,paste0("beta6.", meanMAP.sim[i,"period"], ".")] * (meanMAP.sim[i,"gwbi_4"])
  
  
}


# columns are the different degree-site scenario combinations
meanMAP.sim$idval <- 1:length(meanMAP.sim$site)
# rows are the mcmc values
colnames(int.1) <- 1:length(meanMAP.sim$site)
test.m <- melt(int.1)
colnames(test.m) <- c("MCMC", "idval", "Ypred")
full.pred <- left_join(test.m, meanMAP.sim, by = "idval")
full.pred$RWI.pred <- full.pred$Ypred

full.pred.TR <- full.pred

full.pred.all <- rbind(full.pred.ED, full.pred.GUESS, full.pred.TR)


allgrowth.summary <- full.pred.all %>% group_by(model,period) %>% dplyr::summarise(mean.pred = mean(exp(RWI.pred), na.rm =TRUE),
                                                                             ci.low.pred=quantile(exp(RWI.pred), 0.025, na.rm =TRUE),
                                                                             ci.high.pred=quantile(exp(RWI.pred), 0.975, na.rm =TRUE))


ggplot(allgrowth.summary, aes(x=model,y= mean.pred, fill = period))+geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = ci.low.pred, ymax = ci.high.pred), position = "dodge")

allgrowth.summary.byclim <- full.pred.all %>% group_by(model,period, tmax, Precip) %>% dplyr::summarise(mean.pred = mean(exp(RWI.pred), na.rm =TRUE),
                                                                                   ci.low.pred=quantile(exp(RWI.pred), 0.025, na.rm =TRUE),
                                                                                   ci.high.pred=quantile(exp(RWI.pred), 0.975, na.rm =TRUE))



allgrowth.summary.byclim
lowprecip<- allgrowth.summary.byclim[allgrowth.summary.byclim$Precip <= 0,]

ggplot(lowprecip, aes(x=model,y= mean.pred, fill = period))+geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = ci.low.pred, ymax = ci.high.pred), position = "dodge")+facet_wrap(~tmax)

allgrowth.summary.byclim$time<- ifelse(allgrowth.summary.byclim$period == 2, "1950-2011", "1895-1950")


png(height = 5, width = 7, units = "in", res = 300, "outputs/gwbi_model/growth_model_comparison_all_models.png")
ggplot(allgrowth.summary.byclim[allgrowth.summary.byclim$Precip == 0.1661 & allgrowth.summary.byclim$tmax == 0.1661,], aes(x=model,y= mean.pred, fill = time))+geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = ci.low.pred, ymax = ci.high.pred), position = position_dodge(width = 0.9), width = 0.3)+theme_bw(base_size = 20)+theme(panel.grid = element_blank())+ylab("Mean predicted Growth")+xlab("")+scale_fill_manual(values = c("blue", "red"), name = "Time Period")
dev.off()

# calculate mean differences between parameters:

b1.class <- b1.m %>% group_by(num, model) %>% spread( key = variable, value = value)
b1.class$pct_change <- ((b1.class$Modern - b1.class$Past))

beta1.diff <- b1.class %>% group_by(model) %>% summarise(mean = mean(pct_change),
                                           ci.low = quantile(pct_change, 0.025), 
                                           ci.high = quantile(pct_change, 0.975))

# plot average change in drought sensitivity
pct.drought.change <- ggplot(beta1.diff, aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+scale_fill_manual(values = c('#1b9e77',
                                                                                                                                                                                                                                                         '#d95f02',
                                                                                                                                                                                                                                                         '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ylab("drought sensitvity change \n between Modern and Past")

png(height = 8, width = 10, units = "in", res = 300, "outputs/gwbi_model/Lag4_cohort_re_clim/pct_change_drought_senstivity_updated.png")
pct.drought.change
dev.off()


# calculate pct temperature change
b2.class <- b2.m %>% group_by(num, model) %>% spread( key = variable, value = value)
b2.class$pct_change <- ((b2.class$Modern - b2.class$Past))

beta2.diff <- b2.class %>% group_by(model) %>% summarise(mean = mean(pct_change),
                                                         ci.low = quantile(pct_change, 0.025), 
                                                         ci.high = quantile(pct_change, 0.975))

# plot average change in drought sensitivity
pct.temp.change <- ggplot(beta2.diff, aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+scale_fill_manual(values = c('#1b9e77',
                                                                                                                                                                                                                                                          '#d95f02',
                                                                                                                                                                                                                                                          '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ylab("drought sensitvity change \n between Modern and Past")


#---------------WUE response to climate in models and in data-----------------
mod <- lm(IWUE ~ gwbi + Precip.scaled + Temp.jja.scaled  + Site, data = train.GUESS)
summary(mod)




mod <- lm(rel.IWUE ~ Precip.scaled + Temp.jja.scaled+ CO2+ Site, data = train.GUESS)
summary(mod)

modED <- lm(IWUE ~  Precip.scaled + Temp.jja.scaled + CO2 + Site, data = train.ED[!is.infinite(train.ED$IWUE),])
summary(modED)

ggplot(train.ED[train.ED$WUEt <= 100,], aes(Year, IWUE))+geom_point()
ggplot(train.GUESS, aes(Year, IWUE))+geom_point()


# estimate mean iWUE by cohort:

DiffIWUE <- train.ED %>% group_by(period) %>% summarise(IWUE = mean(IWUE, na.rm = TRUE),
                                                        IWUEt = mean(WUEt, na.rm = TRUE))

DiffIWUE.GUESS <- train.GUESS %>% group_by(period) %>% summarise(IWUE = mean(IWUE, na.rm = TRUE),
                                                                 IWUEt = mean(WUEt, na.rm = TRUE))


# read in the TRee ring iWUE data:

d13 <- read.csv("/Users/kah/Documents/TreeRings/outputs/stable_isotopes/merged_d13_growth.csv")
d13 <- d13[!is.na(d13$DBH),]

full.iso <- merge(ghcn.clean, d13[,c("site", "ID", "year","Cor.d13C.suess", "iWUE")], by = c("site", "ID", "year"), all.x = TRUE)

subset.iso <- full.iso[!is.na(full.iso$iWUE) & !full.iso$site %in% "BON",] # bon isotope data still needs QAQC

DiffIWUE.TR <-subset.iso %>% group_by(ageclass) %>% summarise(iWUE = mean(iWUE, na.rm = TRUE))

CO2.df <- train.GUESS %>% group_by(Year)%>% summarise(CO2 = mean (CO2),
                                            CO2.scaled = mean(CO2.scaled))
colnames(CO2.df) <- c("year", "CO2", "CO2.scaled")

subset.iso <- left_join(subset.iso, CO2.df, by = "year")

# estimate the % increase in iWUE in the models:
msk.iso <- caTools::sample.split( subset.iso, SplitRatio = 3/4, group = NULL )

train.iso <- subset.iso[msk.iso,]
test.iso <- subset.iso[!msk.iso,]


iWUE_intercept_only <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model for iWUE:
d13[i]   ~ dnorm(d13func[i], inv.var) # where Yi is already log transformed

# function g()
d13func[i] <- beta1[struct.cohort[i]]

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(s in 1:length(SF)){
beta1[s] ~ dnorm(mu_beta1, inv_beta1)
}



# use normal hyperpriors for each hyperparamters 

mu_beta1 ~ dnorm(0, 0.1)


inv_beta1   ~ dgamma(0.01, 0.01)
sigma_beta1 <- 1/sqrt(inv_beta1)



# Non-informative Prior for the inverse population variances

inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)

# Predict test data:
for(i in 1:np){
# process model for 13:
d13.p[i]   ~ dnorm(d13func.p[i], inv.var) # where Yi is already log transformed

# function g()
d13func.p[i] <- beta1[struct.cohort.p[i]]

}

}"

iWUE.TR <- jags.model(textConnection(iWUE_intercept_only), 
                       data = list(d13 = train.iso$iWUE, n=length(train.iso$iWUE), struct.cohort = as.numeric(train.iso$ageclass), SF = unique(train.iso$ageclass),
                                   struct.cohort.p =as.numeric(test.iso$ageclass), np = length(as.numeric(test.iso$ageclass)) ), n.chains = 3, n.adapt = 100)

update(iWUE.TR, 1000); # Burnin for 1000 samples to start, then go higher later


iWUE.mean.re <- coda.samples(iWUE.TR, 
                            variable.names=c("beta1", "mu_beta1", "d13.p"), 
                            n.chains = 3, n.iter = 20000, thin = 15)


samps       <- iWUE.mean.re [[1]]
saveRDS(samps, "outputs/iWUE_intercepts/rwi.samps")

alpha.samps  <- samps[,1:2]
iWUEpred.samps  <- samps[,3:(2+length(test.iso$iWUE))]




#------------------ plot predicted vs observed and assess model fit:
Yp.samps <- data.frame(iWUEpred.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.iso$iWUE

pred.obs <- summary(lm(colMeans(Yp.samps)~ test.iso$iWUE))

# this does a poor job representing d13 values by itself, but explains som of the variation
p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note better model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/iWUE_intercepts/rwipred_vs_obs.png")
p.o.plot
dev.off()


# run for ED model:

iWUE.ED <- jags.model(textConnection(iWUE_intercept_only), 
                      data = list(d13 = train.ED$IWUE, n=length(train.ED$IWUE), struct.cohort = as.numeric(train.ED$period_cd), SF = unique(train.ED$period_cd),
                                  struct.cohort.p =as.numeric(test.ED$period_cd), np = length(as.numeric(test.ED$period_cd)) ), n.chains = 3, n.adapt = 100)

update(iWUE.ED, 1000); # Burnin for 1000 samples to start, then go higher later


iWUE.mean.re.ED <- coda.samples(iWUE.ED, 
                             variable.names=c("beta1", "mu_beta1", "d13.p"), 
                             n.chains = 3, n.iter = 20000, thin = 15)

samps.ED       <- iWUE.mean.re.ED [[1]]
saveRDS(samps.ED, "outputs/iWUE_intercepts/ED.samps")

alpha.samps.ED  <- samps.ED[,1:2]
iWUEpred.samps.ED  <- samps.ED[,3:(2+length(test.ED$iWUE))]


# run for GUESS model:
train.GUESS.iso <- train.GUESS[!is.na(train.GUESS$IWUE),]
test.GUESS.iso <- test.GUESS[!is.na(test.GUESS$IWUE),]

iWUE.GUESS <- jags.model(textConnection(iWUE_intercept_only), 
                      data = list(d13 = train.GUESS.iso$IWUE, n=length(train.GUESS.iso$IWUE), struct.cohort = as.numeric(train.GUESS.iso$period_cd), SF = unique(train.GUESS.iso$period_cd),
                                  struct.cohort.p =as.numeric(test.GUESS.iso$period_cd), np = length(as.numeric(test.GUESS.iso$period_cd)) ), n.chains = 3, n.adapt = 100)

update(iWUE.GUESS, 1000); # Burnin for 1000 samples to start, then go higher later


iWUE.mean.re.GUESS <- coda.samples(iWUE.GUESS, 
                                variable.names=c("beta1", "mu_beta1", "d13.p"), 
                                n.chains = 3, n.iter = 20000, thin = 15)

samps.GUESS       <- iWUE.mean.re.GUESS [[1]]
saveRDS(samps.GUESS, "outputs/iWUE_intercepts/GUESS.samps")

alpha.samps.GUESS  <- samps.GUESS[,1:2]
iWUEpred.samps.GUESS  <- samps.GUESS[,3:(2+length(test.GUESS.iso$iWUE))]


# calculate avgs % change between all the models:
alpha.samps <- data.frame(alpha.samps)
alpha.samps$pct_change <- ((alpha.samps[,1]-alpha.samps[,2])/alpha.samps[,2])*100
alpha.samps$model <- "Tree Rings"

alpha.samps.ED <- data.frame(alpha.samps.ED)
alpha.samps.ED$pct_change <- ((alpha.samps.ED[,1]-alpha.samps.ED[,2])/alpha.samps.ED[,2])*100
alpha.samps.ED$model <- "ED2"

alpha.samps.GUESS <- data.frame(alpha.samps.GUESS)
alpha.samps.GUESS$pct_change <- ((alpha.samps.GUESS[,1]-alpha.samps.GUESS[,2])/alpha.samps.GUESS[,2])*100
alpha.samps.GUESS$model <- "LPJ-GUESS"


iWUE.diffs <- bind_rows(alpha.samps[,c("model", "pct_change")],alpha.samps.ED[,c("model", "pct_change")],alpha.samps.GUESS[,c("model", "pct_change")])

iWUE.summary <- iWUE.diffs %>% group_by(model) %>% summarise(iWUEinc = mean(pct_change), 
                                                             ci.low = quantile(pct_change, 0.025),
                                                             ci.high = quantile(pct_change, 0.975))


pct.IWUE.inc <- ggplot(iWUE.summary, aes( x=model, y = iWUEinc, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+scale_fill_manual(values = c('#1b9e77',
  '#d95f02',
  '#7570b3'))+
  ylim(0,20)+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+ylab("% increase in iWUE \n between Modern and Past")

png(height = 8, width = 10, units = "in", res = 300, "outputs/iWUE_intercepts/pct_increase_iWUE.png")
pct.IWUE.inc
dev.off()

#----------------What is relative iWUE sensitive to?-----------------------------
ed.wue <- lm(rel.IWUE ~  Precip.scaled + rel.gwbi + Temp.jja.scaled + period, data = train.ED[train.ED$rel.IWUE <=50,])
summary(ed.wue)

guess.wue <- lm(rel.IWUE ~  Precip.scaled + rel.gwbi + Temp.jja.scaled + CO2 + period, data = train.GUESS)
summary(guess.wue)


# relativise tree ring data:

tr.wue <- lm(iWUE ~  MAP.scaled + T.scaled + ageclass+CO2 , data = train.iso)
summary(tr.wue)


# need to relativise  iWUE for both GUESS, ED and Tree ring derived iWUE:
mean.IWUE.tr <- train.iso %>% group_by(site) %>% summarise(mean.iWUE = mean(iWUE, na.rm =TRUE))
mean.IWUE.tr.test <- train.iso %>% group_by(site) %>% summarise(mean.iWUE = mean(iWUE, na.rm =TRUE))


mean.IWUE.tr <- left_join(train.iso, mean.IWUE.tr, by = "site")

mean.IWUE.tr$rel.IWUE <- mean.IWUE.tr$iWUE - mean.IWUE.tr$mean.iWUE


mean.IWUE.tr.test <- left_join(test.iso, mean.IWUE.tr.test, by = "site")
mean.IWUE.tr.test$rel.IWUE <- mean.IWUE.tr.test$iWUE - mean.IWUE.tr.test$mean.iWUE

ggplot(mean.IWUE.tr.test, aes(year, rel.IWUE, color = site))+geom_point()+theme(legend.position = "none")


train.iso <- left_join(train.iso, mean.IWUE.tr[,c("site", "year", "rel.IWUE")], by = c("year", "site"))
test.iso <- left_join(test.iso, mean.IWUE.tr.test[,c("site", "year", "rel.IWUE")], by = c("year", "site"))


tr.wue <- lm(rel.IWUE ~  MAP.scaled + T.scaled + ageclass+CO2 + mean.diff + ageclass + DBH, data = train.iso)
summary(tr.wue)


ggplot(train.iso, aes( CO2,rel.IWUE))+geom_point()
ggplot(train.ED[train.ED$IWUE <= 50,], aes(CO2 ,rel.IWUE))+geom_point()
ggplot(train.GUESS, aes(Precip.scaled, rel.IWUE))+geom_point()


# ---------------IWUE responses to Precipiation, temperature, CO2 in the models-----------------
#--------ED model IWUE responses:

IWUE_climate_site_period <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is iWUE

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] + beta3[period[i]]*CO2[i] 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)
beta3[c] ~ dnorm(mu_beta3, inv_beta3)

}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)
mu_beta3 ~ dunif(-2, 2)


inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)
inv_beta3   ~ dgamma(0.001, 0.001)
sigma_beta3 <- 1/sqrt(inv_beta3)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*CO2.p[i] 

}

# Probe
for(i in 1:nprobe){
#process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

#function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] + beta3[period.probe[i]]*CO2.probe[i]

}

}"



DIprobe <- round(seq(range(train.ED$Precip.scaled)[1], range(train.ED$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.ED$Temp.jja.scaled)[1], range(train.ED$Temp.jja.scaled)[2], by = 2), 3)
CO2probe <- round(seq(range(train.ED$CO2.scaled)[1], range(train.ED$CO2.scaled)[2], by = 2), 2)

# expand into full probe
probe.ED <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                         CO2.scaled = CO2probe,
                         site_num = unique(train.ED$site_code),struct.cohort.code= 1:2)




IWUE.ED.model <- jags.model(textConnection(IWUE_climate_site_period), 
                                  data = list(Y = train.ED$rel.IWUE, n=length(train.ED$rel.IWUE), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, CO2 = train.ED$CO2.scaled,
                                              period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_num),  C = unique(train.ED$period_cd), sites = train.ED$site_code, np=length(test.ED$period_cd), 
                                              sites.p = train.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, CO2.p = test.ED$CO2.scaled,
                                              period.p = as.numeric(test.ED$period_cd),
                                              
                                              nprobe=length(probe.ED$struct.cohort.code), 
                                              sites.probe = probe.ED$site, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, CO2.probe = probe.ED$CO2.scaled,
                                              period.probe = as.numeric(probe.ED$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(IWUE.ED.model, 1000); # Burnin for 1000 samples to start, then go higher later

samp.IWUE.ED <- coda.samples(IWUE.ED.model, 
                                variable.names=c("alpha", "beta1", "beta2","beta3" ), 
                                n.chains = 3, n.iter=10000, thin = 1)

samp.IWUE.ED.ypred  <- coda.samples(IWUE.ED.model, 
                               variable.names=c("Ypred" ), 
                               n.chains = 3, n.iter=5000, thin = 1)

samp.IWUE.ED.yprobe <- coda.samples(IWUE.ED.model, 
                                variable.names=c("Yprobe" ), 
                                n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.IWUE.ED, "outputs/iWUE_climate_CO2/ED_parameter_samps.rds")
saveRDS(samp.IWUE.ED.ypred, "outputs/iWUE_climate_CO2/ED_Ypred_samps.rds")
saveRDS(samp.IWUE.ED.yprobe, "outputs/iWUE_climate_CO2/ED_Yprobe_samps.rds")
samp.IWUE.ED <- readRDS( "outputs/iWUE_climate_CO2/ED_parameter_samps.rds")
samp.IWUE.ED.yprobe<- readRDS( "outputs/iWUE_climate_CO2/ED_Yprobe_samps.rds")
samp.IWUE.ED.ypred<- readRDS( "outputs/iWUE_climate_CO2/ED_Ypred_samps.rds")

saveRDS(test.ED, "outputs/iWUE_climate_CO2/ED_testdata.rds")
saveRDS(train.ED, "outputs/iWUE_climate_CO2/ED_traindata.rds")
test.ED <- readRDS( "outputs/iWUE_climate_CO2/ED_testdata.rds")
# check for convergence:
summary(samp.IWUE.ED)
gelman.diag(samp.IWUE.ED)
#acfplot(samp.IWUE.ED)

#Extract the samples for each parameter

samps       <- samp.IWUE.ED[[1]]
Yp.samps    <- samp.IWUE.ED.ypred [[1]]
Yprobe.samps <- samp.IWUE.ED.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site))]
beta1.samps  <- samps[,(length(unique(test.ED$site))+1):(length(unique(test.ED$site))+2)]
beta2.samps  <- samps[,(length(unique(test.ED$site))+3):(length(unique(test.ED$site))+4)]
beta3.samps  <- samps[,(length(unique(test.ED$site))+5):(length(unique(test.ED$site))+6)]


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$rel.IWUE

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$rel.IWUE))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/iWUE_climate_CO2/ED_pred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$rel.IWUE)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$rel.IWUE)

#-----------run the model for LPJ-GUESS------------

DIprobe <- round(seq(range(train.GUESS$Precip.scaled)[1], range(train.GUESS$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.GUESS$Temp.jja.scaled)[1], range(train.GUESS$Temp.jja.scaled)[2], by = 2), 3)
CO2probe <- round(seq(range(train.GUESS$CO2.scaled, na.rm = TRUE)[1], range(train.GUESS$CO2.scaled, na.rm = TRUE)[2], by = 2), 2)

# expand into full probe

train.GUESS <- train.GUESS[!is.na(train.GUESS$rel.IWUE) & !is.na(train.GUESS$CO2.scaled),]
test.GUESS <- test.GUESS[!is.na(test.GUESS$rel.IWUE) & !is.na(test.GUESS$CO2.scaled),]
# this removes some of the GUESS sites, so 
site.code.conversion<- data.frame(site_code = unique(test.GUESS$site_code), 
                                  site_code_new = 1:length(unique(test.GUESS$site_code)))

train.GUESS.wue <- left_join(train.GUESS, site.code.conversion, by = "site_code")
test.GUESS.wue <- left_join(test.GUESS, site.code.conversion, by = "site_code")


probe.GUESS <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                           CO2.scaled = CO2probe,
                           site_num = 1:length(unique(train.GUESS$site_code)),struct.cohort.code= 1:2)


IWUE.GUESS.model <- jags.model(textConnection(IWUE_climate_site_period), 
                            data = list(Y = train.GUESS.wue$rel.IWUE, n=length(train.GUESS.wue$rel.IWUE), Precip.scaled = train.GUESS.wue$Precip.scaled, Temp.jja.scaled = train.GUESS.wue$Temp.jja.scaled, CO2 = train.GUESS.wue$CO2.scaled,
                                        period = as.numeric(train.GUESS.wue$period_cd), S = unique(train.GUESS.wue$site_code_new),  C = unique(train.GUESS.wue$period_cd), sites = train.GUESS.wue$site_code_new, np=length(test.GUESS.wue$period_cd), 
                                        sites.p = test.GUESS.wue$site_code_new, Precip.scaled.p = test.GUESS.wue$Precip.scaled, Temp.jja.scaled.p = test.GUESS.wue$Temp.jja.scaled, CO2.p = test.GUESS.wue$CO2.scaled,
                                        period.p = as.numeric(test.GUESS.wue$period_cd),
                                        
                                        nprobe=length(probe.GUESS$struct.cohort.code), 
                                        sites.probe = probe.GUESS$site_num, Precip.scaled.probe = probe.GUESS$DI.scaled, Temp.jja.scaled.probe = probe.GUESS$T.scaled, CO2.probe = probe.GUESS$CO2.scaled,
                                        period.probe = as.numeric(probe.GUESS$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(IWUE.GUESS.model, 1000); # Burnin for 1000 samples to start, then go higher later

samp.IWUE.GUESS <- coda.samples(IWUE.GUESS.model, 
                             variable.names=c("alpha", "beta1", "beta2","beta3" ), 
                             n.chains = 3, n.iter=10000, thin = 1)

samp.IWUE.GUESS.ypred  <- coda.samples(IWUE.GUESS.model, 
                                    variable.names=c("Ypred" ), 
                                    n.chains = 3, n.iter=5000, thin = 1)

samp.IWUE.GUESS.yprobe <- coda.samples(IWUE.GUESS.model, 
                                    variable.names=c("Yprobe" ), 
                                    n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.IWUE.GUESS, "outputs/iWUE_climate_CO2/GUESS_parameter_samps.rds")
saveRDS(samp.IWUE.GUESS.ypred, "outputs/iWUE_climate_CO2/GUESS_Ypred_samps.rds")
saveRDS(samp.IWUE.GUESS.yprobe, "outputs/iWUE_climate_CO2/GUESS_Yprobe_samps.rds")
samp.IWUE.GUESS.yprobe<- readRDS( "outputs/iWUE_climate_CO2/GUESS_Yprobe_samps.rds")

saveRDS(test.GUESS, "outputs/iWUE_climate_CO2/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/iWUE_climate_CO2/GUESS_traindata.rds")

# check for convergence:
summary(samp.IWUE.GUESS)
gelman.diag(samp.IWUE.GUESS)
acfplot(samp.IWUE.GUESS)

#Extract the samples for each parameter

samps       <- samp.IWUE.GUESS[[1]]
Yp.samps    <- samp.IWUE.GUESS.ypred [[1]]
Yprobe.samps <- samp.IWUE.GUESS.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site))+1):(length(unique(test.GUESS$site))+2)]
beta2.samps  <- samps[,(length(unique(test.GUESS$site))+3):(length(unique(test.GUESS$site))+4)]
beta3.samps  <- samps[,(length(unique(test.GUESS$site))+5):(length(unique(test.GUESS$site))+6)]


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.GUESS$rel.IWUE

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.GUESS$rel.IWUE))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/iWUE_climate_CO2/GUESS_pred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$rel.IWUE)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$rel.IWUE)


#-----------------run the model for tree ring derived iWUE----------------------------------

DIprobe <- round(seq(range(train.iso$MAP.scaled)[1], range(train.iso$MAP.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.iso$T.scaled)[1], range(train.iso$T.scaled)[2], by = 2), 3)
CO2probe <- round(seq(range(train.iso$CO2.scaled, na.rm = TRUE)[1], range(train.iso$CO2.scaled, na.rm = TRUE)[2], by = 2), 2)

probe.iso <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                         CO2.scaled = CO2probe,
                         site_num = 1:3,struct.cohort.code= 1:2)

# add dummy site codes for train.iso
site_code_dummy <- data.frame(site = unique(train.iso$site),
                              site_code = 1:length(unique(train.iso$site)))
train.iso.wue <- left_join(train.iso, site_code_dummy, by = "site")
test.iso.wue <- left_join(test.iso, site_code_dummy, by = "site")

test.iso.wue <- test.iso.wue[!is.na(test.iso.wue$CO2.scaled),]
train.iso.wue <- train.iso.wue[!is.na(train.iso.wue$CO2.scaled),]

IWUE.iso.model <- jags.model(textConnection(IWUE_climate_site_period), 
                               data = list(Y = train.iso.wue$rel.IWUE/10, n=length(train.iso.wue$rel.IWUE), Precip.scaled = train.iso.wue$MAP.scaled, Temp.jja.scaled = train.iso.wue$T.scaled, CO2 = train.iso.wue$CO2.scaled,
                                           period = as.numeric(train.iso.wue$ageclass), S = unique(train.iso.wue$site_code),  C = unique(train.iso.wue$ageclass), sites = as.numeric(train.iso.wue$site_code), np=length(test.iso.wue$ageclass), 
                                           sites.p = test.iso.wue$site_code, Precip.scaled.p = test.iso.wue$MAP.scaled, Temp.jja.scaled.p = test.iso.wue$T.scaled, CO2.p = test.iso.wue$CO2.scaled,
                                           period.p = as.numeric(test.iso.wue$ageclass),
                                           
                                           nprobe = length(probe.iso$struct.cohort.code), 
                                           sites.probe = as.numeric(probe.iso$site_num), Precip.scaled.probe = probe.iso$DI.scaled, Temp.jja.scaled.probe = probe.iso$T.scaled, CO2.probe = probe.iso$CO2.scaled,
                                           period.probe = as.numeric(probe.iso$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(IWUE.iso.model, 1000); # Burnin for 1000 samples to start, then go higher later

samp.IWUE.iso <- coda.samples(IWUE.iso.model, 
                                variable.names=c("alpha", "beta1", "beta2","beta3" ), 
                                n.chains = 3, n.iter=10000, thin = 10)

samp.IWUE.iso.ypred  <- coda.samples(IWUE.iso.model, 
                                       variable.names=c("Ypred" ), 
                                       n.chains = 3, n.iter=8000, thin = 10)

samp.IWUE.iso.yprobe <- coda.samples(IWUE.iso.model, 
                                       variable.names=c("Yprobe" ), 
                                       n.chains = 3, n.iter=8000, thin = 10)

saveRDS(samp.IWUE.iso, "outputs/iWUE_climate_CO2/iso_parameter_samps.rds")
saveRDS(samp.IWUE.iso.ypred, "outputs/iWUE_climate_CO2/iso_Ypred_samps.rds")
saveRDS(samp.IWUE.iso.yprobe, "outputs/iWUE_climate_CO2/iso_Yprobe_samps.rds")
samp.IWUE.iso.yprobe<- readRDS( "outputs/iWUE_climate_CO2/iso_Yprobe_samps.rds")

saveRDS(test.iso, "outputs/iWUE_climate_CO2/iso_testdata.rds")
saveRDS(train.iso, "outputs/iWUE_climate_CO2/iso_traindata.rds")

# check for convergence:
summary(samp.IWUE.iso)
gelman.diag(samp.IWUE.iso)
acfplot(samp.IWUE.iso)

#Extract the samples for each parameter

samps       <- samp.IWUE.iso[[1]]
Yp.samps    <- samp.IWUE.iso.ypred [[1]]
Yprobe.samps <- samp.IWUE.iso.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.iso$site))]
beta1.samps  <- samps[,(length(unique(test.iso$site))+1):(length(unique(test.iso$site))+2)]
beta2.samps  <- samps[,(length(unique(test.iso$site))+3):(length(unique(test.iso$site))+4)]
beta3.samps  <- samps[,(length(unique(test.iso$site))+5):(length(unique(test.iso$site))+6)]


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value)*10,
                                                               ci.hi = quantile(value,0.975)*10,
                                                               ci.lo = quantile(value,0.025)*10)
Yp.summary$Observed <- test.iso$rel.IWUE

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.iso$rel.IWUE))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/iWUE_climate_CO2/iso_pred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.iso$rel.IWUE)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.iso$rel.IWUE)


#----------------- pull in all the rel.WUE model outputs and summarise ----------------------------------
samp.rwi.period <- readRDS( "outputs/iWUE_climate_CO2/iso_parameter_samps.rds")
samp.GUESS.period <- readRDS( "outputs/iWUE_climate_CO2/GUESS_parameter_samps.rds")
samp.ED.period <- readRDS( "outputs/iWUE_climate_CO2/ED_parameter_samps.rds")

test.iso.wue <- readRDS( "outputs/iWUE_climate_CO2/iso_testdata.rds")
test.GUESS <- readRDS( "outputs/iWUE_climate_CO2/GUESS_testdata.rds")
test.ED <- readRDS( "outputs/iWUE_climate_CO2/ED_testdata.rds")

samp.rwi.period <- samp.rwi.period[[1]]
samp.GUESS.period <- samp.GUESS.period[[1]]
samp.ED.period <- samp.ED.period[[1]]

# get parameter estimates from TR model:
alpha.samps <- samp.rwi.period[,1:length(unique(test.iso.wue$site))]
beta1.samps  <- samp.rwi.period[,(length(unique(test.iso.wue$site))+1):(length(unique(test.iso.wue$site))+2)]
beta2.samps  <- samp.rwi.period[,(length(unique(test.iso.wue$site))+3):(length(unique(test.iso.wue$site))+4)]
beta3.samps  <- samp.rwi.period[,(length(unique(test.iso.wue$site))+5):(length(unique(test.iso.wue$site))+6)]

a <- data.frame(alpha.samps)
colnames(a) <- unique(train.iso.wue$site)
a$num <- rownames(a)
a.m.TR <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.TR, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <- unique(levels(train.iso.wue$ageclass)) # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.TR <- melt(b1, id.vars=c("num"))
b1.m.TR$model <- "Tree Rings"
b1.mplots <- ggplot(b1.m.TR, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b2 <- data.frame(beta2.samps)
colnames(b2) <-unique(levels(train.iso.wue$ageclass)) 
b2$num <- rownames(b2)
b2.m.TR <- melt(b2, id.vars=c("num"))
b2.m.TR$model <- "Tree Rings"
b2.mplots <- ggplot(b2.m.TR, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b3 <- data.frame(beta3.samps)
colnames(b3) <-unique(levels(train.iso.wue$ageclass)) 
b3$num <- rownames(b3)
b3.m.TR<- melt(b3, id.vars=c("num"))
b3.m.TR$model <- "Tree Rings"
b3.m.mplots <- ggplot(b3.m.TR, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")


# get parameter estimates from ED model:
alpha.sampsED <- samp.ED.period[,1:length(unique(test.ED$site_num))]
beta1.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]
beta3.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+5):(length(unique(test.ED$site_num))+6)]

a <- data.frame(alpha.sampsED)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m.ED <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.ED, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.ED$model <- "ED2"

b1 <- data.frame(beta1.sampsED)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.ED <- melt(b1, id.vars=c("num"))
b1.m.ED$model <- "ED2"

b2 <- data.frame(beta2.sampsED)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.ED <- melt(b2, id.vars=c("num"))
b2.m.ED$model <- "ED2"

b3 <- data.frame(beta3.sampsED)
colnames(b3) <-c("Modern", "Past")
b3$num <- rownames(b3)
b3.m.ED<- melt(b3, id.vars=c("num"))
b3.m.ED$model <- "ED2"



# get parameter estimates from LPJ-GUESS model:
alpha.sampsGUESS <- samp.GUESS.period[,1:length(unique(test.GUESS$site_num))]
beta1.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]
beta3.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+5):(length(unique(test.GUESS$site_num))+6)]

a <- data.frame(alpha.sampsGUESS)
colnames(a) <- unique(train.GUESS$site)
a$num <- rownames(a)
a.m.GUESS <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.GUESS, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.GUESS$model <- "LPJ-GUESS"

b1 <- data.frame(beta1.sampsGUESS)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.GUESS <- melt(b1, id.vars=c("num"))
b1.m.GUESS$model <- "LPJ-GUESS"

b2 <- data.frame(beta2.sampsGUESS)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.GUESS <- melt(b2, id.vars=c("num"))
b2.m.GUESS$model <- "LPJ-GUESS"

b3 <- data.frame(beta3.sampsGUESS)
colnames(b3) <-c("Modern", "Past")
b3$num <- rownames(b3)
b3.m.GUESS<- melt(b3, id.vars=c("num"))
b3.m.GUESS$model <- "LPJ-GUESS"



# now combine all the beta1s together and make a modern past dotplot:
b1.m <- bind_rows(b1.m.TR, b1.m.ED, b1.m.GUESS)
b2.m <- bind_rows(b2.m.TR, b2.m.ED, b2.m.GUESS)
b3.m <- bind_rows(b3.m.TR, b3.m.ED, b3.m.GUESS)


b1.sum <- b1.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))

b1.sum$variable <- factor(b1.sum$variable, levels = c( "Past",  "Modern"))

b2.sum <- b2.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))
b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past",  "Modern"))


b3.sum <- b3.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))
b3.sum$variable <- factor(b3.sum$variable, levels = c( "Past",  "Modern"))



# now plot dotplots:

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size = 5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size = 10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Precipitation \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 35)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.75)

#ggplot(data.frame(b2.sum), aes(x = model, y = mean.val, color = variable), size =  5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
 # geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Temperature \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(-0.155,0)


b2.dot <- ggplot(data.frame(b2.sum), aes(x = model, y = mean.val, color = variable), size = 5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size = 5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size = 10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Temperature \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 35)+theme( axis.title.x = element_blank(), panel.grid = element_blank())

b3.dot <- ggplot(data.frame(b3.sum), aes(x = model, y = mean.val, color = variable), size = 2.5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size = 5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size = 10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+labs(y= expression(CO[2]~sensitivity)) + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 35)+theme( axis.title.x = element_blank(), panel.grid = element_blank())+ylim(0,0.7)


legend <- get_legend(b1.dot)

png(height = 12, width = 12, units = "in", res = 500, "outputs/iWUE_climate_CO2/all_params_dotplot.png")
plot_grid(plot_grid( b1.dot+theme(legend.position = "none"), 
                     b2.dot+theme(legend.position = "none"), 
                     b3.dot+theme(legend.position = "none"), 
                      ncol = 1, align = "hv"),legend, ncol = 2, rel_widths = c(1,0.25))
dev.off()

png(height = 6, width = 26.5, units = "in", res = 500, "outputs/iWUE_climate_CO2/all_params_dotplot_horizontal.png")
plot_grid( b1.dot+theme(legend.position = "none"), 
                     b2.dot+theme(legend.position = "none"), 
                     b3.dot+theme(legend.position = "none"), 
                     ncol = 3, align = "hv")
dev.off()

new.legend<- get_legend(b2.dot + theme(legend.position = "bottom"))

png(height = 6, width = 8, units = "in", res = 500, "outputs/iWUE_climate_CO2/legend.png")
plot_grid(new.legend)
dev.off()

# ------------------------plot posterior predictive distributions-----------
# ------------- read in both GUESS and ED and plot posteriors together to compare ---------
library(DMwR)
GUESS.probe <- readRDS("outputs/iWUE_climate_CO2/GUESS_Yprobe_samps.rds")
GUESS.probe <- data.frame(GUESS.probe[[1]]) 
Yp.m <- melt(GUESS.probe)
colnames(GUESS.probe) <- 1:length(GUESS.probe)
probe.m <- melt(GUESS.probe)
colnames(probe.m) <- c("num", "WUE_pred")

probe.GUESS$num <- 1:length(probe.GUESS[,1])

# summarize by cohort class only:
probe.GUESS$num <- as.factor(as.character(probe.GUESS$num))
full.p <- probe.GUESS

GUESS.probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(GUESS.probtest) <- c("num", "WUE_pred", "MAP_scaled", "JJA.T.scaled", "CO2.scaled", "site_num", "period")
saveRDS(GUESS.probtest, "outputs/iWUE_climate_CO2/GUESS_probtest.rds")


GUESS.probtest$model <- "LPJ-GUESS"
GUESS.probtest$Precip <- as.numeric(round(unscale(vals = GUESS.probtest$MAP_scaled, norm.data = GUESS.sort_lag.Precip.scaled))) 
GUESS.probtest$Temp <- as.numeric(round(unscale(vals = GUESS.probtest$JJA.T.scaled, norm.data = GUESS.sort_lag.Temp.jja.scaled))) 
GUESS.probtest$CO2 <- as.numeric(round(unscale(vals = GUESS.probtest$CO2.scaled, norm.data = GUESS.sort_lag.CO2.scaled))) 




ED.probe <- readRDS("outputs/iWUE_climate_CO2/ED_Yprobe_samps.rds")
ED.probe <- data.frame(ED.probe[[1]])
Yp.m <- melt(ED.probe)
colnames(ED.probe) <- 1:length(ED.probe)
probe.m <- melt(ED.probe)
colnames(probe.m) <- c("num", "WUE_pred")

probe.ED$num <- 1:length(probe.ED[,1])

# summarize by cohort class only:
probe.ED$num <- as.factor(as.character(probe.ED$num))
full.p <- probe.ED

ED.probtest <- dplyr::inner_join(probe.m, full.p, by=c("num"))
colnames(ED.probtest) <- c("num", "WUE_pred", "MAP_scaled", "JJA.T.scaled", "CO2.scaled", "site_num", "period")
saveRDS(ED.probtest, "outputs/iWUE_climate_CO2/ED_probtest.rds")


ED.probtest$model <- "ED2"
ED.probtest$Precip <- as.numeric(round(unscale(vals = ED.probtest$MAP_scaled, norm.data = ED.sort_lag.Precip.scaled))) 
ED.probtest$Temp <- as.numeric(round(unscale(vals = ED.probtest$JJA.T.scaled, norm.data = ED.sort_lag.Temp.jja.scaled))) 
ED.probtest$CO2 <- as.numeric(round(unscale(vals = ED.probtest$CO2.scaled, norm.data = ED.sort_lag.CO2.scaled))) 



RWI.probe <- readRDS("outputs/gwbi_model/RWI_probtest.rds")
RWI.probe$model <- "Tree Rings"
RWI.probe$Precip <- as.numeric(round(unscale(vals = probe.iso$DI.scaled, norm.data = full.ghcn.MAP.scaled))) 
RWI.probe$Temp <- as.numeric(round(unscale(vals = probe.iso$T.scaled, norm.data = full.ghcn.T.scaled))) 
RWI.probe$Temp <-  ((RWI.probe$Temp - 32) * (5 / 9)) 



#model.probe <- bind_rows(GUESS.probe, ED.probe)

model.probe.mod <- bind_rows(GUESS.probe, ED.probe)
model.probe <- bind_rows(model.probe.mod, RWI.probe)

# note 2 == Past and 1 == Modern:

cohort.summary.pr <- model.probe %>% group_by(period, Precip, model) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                          gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                          gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.summary.tm <- model.probe %>% group_by(period, Temp, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                 gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                 gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.summary.pr$period <- as.factor(cohort.summary.pr$period)
cohort.summary.tm$period <- as.factor(cohort.summary.tm$period)
cohort.summary.tm$ageclass <- ifelse(cohort.summary.tm$period == "1", "Modern", "Past")
cohort.summary.pr$ageclass <- ifelse(cohort.summary.pr$period == "1", "Modern", "Past")

# compare sensitivities of the models
ggplot(cohort.summary.pr, aes(Precip, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()

ggplot(cohort.summary.tm, aes(Temp, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()


precip.overall.sens <- ggplot(cohort.summary.pr, aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank())

temp.overall.sens <- ggplot(cohort.summary.tm, aes(Temp, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+labs(x = expression('Summer Temperature ('*~degree*C*')'),y = "Relative Growth")+theme(panel.grid = element_blank())

legend <- get_legend(precip.overall.sens)

png(height = 10, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/marginal_P_T_effects.png")
plot_grid(plot_grid( precip.overall.sens+theme(legend.position = "none"), 
                     temp.overall.sens+theme(legend.position = "none"), 
                     ncol = 1, align = "hv"),legend, ncol = 2, rel_widths = c(1,0.25))
dev.off()


# find the grid cells that are closest to the tree ring sites:


# read in the spatial points data for the Tree ring data:
TREERING_PALEON_GRID <- read.csv("/Users/kah/Documents/TreeRings/data/KH_Treering_sites_PALEON_model_grid.csv - KH_Treering_sites_PALEON_model_grid.csv-2.csv")

# find the closest grid cell:
load("Data/PalEON_siteInfo_all.RData")
TR.locs <- TREERING_PALEON_GRID[TREERING_PALEON_GRID$Site.code %in% c( "AVO",  "BON",  "COR",  "ENG",  "GLA","GLL",  "GLL1", "GLL2", "GLL3", "GLL4", "HIC",  "MOU",  "PLE",  "PVC", 
                                                                       "STC",  "TOW",  "UNC"  ),]
TR.sites <- merge(paleon, TREERING_PALEON_GRID, by.x = "latlon", by.y = "latlon_PALEON")

#get map data for the Midwest:
states <- map_data("state")
states <- subset(states, ! region  %in% c("california", 'nevada','arizona','utah','oregon','washington','new mexico','colorado','montana','wyoming','idaho') )
coordinates(states)<-~long+lat
#class(states)
#proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
#states <- spTransform(states,CRSobj = '+init=epsg:4326')
mapdata <- data.frame(states)

cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")

# map of mean density:

png(height = 12, width = 18, units = 'in', res=500,"outputs/PALEON_MODEL_MAP.png")
ggplot(paleon, aes(lon, lat), fill = "forestgreen")+geom_raster()+coord_cartesian()+geom_point(data = TR.locs, aes(x = longitude, y = latitude), color = "red", fill = "red", shape = 24, size = 6)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal(xlim= c(-98,-64), ylim=c(35,49)) + theme_bw(base_size = 34)+theme(axis.title = element_blank(), panel.grid = element_blank())
dev.off()

library(geosphere)

# create distance matrix
mat <- distm( TR.locs[,c('longitude','latitude')],paleon[,c('lon','lat')], fun=distVincentyEllipsoid)

# assign the name to the point in list1 based on shortest distance in the matrix
TR.locs$lat_PALEON_closest <- paleon$lat[max.col(-mat)]
TR.locs$lon_PALEON_closest <- paleon$lon[max.col(-mat)]
TR.locs$paleon_gridlatlon <- paleon$latlon[max.col(-mat)]

ggplot()+geom_raster(data = paleon, aes(x = lon, y =lat))+coord_cartesian()+geom_point(data = TR.locs, aes(x = longitude, y = latitude, color = Site.code))+geom_raster(data = TR.locs, aes(x = lon_PALEON_closest, y =lat_PALEON_closest), fill = "red")

paleon.sites <- paleon[paleon$latlon %in% TR.locs$paleon_gridlatlon,]

model.site.num <- paleon.sites$num
model.probe.subset <- model.probe.mod %>% filter(site_num %in% paleon.sites$num)
model.TR.sites <- bind_rows(model.probe.subset, RWI.probe)


# plot out only the grid cells where we have TR sites:
# note 2 == Past and 1 == Modern:

cohort.sites.pr <- model.TR.sites %>% group_by(period, Precip, model) %>% dplyr::summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                           gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                           gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.sites.tm <- model.TR.sites %>% group_by(period, Temp, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                  gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                  gwbi.high = quantile(gwbi_pred, 0.975)) 


# need to covert the periods to factors:
cohort.sites.pr$period <- as.factor(cohort.sites.pr$period)
cohort.sites.tm$period <- as.factor(cohort.sites.tm$period)
cohort.sites.tm$ageclass <- ifelse(cohort.sites.tm$period == "1", "Modern", "Past")
cohort.sites.pr$ageclass <- ifelse(cohort.sites.pr$period == "1", "Modern", "Past")

# compare sensitivities of the models
ggplot(cohort.sites.pr, aes(Precip, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.sites.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()

ggplot(cohort.sites.tm, aes(Temp, gwbi, color = model))+geom_line()+geom_ribbon(data = cohort.sites.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = model), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~ageclass, nrow = 2)+theme_bw()


ggplot(cohort.sites.pr, aes(Precip, gwbi, color = ageclass))+geom_line()+geom_ribbon(data = cohort.sites.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 3)+theme_bw(base_size = 12)+ylab("Relativized Woody Growth")+xlab("Annual Precipitation (mm)")

ggplot(cohort.sites.tm, aes(Temp, gwbi, color = ageclass))+geom_line()+geom_ribbon(data = cohort.sites.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 3)+theme_bw(base_size = 12)+ylab("Relativized Woody Growth")+xlab(expression("Temperature " ( degree*C)))

# plot out pretty plot for just grid cells closest to 
precip.sites.sens <- ggplot(cohort.sites.pr, aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.pr,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())

temp.sites.sens <- ggplot(cohort.sites.tm, aes(Temp, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.summary.tm,aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+labs(x = expression('Summer Temperature ('*~degree*C*')'),y = "Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())

legend <- get_legend(precip.sites.sens)

png(height = 10, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/marginal_P_T_effects_TR_sites_only.png")
plot_grid(plot_grid( precip.sites.sens+theme(legend.position = "none"), 
                     temp.sites.sens+theme(legend.position = "none"), 
                     ncol = 1, align = "hv"),legend, ncol = 2, rel_widths = c(1,0.25))
dev.off()



# now lets look at posteriors for different conditions:
cohort.sites.tm.pr <- model.TR.sites %>% group_by(period, Temp, Precip, model) %>% summarise(gwbi = mean(gwbi_pred, na.rm=TRUE), 
                                                                                             gwbi.low = quantile(gwbi_pred, 0.025), 
                                                                                             gwbi.high = quantile(gwbi_pred, 0.975)) 

cohort.sites.tm.pr$period <- as.factor(cohort.sites.tm.pr$period)
cohort.sites.tm.pr$ageclass <- ifelse(cohort.sites.tm.pr$period == "1", "Modern", "Past")
cohort.sites.tm.pr$Precip <- as.numeric(cohort.sites.tm.pr$Precip)

cool <- ggplot(cohort.sites.tm.pr[cohort.sites.tm.pr$Temp < 15,], aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp < 15,],aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())
warm <- ggplot(cohort.sites.tm.pr[cohort.sites.tm.pr$Temp > 21 & cohort.sites.tm.pr$Temp < 24,], aes(Precip, gwbi, color = ageclass))+geom_line()+scale_color_manual(values = c("Past" = "blue","Modern" = "red"))+geom_ribbon(data = cohort.sites.tm.pr[cohort.sites.tm.pr$Temp > 21 & cohort.sites.tm.pr$Temp < 24,],aes(ymin = gwbi.low, ymax = gwbi.high, fill = ageclass), alpha = 0.25, linetype = "dashed", colour = NA)+facet_wrap(~model, nrow = 1)+theme_bw(base_size = 35)+xlab("Annual Precipitation (mm)")+ylab("Relative Growth")+theme(panel.grid = element_blank(), legend.title = element_blank())

title1 <- ggdraw()+draw_label(expression('COOL SUMMERS (>15'*~degree*C*')'), size = 42, fontface = "bold")
title2 <- ggdraw()+draw_label(expression('HOT SUMMERS (>25'*~degree*C*')'), size = 42, fontface = "bold")

legend <- get_legend(cool)

png(height = 12.5, width = 14, units = "in", res = 500, "outputs/gwbi_model/Lag4_cohort_re_clim/P_effects_low_high_temp_TR_sites_only.png")
plot_grid(plot_grid( title1, 
                     cool+theme(legend.position = "none"), 
                     title2,
                     warm+theme(legend.position = "none"), 
                     ncol=1, rel_heights=c(0.1,1,0.1, 1), align = "hv"),
          legend, ncol = 2, rel_widths = c(1,0.25)) # rel_heights values control title margins
dev.off()











#---------------Density response to climate & models-----------------
mod <- lm(Dens ~  Precip.scaled + Temp.jja.scaled  + Site, data = train.GUESS)
summary(mod)

modED <- lm(Dens ~  Precip.scaled + Temp.jja.scaled  + Site, data = train.ED)
summary(modED)



# model density as a function of gwbi only:
# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
dens_gwbi_only <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*gwbi[i] 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)

}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)


inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*gwbi.p[i] 

}

}"


guess.dens.gwbi <- jags.model(textConnection(dens_gwbi_only), 
                                  data = list(Y = train.GUESS$Dens, n=length(train.GUESS$gwbi), gwbi = train.GUESS$gwbi, 
                                              period = as.numeric(train.GUESS$period_cd), S = unique(train.GUESS$site_code),  C = unique(train.GUESS$period_cd), sites = train.GUESS$site_code, np=length(test.GUESS$period_cd), 
                                              sites.p = test.GUESS$site_code, gwbi.p = test.GUESS$gwbi,
                                              period.p = as.numeric(test.GUESS$period_cd)),
                                              
                                                n.chains = 3, n.adapt = 100)


update(guess.dens.gwbi, 1000); # Burnin for 1000 samples to start, then go higher later

# get parameter estimates
dens.GUESS.period <- coda.samples(guess.dens.gwbi, 
                                  variable.names=c("alpha", "beta1"), 
                                  n.chains = 3, n.iter=5000, thin = 1)

dens.GUESS.ypred <- coda.samples(guess.dens.gwbi, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

#dens.GUESS.yprobe <- coda.samples(guess.dens.gwbi, 
 #                                 variable.names=c("Yprobe" ), 
  #                                n.chains = 3, n.iter=5000, thin = 1)

saveRDS(dens.GUESS.period, "outputs/density_gwbi_model/GUESS_parameter_samps.rds")
saveRDS(dens.GUESS.ypred, "outputs/density_gwbi_model/GUESS_Ypred_samps.rds")
#saveRDS(dens.GUESS.yprobe, "outputs/gwbi_model/density_gwbi_model/GUESS_Yprobe_samps.rds")
#dens.GUESS.yprobe<- readRDS( "outputs/gwbi_model/density_gwbi_model/GUESS_Yprobe_samps.rds")
saveRDS(test.GUESS, "outputs/density_gwbi_model/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/density_gwbi_model/GUESS_traindata.rds")

#Extract the samples for each parameter

samps       <- dens.GUESS.period[[1]]
Yp.samps    <- dens.GUESS.ypred [[1]]
#Yprobe.samps <- samp.GUESS.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site_num))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]

#sigma.samps <- samps[,(length(unique(test.GUESS$site_num))+6):(length(test.GUESS$site_num)+length(unique(test.GUESS$site_num))+12)]



# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.GUESS$Dens

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.GUESS$Dens))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")#+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_gwbi_model/GUESSpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

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


#>>>>>>>> plot dot plots from guess model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
GUESS.site.clim <- GUESS.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(GUESS.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated gwbi sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")




# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/density_gwbi_model/GUESS_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot, ncol = 1)
dev.off()


# ---------------------------- now run the density model for ED2:------------------------


ED.dens.gwbi <- jags.model(textConnection(dens_gwbi_only), 
                              data = list(Y = train.ED$Dens, n=length(train.ED$gwbi), gwbi = train.ED$gwbi, 
                                          period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = train.ED$site_code, np=length(test.ED$period_cd), 
                                          sites.p = test.ED$site_code, gwbi.p = test.ED$gwbi,
                                          period.p = as.numeric(test.ED$period_cd)),
                              
                              n.chains = 3, n.adapt = 100)


update(ED.dens.gwbi, 1000); # Burnin for 1000 samples to start, then go higher later

# get parameter estimates
dens.ED.period <- coda.samples(ED.dens.gwbi, 
                                  variable.names=c("alpha", "beta1"), 
                                  n.chains = 3, n.iter=5000, thin = 1)

dens.ED.ypred <- coda.samples(ED.dens.gwbi, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

#dens.ED.yprobe <- coda.samples(ED.dens.gwbi, 
 #                                 variable.names=c("Yprobe" ), 
  #                                n.chains = 3, n.iter=5000, thin = 1)

saveRDS(samp.ED.period, "outputs/density_gwbi_model/ED_parameter_samps.rds")
saveRDS(samp.ED.ypred, "outputs/density_gwbi_model/ED_Ypred_samps.rds")
saveRDS(samp.ED.yprobe, "outputs/density_gwbi_model/ED_Yprobe_samps.rds")
samp.ED.yprobe<- readRDS( "outputs/density_gwbi_model/ED_Yprobe_samps.rds")
saveRDS(test.ED, "outputs/density_gwbi_model/ED_testdata.rds")
saveRDS(train.ED, "outputs/density_gwbi_model/ED_traindata.rds")

#Extract the samples for each parameter

samps       <- dens.ED.period[[1]]
Yp.samps    <- dens.ED.ypred [[1]]
#Yprobe.samps <- samp.ED.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site_num))]
beta1.samps  <- samps[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]

#sigma.samps <- samps[,(length(unique(test.ED$site_num))+6):(length(test.ED$site_num)+length(unique(test.ED$site_num))+12)]



# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$Dens))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_gwbi_model/EDpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

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


#>>>>>>>> plot dot plots from ED model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
ED.site.clim <- ED.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(ED.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")




# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/density_gwbi_model/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot, ncol = 1)
dev.off()




#------------------Models of density by climate--------------------
# model gwbi as a function of Temp, Precip, CO2, with random slops for time period & site random intercept
GUESS_dens_climate <- "model{

# for each the overall population include re for sites:

# Likelihood
for(i in 1:n){
# process model
Y[i]   ~ dnorm(gfunc[i], inv.var) # Y is agbi

# function g()
gfunc[i] <- alpha[sites[i]] + beta1[period[i]]*Precip.scaled[i] + beta2[period[i]]*Temp.jja.scaled[i] 

}


# Assume normal priors for betas, but generate a beta + alpha for each ageclass
for(c in 1:length(C)){
beta1[c] ~ dnorm(mu_beta1, inv_beta1)
beta2[c] ~ dnorm(mu_beta2, inv_beta2)

}

for(s in 1:length(S)){
alpha[s] ~ dnorm(mu_alpha, inv_alpha)
}

# use normal hyperpriors for each hyperparamters 
mu_alpha ~ dunif(-2, 2)
mu_beta1 ~ dunif(-2, 2)
mu_beta2 ~ dunif(-2, 2)

inv_alpha   ~ dgamma(0.001, 0.001)
sigma_alpha <- 1/sqrt(inv_alpha)
inv_beta1   ~ dgamma(0.001, 0.001)
sigma_beta1 <- 1/sqrt(inv_beta1)
inv_beta2   ~ dgamma(0.001, 0.001)
sigma_beta2 <- 1/sqrt(inv_beta2)


# Non-informative Prior for the inverse population variances

#alpha_ref ~ dnorm(0,0.1)
inv.var   ~ dgamma(0.001, 0.001)
sigma     <- 1/sqrt(inv.var)


# Predictions
for(i in 1:np){
# process model
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] 

}

# Probe
for(i in 1:nprobe){
#process model
Yprobe[i]   ~ dnorm(gfunc.probe[i], inv.var) # Y is agbi

#function g()
gfunc.probe[i] <- alpha[sites.probe[i]] + beta1[period.probe[i]]*Precip.scaled.probe[i] + beta2[period.probe[i]]*Temp.jja.scaled.probe[i] 

}


}"



DIprobe <- round(seq(range(train.GUESS.full$Precip.scaled)[1], range(train.GUESS.full$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.GUESS.full$Temp.jja.scaled)[1], range(train.GUESS.full$Temp.jja.scaled)[2], by = 2), 3)

# expand into full probe
probe.GUESS <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                           
                           site_num = 1:221,struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(GUESS_dens_climate), 
                                  data = list(Y = train.GUESS$Dens/100, n=length(train.GUESS$Dens), Precip.scaled = train.GUESS$Precip.scaled, Temp.jja.scaled = train.GUESS$Temp.jja.scaled, 
                                              period = as.numeric(train.GUESS$period_cd), S = unique(train.GUESS$site_code),  C = unique(train.GUESS$period_cd), sites = train.GUESS$site_code, np=length(test.GUESS$period_cd), 
                                              sites.p = test.GUESS$site_code, Precip.scaled.p = test.GUESS$Precip.scaled, Temp.jja.scaled.p = test.GUESS$Temp.jja.scaled, 
                                              period.p = as.numeric(test.GUESS$period_cd),
                                              
                                              nprobe=length(probe.GUESS$struct.cohort.code), 
                                              sites.probe = probe.GUESS$site_num, Precip.scaled.probe = probe.GUESS$DI.scaled, Temp.jja.scaled.probe = probe.GUESS$T.scaled, 
                                              period.probe = as.numeric(probe.GUESS$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.GUESS.period <- coda.samples(reg.GUESSel.by_period, 
#                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
#                          n.chains = 3, n.iter=2000, thin = 10)
dens.clim.GUESS.period <- coda.samples(reg.model.by_period, 
                                  variable.names=c("alpha", "beta1", "beta2" ), 
                                  n.chains = 3, n.iter=5000, thin = 1)

dens.clim.GUESS.ypred <- coda.samples(reg.model.by_period, 
                                 variable.names=c("Ypred" ), 
                                 n.chains = 3, n.iter=5000, thin = 1)

dens.clim.GUESS.yprobe <- coda.samples(reg.model.by_period, 
                                  variable.names=c("Yprobe" ), 
                                  n.chains = 3, n.iter=5000, thin = 1)

saveRDS(dens.clim.GUESS.period, "outputs/density_model_climate/GUESS_parameter_samps.rds")
saveRDS(dens.clim.GUESS.ypred, "outputs/density_model_climate/GUESS_Ypred_samps.rds")
saveRDS(dens.clim.GUESS.yprobe, "outputs/density_model_climate/GUESS_Yprobe_samps.rds")
dens.clim.GUESS.yprobe <- readRDS( "outputs/density_model_climate/GUESS_Yprobe_samps.rds")

saveRDS(test.GUESS, "outputs/density_model_climate/GUESS_testdata.rds")
saveRDS(train.GUESS, "outputs/density_model_climate/GUESS_traindata.rds")


#Extract the samples for each parameter

samps       <- dens.clim.GUESS.period[[1]]
Yp.samps    <- dens.clim.GUESS.ypred [[1]]
Yprobe.samps <- dens.clim.GUESS.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.GUESS$site_num))]
beta1.samps  <- samps[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- (test.GUESS$Dens/100)

pred.obs <- summary(lm(colMeans(Yp.samps) ~ (test.GUESS$Dens/100)))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_model_climate/GUESSpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.GUESS$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.GUESS$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <- unique(train.GUESS$period)[order(unique(train.GUESS$period_cd))]
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <- unique(train.GUESS$period)[order(unique(train.GUESS$period_cd))]
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Temp Index slope")

#>>>>>>>> plot dot plots from ED model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))


#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
GUESS.site.clim <- GUESS.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                             MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(GUESS.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Summer Temperature sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")



# combine all the plots together and save to output:
png(height = 12, width = 5, units = "in", res = 300, "outputs/density_model_climate/GUESS_full_dot_plot_cohort.png")
cowplot::plot_grid(int.dot.MAP, b1.dot,b2.dot, ncol = 1)
dev.off()


#------------------------------Run the density-climate model for ED2---------------------

DIprobe <- round(seq(range(train.ED$Precip.scaled)[1], range(train.ED$Precip.scaled)[2], by = 2), 3)
Tempprobe <- round(seq(range(train.ED$Temp.jja.scaled)[1], range(train.ED$Temp.jja.scaled)[2], by = 2), 3)

# expand into full probe
probe.ED <- expand.grid(DI.scaled = DIprobe,  T.scaled = Tempprobe,
                           
                           site_num = 1:221,struct.cohort.code= 1:2)




reg.model.by_period <- jags.model(textConnection(GUESS_dens_climate), 
                                  data = list(Y = train.ED$Dens/100000, n=length(train.ED$Dens), Precip.scaled = train.ED$Precip.scaled, Temp.jja.scaled = train.ED$Temp.jja.scaled, 
                                              period = as.numeric(train.ED$period_cd), S = unique(train.ED$site_code),  C = unique(train.ED$period_cd), sites = train.ED$site_code, np=length(test.ED$period_cd), 
                                              sites.p = test.ED$site_code, Precip.scaled.p = test.ED$Precip.scaled, Temp.jja.scaled.p = test.ED$Temp.jja.scaled, 
                                              period.p = as.numeric(test.ED$period_cd),
                                              
                                              nprobe=length(probe.ED$struct.cohort.code), 
                                              sites.probe = probe.ED$site_num, Precip.scaled.probe = probe.ED$DI.scaled, Temp.jja.scaled.probe = probe.ED$T.scaled, 
                                              period.probe = as.numeric(probe.ED$struct.cohort.code)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

dens.clim.ED.period <- coda.samples(reg.model.by_period, 
                                       variable.names=c("alpha", "beta1", "beta2" ), 
                                       n.chains = 3, n.iter=5000, thin = 1)

dens.clim.ED.ypred <- coda.samples(reg.model.by_period, 
                                      variable.names=c("Ypred" ), 
                                      n.chains = 3, n.iter=5000, thin = 1)

dens.clim.ED.yprobe <- coda.samples(reg.model.by_period, 
                                       variable.names=c("Yprobe" ), 
                                       n.chains = 3, n.iter=5000, thin = 1)

saveRDS(dens.clim.ED.period , "outputs/density_model_climate/ED_parameter_samps.rds")
saveRDS(dens.clim.ED.ypred, "outputs/density_model_climate/ED_Ypred_samps.rds")
saveRDS(dens.clim.ED.yprobe, "outputs/density_model_climate/ED_Yprobe_samps.rds")

dens.clim.ED.yprobe <- readRDS( "outputs/density_model_climate/ED_Yprobe_samps.rds")
dens.clim.ED.ypred <- readRDS( "outputs/density_model_climate/ED_Ypred_samps.rds")
dens.clim.ED.period <- readRDS( "outputs/density_model_climate/ED_parameter_samps.rds")

saveRDS(test.ED, "outputs/density_model_climate/ED_testdata.rds")
saveRDS(train.ED, "outputs/density_model_climate/ED_traindata.rds")
test.ED <- readRDS("outputs/density_model_climate/ED_testdata.rds")
train.ED <- readRDS("outputs/density_model_climate/ED_traindata.rds")
#Extract the samples for each parameter

samps       <- dens.clim.ED.period[[1]]
Yp.samps    <- dens.clim.ED.ypred [[1]]
Yprobe.samps <- dens.clim.ED.yprobe[[1]]
alpha.samps <- samps[,1:length(unique(test.ED$site_num))]
beta1.samps  <- samps[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.samps  <- samps[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]


# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.ED$Dens

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.ED$Dens))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
png(width = 6, height = 5, units = "in", res = 300, "outputs/density_model_climate/EDpred_vs_obs.png")
p.o.plot
dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.ED$Dens)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.ED$Dens)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)


# plot distributions of the paramters:

# plot marginal distributions of cohort + structure specific parameters:
a <- data.frame(alpha.samps)
colnames(a) <- 
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")

b1 <- data.frame(beta1.samps)
colnames(b1) <- unique(train.ED$period)[order(unique(train.ED$period_cd))]
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b1$num <- rownames(b1)
b1.m <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")


b2 <- data.frame(beta2.samps)
colnames(b2) <- unique(train.ED$period)[order(unique(train.ED$period_cd))]
#colnames(b2) <- c(paste0(c(unique(train.dry$struct.cohort))))
b2$num <- rownames(b2)
b2.m <- melt(b2, id.vars=c("num"))
b2.mplots <- ggplot(b2.m, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Precipitation Index slope")

#>>>>>>>> plot dot plots from ED model:
a.m$variable2 <- paste0("X",a.m$variable)

a1.sum <- a.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                          Ci.low = quantile(value, 0.025), 
                                                          Ci.high = quantile(value, 0.975))
#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b1.sum <- b1.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))


#a1.sum$variable <- factor(a1.sum$variable, levels = c( "Past-Forest", "Past-Savanna", "Modern-Forest", "Modern-Savanna"))
b2.sum <- b2.m %>% group_by(variable) %>% dplyr::summarise(mean.val = mean(value),
                                                           Ci.low = quantile(value, 0.025), 
                                                           Ci.high = quantile(value, 0.975))

# write out all the dotplots
# want to order the sites by mean annual precip and/or mean annual temperatuere and then plot:
# summarize site map and site mat:
ED.site.clim <- ED.sort_lag %>% group_by(Site) %>% summarise(MAP = mean(precip.mm, na.rm=TRUE), 
                                                                   MAT = mean(Tair.C, na.rm=TRUE))

a1.sum$Site <- paste0("X", a1.sum$variable)
a1.clim <- merge(ED.site.clim, a1.sum, by = "Site")
int.dot.MAP <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAP, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")
int.dot.Tmean <- ggplot(data.frame(a1.clim), aes(x = mean.val, y = MAT, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+geom_point()+theme(legend.position = "none")


b1.dot <- ggplot(data.frame(b1.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Precipitation sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")


b2.dot <- ggplot(data.frame(b2.sum), aes(x = mean.val, y = variable, color = variable, size = 2))+geom_errorbarh( aes(xmin = Ci.low, xmax = Ci.high, size = 1,height = 0))+
  geom_point()+scale_color_manual(values = c("industrial-past" = "blue","modern-industrial" = "red"))+theme(legend.position = "none", axis.title.y = element_blank())+xlab("Estimated Summer Temperature sensitivity)")+ geom_vline(xintercept = 0, linetype = "dashed")



# combine all the plots together and save to output:
png(height = 8, width = 5, units = "in", res = 300, "outputs/density_model_climate/ED_full_dot_plot_cohort.png")
cowplot::plot_grid(b1.dot, b2.dot,ncol = 1)
dev.off()


# -------------------------combine dotplots from density models for ED and GUESS------------------
#------------------------Bring in all the coefficient estimates and put in one big graph-----------



samp.GUESS.period <- readRDS( "outputs/density_model_climate/GUESS_parameter_samps.rds")
samp.ED.period <- readRDS( "outputs/density_model_climate/ED_parameter_samps.rds")


samp.GUESS.period <- samp.GUESS.period[[1]]
samp.ED.period <- samp.ED.period[[1]]


# get parameter estimates from ED model:
alpha.sampsED <- samp.ED.period[,1:length(unique(test.ED$site_num))]
beta1.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+1):(length(unique(test.ED$site_num))+2)]
beta2.sampsED  <- samp.ED.period[,(length(unique(test.ED$site_num))+3):(length(unique(test.ED$site_num))+4)]

a <- data.frame(alpha.sampsED)
colnames(a) <- unique(train.ED$site_num)
a$num <- rownames(a)
a.m.ED <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.ED, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.ED$model <- "ED2"

b1 <- data.frame(beta1.sampsED)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.ED <- melt(b1, id.vars=c("num"))
b1.mplots <- ggplot(b1.m.ED, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
b1.m.ED$model <- "ED2"

b2 <- data.frame(beta2.sampsED)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.ED <- melt(b2, id.vars=c("num"))
b2.m.ED$model <- "ED2"


# get parameter estimates from LPJ-GUESS model:
alpha.sampsGUESS <- samp.GUESS.period[,1:length(unique(test.GUESS$site_num))]
beta1.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+1):(length(unique(test.GUESS$site_num))+2)]
beta2.sampsGUESS  <- samp.GUESS.period[,(length(unique(test.GUESS$site_num))+3):(length(unique(test.GUESS$site_num))+4)]

a <- data.frame(alpha.sampsGUESS)
colnames(a) <- unique(train.GUESS$site_num)
a$num <- rownames(a)
a.m.GUESS <- melt(a, id.vars=c("num"))
alpha.mplots <- ggplot(a.m.GUESS, aes(value, fill = variable))+geom_density(alpha = 0.5)+theme_bw()+xlab("Random intercepts")+theme(legend.position = "none")
a.m.GUESS$model <- "LPJ-GUESS"

b1 <- data.frame(beta1.sampsGUESS)
colnames(b1) <- c("Modern", "Past") # past = 2, modern = 1 here
b1$num <- rownames(b1)
b1.m.GUESS <- melt(b1, id.vars=c("num"))
b1.m.GUESS$model <- "LPJ-GUESS"

b2 <- data.frame(beta2.sampsGUESS)
colnames(b2) <-c("Modern", "Past")
b2$num <- rownames(b2)
b2.m.GUESS <- melt(b2, id.vars=c("num"))
b2.m.GUESS$model <- "LPJ-GUESS"



# now combine all the beta1s together and make a modern past dotplot:
b1.m <- bind_rows( b1.m.ED, b1.m.GUESS )
b2.m <- bind_rows( b2.m.ED, b2.m.GUESS )


b1.sum <- b1.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))

b1.sum$variable <- factor(b1.sum$variable, levels = c( "Past",  "Modern"))

b2.sum <- b2.m %>% group_by(variable, model) %>% dplyr::summarise(mean.val = mean(value),
                                                                  Ci.low = quantile(value, 0.025), 
                                                                  Ci.high = quantile(value, 0.975))
b2.sum$variable <- factor(b2.sum$variable, levels = c( "Past",  "Modern"))



# now plot dotplots:

b1.dot <- ggplot(data.frame(b1.sum), aes(x = model, y = mean.val, color = variable), size = 2.5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size = 2, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size = 2.5)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Precipitation \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 35)+theme( axis.title.x = element_blank(), panel.grid = element_blank())

b2.dot <- ggplot(data.frame(b2.sum), aes(x = model, y = mean.val, color = variable), size = 2.5)+geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size = 2, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size = 2.5)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+ylab("Temperature \n sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 35)+theme( axis.title.x = element_blank(), panel.grid = element_blank())


legend <- get_legend(b1.dot)

png(height = 10, width = 10, units = "in", res = 500, "outputs/density_model_climate/all_params_dotplot.png")
plot_grid(plot_grid( b1.dot+theme(legend.position = "none"), 
                     b2.dot+theme(legend.position = "none"), 
                     ncol = 1, align = "hv"),legend, ncol = 2, rel_widths = c(1,0.25))
dev.off()
