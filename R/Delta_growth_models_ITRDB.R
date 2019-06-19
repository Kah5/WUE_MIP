# compare mean GWBI and tree ring growth changes:

pct.change.ed <- readRDS("outputs/itrdb_model_compare/ED2_pct_change_vars.rds")
pct.change.guess <- readRDS("outputs/itrdb_model_compare/GUESS_pct_change_vars.rds")

ggplot(pct.change.ed, aes(PFT, GWBI.change, fill = PFT))+geom_boxplot()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(pct.change.guess, aes(PFT, GWBI.change, fill = PFT))+geom_boxplot()+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))



# read in ITRDB and see if growth changed on average for the time periods in question
itrdb <- readRDS( paste0(getwd(),"/Data/ITRDB/full.clim.prism.rds"))

taxa.trans <- read.csv("Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv", stringsAsFactors = FALSE)
itrdbpft <- left_join(itrdb, taxa.trans, by = "SPEC.CODE")

itrdbpft $RWI <- as.numeric(itrdbpft $RWI) # why is rwi not numeric

mean.RWI.ed <- itrdbpft %>% group_by(ageclass, Latitude, Longitude, ED.PFT ) %>% summarise(RWI.mean = mean(RWI, na.rm=TRUE))
mean.RWI.guess <- itrdbpft %>% group_by(ageclass, Latitude, Longitude, LPJ.GUESS.PFT ) %>% summarise(RWI.mean = mean(RWI, na.rm=TRUE))



# calculate % changes in mean RWI for all PFTS:
mean.RWI.ed.s <- mean.RWI.ed %>% group_by( Latitude, Longitude, ED.PFT) %>% spread(key = ageclass, value = RWI.mean)
mean.RWI.guess.s <- mean.RWI.guess %>% group_by( Latitude, Longitude, LPJ.GUESS.PFT) %>% spread(key = ageclass, value = RWI.mean)


mean.RWI.ed.s$pctdiff <- ((mean.RWI.ed.s$Modern - mean.RWI.ed.s$Past)/mean.RWI.ed.s$Past)*100
mean.RWI.guess.s$pctdiff <- ((mean.RWI.guess.s$Modern - mean.RWI.guess.s$Past)/mean.RWI.guess.s$Past)*100


change.guess.itrdb <- mean.RWI.guess.s %>% group_by(LPJ.GUESS.PFT) %>% summarise(pct.change = mean(pctdiff, na.rm=TRUE),
                                                           low.ci = quantile(pctdiff, 0.025, na.rm = TRUE), 
                                                           high.ci = quantile(pctdiff, 0.975, na.rm = TRUE), 
                                                           model = "ITRDB")

change.guess.itrdb$LPJ.GUESS.PFT <- c("BNE", "BINE", "TeBS", "TelBS")
colnames(change.guess.itrdb)[1] <- "PFT"

change.ed.itrdb <-mean.RWI.ed.s %>% group_by(ED.PFT) %>% summarise(pct.change = mean(pctdiff, na.rm=TRUE),
                                                 low.ci = quantile(pctdiff, 0.025, na.rm = TRUE), 
                                                 high.ci = quantile(pctdiff, 0.975, na.rm = TRUE), 
                                                 model = "ITRDB")
colnames(change.ed.itrdb)[1] <- "PFT"

change.ed.model <-  pct.change.ed  %>% group_by(PFT) %>% summarise(pct.change = mean(GWBI.change, na.rm=TRUE),
                                                 low.ci = quantile(GWBI.change, 0.025, na.rm = TRUE), 
                                                 high.ci = quantile(GWBI.change, 0.975, na.rm = TRUE), 
                                                 model = "ED2")

change.guess.model <- pct.change.guess  %>% group_by(PFT) %>% summarise(pct.change = mean(GWBI.change, na.rm=TRUE),
                                               low.ci = quantile(GWBI.change, 0.025, na.rm = TRUE), 
                                               high.ci = quantile(GWBI.change, 0.975, na.rm = TRUE), 
                                               model = "LPJ-GUESS")

change.guess.model$PFT <- c("TelBS", "BIBS", "BINE", "BNE","TeBE", "TeBS", "Total", "TrIBE")


# combine itrdb + taxa together:
ED.comparison <- rbind(change.ed.model, change.ed.itrdb)
GUESS.comparison <- rbind(change.guess.model, change.guess.itrdb)



ggplot(GUESS.comparison, aes(PFT, pct.change, group = model, fill = model ))+geom_bar(stat = "identity", position = "dodge", width = 0.5)+geom_errorbar()

GUESS.pct.change.pft.facet  <- ggplot(GUESS.comparison[!GUESS.comparison$PFT %in%  c("Total", "TrIBE"),], aes(x = model, y = pct.change, fill = model ))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = low.ci, ymax = high.ci, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("LPJ-GUESS"='#d95f02', "ITRDB"='#7570b3'))+theme_bw(base_size = 30)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("% change growth \n Modern - Past")+facet_wrap(~PFT, scales = "free_y")


GUESS.pct.change.pft  <- ggplot(GUESS.comparison[!GUESS.comparison$PFT %in%  c("Total", "TrIBE"),], aes(x = PFT, y = pct.change, fill = model ))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = low.ci, ymax = high.ci, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("LPJ-GUESS"='#d95f02', "ITRDB"='#7570b3'))+theme_bw(base_size = 30)+theme(axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed", size = 1)+
  ylab("% change growth \n Modern - Past")



ED.pct.change.pft.facet  <- ggplot(ED.comparison[!ED.comparison$PFT %in% "mean.gwbi",], aes(x = model, y = pct.change, fill = model ))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = low.ci, ymax = high.ci, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c('#1b9e77', '#7570b3'))+theme_bw(base_size = 30)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("% change growth \n Modern - Past")+facet_wrap(~PFT, scales = "free_y")


ED.pct.change.pft  <- ggplot(ED.comparison[!ED.comparison$PFT %in% "mean.gwbi",], aes(x = PFT, y = pct.change, fill = model ))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = low.ci, ymax = high.ci, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3'))+theme_bw(base_size = 30)+theme(axis.title.x = element_blank(), panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed", size = 1)+
  ylab("% change growth \n Modern - Past")


# now save to plots
png(height = 12.5, width = 16, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_growth_pfts_ED_GUESS_itrdb.png")
cowplot::plot_grid(ED.pct.change.pft, GUESS.pct.change.pft,ncol = 1, align = "hv", labels = "AUTO", label_size = 30)
dev.off()



# plot ED2 and LPJ-GUESS changes in the total site GWBI : note that while total gwbi increases on average across all sites, total by PFT does not



# calculate % changes overall :
itrdb.all.s <- itrdbpft  %>% group_by(studyCode,ageclass) %>% summarise(mean = mean(RWI)) %>% spread(key = ageclass, value = mean)
  
itrdb.all.s$pctdiff <- ((itrdb.all.s$Modern - itrdb.all.s$Past)/itrdb.all.s$Past)*100


change.all.itrdb <- data.frame(itrdb.all.s)  %>% summarise(PFT = "all",
                                                           pct.change = mean(pctdiff, na.rm=TRUE),
                                                                                 low.ci = quantile(pctdiff, 0.025, na.rm = TRUE), 
                                                                                 high.ci = quantile(pctdiff, 0.975, na.rm = TRUE), 
                                                                                 model = "ITRDB")




total.ecosys.gwbi <- rbind(ED.comparison[ED.comparison$PFT %in% "mean.gwbi",], GUESS.comparison[GUESS.comparison$PFT %in% "Total",], change.all.itrdb)

total.ecosys.gwbi$model <- factor(total.ecosys.gwbi$model, levels = c("ED2", "LPJ-GUESS", "ITRDB"))

Total.gwbi.change <- ggplot(total.ecosys.gwbi, aes(x = model, y = pct.change, fill = model ))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = low.ci, ymax = high.ci, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02'))+theme_bw(base_size = 15)+theme(axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed", size = 1)+
  ylab("% change total site growth \n Modern - Past")


png(height = 4, width = 6, units = "in", res = 400, "outputs/itrdb_model_compare/Total_growth_change_sites_ed_guess_itrdb.png")
Total.gwbi.change
dev.off()

# is this also true for my tree rings???
full.ghcn <- read.csv("/Users/kah/Documents/TreeRings/outputs/data/rwi_age_dbh_ghcn.df")
summary(full.ghcn)





#read in ecological data to see if growth changed on average:
eco.all.s <- full.ghcn %>% group_by(site,ageclass) %>% summarise(mean = mean(RWI, na.rm=TRUE)) %>% spread(key = ageclass, value = mean)

eco.all.s$pctdiff <- ((eco.all.s$Modern - eco.all.s$Past)/eco.all.s$Past)*100


change.all.eco <- data.frame(eco.all.s)  %>% summarise(PFT = "all",
                                                           pct.change = mean(pctdiff, na.rm=TRUE),
                                                           low.ci = quantile(pctdiff, 0.025, na.rm = TRUE), 
                                                           high.ci = quantile(pctdiff, 0.975, na.rm = TRUE), 
                                                           model = "Ecological")




total.ecosys.gwbi <- rbind(ED.comparison[ED.comparison$PFT %in% "mean.gwbi",], GUESS.comparison[GUESS.comparison$PFT %in% "Total",], change.all.itrdb, change.all.eco)

total.ecosys.gwbi$model <- factor(total.ecosys.gwbi$model, levels = c("ED2", "LPJ-GUESS", "ITRDB", "Ecological"))

Total.gwbi.change.eco <- ggplot(total.ecosys.gwbi, aes(x = model, y = pct.change, fill = model ))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = low.ci, ymax = high.ci, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 25)+theme(axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed", size = 1)+
  ylab("% change total site growth \n Modern - Past")


png(height = 4, width = 6, units = "in", res = 400, "outputs/itrdb_model_compare/Total_growth_change_sites_ed_guess_itrdb_eco.png")
Total.gwbi.change.eco
dev.off()


# combining the species level plots with the overall plots


png(height = 13, width = 18, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_growth_pfts_ED_GUESS_itrdb_and_totals.png")
plot_grid(cowplot::plot_grid(ED.pct.change.pft+theme(legend.position = "none"), GUESS.pct.change.pft+theme(legend.position = "none"),
                   ncol = 1, align = "hv",labels = c("A", "B"), label_size = 30),
          Total.gwbi.change.eco+theme(axis.text.x = element_text(angle = 45, hjust = 1)), ncol = 2, rel_widths = c(1, 0.75),labels = c("", "C"), label_size = 30)
dev.off()



ggplot(pct.change.ed, aes(lon, lat,fill = GWBI.change))+geom_raster()+facet_wrap(~PFT)+
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", space = "Lab" )

ggplot(pct.change.guess, aes(lon, lat,fill = GWBI.change))+geom_raster()+facet_wrap(~PFT)+
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white", high = "red", space = "Lab" )



# read in growth model parameters for the models by taxa, and ITRDB
#------------------------------------- LPJ-GUESS pfts-------------------------------------------
# calculate mean differences between parameters:
# read in the full mcmc samples for the parameters:
ITRDB.params <- read.csv("outputs/ITRDB_models/ITRDB_GUESS_PFT_time_re/parameters_full_mcmc.csv")
GUESS.params <- read.csv("outputs/gwbi_model/LPJ_GUESS_time_re/parameters_full_mcmc.csv")

GUESS.params$model <- "LPJ-GUESS"
ITRDB.params$model <- "ITRDB"

# remove the .gwbi from the GUESS species
GUESS.params$species <- do.call(rbind, strsplit(as.character(GUESS.params$species),".gwbi"))
ITRDB.params$species <- as.character(ITRDB.params$species )

GUESS.params$species <- ifelse(GUESS.params$species %in% "BeIBS", "TeIBS", GUESS.params$species)
summary(GUESS.params %>% filter(mcmc >=500))

params <- rbind(GUESS.params, ITRDB.params)

params.m <- melt(params, id.vars = c("mcmc", "model", "species"))

# now get timeperiod and names of params
params.m$time.cd <- substring(params.m$variable, 7, 7)
params.m$timeclass <- ifelse(params.m$time.cd %in% "1", "Past", 
                             ifelse(params.m$time.cd %in% "2", "Modern", "No re"))
params.m$variable2 <- substring(params.m$variable, 1,5)
colnames(params.m) <- c("mcmc", "model", "species", "parameter_full", "value", "time.cd", "timeclass", "parameter")


b1.class <- params.m %>% select(mcmc, model, species, parameter, value, timeclass) %>% group_by(model, mcmc,  species, parameter,timeclass) %>% mutate(grouped_id = row_number())

# get the beta params into dotplots:

beta.summaries.guess <- b1.class %>% group_by(model, species, parameter, timeclass) %>% dplyr::summarise(mean = mean(value, na.rm =TRUE), 
                                                                                              Ci.low = quantile(value, 0.025, na.rm =TRUE),
                                                                                              Ci.high = quantile(value, 0.975, na.rm=TRUE))


#beta.summaries.guess$species <- factor(beta.summaries.guess$species, levels = c("BINE",  "BNE" ,  "TeBS",  "TeIBS",     "BIBS",  "TeBE" , "Total"))
beta1.guess <- ggplot(data.frame(beta.summaries.guess[beta.summaries.guess$parameter %in% "beta1" & beta.summaries.guess$model %in% "LPJ-GUESS" & !beta.summaries.guess$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Precipitation Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta1.guess

beta1.g.itrdb <- ggplot(data.frame(beta.summaries.guess[beta.summaries.guess$parameter %in% "beta1" & beta.summaries.guess$model %in% "ITRDB" & !beta.summaries.guess$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Precipitation Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta1.g.itrdb


beta2.guess <- ggplot(data.frame(beta.summaries.guess[beta.summaries.guess$parameter %in% "beta2" & beta.summaries.guess$model %in% "LPJ-GUESS" & !beta.summaries.guess$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Tmax Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta2.guess

beta2.g.itrdb <- ggplot(data.frame(beta.summaries.guess[beta.summaries.guess$parameter %in% "beta2" & beta.summaries.guess$model %in% "ITRDB" & !beta.summaries.guess$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Tmax Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta2.g.itrdb

params.class <- b1.class %>% select (model, mcmc,  species, parameter, grouped_id, timeclass, value) %>% spread(timeclass, value) %>% select(-grouped_id)


params.class$pct_change <- ((params.class$Modern - params.class$Past))



params.diff <- params.class %>% group_by(model, species, parameter) %>% summarise(mean = mean(pct_change, na.rm=TRUE),
                                                                                  ci.low = quantile(pct_change, 0.025, na.rm=TRUE), 
                                                                                  ci.high = quantile(pct_change, 0.975, na.rm=TRUE))

#params.diff$species2 <- ifelse(params.diff$species %in% c("BNE", "BINE"),"BNE/BINE", params.diff$species)
params.diff$species2 <- params.diff$species
# while the actual parameter estimates vary across GUESS and ITRDB datasets, the direction of drought sensitivity differences between the two time periods are similar in some species. 

# plot average change in drought sensitivity

params.diff$model <- factor(params.diff$model, levels = c("LPJ-GUESS", "ITRDB"))
pct.drought.change.GUESS <- ggplot(params.diff[params.diff$parameter %in% "beta1" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("drought sensitvity change \n between Modern and Past")+facet_wrap(~species2)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_drought_senstivity_ITRDB_GUESS_PFTS.png")
pct.drought.change.GUESS
#dev.off()

pct.drought.change.guess.full <- ggplot(params.diff[params.diff$parameter %in% "beta1" & !params.diff$species2 %in% c("Total", NA),], aes( x=species, y = mean, fill = model))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Change in \n Precip. sensitvity")#+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_drought_senstivity_ITRDB_ED_PFTS.png")
pct.drought.change.guess.full


pct.temp.change.GUESS <- ggplot(params.diff[params.diff$parameter %in% "beta2" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("June Tmax sensitvity change \n between Modern and Past")+facet_wrap(~species2)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_tmax_senstivity_ITRDB_GUESS_PFTS.png")
pct.temp.change
#dev.off()

pct.temp.change.guess.full <- ggplot(params.diff[params.diff$parameter %in% "beta2" & !params.diff$species2 %in% c("Total", NA),], aes( x=species, y = mean, fill = model))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Change in \n Tmax sensitvity")#+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_drought_senstivity_ITRDB_ED_PFTS.png")
pct.temp.change.guess.full

pct.lag1.change.GUESS <- ggplot(params.diff[params.diff$parameter %in% "beta3" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("change in lag -1 parameter \n between Modern and Past")+facet_wrap(~species2)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_lag1_senstivity_ITRDB_GUESS_PFTS.png")
pct.lag1.change.GUESS
#dev.off()

pct.lag2.change.GUESS <- ggplot(params.diff[params.diff$parameter %in% "beta4" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("change in lag -1 parameter \n between Modern and Past")+facet_wrap(~species2, scales = "free_y")

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_lag2_senstivity_ITRDB_GUESS_PFTS.png")
pct.lag2.change
#dev.off()

#------------------------------------- ED2 pfts-------------------------------------------
# now read in the ED2 PFT models and the ITRDB models run like ED2 PFTS:
ITRDB.params <- read.csv("outputs/ITRDB_models/ITRDB_ED_PFT_time_re/parameters_full_mcmc.csv")
ED.params <- read.csv("outputs/gwbi_model/ED2_time_re/parameters_full_mcmc_28000.csv")
ITRDB.params <- ITRDB.params %>% select(-beta5.1., -beta5.2.)
ED.params$model <- "ED2"
ITRDB.params$model <- "ITRDB"

# remove the .gwbi from the ED species
#ED.params$species <- do.call(rbind, strsplit(as.character(ED.params$species),".gwbi"))
ITRDB.params$species <- as.character(ITRDB.params$species )

params <- rbind(ED.params, ITRDB.params)

params.m <- melt(params, id.vars = c("mcmc", "model", "species"))

# now get timeperiod and names of params
params.m$time.cd <- substring(params.m$variable, 7, 7)
params.m$timeclass <- ifelse(params.m$time.cd %in% "1", "Past", 
                             ifelse(params.m$time.cd %in% "2", "Modern", "No re"))
params.m$variable2 <- substring(params.m$variable, 1,5)
colnames(params.m) <- c("mcmc", "model", "species", "parameter_full", "value", "time.cd", "timeclass", "parameter")


b1.class.ed <- params.m %>% select(mcmc, model, species, parameter, value, timeclass) %>% group_by(model, mcmc,  species, parameter,timeclass) %>% mutate(grouped_id = row_number())
# get the beta params into dotplots:

beta.summaries.ed <- b1.class.ed %>% group_by(model, species, parameter, timeclass) %>% dplyr::summarise(mean = mean(value, na.rm =TRUE), 
                                                                                                         Ci.low = quantile(value, 0.025, na.rm =TRUE),
                                                                                                         Ci.high = quantile(value, 0.975, na.rm=TRUE))


#beta.summaries.guess$species <- factor(beta.summaries.guess$species, levels = c("BINE",  "BNE" ,  "TeBS",  "TeIBS",     "BIBS",  "TeBE" , "Total"))
beta1.ed <- ggplot(data.frame(beta.summaries.ed[beta.summaries.ed$parameter %in% "beta1" & beta.summaries.ed$model %in% "ED2" & !beta.summaries.ed$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Precipitation Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta1.ed

beta1.e.itrdb <- ggplot(data.frame(beta.summaries.ed[beta.summaries.ed$parameter %in% "beta1" & beta.summaries.ed$model %in% "ITRDB" & !beta.summaries.ed$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Precipitation Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta1.e.itrdb


beta2.ed <- ggplot(data.frame(beta.summaries.ed[beta.summaries.ed$parameter %in% "beta2" & beta.summaries.ed$model %in% "ED2" & !beta.summaries.ed$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Tmax Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta2.ed

beta2.e.itrdb <- ggplot(data.frame(beta.summaries.ed[beta.summaries.ed$parameter %in% "beta2" & beta.summaries.ed$model %in% "ITRDB" & !beta.summaries.ed$species %in% c(NA, "Total"),]), aes(x = species, y = mean, color = timeclass), size =  5)+
  geom_errorbar( aes(ymin = Ci.low, ymax = Ci.high, width = 0), size =  5, position = position_dodge(width=0.5))+
  geom_point(position=position_dodge(width=0.5), size =  10)+geom_abline(aes(intercept = 0, slope = 0), linetype = "dashed")+scale_color_manual(values = c("Past" = "blue","Modern" = "red"), name = " ")+
  ylab("Tmax Sensitivity") + geom_vline(xintercept = 0, linetype = "dashed")+theme_bw(base_size = 25)+theme( axis.title.x = element_blank(), panel.grid = element_blank())#+ylim(-0.150,0)
beta2.e.itrdb


params.class.ed <- b1.class.ed %>% select (model, mcmc,  species, parameter, grouped_id, timeclass, value) %>% spread(timeclass, value) %>% select(-grouped_id)


params.class.ed$pct_change <- ((params.class.ed$Modern - params.class.ed$Past))

#params.class.ed<- params.class.ed %>% filter (mcmc >=1500 )

params.diff <- params.class.ed %>% group_by(model, species, parameter) %>% summarise(mean = mean(pct_change, na.rm=TRUE),
                                                                                  ci.low = quantile(pct_change, 0.025, na.rm=TRUE), 
                                                                                  ci.high = quantile(pct_change, 0.975, na.rm=TRUE))

#params.diff$species2 <- ifelse(params.diff$species %in% c("BNE", "BINE"),"BNE/BINE", params.diff$species)

# while the actual parameter estimates vary across ED and ITRDB datasets, the direction of drought sensitivity differences between the two time periods are similar in some species. 

# plot average change in drought sensitivity
pct.drought.change.ed <- ggplot(params.diff[params.diff$parameter %in% "beta1" & !params.diff$species %in% c("mean.gwbi"),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c('#1b9e77', '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("drought sensitvity change \n between Modern and Past")+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_drought_senstivity_ITRDB_ED_PFTS.png")
pct.drought.change.ed
#dev.off()

pct.drought.change.ed.full <- ggplot(params.diff[params.diff$parameter %in% "beta1" & !params.diff$species %in% c("mean.gwbi"),], aes( x=species, y = mean, fill = model))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Change in \n Precip. sensitvity")#+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_drought_senstivity_ITRDB_ED_PFTS.png")
pct.drought.change.ed.full

pct.temp.change.ed <- ggplot(params.diff[params.diff$parameter %in% "beta2" & !params.diff$species %in% c("mean.gwbi"),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c('#1b9e77', '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Change in \n Tmax sensitvity change")+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_tmax_senstivity_ITRDB_ED_PFTS.png")
pct.temp.change.ed
#dev.off()


pct.temp.change.ed.full <- ggplot(params.diff[params.diff$parameter %in% "beta2" & !params.diff$species %in% c("mean.gwbi"),], aes( x=species, y = mean, fill = model))+geom_bar(stat="identity", position = position_dodge(width = 1))+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=1))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Change in \n Tmax sensitvity")#+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_drought_senstivity_ITRDB_ED_PFTS.png")
pct.temp.change.ed.full


pct.lag1.change.ed <- ggplot(params.diff[params.diff$parameter %in% "beta3" & !params.diff$species %in% c("mean.gwbi"),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("change in lag -1 parameter \n between Modern and Past")+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_lag1_senstivity_ITRDB_ED_PFTS.png")
pct.lag1.change.ed
#dev.off()

pct.lag2.change.ed <- ggplot(params.diff[params.diff$parameter %in% "beta4" & !params.diff$species %in% c("mean.gwbi"),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c("ED2"='#1b9e77', "ITRDB"='#7570b3', "LPJ-GUESS"='#d95f02', "Ecological" = "#b2abd2"))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("change in lag -1 parameter \n between Modern and Past")+facet_wrap(~species)

#png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_lag2_senstivity_ITRDB_ED_PFTS.png")
pct.lag2.change.ed
#dev.off()


# put all togther in one big figure:

theme.all <- theme_bw(base_size = 15)+theme(legend.position = "none", panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), axis.title.x = element_blank())

png(height = 16, width = 14, units = "in", res = 300, "outputs/itrdb_model_compare/climate_sens_params_and_change_ED_GUESS_ITRDB.png")
plot_grid(
  beta1.ed+ylab("ED2 precipitation sensitivity")+theme.all, beta1.e.itrdb+ylab("ITRDB precipitation sensitivity")+theme.all, pct.drought.change.ed.full+theme.all,
  beta1.guess+ylab("LPJ-GUESS precipitation sensitivity")+theme.all, beta1.g.itrdb+ylab("ITRDB precipitation sensitivity")+theme.all, pct.drought.change.guess.full+theme.all,
  
  beta2.ed+ylab("ED2 Tmax sensitivity")+theme.all, beta2.e.itrdb+ylab("ITRDB Tmax sensitivity")+theme.all, pct.temp.change.ed.full+theme.all,
  beta2.guess+ylab("LPJ-GUESS Tmax sensitivity")+theme.all, beta2.g.itrdb+ylab("ITRDB Tmax sensitivity")+theme.all, pct.temp.change.guess.full+theme.all,
  
  ncol = 3, align = "hv"
)

dev.off()

