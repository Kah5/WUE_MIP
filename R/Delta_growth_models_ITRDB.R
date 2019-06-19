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
