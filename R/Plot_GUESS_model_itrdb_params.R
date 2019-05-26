# Plot comparisons of parameters estimated from the PFT level models and ITRDB pft level models:

itrdb.guess.pft.summary <- read.csv( "outputs/ITRDB_models/ITRDB_GUESS_PFT_time_re/all_parameter_mean_CI.csv")
guess.pft.summary <- read.csv( "outputs/gwbi_model/LPJ_GUESS_time_re/all_parameter_mean_CI.csv")
guess.pft.summary$species <- do.call(rbind, strsplit(as.character(guess.pft.summary$species),".gwbi"))

unique(guess.pft.summary$species)

GUESS.PFT.convert <- data.frame(
  species = c("TeIBS", 
           "TeBS", 
           "TeBE", 
           "BNE", 
           "BINE", 
           "BeIBS", 
           "BIBS"),
  full.spec = c("Temperate broadleaved summergreen (shade intolerant)",
                "Temperate broadleaved summergreen", 
                "Temperate broadleved evergreen", 
                "Boreal needleleaved evergreen", 
                "Boreal needleleaved evergreen (shade intolerant)", 
                "Boreal broadleaved summergreen (shade intolerant)",
                "Boreal broadleaved summergreen"),
  biome = c("Temperate", "Temperate","Temperate","Boreal", "Boreal","Boreal", "Boreal"),
  leaf = c("Broadleaved", "Broadleaved","Broadleaved","Needleleaved","Needleleaved", "Broadleaved", "Broadleaved"),
  decidious = c("decidious", "decidious", "evergreen", "evergreen", "evergreen", "decidious", "decidious")
)


itrdb.guess.pft.summary <- left_join(itrdb.guess.pft.summary, GUESS.PFT.convert)
guess.pft.summary <- left_join(guess.pft.summary, GUESS.PFT.convert)

guess.pft.summary <- guess.pft.summary %>% select(-X)
guess.pft.summary$model <- "LPJ-GUESS"
itrdb.guess.pft.summary$model <- "ITRDB"

pft.summary <- rbind(guess.pft.summary, itrdb.guess.pft.summary)

ggplot(pft.summary [pft.summary $variable2 %in% "beta1",], aes(species, mean, color = timeclass))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = timeclass), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Precipitation Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~model, ncol = 1, scales = "free_y")




ggplot(pft.summary [pft.summary $variable2 %in% "beta1",], aes(timeclass, mean, color = biome))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = biome), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Precipitation Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~model, ncol = 1, scales = "free_y")



# color by leaf type
ggplot(na.omit(pft.summary[pft.summary$variable2 %in% "beta1",]), aes(timeclass, mean, color = leaf))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = leaf), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Precipitation Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~model, ncol = 2, scales = "free_y")



# color by decidiousness
ggplot(na.omit(pft.summary[pft.summary$variable2 %in% "beta1",]), aes(timeclass, mean, color = decidious))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = decidious), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Precipitation Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~model, ncol = 2, scales = "free_y")


ggplot(na.omit(pft.summary[pft.summary$variable2 %in% "beta1",]), aes(timeclass, mean, color = model))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = model), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Precipitation Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~decidious, ncol = 2, scales = "free_y")


ggplot(na.omit(pft.summary[pft.summary$variable2 %in% "beta2",]), aes(timeclass, mean, color = model))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = model), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Tmax Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+facet_wrap(~species, ncol = 2, scales = "free_y")


# calculate the difference between climate sensitivities between guess and itrdb


# calculate mean differences between parameters:
# read in the full mcmc samples for the parameters:
ITRDB.params <- read.csv("outputs/ITRDB_models/ITRDB_GUESS_PFT_time_re/parameters_full_mcmc.csv")
GUESS.params <- read.csv("outputs/gwbi_model/LPJ_GUESS_time_re/parameters_full_mcmc.csv")

GUESS.params$model <- "LPJ-GUESS"
ITRDB.params$model <- "ITRDB"

# remove the .gwbi from the GUESS species
GUESS.params$species <- do.call(rbind, strsplit(as.character(GUESS.params$species),".gwbi"))
ITRDB.params$species <- as.character(ITRDB.params$species )

params <- rbind(GUESS.params, ITRDB.params)

params.m <- melt(params, id.vars = c("mcmc", "model", "species"))

# now get timeperiod and names of params
params.m$time.cd <- substring(params.m$variable, 7, 7)
params.m$timeclass <- ifelse(params.m$time.cd %in% "1", "Past", 
                                      ifelse(params.m$time.cd %in% "2", "Modern", "No re"))
params.m$variable2 <- substring(params.m$variable, 1,5)
colnames(params.m) <- c("mcmc", "model", "species", "parameter_full", "value", "time.cd", "timeclass", "parameter")


b1.class <- params.m %>% select(mcmc, model, species, parameter, value, timeclass) %>% group_by(model, mcmc,  species, parameter,timeclass) %>% mutate(grouped_id = row_number())
params.class <- b1.class %>% select (model, mcmc,  species, parameter, grouped_id, timeclass, value) %>% spread(timeclass, value) %>% select(-grouped_id)


params.class$pct_change <- ((params.class$Modern - params.class$Past))



params.diff <- params.class %>% group_by(model, species, parameter) %>% summarise(mean = mean(pct_change, na.rm=TRUE),
                                                         ci.low = quantile(pct_change, 0.025, na.rm=TRUE), 
                                                         ci.high = quantile(pct_change, 0.975, na.rm=TRUE))

params.diff$species2 <- ifelse(params.diff$species %in% c("BNE", "BINE"),"BNE/BINE", params.diff$species)

# while the actual parameter estimates vary across GUESS and ITRDB datasets, the direction of drought sensitivity differences between the two time periods are similar in some species. 

# plot average change in drought sensitivity

params.diff$model <- factor(params.diff$model, levels = c("LPJ-GUESS", "ITRDB"))
pct.drought.change <- ggplot(params.diff[params.diff$parameter %in% "beta1" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c('#d95f02', '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("drought sensitvity change \n between Modern and Past")+facet_wrap(~species2, scales = "free_y")

png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_drought_senstivity_ITRDB_GUESS_PFTS.png")
pct.drought.change
dev.off()

pct.temp.change <- ggplot(params.diff[params.diff$parameter %in% "beta2" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c('#d95f02', '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("June Tmax sensitvity change \n between Modern and Past")+facet_wrap(~species2, scales = "free_y")

png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_tmax_senstivity_ITRDB_GUESS_PFTS.png")
pct.temp.change
dev.off()


pct.lag1.change <- ggplot(params.diff[params.diff$parameter %in% "beta3" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c('#d95f02', '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("change in lag -1 parameter \n between Modern and Past")+facet_wrap(~species2, scales = "free_y")

png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_lag1_senstivity_ITRDB_GUESS_PFTS.png")
pct.lag1.change
dev.off()

pct.lag2.change <- ggplot(params.diff[params.diff$parameter %in% "beta4" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c('#d95f02', '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("change in lag -1 parameter \n between Modern and Past")+facet_wrap(~species2, scales = "free_y")

png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_lag2_senstivity_ITRDB_GUESS_PFTS.png")
pct.lag2.change
dev.off()

pct.lag3.change <- ggplot(params.diff[params.diff$parameter %in% "beta5" & !params.diff$species2 %in% c("Total", NA),], aes( x=model, y = mean, fill = model))+geom_bar(stat="identity")+geom_errorbar( aes(ymin = ci.low, ymax = ci.high, width = 0.25), size = 1.5, position = position_dodge(width=0.5))+
  scale_fill_manual(values = c('#d95f02', '#7570b3'))+theme_bw(base_size = 35)+theme(legend.position = "none", axis.title.x = element_blank(), panel.grid = element_blank())+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("change in lag -1 parameter \n between Modern and Past")+facet_wrap(~species2, scales = "free_y")

png(height = 9, width = 20, units = "in", res = 300, "outputs/itrdb_model_compare/pct_change_lag3_senstivity_ITRDB_GUESS_PFTS.png")
pct.lag3.change
dev.off()
