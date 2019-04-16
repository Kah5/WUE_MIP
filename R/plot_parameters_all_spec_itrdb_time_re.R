library(ggplot2)
library(dplyr)
library(tidyr)
library(coda)
library(reshape2)
library(cowplot)

# read in all the samples:
filenames <- list.files(path = "outputs/ITRDB_models/ITRDB_species_time_re", pattern = "*.rds")
full.filenames <- paste0("outputs/ITRDB_models/ITRDB_species_time_re/", filenames)

spec <- substring(filenames, 7, 10)
# use lapply to read in the rds files:
all.params <- lapply(full.filenames, readRDS)


# get summaries of all the parameters:
all.params.chain1 <- lapply(all.params, function(x){as.mcmc(x[[1]])})
all.summaries <- lapply(all.params.chain1, summary)

names(all.params.chain1) <- spec

# save traceplots
for(i in 1:length(spec)){
  png(height = 6, width = 12, units = "in", res = 200, paste0("outputs/ITRDB_models/ITRDB_species_time_re/traceplot_", spec[i],".png"))
  par(mfrow = c(3,4))
  traceplot(all.params.chain1[[i]])
  dev.off()
}

# save acfplots:acfplots are based off of trellis, so it wont save yet
for(i in 1:length(spec)){
  #png(height = 6, width = 12, units = "in", res = 200, paste0("outputs/ITRDB_models/ITRDB_species_time_re/acfplot_", spec[i],".png"))
  acf  <- acfplot(all.params.chain1[[i]])
  ggsave(paste0("outputs/ITRDB_models/ITRDB_species_time_re/acfplot_", spec[i],".png"), plot = acf)
 
  #dev.off()
}


# plot paramter estimates alltogether
for(i in 1:length(spec)){
  
  ests <- data.frame(all.params.chain1[[i]])
  ests.summary <- data.frame(mean = apply(ests, 2, mean), 
             Ci.low = apply(ests, 2, function(x){quantile(x, 0.025)}), 
             Ci.high = apply(ests, 2, function(x){quantile(x, 0.975)}))
  
  ests.summary$params <- rownames(ests.summary)                                                                 
  acf <- ggplot(ests.summary[!ests.summary$params %in% "sigmaY",], aes(params, mean ))+
    geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
    ylab("Parameter Estimates")+xlab("parameter")+theme_bw(base_size = 15)+theme(panel.grid = element_blank())
  
 ggsave( paste0("outputs/ITRDB_models/ITRDB_species_time_re/parameter_ests_", spec[i],".png"), plot = acf)
  

}

# now save and plot all the paramter estimates together to compare across species:

all.param.ests <- do.call(rbind, all.params.chain1)

all.param.ests <- data.frame(all.param.ests)
all.param.ests$species <- rep(names(all.params.chain1), sapply(all.params.chain1, nrow)) # add the species code names


# then make boxplots or 95% quantile plots of the parameter estimates
all.param.ests.m <- melt(all.param.ests)
all.param.summary <- all.param.ests.m %>% group_by(species, variable) %>% dplyr::summarise(mean = mean(value), 
                                                                    Ci.low = quantile(value, 0.025), 
                                                                    Ci.high = quantile(value, 0.975))

all.param.summary$time.cd <- substring(all.param.summary$variable, 7, 7)
all.param.summary$timeclass <- ifelse(all.param.summary$time.cd %in% "1", "Past", 
       ifelse(all.param.summary$time.cd %in% "2", "Modern", "No re"))
all.param.summary$variable2 <- substring(all.param.summary$variable, 1,5)
# still need to: assign colors based on Paleon species

INTERCEPTS <- ggplot(all.param.summary[all.param.summary$variable %in% "alpha",], aes(species, mean))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Species intercept Alpha Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

MAPS <- ggplot(all.param.summary[all.param.summary$variable2 %in% "beta1",], aes(species, mean, color = timeclass))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = timeclass), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Precipitation Sensitivity(Beta1) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))


JUNTMAX <- ggplot(all.param.summary[all.param.summary$variable2 %in% "beta2",], aes(species, mean, color = timeclass))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = timeclass), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Jun Tmax Sensitivity (Beta2) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

PREVRWI_1 <- ggplot(all.param.summary[all.param.summary$variable2 %in% "beta3",], aes(species, mean, color = timeclass))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = timeclass), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("PrevRWI_1 (Beta3) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

PREVRWI_2 <- ggplot(all.param.summary[all.param.summary$variable2 %in% "beta4",], aes(species, mean, color = timeclass))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = timeclass), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("PrevRWI_1 (Beta4) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

AGE <- ggplot(all.param.summary[all.param.summary$variable2 %in% "beta5",], aes(species, mean, color = timeclass))+geom_point()+
  geom_point()+geom_errorbar(aes(min = Ci.low, max = Ci.high, color = timeclass), width = 0.1)+geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed")+
  ylab("Age (Beta5) Estimate")+xlab("Species")+theme_bw(base_size = 15)+theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

png(height = 18, width = 16, units = "in", res = 300, "outputs/ITRDB_models/ITRDB_species_time_re/All_spec_parameters_by_time.png")
cowplot::plot_grid(INTERCEPTS, MAPS, JUNTMAX, PREVRWI_1, PREVRWI_2, AGE, align = "hv",ncol = 2, labels = "AUTO")
dev.off()



# ---------Plot predicted vs. observed for each test.dataset

# ---------Plot posterior predictive response of each species to temperature & precipitation pre & post:

