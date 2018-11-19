# Script to run mixed effects models on agbi & density:
library(rjags)
library(ggplot2)
library(caTools)
library(ggridges)
library(tidyr)
library(reshape2)
library(dplyr)


# read in model agbi, dens summaries:
all.df.yr <- readRDS("outputs/data/ED2/dens_agbi_climate_ED2.rds")
#dens.agbi <- readRDS( "outputs/data/ED2/ED2.agbi.dens.site.rds")


sec2yr <- 1*60*60*24*365.25
JJAmeans.ED <- readRDS("outputs/data/ED2/ED2.alldat.jjameans.rds")
JJAmeans.ED$Tair.C.jja <- JJAmeans.ED$Tair - 273.15
JJAmeans.ED$precip.mm.jja <- JJAmeans.ED$precip*(sec2yr*3/12)
colnames(JJAmeans.ED)

ED.all <- left_join(all.df.yr, JJAmeans.ED[,c("Year", "Site", "Tair.C.jja", "precip.mm.jja")], by = c("Year", "Site"))

#----------------------------------------
# Separate Testing and Training Datasets:

# 1. initial data cleaning & checking
# 2. create dummy variables for CO2 classes: "modern", "past", "pre-industrial"
# 3. calculate agbi lagged
# 4. remove NA values for lagged agbi
# 5. scale climate parameters
# 6. Split into testing and training data:

# remove NAS: 
ED <- ED.all[!is.na(ED.all$agbi) & !is.na(ED.all$precip.mm),]

# create classes for modern, past and pre-industrial:
ED$period <- ifelse(ED$Year <= 1850, "pre-industrial", 
                ifelse(ED$Year <= 1950 & ED$Year > 1850,"industrial-past",
                    ifelse(ED$Year >= 1950, "modern-industrial", "NA")))

ED$period_cd <- ifelse(ED$Year <= 1850, "3", 
                    ifelse(ED$Year <= 1950 & ED$Year > 1850,"2",
                           ifelse(ED$Year >= 1950, "1", "NA")))

# calculate lagged agbi:

# get previous years growth
uni.Sites <- unique(ED$Site)
ED.sort <- ED[with(ED, order(Site, Year)),]

ED.sort.wide <- ED.sort[,c("Year", "Site", "GS_gwbi")] %>% spread(key = "Site", value = "GS_gwbi")
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
colnames(ED.sort_1) <- c("Year", "Site", "gwbi_1")
ED.sort_2 <- melt(ED.sort_2.wide, id.vars = c("Year"))
colnames(ED.sort_2) <- c("Year", "Site", "gwbi_2")

ED.sort_3 <- melt(ED.sort_3.wide, id.vars = c("Year"))
colnames(ED.sort_3) <- c("Year", "Site", "gwbi_3")

ED.sort_4 <- melt(ED.sort_4.wide, id.vars = c("Year"))
colnames(ED.sort_4) <- c("Year", "Site", "gwbi_4")

ED.sort_5 <- melt(ED.sort_5.wide, id.vars = c("Year"))
colnames(ED.sort_5) <- c("Year", "Site", "gwbi_5")

ED.sort_lag <- left_join(ED.sort_1, ED.sort_2, by = c("Year", "Site"))
ED.sort_lag34 <- left_join(ED.sort_3, ED.sort_4, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_lag34, by = c("Year", "Site"))
ED.sort_lag <- left_join(ED.sort_lag, ED.sort_5, by = c("Year", "Site"))

ED.sort_lag <- left_join(ED.sort, ED.sort_lag, by = c("Year", "Site"))

# remove NAs from agbi_1 and agbi_2:

ED.sort_lag <- ED.sort_lag[!is.na(ED.sort_lag$gwbi_1) & !is.na(ED.sort_lag$gwbi_2) & !is.na(ED.sort_lag$gwbi_3) & !is.na(ED.sort_lag$gwbi_4) & !is.na(ED.sort_lag$gwbi_5),]

# scale the climate parameters:

ED.sort_lag$Precip.scaled = as.vector(scale(ED.sort_lag$precip.mm, center = TRUE, scale = TRUE))
ED.sort_lag$Temp.jja.scaled = as.vector(scale(ED.sort_lag$Tair.C.jja, center = TRUE, scale = TRUE))
ED.sort_lag$Temp.scaled = as.vector(scale(ED.sort_lag$Tair.C, center = TRUE, scale = TRUE))

# scale the CO2 parameters:
ED.sort_lag$CO2.scaled = as.vector(scale(ED.sort_lag$CO2, center = TRUE, scale = TRUE))
#ED.sort_lag$site_num <- 

#splits <- unlist(strsplit(unique(ED.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = unique(as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2))),
           site_code = 1:length(unique(as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2)))))

ED.sort_lag$site_num <- as.numeric(sapply(strsplit(ED.sort_lag$Site,"X"), `[`, 2))
ED.sort_lag <- left_join(ED.sort_lag, covert_site_codes, by = "site_num")

# split training and testing datasets:
# split training and testing datasets:
msk <- caTools::sample.split( ED.sort_lag, SplitRatio = 3/4, group = NULL )

train.full <- ED.sort_lag[msk,]
test.full <- ED.sort_lag[!msk,]

train.mod <- train.full[train.full$period %in% c("modern-industrial", "industrial-past"),]
test.mod <- test.full[test.full$period %in% c("modern-industrial", "industrial-past"),]

# -------------------------------------------
# Develop bayesian mixed models
#--------------------------------------------
library(mgcv)
mod <- lm(GS_gwbi ~ Precip.scaled + Temp.jja.scaled + CO2 + gwbi_1 +gwbi_2 +gwbi_3 +gwbi_4+gwbi_5 + Site, data = train.mod)
summary(mod)
#plot(mod)
preds <- predict(mod, test.mod)
plot(test.mod$GS_gwbi, preds)
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
Ypred[i]   ~ dnorm(gfunc.p[i], inv.var) # Y is agbi

# function g()
gfunc.p[i] <- alpha[sites.p[i]] + beta1[period.p[i]]*Precip.scaled.p[i] + beta2[period.p[i]]*Temp.jja.scaled.p[i] + beta3[period.p[i]]*agbi_1.p[i] + beta4[period.p[i]]*agbi_2.p[i] + beta5[period.p[i]]*agbi_3.p[i] + beta6[period.p[i]]*agbi_4.p[i]# use Drought index as a scaled variable 

}

}"






reg.model.by_period <- jags.model(textConnection(ED_re_site_time_period), 
                               data = list(Y = train.mod$GS_gwbi, n=length(train.mod$agbi), Precip.scaled = train.mod$Precip.scaled, Temp.jja.scaled = train.mod$Temp.jja.scaled, agbi_1 = train.mod$gwbi_1,agbi_2 = train.mod$gwbi_2, agbi_3 = train.mod$gwbi_3, agbi_4 = train.mod$gwbi_4,
                                           period = as.numeric(train.mod$period_cd), S = unique(train.mod$site_code),  C = unique(train.mod$period_cd), sites = train.mod$site_code, np=length(test.mod$period_cd), 
                                           sites.p = test.mod$site_code, Precip.scaled.p = test.mod$Precip.scaled, Temp.jja.scaled.p = test.mod$Temp.jja.scaled, agbi_1.p = test.mod$gwbi_1, agbi_2.p = test.mod$gwbi_2, agbi_3.p = test.mod$gwbi_3, agbi_4.p = test.mod$gwbi_4,
                                           period.p = as.numeric(test.mod$period_cd)), n.chains = 3, n.adapt = 100)


update(reg.model.by_period, 1000); # Burnin for 1000 samples to start, then go higher later

#samp.ED.period <- coda.samples(reg.model.by_period, 
 #                           variable.names=c("alpha","beta1", "beta2","beta3","beta3","sigma","sigma_alpha", "sigma_beta1", "sigma_beta2","sigma_beta3", "sigma_beta4"), 
  #                          n.chains = 3, n.iter=2000, thin = 10)
samp.ED.period <- coda.samples(reg.model.by_period, 
                              variable.names=c("alpha", "beta1", "beta2" ), 
                              n.chains = 3, n.iter=5000, thin = 1)

samp.ED.ypred <- coda.samples(reg.model.by_period, 
                               variable.names=c("Ypred" ), 
                               n.chains = 3, n.iter=5000, thin = 1)

#Extract the samples for each parameter

samps       <- samp.ED.period[[1]]
Yp.samps    <- samp.ED.ypred [[1]]
alpha.samps <- samps[,(length(test.mod$site_num)+1):(length(test.mod$site_num)+length(unique(test.mod$site_num)))]
beta.samps  <- samps[,(length(test.mod$site_num)+length(unique(test.mod$site_num))+1):(length(test.mod$site_num)+length(unique(test.mod$site_num))+3)]
sigma.samps <- samps[,(length(test.mod$site_num)+length(unique(test.mod$site_num))+4):(length(test.mod$site_num)+length(unique(test.mod$site_num))+7)]


# caluculate 95% CI for alpha and beta samps 
alpha.025 <- quantile(alpha.samps, 0.025)
alpha.975 <- quantile(alpha.samps, 0.975)
beta1.025 <- quantile(beta.samps[,1], 0.025)
beta1.975 <- quantile(beta.samps[,1], 0.975)
beta2.025 <- quantile(beta.samps[,2], 0.025)
beta2.975 <- quantile(beta.samps[,2], 0.975)



a <- data.frame('alpha' = alpha.samps, 'beta.precip' = beta.samps[,1], 'beta.temp' = beta.samps[,2])
colnames(a)<- c("alpha", "beta.precip", "beta.temp")
a$num <- rownames(a)
a.m <- melt(a, id.vars=c("num"))

# make dot + 95% CI plots for each param
a.mplots <- ggplot(a.m, aes(value, color = variable))+geom_density(alpha = 0.5)+theme_bw()

# plot predicted vs. observed
Yp.samps <- data.frame(Yp.samps) 
Yp.m <- melt(Yp.samps)
Yp.summary <- Yp.m %>% group_by(variable) %>% dplyr::summarise(Predicted = mean(value),
                                                               ci.hi = quantile(value,0.975),
                                                               ci.lo = quantile(value,0.025))
Yp.summary$Observed <- test.mod$GS_gwbi

pred.obs <- summary(lm(colMeans(Yp.samps) ~ test.mod$GS_gwbi))

p.o.plot <- ggplot(Yp.summary, aes(Observed, Predicted))+geom_point(color = "black", size = 0.5)+geom_errorbar(data = Yp.summary,aes(ymin=ci.lo, ymax=ci.hi), color = "grey", alpha = 0.5)+geom_point(data = Yp.summary, aes(Observed, Predicted), color = "black", size = 0.5)+geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+ylim(-0.35, 1.5)+xlim(-0.35,1.5)+geom_text(data=data.frame(pred.obs$r.squared), aes( label = paste("R^2: ", pred.obs$r.squared, sep="")),parse=T,x=1, y=7)

# note poor model fit!
#png(width = 6, height = 5, units = "in", res = 300, "outputs/growth_model/basic_reg/pred_vs_obs.png")
p.o.plot
#dev.off()

# calculate MSE & BIAS:

MSE1   <- mean((colMeans(Yp.samps)-test.mod$GS_gwbi)^2)
BIAS1  <- mean(colMeans(Yp.samps)-test.mod$GS_gwbi)

# write model summary output to a file!

model.summary <- data.frame(model = "mixed_effects_reg", 
                            MSE = MSE1, 
                            BIAS = BIAS1, 
                            Rsq = pred.obs$r.squared)
