
library(caTools)
#library(ggridges)
#library(tidyr)
#library(reshape2)
library(dplyr)
library(nimble)
# preliminary models of tree growth for ITRDB data:
rwl.itrdb.clim.nona <- readRDS( paste0(getwd(),"/Data/ITRDB/full.clim.prism.rds"))


# split training and testing data:


rwl.itrdb.clim.nona$Precip.scaled = as.vector(scale(rwl.itrdb.clim.nona$ppt_MAP.wy, center = TRUE, scale = TRUE))
rwl.itrdb.clim.nona.Precip.scaled = scale(rwl.itrdb.clim.nona$ppt_MAP.wy, center = TRUE, scale = TRUE)

rwl.itrdb.clim.nona$Temp.jun.scaled = as.vector(scale(rwl.itrdb.clim.nona$tmax_06, center = TRUE, scale = TRUE))
rwl.itrdb.clim.nona.jun.scaled = scale(rwl.itrdb.clim.nona$tmax_06, center = TRUE, scale = TRUE)

# read in the conversion table for ITRDB species:
taxa.trans <- read.csv("Data/ITRDB/SPEC.CODE.TAXA.TRANSLATION.csv", stringsAsFactors = FALSE)
rwl.itrdb.clim.pft <- left_join(rwl.itrdb.clim.nona, taxa.trans, by = "SPEC.CODE")

rwl.itrdb.clim.pft$RWI <- as.numeric(rwl.itrdb.clim.pft$RWI) # why is rwi numeric

#splits <- unlist(strsplit(unique(ED.sort_lag$Site), "X"))
covert_site_codes <- data.frame(site_num = 1:length(unique(rwl.itrdb.clim.pft$studyCode)),
                                studyCode = unique(rwl.itrdb.clim.pft$studyCode))

covert_spec_codes <- data.frame(spec = 1:length(unique(rwl.itrdb.clim.pft$ED.PFT)),
                                ED.PFT = unique(rwl.itrdb.clim.pft$ED.PFT))


rwl.itrdb.clim.pft1 <- left_join(rwl.itrdb.clim.pft, covert_site_codes, by = "studyCode")
rwl.itrdb.clim.nona <- left_join(rwl.itrdb.clim.pft1, covert_spec_codes, by = "ED.PFT")


# clean up the data and split testing and training:
rwl.full <- rwl.itrdb.clim.nona[!is.na(rwl.itrdb.clim.nona$RWI_1) & !is.na(rwl.itrdb.clim.nona$RWI_2)  ,]
rwl.full$RWI <- as.numeric(rwl.full$RWI)
rwl.full$RWI_1 <- as.numeric(rwl.full$RWI_1)
rwl.full$RWI_2 <- as.numeric(rwl.full$RWI_2)
rwl.full$Age <- as.numeric(rwl.full$Age)

# also get rid of 0 values??
rwl.full <- rwl.full[!rwl.full$RWI == 0, ]
head(rwl.full)

# save rwl.full with GUESS PFTS:
saveRDS(rwl.full, "Data/ITRDB/full.clim.prism.ED.PFTS.rds")

# develop function to split testing and training datasets by species:
split.test.train.spec <- function( spec){

      spec.full <- rwl.full[rwl.full$ED.PFT %in% spec,]
      
      spec.full$spec <- ifelse(spec.full$ED.PFT %in% spec, 1, 2)
      
      covert_site_codes.spec <- data.frame(site_num.spec = 1:length(unique(spec.full$studyCode)),
                                           studyCode = unique(spec.full$studyCode))
      
      spec.df <- left_join(spec.full, covert_site_codes.spec, by = "studyCode")
      
      msk <- caTools::sample.split( spec.df, SplitRatio = 3/4, group = NULL )
      
      train.spec <- spec.df[msk,]
      test.spec <- spec.df[!msk,]
      
      
      saveRDS(test.spec, paste0("outputs/ITRDB_models/train_test_data/ED_PFT_train_", spec, "_nimble.rds"))
      saveRDS(test.spec, paste0("outputs/ITRDB_models/train_test_data/ED_PFT_test_", spec, "_nimble.rds"))

      cat(spec)
}


spec.list  <- as.character( unique(rwl.full$ED.PFT))


for(i in 1:length(spec.list)){
  split.test.train.spec(spec.list[i])
}


