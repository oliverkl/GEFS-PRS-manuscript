library(tidyverse)

concordant_database <- readRDS("pairwise_database.20231127.rds") %>% 
  filter(diff_group == 0) 

discordant_database <- readRDS("pairwise_database.20231127.rds") %>% 
  filter(diff_group != 0) 

## Switch signs

# Randomly re-assign +/- to model different relative minus the other for each pair

meanObt <- mean(concordant_database$prs_diff_all)
difference <- concordant_database$prs_diff_all

nreps <- 9066
set.seed(1086)

resampMeanDiff <- numeric(nreps)
for (i in 1:nreps) {
       signs <- sample( c(1,-1),length(difference), replace = T)
       resamp <- difference * signs
       resampMeanDiff[i] <- mean(resamp)
       }

# How representative was the mean we were using initially?

#original mean
concordant_database %>% 
  summarise(mean = mean(prs_diff_all))

#resampled distribution
summary(resampMeanDiff)

#store optimal mean as determined by simulations
opt_mean <- -0.0004332

# Re-run the simulation and compare output values with optimal mean.

difference <- concordant_database$prs_diff

nreps <- 10000
set.seed(1086)

resampMeanDiff <- numeric(nreps)
for (i in 1:nreps) {
       signs <- sample( c(1,-1),length(difference), replace = T)
       resamp <- difference * signs
       resampMeanDiff[i] <- mean(resamp)
       }

resampMeanDiff.df <- as.data.frame(resampMeanDiff)

opt_run <- which.min(abs(opt_mean - resampMeanDiff.df$resampMeanDiff))

# re-run simulations stopping at the optimal run

nreps2 <- opt_run
set.seed(1086)

resampMeanDiff2 <- numeric(nreps2)
for (i in 1:nreps2) {
       signs <- sample( c(1,-1),length(difference), replace = T)
       resamp2 <- difference * signs
       resampMeanDiff2[i] <- mean(resamp2)
       }

new_prs_diff <- as.data.frame(resamp2)

# check it worked
mean(new_prs_diff$resamp2)

# new_prs_diff$resamp2 becomes the new prs_diff for the concordant group (total cohort)

concordant_database2 <- cbind(concordant_database, new_prs_diff) 

concordant_database3 <- concordant_database2 %>% 
  mutate(prs_diff = resamp2) %>% 
  select(-(resamp2))

# rejoin with discordant pair data
total_database <- rbind(disconcordant_database, concordant_database3)

total_database %>% 
  group_by(diff_group, known_mutn.x) %>% 
  summarise(mean = mean(prs_diff))

saveRDS(total_database, "pairwise_database_optimised.20230803.rds")
