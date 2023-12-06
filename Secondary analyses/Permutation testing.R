library(lme4)
library(lmerTest)

pairwise_database <- readRDS("pairwise_database_optimised.20230803.rds") 

fam_cnt <- pairwise_database %>% 
  ungroup() %>% 
  count(PFN2)

pairwise_database <- pairwise_database %>% 
  left_join(fam_cnt, by = "PFN2")

### Concordant versus discordant

# LMM original

est.obs <- lme4::lmer(prs_diff ~ as.factor(sev_diff) + Kinship + (1|PFN2),
                      data = pairwise_database)

# store original observed estimate statistic to compare to all permuted examples
orig.mod <- lmerTest:::get_coefmat(est.obs) 

comp01.orig <- orig.mod["as.factor(sev_diff)yes", "Estimate"]

data.frame("comp01" = orig.mod["as.factor(sev_diff)yes", "Estimate"])

# Rerun the test on a new shuffled data set
# at 10,000 iterations this takes too to run....
start <- Sys.time()

# set seed to ensure result reproducible
set.seed(100)

# Initialize the counter
perm <- 0

# Create destination frame for permuted results so can determine 
# sampling/null distribution

est.star.dist <- data.frame("comp01" = 0)
est.star.dist <- est.star.dist[0, ]

# While loop
while (perm < 10000) { 
  # Update counter
  perm <- perm + 1
  
  # Shuffle/permute the match indicator with replacement
  
  pairwise_database_n1 = pairwise_database %>%
    filter(n == 1) %>% 
    mutate(sev_diff_shuffle = sev_diff) %>% 
    ungroup()
  
  pairwise_database_n2 = pairwise_database %>%
    filter(n != 1) %>% 
    group_by(PFN2) %>% 
    mutate(sev_diff_shuffle = sample(sev_diff, replace = FALSE)) %>% 
    ungroup()
    
  pairwise_database_n3 = rbind(pairwise_database_n1, pairwise_database_n2)

  est.result1 <- lmer(prs_diff ~ as.factor(sev_diff_shuffle) + 
                                Kinship + (1|PFN2),
                      data = pairwise_database_n3)

  est.result2 <- lmerTest:::get_coefmat(est.result1) 
  
  est.result3 <- data.frame("perm" = perm, 
                  "comp01" = est.result2["as.factor(sev_diff_shuffle)yes", "Estimate"])
  
  # Append new results to previous iterations
  est.star.dist <- rbind(est.star.dist, est.result3)
}

end <- Sys.time()
difftime(end, start, units="mins")

# calculate p-value - proportion of permuted Estimates greater than the observed Estimate

# estimate 1
sum(est.star.dist$comp01 > abs(comp01.orig)) / length(est.star.dist$comp01)

### Concordant versus all discordant groups

# LMM
est.obs <- lme4::lmer(prs_diff ~ as.factor(diff_group) + Kinship + (1|PFN2),
                      data = pairwise_database)

summary(est.obs)

# store original observed estimate statistic to compare to all permuted examples
orig.mod <- lmerTest:::get_coefmat(est.obs) 

comp01.orig <- orig.mod["as.factor(diff_group)1", "Estimate"]
comp02.orig <- orig.mod["as.factor(diff_group)2", "Estimate"]
comp03.orig <- orig.mod["as.factor(diff_group)3", "Estimate"]
comp04.orig <- orig.mod["as.factor(diff_group)4", "Estimate"]

data.frame("comp01" = orig.mod["as.factor(diff_group)1", "Estimate"],
               "comp02" = orig.mod["as.factor(diff_group)2", "Estimate"],
               "comp03" = orig.mod["as.factor(diff_group)3", "Estimate"],
           "comp04" = orig.mod["as.factor(diff_group)4", "Estimate"])

# Rerun the test on a new shuffled data set
# at 100,000 iterations this takes too to run....
start <- Sys.time()

# set seed to ensure result reproducible
set.seed(1000)

# Initialize the counter
perm <- 0

# Create destination frame for permuted results so can determine 
# sampling/null distribution

est.star.dist <- data.frame("comp01" = 0, "comp02" = 0, "comp03" = 0,
                            "comp04" = 0)
est.star.dist <- est.star.dist[0, ]

# While loop
while (perm < 1000) { 
  # Update counter
  perm <- perm + 1
  
  # Shuffle/permute the match indicator with replacement
  
  pairwise_database_n1 = pairwise_database %>%
    filter(n == 1) %>% 
    mutate(diff_group_shuffle = diff_group) %>% 
    ungroup()
  
  pairwise_database_n2 = pairwise_database %>%
    filter(n != 1) %>% 
    group_by(PFN2) %>% 
    mutate(diff_group_shuffle = sample(diff_group, replace = FALSE)) %>% 
    ungroup()
    
  pairwise_database_n3 = rbind(pairwise_database_n1, pairwise_database_n2)

  est.result1 <- lmer(prs_diff ~ as.factor(diff_group_shuffle) + 
                                Kinship + (1|PFN2),
                      data = pairwise_database_n3)

  est.result2 <- lmerTest:::get_coefmat(est.result1) 
  
  est.result3 <- data.frame("perm" = perm, 
                                  "comp01" = est.result2["as.factor(diff_group_shuffle)1", "Estimate"],
                                  "comp02" = est.result2["as.factor(diff_group_shuffle)2", "Estimate"],
                                 "comp03" = est.result2["as.factor(diff_group_shuffle)3", "Estimate"],
                            "comp04" = 
est.result2["as.factor(diff_group_shuffle)4", "Estimate"])
  
  # Append new results to previous iterations
  est.star.dist <- rbind(est.star.dist, est.result3)
}

end <- Sys.time()
difftime(end, start, units="mins")

# calculate p-value - proportion of permuted Estimates greater than the observed Estimate

# estimate 1
sum(est.star.dist$comp01 > abs(comp01.orig)) / length(est.star.dist$comp01)
# estimate 2
sum(est.star.dist$comp02 > abs(comp02.orig)) / length(est.star.dist$comp02)
# estimate 3
sum(est.star.dist$comp03 > abs(comp03.orig)) / length(est.star.dist$comp03)
# estimate 4
sum(est.star.dist$comp04 > abs(comp04.orig)) / length(est.star.dist$comp04)
