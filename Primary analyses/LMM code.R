library(lme4)
library(lmerTest)

# PRS models tested:
## `allEpi_0.5.PRS.norm`
## `ggeEpi_0.5.PRS.norm`
## `focalEpi_0.5.PRS.norm`
## `fs_0.5.PRS.norm`

primary <- glmer(outcome2 ~ `allEpi_0.5.PRS.norm` + SEX + cohort + FS + RV 
                                + PC1.std + PC2.std + PC3.std + PC4.std + PC5.std +
                     (1|PFN2), family = binomial, data = gefsplus_database)

summary(primary)

# calc OR
cc <- confint(primary,parm="beta_", method="Wald")
ctab <- cbind(est=fixef(primary),cc)
rtab <- exp(ctab)
print(rtab,digits=3)

# correct for multiple testing
p_vals <- c(0.01,0.04,0.11,0.31)
p.adjust (p_vals, method="bonferroni")
