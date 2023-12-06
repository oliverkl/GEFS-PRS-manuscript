library(lme4)
library(lmerTest)

primary <- glmer(outcome2 ~ `allEpi3_0.5.norm` + SEX + cohort + FS + RV 
                                + PC1.std + PC2.std + PC3.std + PC4.std + PC5.std +
                     (1|PFN2), family = binomial, data = gefsplus_database_nocntrl)

summary(primary)

#calc OR
cc <- confint(primary,parm="beta_", method="Wald")
ctab <- cbind(est=fixef(primary),cc)
rtab <- exp(ctab)
print(rtab,digits=3)
