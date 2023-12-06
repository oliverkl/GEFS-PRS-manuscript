library(patchwork)
library(ggpubr)
library(ggridges)

pairwise_database <- readRDS("pairwise_database_optimised.20230803.rds")

## Figure 2

p0 <- pairwise_database %>%  
  ggerrorplot(x = "sev_diff", y = "prs_diff", desc_stat = "mean_ci", size = 0.8, color = "sev_diff",
              order = c("no", "yes")) +
  ggtitle("A") +
  labs(y = "Mean PRS difference", x = " ") +
  theme_minimal() +
  scale_fill_manual(values = c("black", "black")) +
  scale_color_manual(values = c("black", "black")) +
  theme(legend.position = "right", text = element_text(size=16)) +
  scale_x_discrete(labels = c("Concordant \npairs \nN = 784", "Discordant \npairs \nN = 1615")) +
  font("ylab", size = 6, family = "URWHelvetica") +
  font("xlab", size = 6, family = "URWHelvetica") +
  theme(
    plot.margin = unit(c(0,0.5,0,0), "cm"),
    legend.position = "none",
    axis.text.x = element_text(colour="black", size = 6, margin = margin(10, 0, 0), family = "URWHelvetica"),
    axis.text.y = element_text(colour="black", size = 6, family = "URWHelvetica"),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(color = "black", size = 9, margin = margin(0,15,0), family = "URWHelvetica"),
    plot.title = element_text(color = "black", size = 9, margin=margin(0,0,20), family = "URWHelvetica"), 
    plot.title.position = "plot"
  ) +
  geom_signif(comparisons = list(c(1,2)), annotations = "list('*')", parse = TRUE, y_position = 0.5, tip_length = 0.004, color = "black", family = "URWHelvetica", textsize = 2.5)

p1 <- pairwise_database %>%  
  ggerrorplot(x = "diff_group", y = "prs_diff", desc_stat = "mean_ci", size = 0.8, color = "diff_group",
              order = c("0", "1", "2", "3", "4")) +
  ggtitle("B") +
  labs(y = "Mean PRS difference", x = " ") +
  theme_minimal() +
  scale_fill_manual(values = c("black", "black", "black", "black", "black")) +
  scale_color_manual(values = c("black", "black", "black", "black", "black")) +
  theme(legend.position = "right", text = element_text(size=16)) +
  scale_x_discrete(labels = c("0-grade \npairs \nN = 784", "1-grade \npairs \nN = 659", "2-grade \npairs \nN = 484",
                              "3-grade \npairs \nN = 436", "4-grade \npairs \nN = 36")) +
  font("ylab", size = 6, family = "URWHelvetica") +
  font("xlab", size = 6, family = "URWHelvetica") +
  theme(
    plot.margin = unit(c(0,0.5,0,0), "cm"),
    legend.position = "none",
    axis.text.x = element_text(colour="black", size = 6, margin = margin(10, 0, 0), family = "URWHelvetica"),
    axis.text.y = element_text(colour="black", size = 6, family = "URWHelvetica"),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(color = "black", size = 9, margin = margin(0,15,0), family = "URWHelvetica"),
    plot.title = element_text(color = "black", size = 9, margin=margin(0,0,20), family = "URWHelvetica"), 
    plot.title.position = "plot"
  ) +
  geom_signif(comparisons = list(c(1,4), c(1,2), c(1,3), c(1,5)), annotations = c("list('***')", "ns", "ns", "ns"), parse = TRUE, y_position = c(0.6, 0.25, 0.45, 0.75), tip_length = 0.004, color = c("black", "black", "black", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey","darkgrey", "darkgrey"), family = "URWHelvetica", textsize = 2.5)

tiff("Fig2_GEFS+_PRSdiff.tiff", units="cm", width=17, height=7, res=300)
ggpar(p0, ylim = c(-0.45, 0.8), ticks = FALSE) + ggpar(p1, ylim = c(-0.45, 0.8), ticks = FALSE)
dev.off()

## Figure 3A

p2 <- scn1b_gabrg2_combined %>% 
  mutate(variant_cohort = fct_relevel(variant_cohort, 
            "SCN1B p.Cys121Trp", "GABRG2 p.Arg82Gln")) %>% 
  ggerrorplot(x = "sev_diff", y = "prs_diff", desc_stat = "mean_ci", size = 0.8, color = "variant_cohort",
              order = c("no", "yes")) +
  ggtitle("A") +
  labs(y = "Mean PRS difference", x = " ") +
  theme_minimal() +
  scale_fill_manual(values = c("#7CA1CC", "#E57A77")) +
  scale_color_manual(values = c("#7CA1CC", "#E57A77")) +
  theme(legend.position = "bottom", text = element_text(size=14)) +
  scale_x_discrete(labels = c("Concordant \npairs", "Discordant \npairs")) +
  font("ylab", size = 6, family = "URWHelvetica") +
  font("xlab", size = 6, family = "URWHelvetica") +
  theme(
    plot.margin = unit(c(0,0.25,0,0), "cm"),
    legend.title = element_blank(),
    legend.text = element_text(colour="black", size = 6, margin = margin(0, 0, 0), family = "URWHelvetica"),
    axis.text.x = element_text(colour="black", size = 6, margin = margin(5, 0, 0), family = "URWHelvetica"),
    axis.text.y = element_text(colour="black", size = 6, family = "URWHelvetica"),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(color = "black", size = 9, margin = margin(0,5,0), family = "URWHelvetica"),
    plot.title = element_text(color = "black", size = 9, margin=margin(0,0,5), family = "URWHelvetica"), 
    plot.title.position = "plot"
  )

tiff("Figure3A.tiff", units="cm", width=8, height=12, res=300)
p2
dev.off()

## Figure 3BC

#SCN1B 

p3 <- scn1b_gabrg2_combined %>% 
 filter(variant_cohort == "SCN1B p.Cys121Trp") %>% 
  ggerrorplot(x = "diff_group", y = "prs_diff", desc_stat = "mean_ci", size = 0.8, color = "variant_cohort",
              order = c("0", "1", "2", "3", "4")) +
  ggtitle("C") +
  labs(y = "Mean PRS difference", x = " ") +
  theme_minimal() +
  scale_fill_manual(values = c("#7CA1CC")) +
  scale_color_manual(values = c("#7CA1CC")) +
  scale_x_discrete(labels = c("0-grade \npairs \nN=176",
    "1-grade \npairs \nN=284",
                            "2-grade \npairs \nN=204",
                            "3-grade \npairs \nN=136",
                            "4-grade \npairs \nN=20")) +
  font("ylab", size = 6, family = "URWHelvetica") +
  font("xlab", size = 6, family = "URWHelvetica") +
  theme(
    plot.margin = unit(c(0,0.2,0,0), "cm"),
    legend.title = element_blank(),
    legend.text = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(colour="black", size = 6, margin = margin(0, 0, 0), family = "URWHelvetica"),
    axis.text.y = element_text(colour="black", size = 6, family = "URWHelvetica"),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(color = "black", size = 9, margin = margin(0,5,0), family = "URWHelvetica"),
    plot.title = element_text(color = "black", size = 9, margin=margin(0,0,5), family = "URWHelvetica"), 
    plot.title.position = "plot"
  ) +
  geom_signif(comparisons = list(c(1,4), c(1,2), c(1,3), c(1,5)), 
              annotations = c("ns", "ns", "ns", "ns"), parse = TRUE, y_position = c(1.6, 0.8, 1.25, 1.9), 
              tip_length = 0.004, 
              color = c("darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey", "darkgrey","darkgrey", "darkgrey"), 
              family = "URWHelvetica", textsize = 2.0)

# GABRG2

p4 <- scn1b_gabrg2_combined %>% 
 filter(variant_cohort == "GABRG2 p.Arg82Gln") %>% 
  ggerrorplot(x = "diff_group", y = "prs_diff", desc_stat = "mean_ci", size = 0.8, color = "variant_cohort",
              order = c("0", "1", "2", "3", "4")) +
  ggtitle("B") +
  labs(y = "Mean PRS difference", x = " ") +
  theme_minimal() +
  scale_fill_manual(values = c("#E57A77")) +
  scale_color_manual(values = c("#E57A77")) +
  scale_x_discrete(labels = c("0-grade \npairs \nN=128",
    "1-grade \npairs \nN=132",
                            "2-grade \npairs \nN=119",
                            "3-grade \npairs \nN=108",
                            "4-grade \npairs \nN=9")) +
  font("ylab", size = 6, family = "URWHelvetica") +
  font("xlab", size = 6, family = "URWHelvetica") +
  theme(
    plot.margin = unit(c(0,0.2,0,0), "cm"),
    legend.title = element_blank(),
    legend.text = element_blank(),
        legend.position = "none",
    axis.text.x = element_text(colour="black", size = 6, margin = margin(0, 0, 0), family = "URWHelvetica"),
    axis.text.y = element_text(colour="black", size = 6, family = "URWHelvetica"),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(color = "black", size = 9, margin = margin(0,5,0), family = "URWHelvetica"),
    plot.title = element_text(color = "black", size = 9, margin=margin(0,0,5), family = "URWHelvetica"), 
    plot.title.position = "plot"
  ) +
  geom_signif(comparisons = list(c(1,4), c(1,2), c(1,3), c(1,5)), annotations = c("'***'", "ns", "'**'", "'*'"), 
              parse = TRUE, y_position = c(1.5, 0.6, 1.1, 2.0), tip_length = 0.004, 
              color = c("black", "black", "black", "darkgrey", "darkgrey", "darkgrey", "black", "black", "black", "black","black", "black"), 
              family = "URWHelvetica", textsize = 2.0)


tiff("Figure3BC.tiff", units="cm", width=8, height=12, res=300)
ggpar(p4, ylim = c(-1.0, 2.1), ticks = FALSE) / ggpar(p3, ylim = c(-1.0, 2.2), ticks = FALSE)
dev.off()
