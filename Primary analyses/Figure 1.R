# Figure 1

# Ridges plot of score distributions

sd <- sd(gefsplus_database_nocntrl$`allEpi3_0.5.norm`)
mean <- mean(gefsplus_database_nocntrl$`allEpi3_0.5.norm`)

Epilepsy <- factor(gefsplus_database_nocntrl$outcome2, levels = c(0 , 1), labels = c("No", "Yes"))

p1 <- ggplot(gefsplus_database_nocntrl, aes(x = `allEpi3_0.5.norm`, y = Epilepsy, color = Epilepsy, fill = Epilepsy)) + 
  stat_density_ridges(alpha = 0.2, show.legend = TRUE, bandwidth = 0.5,
                      quantile_lines = TRUE, quantile_fun=function(x,...)mean(x)) + 
  labs(title ="A", x = "Polygenic Risk Score\n(Units of SD)", y = "Density") +
  theme_ridges(center_axis_labels = TRUE) + # simpler theme
  scale_fill_manual(values = c("gray30", "coral3")) +
  scale_color_manual(values = c("black", "coral3")) +
  scale_y_discrete(expand = expansion(add = c(0, 1.9))) +
  coord_cartesian(xlim = c(-4, 4)) +
  theme(
    plot.margin = unit(c(0,2,0,0), "cm"),
    legend.position = c(0.95, 0.95),
    legend.title = element_text(size = 9, family = "URWHelvetica"),
    legend.text = element_text(size = 6, family = "URWHelvetica"),
    axis.text.x = element_text(colour="black", size = 6, family = "URWHelvetica"),
    axis.text.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y = element_text(color = "black", size = 9, margin = margin(0,0,10), family = "URWHelvetica"),
    axis.title.x = element_text(color = "black", size = 6, margin = margin(-20), family = "URWHelvetica"),
    plot.title = element_text(color = "black", size = 9, margin=margin(0,0,20), face="plain", family = "URWHelvetica"), 
    plot.title.position = "plot"
  ) +
  guides(fill = guide_legend(reverse = TRUE)) + guides(color = guide_legend(reverse = TRUE))


p1

p2 <- gefsplus_database %>%  
  filter(severity %in% c(1,2,3,4,5)) %>% 
  ggerrorplot(x = "outcome2", y = "allEpi3_0.5.norm", desc_stat = "mean_ci", size = 0.8, color = "outcome2",
              order = c("0", "1")) +
  ggtitle("B") +
  labs(y = "PRS mean +/- 95% CI", x = " ") +
  theme_minimal() +
  scale_fill_manual(values = c("black", "coral3")) +
  scale_color_manual(values = c("black", "coral3")) +
  theme(legend.position = "right", text = element_text(size=16)) +
  scale_x_discrete(labels = c("Unaffected \nrelatives \nN = 149", "Affected \nrelatives \nN = 155")) +
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
  geom_signif(comparisons = list(c(1,2)), annotations = "list(italic(Padj)~'='~0.04)", parse = TRUE, y_position = 0.55, tip_length = 0.004, color = "black", family = "URWHelvetica", textsize = 2.5) + 
  geom_hline(yintercept=0, linetype = "solid", 
                color = "black", linewidth=0.8)

p1 + ggpar(p2, ylim = c(-0.15, 0.65), ticks = FALSE)

tiff("Fig1_GEFS+_PRS.tiff", units="cm", width=17, height=7, res=300)
p1 + ggpar(p2, ylim = c(-0.15, 0.65), ticks = FALSE)
dev.off()

