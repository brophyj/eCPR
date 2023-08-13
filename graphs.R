############
# graphs
# need to first run brms.identity.R to create the  models
###########

library(ggplot2)
library(ggdist)
library(patchwork)
# colours
clrs_saguaro <- NatParksPalettes::natparks.pals("Saguaro")
clr_grp20 <- clrs_saguaro[1]
clr_grp21 <- clrs_saguaro[6]
clr_diff <- clrs_saguaro[4]

#######################
#
# ITT models
#
#######################

# graph of INCEPTION with vague priors
draws <- as_draws_df(m) %>% 
  # rename and drop the unneeded columns
  transmute(p0 = b_grp2cCPR,
            p1 = b_grp2eCPR) %>% 
  # compute the OR
  mutate(or = (p1 / (1 - p1)) / (p0 / (1 - p0)),
         dd = p1 - p0)


fig1a <- draws %>% 
  ggplot(aes(x = dd*100)) +
  stat_halfeye(point_interval = mean_qi, .width = .95) +
  scale_y_continuous(name="Density", breaks = NULL) +
  xlab("Success risk difference (RD) eCPR - cCPR (%)") +
  stat_halfeye(aes(fill = after_stat(abs(x) < 1))) +
  scale_fill_manual(values = c("#CD8A39", "#57643C")) +
  guides(fill = "none") + 
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = .75)) +
  labs(title = "A) INCEPTION with vague prior", caption = "P(superiority) = area under the curve (AUC) to right of vertical line at RD = 0 = 71.2%\nP(practical equivalence, +/- 1%) = green AUC = 10%\nP(clinical superiorty, >1%) = AUC to right of green = 66.0%\nAt y=0 black circle = RD point estimate + thin horizontal line  = 95% CrI, thick line = +/- 68% CrI", hjust=0) 


# Informative priors graph
draws_inf <- as_draws_df(m_combined) %>% 
  # rename and drop the unneeded columns
  transmute(p0 = b_grp2cCPR,
            p1 = b_grp2eCPR) %>% 
  # compute the OR
  mutate(or = (p1 / (1 - p1)) / (p0 / (1 - p0)),
         dd = p1 - p0)

fig1b <- draws_inf %>% 
  ggplot(aes(x = dd*100)) +
  stat_halfeye(point_interval = mean_qi, .width = .95) +
  scale_y_continuous(name="Density", breaks = NULL) +
  xlab("Success risk difference (RD) eCPR - cCPR (%)") +
  stat_halfeye(aes(fill = after_stat(abs(x) < 1))) +
  scale_fill_manual(values = c("#CD8A39", "#57643C")) +
  guides(fill = "none") + 
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = .025)) +
  labs(title = "B) INCEPTION with informative prior", caption = "P(superiority) = area under the curve (AUC) to right of vertical line at RD = 0 = 99.6%\nP(practical equivalence, +/- 1%) = green AUC = 0.6%%\nP(clinical superiorty, >1%) = AUC to right of green = 99.2%\nAt y=0 black circle = RD point estimate + thin horizontal line  = 95% CrI, thick line = +/- 68% CrI", hjust=0) 

# Add title, etc. to a patchwork
fig1 <- fig1a + fig1b + plot_annotation('INCEPTION probability density functions (ITYT analyses)', caption = 'ITT analysis')
ggsave("Figure 1.png", plot=fig1, dpi = 600)
