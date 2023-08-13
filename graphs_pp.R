############
# graphs
# need to first run brms.identity_pp.R to create the  models
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
# PP models
#
#######################

# graph of INCEPTION with vague priors
draws_pp <- as_draws_df(m_pp) %>% 
  # rename and drop the unneeded columns
  transmute(p0 = b_grp2cCPR,
            p1 = b_grp2eCPR) %>% 
  # compute the OR
  mutate(or = (p1 / (1 - p1)) / (p0 / (1 - p0)),
         dd = p1 - p0)


fig1a_pp <- draws_pp %>% 
  ggplot(aes(x = dd*100)) +
  stat_halfeye(point_interval = mean_qi, .width = .95) +
  scale_y_continuous(name="Density", breaks = NULL) +
  xlab("Success risk difference (RD) eCPR - cCPR (%)") +
  stat_halfeye(aes(fill = after_stat(abs(x) < 1))) +
  scale_fill_manual(values = c("#CD8A39", "#57643C")) +
  guides(fill = "none") + 
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = .075)) +
  labs(title = "A) INCEPTION with vague prior", caption = "P(superiority) = area under the curve (AUC) to right of vertical line at RD = 0 = 1.3%\nP(practical equivalence, +/- 1%) = green AUC = 1.1%\nP(clinical superiorty, >1%) = AUC to right of green = 0.9%\nAt y=0 black circle = RD point estimate + thin horizontal line  = 95% CrI, thick line = +/- 68% CrI", hjust=0) 


# Informative priors graph
draws_inf_pp <- as_draws_df(m_combined_pp) %>% 
  # rename and drop the unneeded columns
  transmute(p0 = b_grp2cCPR,
            p1 = b_grp2eCPR) %>% 
  # compute the OR
  mutate(or = (p1 / (1 - p1)) / (p0 / (1 - p0)),
         dd = p1 - p0)

fig1b_pp <- draws_inf_pp %>% 
  ggplot(aes(x = dd*100)) +
  stat_halfeye(point_interval = mean_qi, .width = .95) +
  scale_y_continuous(name="Density", breaks = NULL) +
  xlab("Success risk difference (RD) eCPR - cCPR (%)") +
  stat_halfeye(aes(fill = after_stat(abs(x) < 1))) +
  scale_fill_manual(values = c("#CD8A39", "#57643C")) +
  guides(fill = "none") + 
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0)) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = .36)) +
  labs(title = "B) INCEPTION with informative prior", caption = "P(superiority) = area under the curve (AUC) to right of vertical line at RD = 0 = 91.1%\nP(practical equivalence, +/- 1%) = green AUC = 8.2%%\nP(clinical superiorty, >1%) = AUC to right of green = 86.4%\nAt y=0 black circle = RD point estimate + thin horizontal line  = 95% CrI, thick line = +/- 68% CrI", hjust=0) 

# Add title, etc. to a patchwork
fig1_pp <- fig1a_pp + fig1b_pp + plot_annotation('INCEPTION probability density functions (per protocol analyses)', caption = 'PP analysis')
ggsave("Figure 2.png", plot=fig1, dpi = 600)
