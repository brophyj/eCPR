# brms package using identity link which gives risk differences
# discussed online at https://discourse.mc-stan.org/t/brms-iformative-prior-for-bionomial-regression/32368/11

# default vague priors with identity link


pacman::p_load(brms, tidyverse)
data_bin2 <- data.frame(N = c(62,70), y = c(10,14), grp2 = as.factor(c("cCPR","eCPR"))) 
f = bf(y | trials(N) ~ 0 + grp2)

get_prior(formula = f,
          data = data_bin2,
          family = binomial(link = "identity"))


m <- brm(
  formula = f,
  data = data_bin2,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1)), # gets rid os a bunch of unhelpful warnings
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

prior_summary(m)

print(m)


draws_identity <- m %>% 
  epred_draws(newdata = data_bin2)  # from tidybayes, gives counts

# brms::as_draws_df makes a tibble with proportions
# tidybayes::tidy_draws gives same results

# can also plot model results
# colour scheme
clrs_saguaro <- NatParksPalettes::natparks.pals("Saguaro")
clr_grp20 <- clrs_saguaro[1]
clr_grp21 <- clrs_saguaro[6]
clr_diff <- clrs_saguaro[4]

p1 <- draws_identity %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "Vague non-informative prior")

p2 <- draws_identity %>% 
  ungroup() %>% 
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") %>%  # compare_levels() subtracts things using alphabetical order
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes (eCRP - cCRP)",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "Vague non-informative prior")
  
(p1 / plot_spacer() / p2) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

p2df <- draws_identity %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(p2df$.epred), sd(p2df$.epred))

# informative prior ARREST

m_arrest <- brm(
  formula = f,
  data = data_bin2,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1),
            prior(beta(1,14), class = b, coef = "grp2cCPR"),
            prior(beta(6,8),class = b, coef = "grp2eCPR")),
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_arrest)
prior_summary(m_arrest)

draws_identity_arrest <- m_arrest %>% 
  # This gives us counts...
  epred_draws(newdata = data_bin2) 

p3 <- draws_identity_arrest %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "ARREST informative prior")

p4 <- draws_identity_arrest %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") %>% 
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "ARREST informative prior")

(p3 / plot_spacer() / p4) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

p_arrest_df <- draws_identity_arrest %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(p_arrest_df$.epred), sd(p_arrest_df$.epred))

# arrest alone with default priors

arrest_alone <- data.frame(N = c(15,14), y = c(1,6), grp2 = as.factor(c("cCPR","eCPR"))) 
f = bf(y | trials(N) ~ 0 + grp2)

get_prior(formula = f,
          data = data_bin2,
          family = binomial(link = "identity"))


m_arrest_alone <- brm(
  formula = f,
  data = arrest_alone,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1)), # gets rid os a bunch of unhelpful warnings
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_arrest_alone)

draws_identity_arrest_alone <- m_arrest_alone %>% 
  # This gives us counts...
  epred_draws(newdata = data_bin2) 

p_arrest_df_alone <- draws_identity_arrest_alone %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(p_arrest_df_alone$.epred), sd(p_arrest_df_alone$.epred))

########

# informative prior PRAGUE

m_prague <- brm(
  formula = f,
  data = data_bin2,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1),
            prior(beta(24,108), class = b, coef = "grp2cCPR"),
            prior(beta(38,86),class = b, coef = "grp2eCPR")),
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_prague)
prior_summary(m_prague)

draws_identity_prague <- m_prague %>% 
  # This gives us counts...
  epred_draws(newdata = data_bin2) 

p5 <- draws_identity_prague %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "PRAGUE informative prior")

p6 <- draws_identity_prague %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") %>% 
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "PRAGUE informative prior")

(p5 / plot_spacer() / p6) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

p_prague_df <- draws_identity_prague %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(p_prague_df$.epred), sd(p_prague_df$.epred))


###########

# informative prior PRAGUE + ARREST (combined)

m_combined <- brm(
  formula = f,
  data = data_bin2,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1),
            prior(beta(25,122), class = b, coef = "grp2cCPR"),
            prior(beta(44,94),class = b, coef = "grp2eCPR")),
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_combined)
prior_summary(m_combined)

draws_identity_combined <- m_combined %>% 
  # This gives us counts...
  epred_draws(newdata = data_bin2) 

p7 <- draws_identity_combined %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "Combined informative prior")

p8 <- draws_identity_combined %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") %>% 
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "Combined informative prior")

(p7 / plot_spacer() / p8) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

p_combined_df <- draws_identity_combined %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(p_combined_df$.epred), sd(p_combined_df$.epred))

#########################
#########################

# graph of INCEPTION with vague priors

draws <- as_draws_df(m) %>% 
  # rename and drop the unneeded columns
  transmute(p0 = b_grp2cCPR,
            p1 = b_grp2eCPR) %>% 
  # compute the OR
  mutate(or = (p1 / (1 - p1)) / (p0 / (1 - p0)),
         dd = p1 - p0)
         
draws

fig1a <- draws %>% 
  ggplot(aes(x = dd*100)) +
  stat_halfeye(point_interval = mean_qi, .width = .95) +
  scale_y_continuous(name="Density", breaks = NULL) +
  xlab("Success risk difference eCPR - cCPR (%)") +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  theme_classic() +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = .75)) +
  labs(title = "A) INCEPTION ITT analysis with vague prior", caption = "P(superiority) = area under the curve (AUC) to right of vertical line at RD = 0 = 71.2%\nthin horizontal line at 0 = 95% CrI with black circle = point estimate\nthick horizontal line = +/- 68% CrI", hjust=0) 

fig1a

ggsave("figure1a.png")

############### 
# Informative priors graph


draws_inf <- as_draws_df(m_combined) %>% 
  # rename and drop the unneeded columns
  transmute(p0 = b_grp2cCPR,
            p1 = b_grp2eCPR) %>% 
  # compute the OR
  mutate(or = (p1 / (1 - p1)) / (p0 / (1 - p0)),
         dd = p1 - p0)

draws_inf

fig1b <- draws_inf %>% 
  ggplot(aes(x = dd*100)) +
  stat_halfeye(point_interval = mean_qi, .width = .95) +
  scale_y_continuous(name="Density", breaks = NULL) +
  xlab("Success risk difference eCPR - cCPR (%)") +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  theme_classic() +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = .025)) +
  labs(title = "B) INCEPTION ITT analysis with informative prior", caption = "P(superiority) = area under the curve (AUC) to right of vertical line at RD = 0 = 71.2%\nthin horizontal line at 0 = 95% CrI with black circle = point estimate\nthick horizontal line = +/- 68% CrI", hjust=0) 

fig1b

ggsave("figure1b.png")

fig1a +fig1b
