# brms package using identity link which gives risk differences
# discussed online at https://discourse.mc-stan.org/t/brms-iformative-prior-for-bionomial-regression/32368/11

# default vague priors with identity link

##########################
#
# Per protocol analysis
#
#########################



pacman::p_load(brms, tidyverse)
data_pp <- data.frame(N = c(62,70), y = c(13,5), grp2 = as.factor(c("cCPR","eCPR"))) 
f = bf(y | trials(N) ~ 0 + grp2)

get_prior(formula = f,
          data = data_pp,
          family = binomial(link = "identity"))


m_pp <- brm(
  formula = f,
  data = data_pp,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1)), # gets rid os a bunch of unhelpful warnings
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

prior_summary(m_pp)

print(m_pp)


draws_identity_pp <- m_pp %>% 
  # This gives us counts...
  epred_draws(newdata = data_pp)  # from tidybayes

# brms::as_draws_df makes a tibble with proportions
# tidybayes::tidy_draws gives same results

# colours
clrs_saguaro <- NatParksPalettes::natparks.pals("Saguaro")
clr_grp20 <- clrs_saguaro[1]
clr_grp21 <- clrs_saguaro[6]
clr_diff <- clrs_saguaro[4]

p1_pp <- draws_identity_pp %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes (eCPR - cCPR)",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "Vague non-informative prior")

p2_pp <- draws_identity_pp %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") %>% 
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes (eCPR - cCPR)",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "Vague non-informative prior")

(p1_pp / plot_spacer() / p2_pp) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

p2df_pp <- draws_identity_pp %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(p2df_pp$.epred), sd(p2df_pp$.epred))

# informative prior ARREST

m_arrest_pp <- brm(
  formula = f,
  data = data_pp,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1),
            prior(beta(1,14), class = b, coef = "grp2cCPR"),
            prior(beta(6,8),class = b, coef = "grp2eCPR")),
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_arrest_pp)
prior_summary(m_arrest_pp)

draws_identity_pp_arrest <- m_arrest_pp %>% 
  # This gives us counts...
  epred_draws(newdata = data_pp) 

p3_pp <- draws_identity_pp_arrest %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "ARREST informative prior")

p4_pp <- draws_identity_pp_arrest %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") %>% 
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes (eCPR - cCPR)",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "ARREST informative prior")

(p3_pp / plot_spacer() / p4_pp) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

pp_arrest_df <- draws_identity_pp_arrest %>% 
  ungroup() %>% 
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(pp_arrest_df$.epred), sd(pp_arrest_df$.epred))

# arrest alone with default priors

arrest_pp_alone <- data.frame(N = c(15,14), y = c(1,6), grp2 = as.factor(c("cCPR","eCPR"))) 
f = bf(y | trials(N) ~ 0 + grp2)

get_prior(formula = f,
          data = data_pp,
          family = binomial(link = "identity"))


m_arrest_pp_alone <- brm(
  formula = f,
  data = arrest_alone,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1)), # gets rid os a bunch of unhelpful warnings
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_arrest_pp_alone)

draws_identity_pp_arrest_alone <- m_arrest_pp_alone %>% 
  # This gives us counts...
  epred_draws(newdata = data_pp) 

p_arrest_df_alone <- draws_identity_pp_arrest_alone %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(p_arrest_df_alone$.epred), sd(p_arrest_df_alone$.epred))

########

# informative prior PRAGUE

m_prague_pp <- brm(
  formula = f,
  data = data_pp,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1),
            prior(beta(24,108), class = b, coef = "grp2cCPR"),
            prior(beta(38,86),class = b, coef = "grp2eCPR")),
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_prague_pp)
prior_summary(m_prague_pp)

draws_identity_pp_prague <- m_prague_pp %>% 
  # This gives us counts...
  epred_draws(newdata = data_pp) 

p5_pp <- draws_identity_pp_prague %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage of successes",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "PRAGUE informative prior")

p6_pp <- draws_identity_pp_prague %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") %>% 
  ggplot(aes(x = .epred)) +
  stat_halfeye(fill = clr_diff) +
  guides(fill = "none") +
  labs(x = "Percentage difference of successes (eCRP - cCRP)",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "PRAGUE informative prior")

(p5_pp / plot_spacer() / p6_pp) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

pp_prague_df <- draws_identity_pp_prague %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(pp_prague_df$.epred), sd(pp_prague_df$.epred))


###########

# informative prior PRAGUE + ARREST (combined)

m_combined_pp <- brm(
  formula = f,
  data = data_pp,
  family = binomial(link = "identity"),
  prior = c(prior(beta(1, 1), class = b, lb = 0, ub = 1),
            prior(beta(25,122), class = b, coef = "grp2cCPR"),
            prior(beta(44,94),class = b, coef = "grp2eCPR")),
  chains = 4, warmup = 1000, iter = 2000, seed = 123,
  refresh = 0
)

print(m_combined_pp)
prior_summary(m_combined_pp)

draws_identity_pp_combined <- m_combined_pp %>% 
  # This gives us counts...
  epred_draws(newdata = data_pp) 

p7_pp <- draws_identity_pp_combined %>% 
  ggplot(aes(x = .epred, y = grp2, fill = grp2)) +
  stat_halfeye() +
  scale_fill_manual(values = c(clr_grp20, clr_grp21)) +
  guides(fill = "none") +
  labs(x = "Percentage of successes (eCRP - cCRP)",
       y = NULL) +
  theme_classic() +
  labs(title = "INCEPTION trial results", subtitle = "Combined informative prior")

p8_pp <- draws_identity_pp_combined %>% 
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

(p7_pp / plot_spacer() / p8_pp) + 
  plot_layout(heights = c(0.785, 0.03, 0.185))

pp_combined_df <- draws_identity_pp_combined %>% 
  ungroup() %>% 
  # compare_levels() subtracts things using alphabetical order, so so we have to
  # make the United States the first level
  mutate(grp2 = fct_relevel(grp2, "cCPR")) %>% 
  compare_levels(.epred, by = "grp2") 

# prob eCPR better
1- pnorm(0, mean(pp_combined_df$.epred), sd(pp_combined_df$.epred))

#########################
#########################


