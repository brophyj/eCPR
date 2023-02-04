cbind(switch, n - switch) ~ dist100 + arsenic

df <- data.frame(dead=c(56, 52), n=c(70,62), Rx=c(1,0))
df
glm(cbind(dead, n-dead) ~ Rx, data=df, family = binomial, weights = n )


library(dplyr)
df <- tibble(Tx = c("CPR", "eCPR"),
             dead = c(52, 56),
             alive = c(10,14)) %>% 
  mutate(total = dead + alive,
         prop_alive = alive / total) 

fit <- glm(prop_alive ~ Tx, data = df, family = binomial(link="logit"), weights = total)
summary(fit, digits=3)
print(c(exp(.262),exp(.262+c(-1,1)*1.96*.457)))

df


library(rstanarm)
fit1 <- stan_glm(prop_alive ~ Tx, data = df, family = binomial(link="logit"), weights = total, refresh=0)
summary(fit1, digits=4)

library(tidybayes)
get_variables(fit1)
prior_summary(fit1)
help('prior_summary.stanreg') 

fit1 %>%
  spread_draws(`(Intercept)`, `TxeCPR`) %>%
  head(10)

posterior_vs_prior(fit1, pars = "beta")
plot(fit1)

fit1 %>% 
  tidybayes::tidy_draws() %>% 
  ggdist::mean_qi(exp(TxeCPR))

fit1 %>%
  spread_draws(`(Intercept)`, `TxeCPR`) %>%
  median_qi(`(Intercept)`, `TxeCPR`)


fit1 %>%
  spread_draws(`(Intercept)`, `TxeCPR`) %>%
  median_qi(condition_mean = `(Intercept)` + `TxeCPR`, .width = c(.95, .68)) %>%
  ggplot(aes(y = `TxeCPR`, x = condition_mean, xmin = .lower, xmax = .upper)) +
  geom_pointinterval()

fit1 %>% tidybayes::tidy_draws() |> 
  ggplot(aes(x = exp(TxeCPR),
             fill = after_stat(x > 1),
             slab_alpha = after_stat((x)))
  ) +
  ggdist::stat_halfeye() +
  scale_x_continuous(name="HR being alive with extracorporeal CPR", limits=c(0, 5)) +
  scale_y_continuous(name="Density") +
  labs(title = "INCEPTION ITT analysis with vague prior") +
  annotate(geom="text", x=2.75,y=.6, label = "Probability of increased survival \n(with intact neurological status) \nwith extracorporeal CPR = 71%") +
  theme_classic() +
  theme(legend.position="none") 

tt <- fit1 %>%
  spread_draws(`(Intercept)`, `TxeCPR`)
sum(tt$TxeCPR>0)/nrow(tt)

#prior for CPR arm
x=seq(0,1, by = .001)
x=seq(0,1, length.out =1000)
ggplot(data.frame(x=x, y=dbeta(x,25,124)),aes(x,y)) +
  geom_line() +
  geom_line(data.frame(x=x, y=dbeta(x,45,95)),aes(x,y))

#prior for eCPR arm
ggplot(data.frame(x=x, y=dbeta(x,45,95)),aes(x,y)) +
  geom_line()

t1 <- rbeta(100000,25,124)
t2 <- rbeta(100000,45,95)
t3 <- t2 - t1

mean(t3)
sd(t3)

p1 <- ggplot(data = data.frame(x = c(0, .3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean(t3), sd = sd(t3))) + ylab("") +
  scale_y_continuous(breaks = NULL)
p1

library(LearnBayes)
quantile1=list(p=.025, x=0.0556 )     # 2.5% quantile should be 0.0556 
quantile2=list(p=.975, x=0.251)      # 97.5% quantile should be 0..251
par_beta <- beta.select(quantile1, quantile2)
m_beta <-  par_beta[1]/(par_beta[1]+par_beta[2])

v_beta <- (par_beta[1] * par_beta[2])/((par_beta[1]+par_beta[2])^2 * (par_beta[1] +par_beta[2] + 1))

p2 <- ggplot(data = data.frame(x = c(0, .3)), aes(x)) +
  stat_function(fun = dbeta, n = 101, args = list(6.36,39.65)) + ylab("") +
  scale_y_continuous(breaks = NULL)
p2

fit2 <- stan_glm(prop_success ~ Tx, data = df[c(1:2),], family = binomial(link="logit"), weights = total, refresh=0,
                 prior = normal(mean(3), sd(t3)))
summary(fit2, digits=4)

fit2 <- stan_glm(prop_success ~ Tx, data = df[c(1:2),], family = binomial(link="logit"), weights = total, refresh=0,
                 prior = beta(6.36,39.65))
summary(fit2, digits=4)

ggplot(data.frame(y=t3),aes(y)) +
  geom_histogram(bins = 200)

#FIRST WE CREATE THE VECTOR
x <- seq(0, .5, by = 0.001)

#THEN EACH OF OUR BETAS DISTR

beta1 <- dbeta(x, shape1 = 25, shape2 = 124)
beta2 <- dbeta(x, shape1 = 45, shape2 = 95)
beta3 <- dbeta(x, shape1 = 6.36, shape2 = 39.65)

mixture <- ggplot()+
  geom_line(aes(x, beta1),  color = "red",
            linewidth = 1.2) +
  geom_line(aes(x, beta2), color = "green",
            linewidth = 1.2) +
  geom_line(aes(x, beta3), color = "black",
            linewidth = 1.2) +
  labs(title = "Beta Distribution of prior studies",
       x = "Sequence",
       y = "Beta",
       caption = "Green = p(+outcome | eCPR) \nRed = p(+outcome | CPR) \nBlack= difference of the 2 probabilities")

mixture


