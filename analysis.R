library(LearnBayes)
quantile1=list(p=.025, x=0.0556 )     # 2.5% quantile should be 0.0556 
quantile2=list(p=.975, x=0.251)      # 97.5% quantile should be 0..251
par_beta <- beta.select(quantile1, quantile2)
m_beta <-  par_beta[1]/(par_beta[1]+par_beta[2])

v_beta <- (par_beta[1] * par_beta[2])/((par_beta[1]+par_beta[2])^2 * (par_beta[1] +par_beta[2] + 1))

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


