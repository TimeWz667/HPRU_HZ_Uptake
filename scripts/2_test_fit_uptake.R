library(tidyverse)



load(file = here::here("data", "uptake.rdata"))



uptake


fit <- lm(log(1 - Coverage) ~ (Q - 1) + Cohort, data = uptake)


1 - exp(coef(fit)['Q'])


p_est <- coef(summary(fit))["Q", 1:2]

p_hat <- 1 - exp(p_est[1])

pini <- 0.2
pcat <- p_hat



ava <- rep(1 / 4, 4)
ava


ava1 <- ava * (1 - c(pini, 0, 0, 0)) * (1 - c(pcat, pini, 0, 0)) * (1 - c(pcat, pcat, pini, 0)) * (1 - c(pcat, pcat, pcat, pini)) 
ava1 <- 1 - sum(ava1)


ava2 <- 1 - (1 - pini) * (1 - pcat) ** 3


ava1;ava2
