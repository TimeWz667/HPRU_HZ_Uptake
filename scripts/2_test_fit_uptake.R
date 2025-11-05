library(tidyverse)
library(simpleboot)


load(file = here::here("data", "uptake.rdata"))


lm.boot(fit, 100)
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



uptake %>% 
  select(Cohort) %>% 
  distinct() %>% 
  mutate(Q = 0, Dose = 0, Coverage = 0) %>% 
  bind_rows(uptake) %>% 
  group_by(Cohort) %>% 
  arrange(Q) %>% 
  summarise(
    Uptake = diff(Coverage), 
    Uptake2 = Uptake / (1 - Coverage[-n()]), 
    Q = Q[-1]
  ) %>% 
  ggplot() +
  geom_line(aes(x = Q, y = Uptake, colour = Cohort), size = 1.5) +
  geom_line(aes(x = Q, y = Uptake2, colour = Cohort)) +
  expand_limits(y = 0)



uptake %>% 
  ggplot() +
  geom_line(aes(x = Q, y = log(1 - Coverage), colour = Cohort))


uptake %>% 
  extract(Cohort, c("YearQ", "Age0"), "(\\S+)_(65|70)")%>% 
  ggplot() +
  geom_line(aes(x = Q, y = log(1 - Coverage), colour = YearQ)) +
  facet_wrap(.~Age0)

uptake %>% 
  extract(Cohort, c("YearQ", "Age0"), "(\\S+)_(65|70)") %>% 
  ggplot() +
  geom_line(aes(x = Q, y = Coverage, colour = YearQ)) +
  facet_wrap(.~Age0)



uptake %>% 
  extract(Cohort, c("Year", "Q0", "Age0"), "(\\S+)_Q(1|2|3|4)_(65|70)") %>% 
  filter(Q == 1) %>% 
  mutate(odd = log(Coverage / (1 - Coverage))) %>% 
  ggplot() +
  geom_point(aes(x = Q0, y = odd, colour = Year)) +
  facet_wrap(.~Age0)
 




## Model p0
dat_p0 <- uptake %>% 
  extract(Cohort, c("Year", "Q0", "Age0"), "(\\S+)_Q(1|2|3|4)_(65|70)") %>% 
  filter(Q == 1) %>% 
  mutate(odd = log(Coverage / (1 - Coverage)))

fit_p0 <- lm(odd ~ Year + Age0, data = dat_p0)

fit_p0

newdata_p0 <- crossing(Year = c("23/24", "24/25"), Age0 = c("65", "70"))

pred_p0 <- bind_cols(
    newdata_p0, 
    predict(fit_p0, newdata = newdata_p0, interval = "prediction")
  ) %>% 
  mutate(
    p0 = 1 / (1 + exp(-fit)),
    p0_l = 1 / (1 + exp(-lwr)),
    p0_u = 1 / (1 + exp(-upr))
  )

pred_p0 %>% 
  crossing(Q0 = 1:4) %>% 
  ggplot() + 
  geom_ribbon(aes(x = Q0, ymin = p0_l, ymax = p0_u, fill = Age0), alpha = 0.4) + 
  geom_line(aes(x = Q0, y = p0, colour = Age0)) +
  geom_point(data = dat_p0, aes(x = as.numeric(Q0), y = Coverage, shape = Age0)) +
  facet_grid(.~Year) +
  expand_limits(y = c(0, 0.5))



## Model pc

dat_pc <- uptake %>% 
  extract(Cohort, c("Year", "Q0", "Age0"), "(\\S+)_Q(1|2|3|4)_(65|70)") %>% 
  mutate(
    y = log(1 - Coverage),
    ti = Q - 1
  )

fit1 <- lm(y ~ ti + Age0 + Year, data = dat_pc)
summary(fit1)


fit2 <- lm(y ~ ti * Age0 + Year, data = dat_pc)
summary(fit2)


fit3 <- lm(y ~ ti * Year + Age0, data = dat_pc)
summary(fit3)


fit1




## Simulation

n_sims <- 2000

co_pc <- coef(summary(fit1))["ti", ]

sims_pc <- tibble(Key = 1:n_sims) %>% 
  mutate(
    p_catch = rnorm(n_sims, co_pc[1], co_pc[2]),
    p_catch = 1 - exp(p_catch)
  )

sims_pc


sims_p0 <- bind_cols(
  newdata_p0,
  predict(fit_p0, newdata = newdata_p0, se = T) %>% as_tibble
) %>% 
  crossing(Key = 1:n_sims) %>% 
  mutate(
    p_ini = rnorm(n(), fit, se.fit),
    p_ini = 1 / (1 + exp(-p_ini))
  ) %>% 
  select(Key, Year, Age0, p_ini)


sims_p0



forecast_coverage <- crossing(
  Key = 1:n_sims,
  Year = 23:24,
  Age0 = c(65, 70),
  Q0 = 1:4,
  dQ = 1:60
) %>% 
  mutate(
    Year0 = paste0(Year, "/", Year + 1),
    Year = Year + 2000 + ceiling((dQ - 2) / 4),
    Age = Age0 + ceiling(dQ / 4) - 1,
    Age0 = as.character(Age0)
  ) %>% 
  left_join(sims_p0 %>% rename(Year0 = Year)) %>% 
  left_join(sims_pc) %>% 
  mutate(
    Coverage = 1 - (1 - p_ini) * (1 - p_catch) ** (dQ - 1)
  ) %>% 
  group_by(Year, Age0, Q0, dQ, Year0, Age) %>% 
  summarise(
    M = mean(Coverage),
    L = quantile(Coverage, 0.025),
    U = quantile(Coverage, 0.975)
  ) %>% 
  ungroup() %>% 
  mutate(
    Cohort = paste(Year0, paste0("Q", Q0), Age0, sep = "_"),
    Q0 = as.character(Q0)
  )


uptake %>% left_join(
  forecast_coverage %>% 
    select(Cohort, Age0, Year0, Q = dQ, Q0, M, L, U),
  by = c("Cohort", "Q")
) %>% 
  ggplot() +
  geom_ribbon(aes(x = Q, ymin = L, ymax = U, fill = Year0), alpha = 0.4) +
  geom_line(aes(x = Q, y = M, colour = Year0)) +
  geom_point(aes(x = Q, y = Coverage)) +
  facet_grid(Q0~Age0, labeller = "label_both") +
  scale_y_continuous("Coverage", limits = c(0, 0.6), labels = scales::percent)



forecast_coverage %>% 
  filter(dQ == 5) %>% 
  group_by(Year, Year0, Age, Age0) %>% 
  summarise(
    across(c(M, L, U), mean)
  )


forecast_coverage %>% 
  mutate(
    Q = dQ %% 4
  ) %>% 
  filter(Q == 1) %>% 
  group_by(Year, Year0, Age, Age0) %>% 
  summarise(
    across(c(M, L, U), mean)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Age, ymin = L, ymax = U, fill = Year0), alpha = 0.4) +
  geom_line(aes(x = Age, y = M, colour = Year0)) +
  facet_grid(.~Age0, labeller = "label_both") +
  scale_y_continuous("Coverage", limits = c(0, 1), labels = scales::percent)
  
  
forecast_coverage %>% 
  mutate(
    Q = dQ %% 4
  ) %>% 
  filter(Q == 1) %>% 
  group_by(Year, Year0, Age, Age0) %>% 
  summarise(
    across(c(M, L, U), mean)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Age, ymin = L, ymax = U, fill = Age0), alpha = 0.4) +
  geom_line(aes(x = Age, y = M, colour = Age0)) +
  facet_grid(.~Year0, labeller = "label_both") +
  scale_y_continuous("Coverage", limits = c(0, 1), labels = scales::percent)


forecast_coverage %>% 
  filter(dQ == 5) %>% 
  group_by(Year, Year0, Age, Age0) %>% 
  summarise(
    across(c(M, L, U), mean)
  )



forecast_coverage %>% 
  mutate(
    S = (as.numeric(Q0) + dQ) %% 4 + 1
  ) %>% 
  filter(S == 1) %>% 
  group_by(Year, Year0, Age0) %>% 
  summarise(
    across(c(M, L, U), mean)
  ) %>% 
  ggplot() +
  geom_ribbon(aes(x = Year, ymin = L, ymax = U, fill = Age0), alpha = 0.4) +
  geom_line(aes(x = Year, y = M, colour = Age0)) +
  facet_grid(.~Year0, labeller = "label_both") +
  scale_y_continuous("Coverage", limits = c(0, 1), labels = scales::percent)



forecast_coverage %>% 
  mutate(
    S = (as.numeric(Q0) + dQ) %% 4 + 1
  ) %>% 
  filter(S == 1) %>% 
  group_by(Year, Year0, Age0) %>% 
  summarise(
    across(c(M, L, U), mean)
  ) %>% 
  filter(Year <= 2025)



forecast_coverage %>% 
  mutate(
    S = (as.numeric(Q0) + dQ) %% 4 + 1
  ) %>% 
  filter(S == 3) %>%
  filter(dQ <= 4 ) %>%
  group_by(Year, Year0, Age0) %>% 
  summarise(
    across(c(M, L, U), mean)
  ) %>% 
  filter(Year <= 2025)
