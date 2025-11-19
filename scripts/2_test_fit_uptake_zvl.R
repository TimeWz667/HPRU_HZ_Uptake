library(tidyverse)



up_zvl <- read_csv(here::here("data", "uptake_zvl.csv"))

dat <- up_zvl %>% 
  pivot_longer(c(Q1, Q2, Q3)) %>% 
  mutate(
    Cohort = paste(Year, name),
    Coverage = value / 100
  ) %>% 
  arrange(Cohort, Time) %>% 
  group_by(Cohort) %>% 
  mutate(
    Coverage = cumsum(Coverage),
    Year = as.character(Year),
  ) %>% 
  filter(Coverage > 0) %>% 
  mutate(Time = 1:n()) %>% 
  select(Year, Month, Time, Cohort, Coverage)


dat

dat %>% 
  group_by(Cohort) %>% 
  mutate(
    Incre =  c(Coverage[1], diff(Coverage)) / c(1, 1 - Coverage[-n()])
  ) %>% 
  ggplot() + 
  geom_line(aes(x = Time, y = Incre, colour = Cohort))


fit <- lm(log(1 - Coverage) ~ (Time - 1) + 1, data = dat)
fit2 <- lm(log(1 - Coverage) ~ as.character(Time - 1) + 1, data = dat)



1 - exp(coef(fit)['Time'])


p_est <- coef(summary(fit))["Time", 1:2]

p_hat <- 1 - exp(p_est[1])

pini <- 0.2
pcat <- p_hat


ps <- 1 - exp(coef(fit))

p0 <- ps[1]
p0 


mean(1 - (1 - ps[2]) ^ (0:3 + 3) * (1 - p0))



1 - (1 - ps[2]) ^ 3 * (1 - p0)

ps
