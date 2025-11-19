library(tidyverse)


# ps <- 1 - exp(coef(summary(fit))[, 1])
# tibble(var = names(ps), p = ps) %>% 
#   pivot_wider(names_from = var, values_from = p) %>% 
#   pivot_longer(-Q, names_pattern = "Cohort(\\S+)_(Q[1-4])_(65|70)", names_to = c("Year", "QU", "Age0")) %>% 
#   group_by(Year, Age0) %>% 
#   summarise(mean(value))
# Year  Age0  `mean(value)`
# <chr> <chr>         <dbl>
# 23/24 65            0.165
# 23/24 70            0.290
# 24/25 65            0.101
# 24/25 70            0.204

# ZosterVax 2018 - 2022
p0y_zvl <- 0.48
pty_zvl <- 0.136

p0q_65 <- 0.101
p0q_70 <- 0.204
ptq_rzv <- 0.07243710

ptq_zvl <- 0.1000767


mean(1 - (1 - ptq_rzv) ^ (0:3) * (1 - p0q_65))
1 - (1 - ptq_rzv) ^ 3 * (1 - p0q_65)


mean(1 - (1 - ptq_rzv) ^ (0:3) * (1 - p0q_70))
1 - (1 - ptq_rzv) ^ 3 * (1 - p0q_70)


stats_uv <- read_csv(here::here("data", "stats_uv_ce.csv")) %>% 
  select(Age0, Arm, Index, A:U)


simulate_uptake <- function(age0, age1, year0 = 2024, p0, ptq, pty, mode = c("AC", "Cohort")) {
  mode <- match.arg(mode)
  
  sims <- crossing(Age = age0:(age1 - 1), Q = 1:4) %>% 
    mutate(
      YearMea = year0 + (Age - age0) + 1,
      p_uptake = c(p0, rep(ptq, 3), rep(1 - (1 - pty) ^ 0.25, n() - 4)),
      Vac = 1 - cumprod(1 - p_uptake),
      NewVac = diff(c(0, Vac))
    ) %>% 
    summarise(
      Age0 = min(Age),
      Age = min(Age) + 1,
      Vac = Vac[n()],
      NewVac = sum(NewVac),
      .by = "YearMea"
    )
  
  return(sims)
}


sims_60_s1 <- simulate_uptake(age0 = 60, age1 = 80, p0 = p0q_65, ptq = ptq_rzv, pty = pty_zvl) %>% mutate(AgeStart = 60, Sc = "S1")
sims_60_s2 <- simulate_uptake(age0 = 60, age1 = 80, p0 = p0q_65, ptq = ptq_rzv, pty = pty_zvl * ptq_rzv / ptq_zvl) %>% mutate(AgeStart = 60, Sc = "S2")

sims_65_s1 <- simulate_uptake(age0 = 65, age1 = 80, p0 = p0q_65, ptq = ptq_rzv, pty = pty_zvl) %>% mutate(AgeStart = 65, Sc = "S1")
sims_65_s2 <- simulate_uptake(age0 = 65, age1 = 80, p0 = p0q_65, ptq = ptq_rzv, pty = pty_zvl * ptq_rzv / ptq_zvl) %>% mutate(AgeStart = 65, Sc = "S2")

sims_70_s1 <- simulate_uptake(age0 = 70, age1 = 80, p0 = p0q_70, ptq = ptq_rzv, pty = pty_zvl) %>% mutate(AgeStart = 70, Sc = "S1")
sims_70_s2 <- simulate_uptake(age0 = 70, age1 = 80, p0 = p0q_70, ptq = ptq_rzv, pty = pty_zvl * ptq_rzv / ptq_zvl) %>% mutate(AgeStart = 70, Sc = "S2")


ces <- lapply(
  list(sims_60_s1, sims_60_s2, sims_65_s1, sims_65_s2, sims_70_s1, sims_70_s2),
  function(sims) {
    sims %>% 
      left_join(stats_uv, by = "Age0") %>% 
      mutate(dis = exp(-0.035 * (YearMea - 2025))) %>% 
      summarise(
        across(A:U, \(x) mean(x * NewVac * dis)),
        .by = c("AgeStart", "Sc", "Arm", "Index")
      )
  }
) %>% 
  bind_rows()




ces %>% 
  filter(Index %in% c("dC_Med_d", "dQ_All_d")) %>% 
  ggplot() + 
  geom_pointrange(aes(x = AgeStart, y = A, ymin = L, ymax = U)) +
  facet_wrap(Arm~Index, scale = "free_y")



ces %>% 
  filter(Index %in% c("dN_VacRZV", "dC_Med_d", "dQ_All_d")) %>% 
  select(AgeStart, Sc, Arm, M, Index) %>% 
  pivot_wider(names_from = Index, values_from = M) %>% 
  mutate(Thres = (dQ_All_d * 2e4 + dC_Med_d) / dN_VacRZV) %>% 
  filter(Arm == "RZV_2d") %>% 
  ggplot() +
  geom_point(aes(x = dQ_All_d, y =  + dC_Med_d + 114 * dN_VacRZV, colour = Sc)) +
  geom_abline(slope = 2e4) +
  scale_x_continuous("Difference in QALY (per 1 M)", labels = scales::number_format(scale = 1e6)) +
  scale_y_continuous("Difference in Cost (M GBP, per 1 M, 114GBP per dose)", labels = scales::number_format(scale = 1e6 * 1e-6)) +
  facet_grid(.~AgeStart) +
  expand_limits(y = 0, x = 0)
 

stats_uv %>% 
  filter(Index == "Thres") %>% 
  select(Age0, Arm, Index, M) %>% 
  group_by(Arm, Index) %>% 
  summarise(
    AgeM = Age0[which.max(M)],
    M = M[which.max(M)]
  )








