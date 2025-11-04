library(tidyverse)


uptake <- bind_rows(
  tibble(
    Cohort = "24/25_Q1_65",
    Dose = 1,
    Q = 1:3,
    Coverage = c(16.2, 23.2, 27.8)
  ),
  tibble(
    Cohort = "24/25_Q2_65",
    Dose = 1,
    Q = 1:2,
    Coverage = c(15.9, 22.9)
  ),
  tibble(
    Cohort = "24/25_Q3_65",
    Dose = 1,
    Q = 1,
    Coverage = c(16.9)
  ),
  tibble(
    Cohort = "24/25_Q1_70",
    Dose = 1,
    Q = 1:3,
    Coverage = c(25, 33.7, 39.2)
  ),
  tibble(
    Cohort = "24/25_Q2_70",
    Dose = 1,
    Q = 1:2,
    Coverage = c(24.2, 32.9)
  ),
  tibble(
    Cohort = "24/25_Q3_70",
    Dose = 1,
    Q = 1,
    Coverage = c(24.8)
  ),
  tibble(
    Cohort = "23/24_Q1_65",
    Dose = 1,
    Q = 1:4,
    Coverage = c(25.1, 30.6, 34.0, 37.4)
  ),
  tibble(
    Cohort = "23/24_Q2_65",
    Dose = 1,
    Q = 1:3,
    Coverage = c(23.8, 28.7, 33.1)
  ),
  tibble(
    Cohort = "23/24_Q3_65",
    Dose = 1,
    Q = 1:2,
    Coverage = c(22.5, 28.6)
  ),
  tibble(
    Cohort = "23/24_Q4_65",
    Dose = 1,
    Q = 1,
    Coverage = c(20.7)
  ),
  
  tibble(
    Cohort = "23/24_Q1_70",
    Dose = 1,
    Q = 1:4,
    Coverage = c(34.2, 39.5, 43.0, 46.4)
  ),
  tibble(
    Cohort = "23/24_Q2_70",
    Dose = 1,
    Q = 1:3,
    Coverage = c(34.6, 40.6, 45.1)
  ),
  tibble(
    Cohort = "23/24_Q3_70",
    Dose = 1,
    Q = 1:2,
    Coverage = c(33.1, 41.0)
  ),
  tibble(
    Cohort = "23/24_Q4_70",
    Dose = 1,
    Q = 1,
    Coverage = c(32.4)
  )
) %>% 
  mutate(
    Coverage = Coverage / 100
  )


uptake %>% 
  ggplot() +
  geom_line(aes(x = Q, y = Coverage, colour = Cohort))


