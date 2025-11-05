library(tidyverse)




coverage_mea_cprd <- read_delim(here::here("data", "coverage_cprd_extrapol.csv"), delim = ";")
coverage_mea_cover <- read_delim(here::here("data", "coverage_cover_extrapol.csv"), delim = ";")

pop <- read_delim(here::here("data", "regional_population.csv"), delim = ";")
pop


coverage_mea_cover %>% 
  filter(year == 2018)


coverage_mea_cprd %>% 
  filter(year == 2018)




ve <- 0.97


p_vac <- 0.95
prop <- 0.075


ve <- 1 - (prop / (1 - prop)) * ((1 - p_vac) / p_vac)
ve



x <- 0.995

x * (1 - ve) / (x * (1 - ve) + 1 - x)




