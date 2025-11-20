library(tidyverse)
library(odbc)
library(pacman)



## Main -----


# [UKHSA server required]

pacman::p_load("DBI", "odbc")

# establish connection to V997
con_V997 <- odbc::dbConnect(odbc::odbc(),
                            Driver = "SQL Server",
                            Server = "SQLCLUSCOLvac19\\vac19",
                            Database = "V997_DfESchoolsCensus",
                            Trusted_connection = "True")


yr <- 24




uptakes <- dbGetQuery(con_V997, read_file(here::here("SQL", "SQL_uptake_bo_" + glue::as_glue(yr) + ".sql")))
uptakes <- uptakes %>% filter(BO <= N_CHD) %>% 
  mutate(MMR = ifelse(is.na(MMR_date), 0, 1))

save(uptakes, file = here::here("mmr_hh.rdata"))

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("Age"))

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("TypeOfClass"))

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD"))

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("BO")) %>% 
  arrange(BO)


uptakes %>% 
  summarise(Coverage = mean(MMR), First = max(Age), .by = c("N_CHD", "BO")) %>% 
  arrange(N_CHD, BO)


uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD", "BO")) %>% 
  arrange(N_CHD, BO) %>% 
  ggplot() +
  geom_line(aes(x = BO, y = Coverage, colour = as.character(N_CHD)))


uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD", "Age")) %>% 
  arrange(N_CHD, Age) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = Coverage, colour = as.character(N_CHD)))



uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("BO", "Age")) %>% 
  arrange(BO, Age) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = Coverage, colour = as.character(BO)))

uptakes %>% 
  filter(Age <= 18) %>% 
  summarise(Coverage = mean(MMR), .by = c("Age")) %>% 
  arrange(Age) %>% 
  ggplot() +
  geom_point(aes(x = Age, y = Coverage)) + 
  scale_y_continuous("Coverage, %", limits = 0:1, labels = scales::percent)

uptakes %>% 
  mutate(
    NCyearActual = factor(NCyearActual, c("X", "E1", "E2", "N1", "N2", "R", 1:14))
  ) %>% 
  summarise(Coverage = mean(MMR), .by = c("NCyearActual")) %>% 
  arrange(NCyearActual) %>% 
  ggplot() +
  geom_point(aes(x = NCyearActual, y = Coverage)) + 
  scale_y_continuous("Coverage, %", limits = 0:1, labels = scales::percent)



uptakes %>% 
  filter(N_CHD == 4) %>% 
  summarise(Pattern = paste(MMR, collapse = "_"))



uptakes %>% 
  filter(N_CHD == 5) %>% 
  select(BO, MMR, HKey) %>% 
  pivot_wider(names_from = BO, names_prefix = "BO", values_from = MMR) %>% 
  mutate(
    Pattern = paste(BO1, BO2, BO3, BO4, BO5)
  ) %>% 
  group_by(Pattern) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n)



uptakes %>% 
  filter(N_CHD == 3) %>% 
  select(BO, MMR, HKey) %>% 
  pivot_wider(names_from = BO, names_prefix = "BO", values_from = MMR) %>% 
  mutate(
    Pattern = paste(BO1, BO2, BO3)
  ) %>% 
  group_by(Pattern) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n)
