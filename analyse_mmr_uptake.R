library(tidyverse)

theme_set(theme_bw())


load(file = here::here("data", "mmr_hh.rdata"))


uptakes %>% 
  nrow()

uptakes %>% 
  pull(HKey) %>% 
  unique() %>% 
  length()

uptakes %>% 
  summarise(Coverage = mean(MMR))

uptakes %>% 
  summarise(N = n(), .by = N_CHD) %>% 
  arrange(N_CHD) %>% 
  mutate(P = N / sum(N))


glm(as.integer(MMR) ~ Age + as.character(BO) + Gender + TypeOfClass + as.character(N_CHD), data = uptakes, family = binomial) %>% 
  summary()

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("Age"))

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("TypeOfClass"))

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD"))

uptakes %>% 
  summarise(Coverage = mean(MMR), First = max(Age), .by = c("N_CHD", "BO")) %>% 
  arrange(N_CHD, BO)


uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD", "BO")) %>% 
  arrange(N_CHD, BO) %>% 
  ggplot() +
  geom_line(aes(x = BO, y = Coverage, colour = as.character(N_CHD))) +
  scale_color_discrete("Size") +
  scale_x_continuous("Birth order") +
  scale_y_continuous("MMR %", labels = scales::percent, limits = 0:1)


uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD", "Age")) %>% 
  arrange(N_CHD, Age) %>% 
  ggplot() +
  geom_line(aes(x = Age, y = Coverage, colour = as.character(N_CHD))) +
  scale_color_discrete("Size") +
  scale_x_continuous("Age") +
  scale_y_continuous("MMR %", labels = scales::percent, limits = 0:1)

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD", "TypeOfClass")) %>% 
  arrange(N_CHD, TypeOfClass) %>% 
  ggplot() +
  geom_point(aes(x = TypeOfClass, y = Coverage, colour = as.character(N_CHD))) +
  scale_color_discrete("Size") +
  scale_x_discrete("Type of class") +
  scale_y_continuous("MMR %", labels = scales::percent, limits = 0:1)

uptakes %>% 
  summarise(Coverage = mean(MMR), .by = c("N_CHD", "Gender")) %>% 
  arrange(N_CHD, Gender) %>% 
  ggplot() +
  geom_point(aes(x = Gender, y = Coverage, colour = as.character(N_CHD))) +
  scale_color_discrete("Size") +
  scale_x_discrete("Sex") +
  scale_y_continuous("MMR %", labels = scales::percent, limits = 0:1)


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
  select(BO, MMR, HKey) %>% 
  pivot_wider(names_from = BO, names_prefix = "BO", values_from = MMR) %>% 
  mutate(
    Pattern = paste(BO1, BO2, BO3, BO4)
  ) %>% 
  group_by(Pattern) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  mutate(pr = n / sum(n)) %>% 
  head(10)

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


ss <- uptakes %>% 
  filter(N_CHD == 4) %>% 
  select(BO, MMR, HKey) %>% 
  pivot_wider(names_from = BO, names_prefix = "BO", values_from = MMR) %>% 
  group_by(BO1, BO2, BO3, BO4) %>% 
  count() %>% 
  ungroup()


ss0 <- ss %>% 
  summarise(n = sum(n), .by = "BO1") %>% 
  mutate(p = n / sum(n))


trm <- ss %>% 
  summarise(
    p00 = sum(n *(BO1 == 0) *(BO2 == 0) + n *(BO2 == 0) *(BO3 == 0)) / (sum(n *(BO1 == 0) + n *(BO2 == 0))),
    p01 = sum(n *(BO1 == 0) *(BO2 == 1) + n *(BO2 == 0) *(BO3 == 1)) / (sum(n *(BO1 == 0) + n *(BO2 == 0))),
    p10 = sum(n *(BO1 == 1) *(BO2 == 0) + n *(BO2 == 1) *(BO3 == 0)) / (sum(n *(BO1 == 1) + n *(BO2 == 1))),
    p11 = sum(n *(BO1 == 1) *(BO2 == 1) + n *(BO2 == 1) *(BO3 == 1)) / (sum(n *(BO1 == 1) + n *(BO2 == 1)))
  ) %>% 
  as.list()
  


ss0                      
trm 


trm <- ss %>% 
  group_by()
  summarise(
    p00 = sum(n *(BO1 == 0) *(BO2 == 0) + n *(BO2 == 0) *(BO3 == 0)) / (sum(n *(BO1 == 0) + n *(BO2 == 0))),
    p01 = sum(n *(BO1 == 0) *(BO2 == 1) + n *(BO2 == 0) *(BO3 == 1)) / (sum(n *(BO1 == 0) + n *(BO2 == 0))),
    p10 = sum(n *(BO1 == 1) *(BO2 == 0) + n *(BO2 == 1) *(BO3 == 0)) / (sum(n *(BO1 == 1) + n *(BO2 == 1))),
    p11 = sum(n *(BO1 == 1) *(BO2 == 1) + n *(BO2 == 1) *(BO3 == 1)) / (sum(n *(BO1 == 1) + n *(BO2 == 1)))
  ) %>% 
  as.list()


sims <- tibble(i = 1:62072) %>% 
  mutate(
    BO1 = rbinom(n(), size = 1, prob = sample(ss0$BO1, n(), prob = ss0$p, rep = T)),
    BO2 = (runif(n()) < ifelse(BO1 == 0, trm$p01, trm$p11)) + 0,
    BO3 = (runif(n()) < ifelse(BO2 == 0, trm$p01, trm$p11)) + 0,
    BO4 = (runif(n()) < ifelse(BO3 == 0, trm$p01, trm$p11)) + 0
  )

sims %>% 
  group_by(BO1, BO2, BO3, BO4) %>% 
  count() %>% 
  mutate(
    Pattern = paste(BO1, BO2, BO3, BO4)
  ) %>%
  arrange(-n) %>% 
  ungroup() %>% 
  mutate(pr = n / sum(n)) %>% 
  select(Pattern, n, pr) %>% 
  write_csv("bo2.csv")


ss %>% 
  arrange(-n)

ss0


