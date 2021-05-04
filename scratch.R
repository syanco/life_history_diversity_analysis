max_kaps <- comb %>% 
  group_by(g) %>% 
  filter(kappa == max(kappa)) %>% 
  summarise(max_kap = kappa, g = g, repro_reserves = repro_reserves)
