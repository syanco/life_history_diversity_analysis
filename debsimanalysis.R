source("../GAEMM/R/seasonalworld.R")
source("debsim.R")


plotworld <- function(world){
  worlddata <- as.data.frame(world)
  names(worlddata) <- 1:181
  worlddata$t <- 1:365
  
  pivot_longer(worlddata, cols = -t, names_to = "loc", values_to = "f") %>%
    filter(loc == 175 | loc == 133 | loc == 91) %>%
    mutate(loc = factor(loc, levels = c("175", "133", "91"))) %>%
    ggplot() +
    geom_path(aes(x = t, y = f, color = loc, group = loc)) +
    scale_color_viridis_d(option = "C", end = 0.6, begin = 0.15) +
    theme_classic()
}

debsim <- function(modelparams, klow = 0.01, khigh = 0.999, glow = 0.01, ghigh = 5){
  
  kappa <- seq(klow, khigh, by = 0.01)
  g <- seq(glow, ghigh, by = 0.1)
  #EgEmrat <- seq(ratlow, rathigh, by = 0.01)
  
  #generate combinations of all kappas and gs
  #kappa_combo <- expand.grid(kappa = kappa, EgEmrat = EgEmrat)
  kappa_combo <- expand.grid(kappa = kappa, g = g)
  #kappa_combo$g <- kappa_combo$EgEmrat / kappa_combo$kappa
  
  #create initial data.frame    
  debdata <- data.frame(kappa = kappa_combo$kappa,
                        g = kappa_combo$g,
                        reserves = world[1, modelparams$startloc],
                        repro_reserves = 0,
                        l = modelparams$l_p,
                        l_p = modelparams$l_p,
                        startloc = modelparams$startloc,
                        loc = modelparams$startloc,
                        alive = T)
  
  #run steps for each parameter set
  for (i in 1:modelparams$steps) {
    debdata <- rundeb(debdata, i, world)
  }
  
  #return dataframe of values
  return(debdata)
}

#set parameters for simulations
modelparams <- list(worldsize = 181,
                    steps = 365,
                    startloc = 175,
                    l_p = 0.7)

#generate world of f values
world <- make_seasonalworld(modelparams, type = "sine", meanrange = c(0.7,0.85))
plotworld(world)

modelparams$startloc <- 175
debdata_175 <- debsim(modelparams)

modelparams$startloc <- 133
debdata_133 <- debsim(modelparams)

modelparams$startloc <- 91
debdata_91 <- debsim(modelparams)

debdata <- bind_rows(debdata_175, debdata_133, debdata_91)

debdata %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(startloc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdata %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(startloc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdata %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(startloc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdata %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = l), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(startloc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdata %>%
  filter(alive == T) %>%
  filter(loc == 91) %>%
  filter(l == max(l))


#generate world of f values - higher amplitude
world <- make_seasonalworld(modelparams, type = "sine", meanrange = c(0.5,0.85))
plotworld(world)

modelparams$startloc <- 175
debdata_175ha <- debsim(modelparams)

modelparams$startloc <- 133
debdata_133ha <- debsim(modelparams)

modelparams$startloc <- 91
debdata_91ha <- debsim(modelparams)

debdataha <- bind_rows(debdata_175ha, debdata_133ha, debdata_91ha)

debdataha %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdataha %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdataha %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

#generate world of f values - constant mean - high
world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.85))
plotworld(world)

modelparams$startloc <- 175
debdata_175hm <- debsim(modelparams)

modelparams$startloc <- 133
debdata_133hm <- debsim(modelparams)

modelparams$startloc <- 91
debdata_91hm <- debsim(modelparams)

debdatahm <- bind_rows(debdata_175hm, debdata_133hm, debdata_91hm)

debdatahm %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdatahm %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdatahm %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

#generate world of f values - constant mean = l_p
world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.7))
plotworld(world)

modelparams$startloc <- 175
debdata_175cm <- debsim(modelparams)

modelparams$startloc <- 133
debdata_133cm <- debsim(modelparams)

modelparams$startloc <- 91
debdata_91cm <- debsim(modelparams)

debdatacm <- bind_rows(debdata_175cm, debdata_133cm, debdata_91cm)

debdatacm %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdatahm %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()

debdata %>%
  filter(alive == T) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
  scale_color_viridis_c() +
  facet_grid(loc ~ .) +
  xlim(c(0,1)) +
  theme_classic()
                                                    
