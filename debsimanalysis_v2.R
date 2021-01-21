source("../GAEMM/R/seasonalworld.R")
source("debsim_v2.R")

library(tidyverse)


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
#klow = 0.01; khigh = 0.999; glow = 0.01; ghigh = 5

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

# debsim <- function(modelparams, klow = 0.01, khigh = 0.999, glow = 0.01, ghigh = 5){
#   
#   kappa <- seq(klow, khigh, by = 0.01)
#   g <- seq(glow, ghigh, by = 0.1)
#   #EgEmrat <- seq(ratlow, rathigh, by = 0.01)
#   
#   #generate combinations of all kappas and gs
#   #kappa_combo <- expand.grid(kappa = kappa, EgEmrat = EgEmrat)
#   kappa_combo <- expand.grid(kappa = kappa, g = g)
#   #kappa_combo$g <- kappa_combo$EgEmrat / kappa_combo$kappa
#   
#   #create initial data.frame    
#   debparms <- data.frame(kappa = kappa_combo$kappa,
#                         g = kappa_combo$g,
#                         reserves = world[1, modelparams$startloc],
#                         repro_reserves = 0,
#                         l = modelparams$l_p,
#                         l_p = modelparams$l_p,
#                         startloc = modelparams$startloc,
#                         loc = modelparams$startloc,
#                         alive = T)
#   
#   datalist <- list()
#   #run steps for each parameter set
#   for (i in 1:nrow(debparms)) {
#     debdata <- debparms[i,]
#     debdata$step <- 0
#     for (j in 1:modelparams$steps) {
#       if(any(debdata$alive == F)) break
#       debdata[j+1,] <- rundeb(debparms[i,], j, world)
#       debdata$step[j+1] <- j
#     }
#     datalist[[i]] <- debdata 
#   }
#   
#   data <- bind_rows(datalist)
#   #return dataframe of values
#   return(data)
# }

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
  filter(alive == T & repro_reserves > 0) %>%
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

debdata_175 %>%
  filter(step == 365) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = , group = g)) +
  scale_color_viridis_c() +
  xlim(c(0,1)) +
  theme_classic()

debdata_133 %>%
  filter(step == 365) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = , group = g)) +
  scale_color_viridis_c() +
  xlim(c(0,1)) +
  theme_classic()

debdata_91 %>%
  filter(step == 365) %>%
  ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = , group = g)) +
  scale_color_viridis_c() +
  xlim(c(0,1)) +
  theme_classic()

#### still need to edit
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(startloc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(startloc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(y = mr/modelparams$steps, x = kappa, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(startloc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = l), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(startloc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = mr/modelparams$steps, y = l), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(startloc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdata %>%
#   filter(alive == T) %>%
#   filter(loc == 91) %>%
#   filter(l == max(l))
# 
# debdata %>%
#   mutate(f = c(world[1,modelparams$startloc], world[1:nrow(debdata)-1,modelparams$startloc])) %>%
#   ggplot() + geom_line(aes(x = step, y = reserves, color = repro_reserves)) +
#   scale_color_viridis_c() +
#   geom_line(aes( x = step, y = f), color = "red")
# theme_classic()
# 
# debdata_175 %>%
#   mutate(f = ifelse(step == 0, world[1,modelparams$startloc], world[step,modelparams$startloc])) %>%
#   ggplot() + geom_line(aes(x = step, y = reserves, color = repro_reserves, group = interaction(kappa, g))) +
#   scale_color_viridis_c() +
#   theme_classic()
# 
# 
# #generate world of f values - higher amplitude, wider range in meanf
# world <- make_seasonalworld(modelparams, type = "sine", meanrange = c(0.5,0.85))
# plotworld(world)
# 
# modelparams$startloc <- 175
# debdata_175ha <- debsim(modelparams)
# 
# modelparams$startloc <- 133
# debdata_133ha <- debsim(modelparams)
# 
# modelparams$startloc <- 91
# debdata_91ha <- debsim(modelparams)
# 
# debdataha <- bind_rows(debdata_175ha, debdata_133ha, debdata_91ha)
# 
# debdataha %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdataha %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdataha %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# #generate world of f values - constant mean - high
# world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.5))
# plotworld(world)
# 
# modelparams$startloc <- 175
# debdata_175hm <- debsim(modelparams)
# 
# modelparams$startloc <- 133
# debdata_133hm <- debsim(modelparams)
# 
# modelparams$startloc <- 91
# debdata_91hm <- debsim(modelparams)
# 
# debdatahm <- bind_rows(debdata_175hm, debdata_133hm, debdata_91hm)
# 
# debdatahm %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdatahm %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdatahm %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# #generate world of f values - constant mean = l_p
# world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.7))
# plotworld(world)
# 
# modelparams$startloc <- 175
# debdata_175cm <- debsim(modelparams)
# 
# modelparams$startloc <- 133
# debdata_133cm <- debsim(modelparams)
# 
# modelparams$startloc <- 91
# debdata_91cm <- debsim(modelparams)
# 
# debdatacm <- bind_rows(debdata_175cm, debdata_133cm, debdata_91cm)
# 
# debdatacm %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdatacm %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# debdatacm %>%
#   filter(alive == T) %>%
#   ggplot() + geom_line(aes(x = kappa, y = repro_reserves/reserves, color = g, group = g), alpha = 0.5) +
#   scale_color_viridis_c() +
#   facet_grid(loc ~ .) +
#   xlim(c(0,1)) +
#   theme_classic()
# 
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + 
#   geom_line(aes(x = mr/modelparams$steps, y = repro_reserves, color = g, group = g), 
#             alpha = 0.5) +
#   scale_color_viridis_c(name = 
#                           expression(atop(atop("Energy", "Investment"), 
#                                           atop("Ratio (g)", "",)))) +
#   facet_wrap(~startloc, ncol = 1, labeller = labeller(startloc = lat_labs)) +
#   xlim(c(0,1)) +
#   xlab(expression(paste("Metabolic rate"))) +
#   ylab("Reproductive Potential")
# 
# 
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + 
#   geom_line(aes(x = kappa, y = mr/modelparams$steps, color = g, group = g), 
#             alpha = 0.5) +
#   scale_color_viridis_c(name = 
#                           expression(atop(atop("Energy", "Investment"), 
#                                           atop("Ratio (g)", "",)))) +
#   facet_wrap(~startloc, ncol = 1, labeller = labeller(startloc = lat_labs)) +
#   xlim(c(0,1)) +
#   xlab(expression(paste("Somatic Allocation Fraction (", kappa, ")"))) +
#   ylab("Metabolic rate")        
# 
# 
# modelparams$startloc <- 91
# 
# world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.7))
# debdata_f7$meanf <- 0.7
# debdata_f7 <- debsim(modelparams)
# 
# world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.8))
# debdata_f8 <- debsim(modelparams)
# debdata_f8$meanf <- 0.8
# 
# world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.9))
# debdata_f9 <- debsim(modelparams)
# debdata_f9$meanf <- 0.9
# 
# world <- make_seasonalworld(modelparams, type = "sine", meanf = c(1))
# debdata_f10 <- debsim(modelparams)
# debdata_f10$meanf <- 1
# 
# debdata <- bind_rows(debdata_f7, debdata_f8, debdata_f9, debdata_f10)
# 
# debdata %>%
#   filter(alive == T) %>%
#   ggplot() + 
#   geom_line(aes(x = kappa, y = repro_reserves, color = g, group = g), 
#             alpha = 0.5) +
#   scale_color_viridis_c(name = 
#                           expression(atop(atop("Energy", "Investment"), 
#                                           atop("Ratio (g)", "",)))) +
#   facet_grid(meanf ~ .) +
#   xlim(c(0,1)) +
#   xlab(expression(paste("Somatic Allocation Fraction (", kappa, ")"))) +
#   ylab("Reproductive Potential")
# 
# cdata %>%
#   filter(class == "Mammalia" | class == "Aves" | class == "Reptilia") %>%
#   filter(COMPLETE > 2) %>%
#   filter(model =="std") %>% 
#   ggplot() +
#   geom_jitter(aes(y = ecoCode_climate_top, x = (p.M/p.Am), color = ecoCode_climate_top, alpha = 0.2)) +
#   facet_grid(class ~ .) +
#   scale_color_viridis_d() + theme(legend.position = "none")
