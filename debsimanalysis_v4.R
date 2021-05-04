source("../GAEMM/R/seasonalworld.R")
source("debsim_v4.R")

library(tidyverse)
library(wesanderson)
library(cowplot)
library(ggthemes)


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

debsim <- function(modelparams){
  #allocation - self/reproduction -
  #fast ---> slow
  kappa <- seq(modelparams$klim[1], modelparams$klim[2], by = modelparams$increment)
  
  #somatic maintenance rate coeffcient -
  #maint relative to growth per unit volume [pm]/[Eg]
  #higher = higher maintenance costs and lower growth costs
  #slow ---> fast
  km <- modelparams$kmlim[1]
  if(length(modelparams$kmlim) > 1){
    km <- seq(modelparams$kmlim[1], modelparams$kmlim[2], 
              by = modelparams$increment)
  } 
               
  #energy conductance - 
  #represents reserve capacity (lower v = higher Em) 
  #or higher v = higher maximum assimilation rate
  v <-  modelparams$vlim[1]
  if(length(modelparams$vlim) > 1){
    v <- seq(modelparams$vlim[1], modelparams$vlim[2], 
             by = modelparams$increment)
  }
  
  #zoom factor (Maximum structural length)
  #can be changed to a vector to iterate over multiple sizes
  z <- 1

  
  #scaled length - length at birth relative to maximum length
  l_b <- modelparams$l_startlim[1]
  if(length(modelparams$l_startlim) > 1){
    seq(modelparams$l_startlim[1], modelparams$l_startlim[2], 
        length.out = 10)
  } 
  
  if(modelparams$startstate == "birth"){
    shbp <- seq(modelparams$shbp[1], modelparams$shbp[2], length.out = 5)
    debdata <- expand.grid(kappa = kappa, km = km, v = v, z = z, l_b = l_b, shbp = shbp)
    #assumes k = 1 (k.j/k.m)
    debdata$ubh <- (1 - debdata$kappa) * debdata$l_b^3
    debdata$uph <- debdata$ubh/debdata$shbp
    
  } else {
    debdata <- expand.grid(kappa = kappa, km = km, v = v, z = z, l_b = l_b)
    debdata$ubh <- 0
    debdata$uph <- 0
  }

  #calc additional compound params
  #Energy investment ratio [Eg]/kappa*[Em]
  #growth cost per unit of vol relative to maximum energy per unit of volume allocated to soma
  #higher is higher energy investment into growth
  #g is only compound param that scales with z
  debdata$g <- debdata$v/(debdata$km*debdata$z)
  
  debdata <- initialize(data = debdata, w = world, mp = modelparams)
  
  #run steps for each parameter set
  for (i in 1:modelparams$steps) {
    debdata <- rundeb(data = debdata, t = i, w = world)
  }
  
  #return data frame of values
  return(debdata)
}

# #GROWTH FROM BIRTH 
# #set parameters for simulations
# modelparams <- list(worldsize = 181,
#                     steps = 365,
#                     startloc = 175,
#                     klim = c(0.01, 0.99),
#                     kmlim = c(0.01, 0.99),
#                     vlim = c(0.01, 0.99),
#                     zlim = c(1, 25),
#                     l_startlim = c(0.01, 0.5),
#                     shbplim = c(0.01, 0.5))
# 
# #generate world of f values
# world <- make_seasonalworld(modelparams, type = "sine", meanrange = c(0.7,0.85))
# plotworld(world)
# 
# modelparams$startloc <- 175
# debdata_175 <- debsim(modelparams)
# 
# modelparams$startloc <- 133
# debdata_133 <- debsim(modelparams)
# 
# modelparams$startloc <- 91
# debdata_91 <- debsim(modelparams)
# 
# debdata <- bind_rows(debdata_175, debdata_133, debdata_91)

#START AT PUBERTY
#set parameters for simulations
modelparams <- list(worldsize = 181,
                    steps = 365,
                    startloc = 175,
                    klim = c(0.01, 0.99),
                    kmlim = c(0.01, 0.99),
                    vlim = 1, #fixed v
                    l_startlim = 0.7, #fixed l
                    increment = 0.01,
                    startstate = "puberty")

#generate world of f values
world <- make_seasonalworld(modelparams, type = "sine", meanrange = c(0.7,0.85))
plotworld(world)

#run simulation for each scenario
modelparams$startloc <- 175
debdata_175 <- debsim(modelparams)

modelparams$startloc <- 133
debdata_133 <- debsim(modelparams)

modelparams$startloc <- 91
debdata_91 <- debsim(modelparams)

debdata <- bind_rows(debdata_175, debdata_133, debdata_91)

pal_3 <- wes_palette("Darjeeling1", 3)


#remove dead and label for seasonality
debfiltered <- debdata %>%
  filter(alive == T) %>%
  mutate(seasonality = factor(startloc, labels = c("aseasonal", "low", "high"))) %>%
  mutate(seasonality = forcats::fct_relevel(seasonality, "high", "low", "aseasonal"))

#kappa as a function of average reserves per unit of volume dedicated to reproduction per day

debfiltered %>%
    ggplot() + 
  geom_line(aes(x = kappa, y  = repro_reserves/365,
                           color = g,
                           group = interaction(g, seasonality)),
            key_glyph = "abline") +
  scale_color_viridis_c(direction = 1, begin = 0.1, end = 0.8,
                        option = "inferno",
                        guide = guide_colourbar(
                          title = "Energy\nInvestment\nRatio (g)\n",
                         # direction = "vertical",
                          #title.position = "top",
                          barwidth = 0.75, barheight = 4)
                        ) +
 
  xlim(c(0,1)) +
  facet_wrap(~seasonality, ncol = 1, 
             labeller = labeller(
               seasonality = c("aseasonal" = "Aseasonal", 
                               "low" = "Moderate Seasonality", 
                               "high" = "High Seasonality"))) +
  ylab(label = "Daily Reproductive Potential") +
  xlab(expression(paste("Somatic Allocation Fraction (", kappa, ")"))) +
  coord_fixed(0.5) +
  theme_few() +
  
  theme(
    legend.position = c(.99, 0.98),
    legend.justification = c("right", "top"),
    #legend.box.just = "center",
    legend.box.background = element_blank(),
    #legend.key.size = unit(0.25, 'lines'), #change legend key size
    #legend.key.height = unit(.25, 'lines'), #change legend key height
    #legend.key.width = unit(.25, 'lines'), #change legend key width
    legend.title = element_text(size=8), #change legend title font size
    legend.title.align = 0.5,
    legend.text = element_text(size=8),
    legend.margin = margin(0,0,0,0),
    legend.spacing = unit(0,"lines"),
    strip.background = element_blank())

ggsave("SimKappaGPlot.pdf", dpi = 300)


debfiltered %>%
  ggplot() + 
  geom_line(aes(x = kappa, y  = repro_reserves/365,
                color = l,
                group = interaction(km, v, z, seasonality)),
            key_glyph = "abline") +
  scale_color_viridis_c(direction = 1, begin = 0.1, end = 0.8,
                        option = "inferno",
                        
                        guide = guide_colourbar(
                          title = "Scaled\nlength",
                          # direction = "vertical",
                          #title.position = "top",
                          barwidth = 0.75, barheight = 4)
  ) +
  
  xlim(c(0,1)) +
  facet_wrap(~seasonality, ncol = 1, 
             labeller = labeller(
               seasonality = c("aseasonal" = "Aseasonal", 
                               "low" = "Moderate Seasonality", 
                               "high" = "High Seasonality"))) +
  ylab(label = "Daily Reproductive Potential") +
  xlab(expression(paste("Somatic Allocation Fraction (", kappa, ")"))) +
  coord_fixed(0.5) +
  theme_few() +
  
  theme(
    legend.position = c(.98, 0.98),
    legend.justification = c("right", "top"),
    #legend.box.just = "center",
    legend.box.background = element_blank(),
    #legend.key.size = unit(0.25, 'lines'), #change legend key size
    #legend.key.height = unit(.25, 'lines'), #change legend key height
    #legend.key.width = unit(.25, 'lines'), #change legend key width
    legend.title = element_text(size=8), #change legend title font size
    legend.title.align = 0.5,
    legend.text = element_text(size=8),
    legend.margin = margin(0,0,0,0),
    legend.spacing = unit(0,"lines"),
    strip.background = element_blank())

ggsave("SimKappaLengthPlot.pdf", dpi = 300)


#EQUAL MEAN RUN - SUPPLEMENTS
#START AT PUBERTY
#set parameters for simulations
modelparams <- list(worldsize = 181,
                    steps = 365,
                    startloc = 175,
                    klim = c(0.01, 0.99),
                    kmlim = c(0.01, 0.99),
                    vlim = 1, #fixed v
                    l_startlim = 0.7, #fixed l
                    increment = 0.01,
                    startstate = "puberty")

#generate world of f values
world <- make_seasonalworld(modelparams, type = "sine", meanf = c(0.75))
plotworld(world)

#run simulation for each scenario
modelparams$startloc <- 175
debdata_175 <- debsim(modelparams)

modelparams$startloc <- 133
debdata_133 <- debsim(modelparams)

modelparams$startloc <- 91
debdata_91 <- debsim(modelparams)

debdata_em <- bind_rows(debdata_175, debdata_133, debdata_91)

#remove dead and label for seasonality
#remove dead and label for seasonality
debfiltered_em <- debdata_em %>%
  filter(alive == T) %>%
  mutate(seasonality = factor(startloc, labels = c("aseasonal", "low", "high"))) %>%
  mutate(seasonality = forcats::fct_relevel(seasonality, "high", "low", "aseasonal"))


debfiltered_em %>%
  ggplot() + 
  geom_line(aes(x = kappa, y  = repro_reserves/365,
                color = g,
                group = interaction(g, seasonality)),
            key_glyph = "abline") +
  scale_color_viridis_c(direction = 1, begin = 0.1, end = 0.8,
                        option = "inferno",
                        guide = guide_colourbar(
                          title = "Energy\nInvestment\nRatio (g)\n",
                          # direction = "vertical",
                          #title.position = "top",
                          barwidth = 0.75, barheight = 4)
  ) +
  
  xlim(c(0,1)) +
  facet_wrap(~seasonality, ncol = 1, 
             labeller = labeller(
               seasonality = c("aseasonal" = "Aseasonal", 
                               "low" = "Moderate Seasonality", 
                               "high" = "High Seasonality"))) +
  ylab(label = "Daily Reproductive Potential") +
  xlab(expression(paste("Somatic Allocation Fraction (", kappa, ")"))) +
  coord_fixed(0.5) +
  theme_few() +
  
  theme(
    legend.position = c(.99, 0.98),
    legend.justification = c("right", "top"),
    #legend.box.just = "center",
    legend.box.background = element_blank(),
    #legend.key.size = unit(0.25, 'lines'), #change legend key size
    #legend.key.height = unit(.25, 'lines'), #change legend key height
    #legend.key.width = unit(.25, 'lines'), #change legend key width
    legend.title = element_text(size=8), #change legend title font size
    legend.title.align = 0.5,
    legend.text = element_text(size=8),
    legend.margin = margin(0,0,0,0),
    legend.spacing = unit(0,"lines"),
    strip.background = element_blank())

ggsave("SimKappaGEMPlot.pdf", dpi = 300)  

debfiltered_em %>%
  ggplot() + 
  geom_line(aes(x = kappa, y  = repro_reserves/365,
                color = l,
                group = interaction(km, v, z, seasonality)),
            key_glyph = "abline") +
  scale_color_viridis_c(direction = 1, begin = 0.1, end = 0.8,
                        option = "inferno",
                        
                        guide = guide_colourbar(
                          title = "Scaled\nlength",
                          # direction = "vertical",
                          #title.position = "top",
                          barwidth = 0.75, barheight = 4)
  ) +
  
  xlim(c(0,1)) +
  facet_wrap(~seasonality, ncol = 1, 
             labeller = labeller(
               seasonality = c("aseasonal" = "Aseasonal", 
                               "low" = "Moderate Seasonality", 
                               "high" = "High Seasonality"))) +
  ylab(label = "Daily Reproductive Potential") +
  xlab(expression(paste("Somatic Allocation Fraction (", kappa, ")"))) +
  coord_fixed(0.5) +
  theme_few() +
  
  theme(
    legend.position = c(.98, 0.98),
    legend.justification = c("right", "top"),
    #legend.box.just = "center",
    legend.box.background = element_blank(),
    #legend.key.size = unit(0.25, 'lines'), #change legend key size
    #legend.key.height = unit(.25, 'lines'), #change legend key height
    #legend.key.width = unit(.25, 'lines'), #change legend key width
    legend.title = element_text(size=8), #change legend title font size
    legend.title.align = 0.5,
    legend.text = element_text(size=8),
    legend.margin = margin(0,0,0,0),
    legend.spacing = unit(0,"lines"),
    strip.background = element_blank())

ggsave("SimKappaLengthEMPlot.pdf", dpi = 300)  