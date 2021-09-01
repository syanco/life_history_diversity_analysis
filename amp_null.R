################################
####                        ####
####    AMP Null Models     ####
####                        ####
################################

##---- Inits ----##
library(R.matlab)
library(glue)
library(tidyverse)
library(viridis)

source("../life_history_diversity_analysis/AmP_functions.R")

##---- Prep Data ----##

# Prep data steps exactly match analysis workflow from AmpNewPlots.R

allStat <- readMat("../data/AmPdata/allStat.mat")

###NEW AMP FUNCTIONS
getMig <- function(species, deb_data) {
  splist <- unlist(labels(deb_data$allStat)) #get list of all species
  par.names <- unlist(labels(deb_data$allStat[[ #extract labels for target species
    which(splist == species)
  ]]))
  eco <- deb_data$allStat[[
    which(splist == species)
  ]][which(par.names == "ecoCode")]
  mig <- ifelse(length(eco[[1]][[5]]) == 0, "No", eco[[1]][[5]])
  return(mig)
}

getHab <- function(species, deb_data) {
  splist <- unlist(labels(deb_data$allStat)) #get list of all species
  par.names <- unlist(labels(deb_data$allStat[[ #extract labels for target species
    which(splist == species)
  ]]))
  eco <- deb_data$allStat[[
    which(splist == species)
  ]][which(par.names == "ecoCode")]
  hab <- ifelse(length(eco[[1]][[3]]) == 0, NA, eco[[1]][[3]])
  return(hab)
}
###


deb <- allStat

#pull species list over which to iterate accessor functions
splist <- unlist(labels(deb$allStat))
splist <- splist[1:(length(splist)-2)] #remove last two entries which are not species

#extract ecoCode vector
eco <- lapply(splist, getPar, par = "ecoCode", deb_data = deb)


#pull species list over which to iterate accessor functions
splist <- unlist(labels(deb$allStat))

splist <- splist[1:(length(splist)-2)] #remove last two entries which are not species

deb_df <- data.frame(
  species = unlist(splist),
  model = unlist(lapply(splist, getPar, par ="model", deb_data = deb)), #which version of DEB model
  complete = unlist(lapply(splist, getPar, par ="COMPLETE", deb_data = deb)), #model completeness score
  climate = unlist(lapply(eco, getPrimeClim)), #primary climate code
  climate_full = unlist(lapply(eco, getClim)), #full climate code
  class = unlist(lapply(splist, getPar, par ="class", deb_data = deb)), #taxonomic class
  family = unlist(lapply(splist, getPar, par ="family", deb_data = deb)), #taxonomic family
  phylum = unlist(lapply(splist, getPar, par ="phylum", deb_data = deb)), #taxonomic phylum
  #mig = unlist(lapply(eco, getMig)), #migratory status
  mig = unlist(lapply(splist, getMig, deb_data = deb)),
  habitat = unlist(lapply(splist, getHab, deb_data = deb)),
  max_l = unlist(lapply(splist, getPar, par ="L.m", deb_data = deb)), #max length
  E_0 = unlist(lapply(splist, getPar, par ="E.0", deb_data = deb)), #embryo cost
  z = unlist(lapply(splist, getPar, par ="z", deb_data = deb)), #zoom factor
  kappa = unlist(lapply(splist, getPar, par = "kap", deb_data = deb)), #somatic allocation fraction (kappa)
  k.m = unlist(lapply(splist, getPar, par ="k.M", deb_data = deb)), #maintenance ratio (k)
  g = unlist(lapply(splist, getPar, par ="g", deb_data = deb)), #energy investment ratio (g)
  v=unlist(lapply(splist, getPar, par ="v", deb_data = deb)),
  E.m=unlist(lapply(splist, getPar, par ="E.m", deb_data = deb)),
  p.m = unlist(lapply(splist, getPar, par ="p.M", deb_data = deb)),
  l.i = unlist(lapply(splist, getPar, par ="L.i", deb_data = deb))) #energy conductance (v)

#filter for terrestrial non-marine animals
deb_df <- deb_df %>%
  filter(complete > 2) %>%
  filter(phylum == "Chordata") %>%
  filter(str_detect(habitat, "T")) %>%
  filter(!str_starts(climate, "M")) %>%
  mutate(climate = factor(climate), mig = factor(mig)) %>% 
  filter(mig == "No") %>% 
  filter(class != "Amphibia") %>% 
  mutate(kappa_scaled = kappa*v)

levels(deb_df$climate) <- c("Tropical","Arid","Temperate","Boreal","Polar")
# levels(deb_df$mig) <- c("long-distance","short-distance","no migration", "topor/hibernation (temp)", "torpor/hibernation (moisture)")


##---- Build Null Models ----##

# Build full set of null models allowing conditioning on each climate 
# classification.  Idea is to draw many samples of size n (w/ replacement) 
# from each climate group, where n = sample size of some other group.  Then, 
# ask about the quantile of the resultant distribution that matches the 
# observed value of the summary statistic.  Stats to use include: range, 
# median, and max.


#-- New Functions

# get samples of size n(set1) from set2
getSampStats <- function(real, null){
  n <- nrow(real) # set n
  samp <- sample(x = unlist(null), size = n, replace = T) # draw samples
  out <- data.frame( #output as 1 row df
    med = median(samp),
    mx = max(samp),
    rng = max(samp)-min(samp),
    mu = mean(samp),
    iqr = IQR(samp),
    q25 = quantile(samp, probs = 0.25),
    q50 = quantile(samp, probs = 0.50),
    q75 = quantile(samp, probs = 0.75),
    var = var(samp)
  ) %>% 
    mutate(q50to75 = q75-q50)
  return(out)
}

repSampStats <- function(real, null, reps){
  out0 <- replicate(reps, getSampStats(real=real, null=null), simplify = F)
  out <- do.call("rbind", out0)
  return(out)
}

#TODO - fix this so it's not hardcoded to kappa_scaled
getReal <- function(data, clim, var){
  dat1 <- data %>% 
    filter(climate == clim) %>% 
    select(!!var) 
  
  real <- data.frame(
    med = median(dat1[,1]),
    mx = max(dat1[,1]),
    rng = max(dat1[,1])-min(dat1[,1]),
    mu = mean(dat1[,1]),
    q25 = quantile(dat1[,1], probs = 0.25),
    q50 = quantile(dat1[,1], probs = 0.50),
    q75 = quantile(dat1[,1], probs = 0.75),
    var = var(dat1[,1])
  ) %>% 
    mutate(q50to75 = q75-q50,
           iqr = q75-q25
    )
  return(real)
}

compare <- function(nullclim, realclim, data, var, reps = 10000){
  
  null_dat <- data %>% 
    filter(climate == nullclim) %>% 
    select(!!var)
  
  real_dat <- data %>% 
    filter(climate == realclim) %>% 
    select(!!var)
  
  reals <- getReal(data, clim = realclim, var=var)
  
  nulldist <- repSampStats(real = real_dat, null = null_dat, reps)
  
  pvals <- data.frame(
    med = sum(nulldist$med < reals$med)/reps,
    mx = sum(nulldist$mx < reals$mx)/reps,
    rng = sum(nulldist$rng < reals$rng)/reps,
    mu = sum(nulldist$mu < reals$mu)/reps,
    q25 = sum(nulldist$q25 < reals$q25)/reps,
    q50 = sum(nulldist$q50 < reals$q50)/reps,
    q75 = sum(nulldist$q75 < reals$q75)/reps,
    q50to75 = sum(nulldist$q50to75 < reals$q50to75)/reps,
    iqr = sum(nulldist$iqr < reals$iqr)/reps,
    var = sum(nulldist$var < reals$var)/reps,
    null = nullclim,
    test = realclim
  )
  
  return(pvals)
}


#-- Tests

S <- unique(as.character(deb_df$climate))

# Kappa Scaled
res_list <- list()
nms <- list()
for(i in 1:length(clims)){
  for(j in 1: length(clims)){
    idx <- j+(length(clims)*(i-1))
    res_list[[idx]] <- compare(nullclim = clims[i], realclim = clims[j], 
                               data = deb_df, var = "kappa_scaled", reps = 10000)
    nms[[idx]] <- glue("Null: {clims[i]}; Test: {clims[j]}")
  }
}

names(res_list) <- nms

res_df <- do.call("rbind", res_list)
levels(res_df$null) <- c("Tropical","Arid","Temperate","Boreal","Polar")
levels(res_df$test) <- c("Tropical","Arid","Temperate","Boreal","Polar")

# G

res_list_g <- list()
nms_g <- list()
for(i in 1:length(clims)){
  for(j in 1: length(clims)){
    idx <- j+(length(clims)*(i-1))
    res_list_g[[idx]] <- compare(nullclim = clims[i], realclim = clims[j], 
                               data = deb_df, var = "g", reps = 10000)
    nms_g[[idx]] <- glue("Null: {clims[i]}; Test: {clims[j]}")
  }
}

names(res_list_g) <- nms_g

res_g_df <- do.call("rbind", res_list_g)

#-- Plot

# IQR - Kappa
ggplot(res_df, aes(x=null, y=test)) +
  geom_tile(aes(fill = iqr)) +
  scale_fill_viridis(direction = -1) +
  geom_text(aes(label=iqr)) +
  ylim(levels(res_df$test)) +
  xlim(levels(res_df$null)) +
  ggtitle(expression(paste("Probability of observed IQR given null IQR dsitribution for ", kappa['scaled']))) +
  theme_minimal()

# IQR - g
ggplot(res_g_df) +
  geom_tile(aes(x=null, y=test, fill = iqr)) +
  scale_fill_viridis(direction = -1) +
  ylim(levels(res_df$test)) +
  xlim(levels(res_df$null)) +
  ggtitle("Probability of observed IQR given null IQR dsitribution for g") +
  theme_minimal()

# var - Kappa
ggplot(res_df, aes(x=null, y=test)) +
  geom_tile(aes(fill = var)) +
  scale_fill_viridis(direction = -1) +
  geom_text(aes(label=var)) +
  ylim(levels(res_df$test)) +
  xlim(levels(res_df$null)) +
  ggtitle(expression(paste("Probability of observed variance given null variance dsitribution for ", kappa['scaled']))) +
  theme_minimal()

# var - g
ggplot(res_g_df,aes(x=null, y=test)) +
  geom_tile(aes(fill = var)) +
  scale_fill_viridis(direction = -1) +
  geom_text(aes(label=var)) +
  ylim(levels(res_df$test)) +
  xlim(levels(res_df$null)) +
  ggtitle("Probability of observed variance given null variance dsitribution for g") +
  theme_minimal()

#-- Scratch



# compare(nullclim = "Tropical", realclim = "Polar", data = deb_df, var = "kappa_scaled", reps = 10000)
# 
# deb_split <- deb_df %>% 
#   group_split(climate)
# 
# getSampStats(deb_split[[1]]$kappa_scaled, deb_split[[2]]$kappa_scaled)
# testrep <- repSampStats(deb_split[[5]]$kappa_scaled, deb_split[[1]]$kappa_scaled, 10000)
# 
# deb_split[[1]]$climate[1]
# deb_split[[5]]$climate[1]
# 
# 
# 
# polar <- getReal(deb_df, clim = "Polar")
# 
# 
# sum(testrep$rng<real$rng)/length(testrep$rng)
# sum(testrep$q50to75<real$q50to75)/length(testrep$q50to75)
# sum(testrep$q75<real$q75)/length(testrep$q75)
# 
# 
# sum(testrep$iqr<real$iqr)/length(testrep$iqr)
# 
# sum(testrep$med<real$med)/length(testrep$med)
# sum(testrep$mx<real$mx)/length(testrep$mx)
# sum(testrep$mu<real$mu)/length(testrep$mu)
# 
# 
# hist(testrep$med)
# abline(v=real$med, col = "red")
# 
# hist(testrep$rng)
# abline(v=real$rng, col = "red")
# 
# hist(testrep$mx)
# abline(v=real$mx, col = "red")
# 
# hist(testrep$mu)
# abline(v=real$mu, col = "red")
