library(R.matlab)
allStat <- readMat("~/Downloads/AmPdata/allStat.mat")
source("../life_history_diversity_analysis/AmP_functions.R")

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

deb <- allStat

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

library(tidyverse)

#filter for terrestrial non-marine animals
deb_df <- deb_df %>%
  filter(complete > 2) %>%
  filter(phylum == "Chordata") %>%
  filter(str_detect(habitat, "T")) %>%
  filter(!str_starts(climate, "M")) %>%
  mutate(climate = factor(climate), mig = factor(mig))

levels(deb_df$climate) <- c("Tropical","Arid","Temperate","Boreal","Polar")
levels(deb_df$mig) <- c("long-distance","short-distance","no migration", "topor/hibernation (temp)", "torpor/hibernation (moisture)")

library(wesanderson)
library(cowplot)
library(ggthemes)

pal_5 <- wes_palette("Darjeeling1", 5)

arrow <- data.frame(x = seq(0.5,1, by = 0.005), xend = 1, y = 1, yend = 1)
POLarrow <-
  ggplot() +
  geom_segment(data = arrow, aes(x = x, y = y,
           xend = xend, yend = yend, color = x),
           lineend = "butt",
           size = 5) +
  geom_segment(data = arrow, aes(x = x[100], y = yend,
                               xend = xend, yend = yend, color = x[96]),
             lineend = "butt",
             linejoin = "mitre",
             arrow = arrow(type = "closed", ends = "last"),
             size = 4) +
  geom_segment(data = arrow,aes(x = x[1], y = y[1],
                                   xend = x[1], yend = y[1], color = x[5]),
                 lineend = "butt",
                 linejoin = "mitre",
                 arrow = arrow(type = "closed", ends = "first"),
                 size = 4) +
  scale_color_gradient(low = "grey70", high = "black") +
  annotate("text", x = 0.54, y = 1, label = "Faster", size = 3.5, fontface = "bold") +
  annotate("text", x = 0.96, y = 1, label = "Slower", size = 3.5, color = "white", fontface = "bold") +
  ylim(1,1) +
  xlim(0.4,1.1) +
  coord_fixed() +
  theme_nothing()+
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


POLarrow
## scaled kappa
kapscaledplot <-
deb_df %>%
  filter(mig != "short-distance" | mig != "long-distance") %>%
  ggplot() +
  geom_jitter(aes(y = climate, x = kappa * v, color = climate), alpha = 0.7) +
  scale_color_manual(values = pal_5) +
  scale_size_manual(breaks = seq(0,1, by = 0.1)) +
  guides(color = "none", fill = "none") +
  ylab(label = "") +
  xlab(expression(paste("Somatic Allocation Fraction (", kappa['scaled'], ")"))) +
  theme_half_open() +
  background_grid("x") +
  coord_fixed(max(deb_df$kappa*deb_df$v)/6) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


gplot <-
deb_df %>%
  filter(mig != "short-distance" | mig != "long-distance") %>%
  ggplot() +
  geom_jitter(aes(y = climate, x = g, color = climate), alpha = 0.7) +
  scale_color_manual(values = pal_5) +
  guides(color = "none", fill = "none") +
  ylab(label = "") +
  xlab("Energy Investment Ratio (g)") +
  theme_half_open() +
  background_grid("x") +
  coord_fixed(max(deb_df$g)/6) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


library(patchwork)

f3 <-
(kapscaledplot +
    ggtitle('A') +
    inset_element(POLarrow, left = -0.15, bottom = -.75, right = 1.2, top = 0.5, align_to = 'panel')) +
  (gplot +
     ggtitle('B') +
     inset_element(POLarrow, left = -0.15, bottom = -.75, right = 1.2, top = 0.5, align_to = 'panel'))

ggsave2("fig3.pdf", dpi = 300)
