## DEB functions

#assimilation function
#amount of scale energy assimilated is a function of surface area (l^2)
assimilate <- function(l, f) {
  f * l^2
}

# mobilization function
# amount mobilized into the body depends on energy reserves, current length and investment ratio
mobilize <- function(e, l, g) {
  e * l^2 * ((g + l + l_t)/(g + e))
}

# somatic maintenance function
# amount allocated to maintenance of somatic processes which depends on volume. l_t = 0 if environmental temp is constant
maintain_soma <- function(kappa, l, l_t = 0) {
  kappa * l^2 * (l + l_t)
}

##removed because they are not maturing
# maturity maintenance function
# energy allocated to maintaining reproductively mature status calculated as a fraction of scaled cost of maturity (fraction is cost to maturity for each step)
maintain_maturity <- function(k = 1, uh) {
  k*uh
}

reproduce <- function(kappa, e, l, g, l_t = 0, k = 1, uh = uh){
  (1-kappa) * (e * l^2) * ((g + l + l_t)/(g + e)) - k * uh
}


#growth cost function, when e >= l this is equal to (kappa * mobilize) - maintain_soma 
#energy allocated to growth based on current scaled length, reserves, and energy investment ratio
growcost <- function(kappa, l, e, g, l_t = 0) {
  kappa * l^2 * ((e - l - l_t)/(1 + (e/g)))
}

#convert energy to length (grow)
grow <- function(p_g, l, g, kappa, km){
  p_g * km / (3 * l^2 * kappa)
}


rundeb <- function(data, t, w){
  
  #get available energy for current location
  f <- w[t, data$loc]
  
  #unpack DEB compound parameters
  l <- data$l
  e <- data$reserves
  g <- data$g
  km <- data$km
  kappa <- data$kappa
  uh <- data$uh
  uph <- data$uph
  age <- data$age
  
  er <- data$repro_reserves
  
  #calculate what proportion of avail energy is assimilated based on scaled body size
  p_a <- assimilate(l = l, f = f)
  
  #mobilize from reserves based current scaled length and investment ratio
  #this is calculated in parts at p_s and p_g (p_c = p_s + p_g)
  #equal to p_s + p_g + p_r
  #p_c <- mobilize(e = e, l = l, g = g)
  
  #calculate somatic maint costs
  p_s <- maintain_soma(kappa = kappa, l = l)
  
  #calc energy allocated to growth (if this value is negative than maint exceeds mobilization - starvation) - pull from repro buffer
  p_g <- growcost(kappa = kappa, l = l, e = e, g = g)
  #p_g <- kappa*p_c - p_s
  
  #calc repro costs
  p_repro <- reproduce(kappa = kappa, l = l, e = e, g = g, uh = uh)
  
  p_j <- ifelse(uh < uph, uh, uph)
  
  #if starving, subtract deficit in p_g from p_repro
  p_repro <- ifelse(p_g < 0, p_repro + p_g, p_repro)
  
  der <- (km * g * p_repro)/l^3
  
  er <- ifelse(uh < uph, 0, er + der)
  uh <- ifelse(uh < uph, uh + (p_repro * km), uph)
  pub_age <- ifelse(uh < uph, age, t)
  age <- ifelse(age > 0 & pub_age > age, age, pub_age)
  
  #if starving then no growth so p_g == 0
  p_c <- ifelse(p_g < 0, p_s + p_repro + p_j, p_s + p_g + p_j + p_repro)
  
  #calc change in reserves - if growth is zero don't adjust e for increase in length
  de <- ifelse(p_g < 0, g*km/l^3 * (p_a - p_c),  g*km/l^3 * ( p_a - p_c - (e/(g*kappa) * p_g)))
  
  #adjust reserves - if p_repro was insufficent to meet deficit, set e to -1 to flag as dead
  e <- ifelse(p_repro < 0, -1, e + de)
  
  #if not starving - grow
  newl <-ifelse(p_g < 0, l, l + grow(p_g, l, g, kappa, km))
  
  #check if e > 0 if so update params (don't update dead ones otherwise they resurrect...)
  alive <- ifelse(e < 0, F, T)
  data$alive <- alive
  if(any(alive)){
    data$reserves[alive] <- e[alive]
    data$l[alive] <- newl[alive]
    data$repro_reserves[alive] <- er[alive]
    data$step[alive] <- t
    data$uh[alive] <- uh[alive]
    data$age[alive] <- age[alive]
  }
  print(t)
  return(data)
}

initialize <- function(data, w, mp, start) {

  #initial values
  data$uh <- data$ubh
  data$age <- 0
  data$reserves <- w[1, mp$startloc]
  data$repro_reserves <- 0
  data$l <- data$l_b
  data$startloc <- mp$startloc
  data$loc <- mp$startloc
  data$alive <- T
  
  return(data)
}