## DEB functions

#assimilation function
#amount of scale energy assimilated is a function of surface area (l^2)
assimilate <- function(l, f) {
  f * l^2
}

# mobilization function
# amount mobilized into the body depends on energy reserves, current length and investment ratio
mobilize <- function(e, l, g, l_t = 0) {
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
# maintain_maturity <- function(k = 1, u_pH) {
#  k*u_pH
# }

reproduction <- function(kappa, e, l, g, l_t = 0){
  (1-kappa) * (e * l^2) * ((g + l + l_t)/(g + e))
}

#growth cost function, when e >= l this is equal to (kappa * mobilize) - maintain_soma 
#energy allocated to growth based on current scaled length, reserves, and energy investment ratio
growcost <- function(kappa, l, e, g, l_t = 0) {
  kappa * l^2 * ((e - l - l_t)/(1 + (e/g)))
}

grow <- function(p_g, l, g, kappa){
  p_g/(3 * l^2 * g * kappa)
}

rundeb <- function(data, t, world, modelparams){
  
  #get available energy for current location
  f <- world[t, data$loc]
  
  #unpack DEB compound parameters
  l <- data$l
  l_p <- data$l_p
  e <- data$reserves
  g <- data$g
  kappa <- data$kappa
  er <- data$repro_reserves
  
  #calculate what proportion of avail energy is assimilated based on scaled body size
  
  p_a <- assimilate(l = l, f = f)
  
  #mobilize from reserves based current scaled length and investment ratio
  #this is calcuated in parts at p_s and p_g (p_c = p_s + p_g)
  #equal to p_s + p_g + p_r
  
  p_c <- mobilize(e = e, l = l, g = g)
  
  #calculate somatic maint costs
  p_s <- maintain_soma(kappa = kappa, l = l)
  
  #calc energy allocated to growth (if this value is negative than maint exceeds mobilization - starvation) - pull from repro
  p_g <- growcost(kappa = kappa, l = l, e = e, g = g)
  #p_g <- kappa*p_c - p_s

  p_repro <- reproduction(kappa = kappa, l = l, e = e, g = g)
  
  #check for starvation p_g < 0 - pull from repro_buffer and adjust reserve calc
  if(p_g < 0){
    p_repro <- p_repro + p_g
    er <- er + p_repro
    p_c <- ifelse(er < 0, p_s + er, p_s)
    de <- 1/l^3 * ( p_a - p_c)
  }
  
  p_c <- p_s + p_g + p_repro
  de <- 1/l^3 * ( p_a - p_c - (e/(g*kappa) * p_g))
  e <- e + de
  
  #e <- e + (p_a - (p_s + p_g + p_repro))
  e <- ifelse(e > 1, 1, e)
  
  newl <-ifelse(p_g < 0, 0, l + grow(p_g, l, g, kappa))
  #newl <- ifelse(l > 1, 1, newl)
  er <- ifelse(er < 0, 0, er)
  
  data$reserves <- e
  data$l <- newl
  data$repro_reserves <- er
  #kill individuals with e < 0
  data[data$reserves < 0, "alive"] <- F
  return(data)
}



