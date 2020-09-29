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
grow <- function(kappa, l, e, g, l_t = 0) {
  kappa * l^2 * ((e - l - l_t)/(1 + (e/g)))
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
  
  #calculate what proportion of avail energy is assimilated based on scaled body size
  p_a <- assimilate(l = l, f = f)
  
  #mobilize from reserves based current scaled length and investment ratio
  #this is calcuated in parts at p_s and p_g (p_c = p_s + p_g)
  #equal to p_s + p_g + p_r
  p_c <-  mobilize(e = e, l = l, g = g)
  
  #calculate somatic maint costs
  p_s <- maintain_soma(kappa = kappa, l = l)
  
  #calc energy allocated to growth
  #p_g <- grow(kappa = kappa, l = l, e = e, g = g)
  p_g <- (kappa * p_c) - p_s
  p_g <- ifelse(p_g < 0, 0, p_g)
  
  p_repro <- reproduction(kappa = kappa, l = l, e = e, g = g)
  #if e > l then equal to reproduction equation above but if starving (e < l) then will be only a portion of repro value above.
  p_repro <- ifelse(l < l_p, 0, p_repro)
  er <- data$repro_reserves + p_repro
  
  e <- e + (p_a - (p_s + p_g + p_repro))
  e <- ifelse(e > 1, 1, e)
  
  if(e < 0 && abs(e) <= p_s){
    er <- er + e
    e <- 0
  }
  
  newl <- l + p_g
  newl <- ifelse(l > 1, 1, newl)
  er <- ifelse(er < 0, 0, er)
  
  data$reserves <- e
  data$l <- newl
  data$repro_reserves <- er
  #kill individuals with e < 0
  data[data$reserves < 0, "alive"] <- F
  
  return(data)
}
