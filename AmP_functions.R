#Set of functions to extract data from the Add-My-Pet (AmP) Dynamic Energy 
#Budget (DEB) database.  Assumes the database is read into are using the 
#`R.matlab` package.

require(R.matlab)

#extracts DEB parameter estimates for given species and returns as a vector
getPar <- function(species, par, deb_data) {
  splist <- unlist(labels(deb_data$allStat)) #get list of all species
  par.names <- unlist(labels(deb_data$allStat[[ #extract labels for target species
    which(splist == species)
  ]]))
  par <- unlist(deb_data$allStat[[
    which(splist == species)
  ]][
    which(par.names == par)]
  )
  return(par)
}

#extracts primary climate code for given species
getPrimeClim <- function(x){
  full <- x[[1]]
  return(substring(full,1,1))
}

#extarcts full set of climate codes for given species
getClim <- function(x){
  full <- x[[1]]
  return(full)
}

#extracts migratory status code for given species
getMig <- function(x){
  "Ml" %in% x
}