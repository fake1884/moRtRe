# This file contains functions to estimate the goodness of fit of the death models

##########################################################################################
GoodnessEinfachSim = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem simulierten Datensatz
  #               mit der einfachen Schätzmethode erzeugt wurde.

  # initialize constants
  # exposure = 1000 = Todesfälle_gesamt
  m = 100
  mu = 70
  sigma = 15
  X = 0:110
  errors_sum = rep(NA, m)

  # estimate the errors
  for(i in (1+2):(m+2)){
    erg = einfachstesModellAlterProjektion(
      cbind(simple_rates[,1], simple_rates[,2], simple_rates[,i]),
      cbind(simple_rates[,1], simple_rates[,2], simple_exposure[,i]))
    par = einfachstesModellAlterEstimateParameters(erg$X, erg$Y)
    errors = ((2*pi*sigma^2)^(-1/2) * exp(- (X-mu)^2/(2*sigma^2)) -
                (2*pi*par$sigma^2)^(-1/2) * exp(- (X-par$mu)^2/(2*par$sigma^2)))^2
    errors_sum[i-2] = sum(errors)
  }
  plot(X, errors)

  return(sum(errors_sum))
}


GoodnessWhittakerSim = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem simulierten Datensatz
  #               mit der Whittaker Schätzmethode erzeugt wurde.

  # initialize constants
  m = 100
  mu = 70
  sigma = 15
  X = 0:110
  Y = (2*pi*sigma^2)^(-1/2) * exp(- (X-mu)^2/(2*sigma^2))
  errors_sum = rep(NA, m)

  # estimate the errors
  for(i in (1+3):(m+3)){
    # let us for now only take the year 1900 (the first year)
    erg = Whittaker(simple_period_data[1:111,i]/sum(simple_period_data[1:111,i]))
    Y.hat = erg
    errors = (Y-Y.hat)^2
    errors_sum[i-3] = sum(errors)
  }
  plot(X, errors)

  return(sum(errors_sum))
}

##########################################################################################
GoodnessEinfachData = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem original Datensatz
  #               mit der einfachen Schätzmethode erzeugt wurde.

  # select half the data points
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,2] %% 2 == 0)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,2] %% 2 == 0)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  X = erg$X
  Y = erg$Y

  # estimate the parameters
  par = einfachstesModellAlterEstimateParameters(X, Y)
  plot(X, Y, type = "l")
  curve((2*pi*par$sigma^2)^(-1/2) * exp(- (x-par$mu)^2/(2*par$sigma^2)), add = T)

  # select the other half of the data points
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,2] %% 2 == 1)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,2] %% 2 == 1)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  X = 0:110
  Y = erg$Y
  Y[1] = 0 # due to the mod-calculation there is no value 0
  Y[111] = 1 - sum(Y[-111])

  # estimate the error
  return(sum((Y-(2*pi*par$sigma^2)^(-1/2) * exp(- (X-par$mu)^2/(2*par$sigma^2)))^2))
}


GoodnessWhittakerData = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem originalen Datensatz
  #               mit der Whittaker Schätzmethode erzeugt wurde.

  # select half the data points
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,2] %% 2 == 0)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,2] %% 2 == 0)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  X = erg$X
  Y = erg$Y

  # estimate death-probs
  v = Whittaker(Y)
  X = 0:110
  plot(X, Y, type = "l")
  lines(X, v)

  # select the other half of the data points
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,2] %% 2 == 1)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,2] %% 2 == 1)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  X = 0:110
  Y = erg$Y
  Y[1] = 0 # due to the mod-calculation there is no value 0
  Y[111] = 1 - sum(Y[-111])

  # estimate the error
  return(sum((v-Y)^2))
}

#########################################################################################
GoodneesLeeSim = function(){


}


GoodneesLeeData = function(){


}
