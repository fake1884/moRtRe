# The Whittaker Functions will most likely not be used

#########################################################################################
GoodnessWhittakerData = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem originalen Datensatz
  #               mit der Whittaker Schätzmethode erzeugt wurde.

  # select half the data points
  firsthalf = min(deathrates1879westmatrix[,1]) +
    length(min(deathrates1879westmatrix[,1]):max(deathrates1879westmatrix[,1]))/2
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,1] <= firsthalf)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,1] <= firsthalf)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  X = erg$X
  Y_first_half = erg$Y

  # estimate death-probs
  v = Whittaker(Y_first_half)

  # select the other half of the data points
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,1] > firsthalf)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,1] > firsthalf)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  Y_second_half = erg$Y

  # just to check
  pdf("../../1 Doku/graphics/ErrorWhittakerDataExample.pdf", width = 10, height = 8)
  plot(X, v, type = "l")
  points(X, Y_first_half, pch = 4)
  points(X,Y_second_half, pch = 4)
  dev.off()

  #calculate the errores
  errors = (v-Y_second_half)^2

  # plot the errors to find outliers
  pdf("../../1 Doku/graphics/ErrorWhittakerDataErrors.pdf", width = 10, height = 8)
  par(mfrow = c(1,2))
  boxplot(errors)
  hist(errors)
  dev.off()
  par(mfrow = c(1,1))

  # estimate the error
  return(sum(errors))
}

GoodnessWhittakerEinfachSim = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem simulierten Datensatz
  #               mit der Whittaker Schätzmethode erzeugt wurde.

  # initialize constants
  m = 100
  mu = 70
  sigma = 15
  X = 0:95
  Y = (2*pi*sigma^2)^(-1/2) * exp(- (X-mu)^2/(2*sigma^2))
  errors_sum = rep(NA, m)

  # estimate the errors
  for(i in (1+3):(m+3)){
    # let us for now only take the year 1900 (the first year)
    erg = Whittaker(simple_data[1:96,i]/sum(simple_data[1:96,i]))
    Y.hat = erg
    errors = (Y-Y.hat)^2
    errors_sum[i-3] = sum(errors)
  }
  plot(X, errors)

  return(sum(errors_sum))
}
