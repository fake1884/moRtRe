# This file contains functions to estimate the goodness of fit of the death models

##########################################################################################
# use the historical data
GoodnessEinfachData = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem original Datensatz
  #               mit der einfachen Schätzmethode erzeugt wurde.

  # select half the data points
  firsthalf = min(deathrates1879westmatrix[,1]) +
    length(min(deathrates1879westmatrix[,1]):max(deathrates1879westmatrix[,1]))/2
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,1] <= firsthalf)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,1] <= firsthalf)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  X = erg$X
  Y = erg$Y

  # estimate the parameters
  par = einfachstesModellAlterEstimateParameters(X, Y)

  # select the other half of the data points
  rates_last = subset(deathrates1879westmatrix, deathrates1879westmatrix[,1] > firsthalf)
  exposure_last = subset(exposure1879westmatrix, exposure1879westmatrix[,1] > firsthalf)
  erg_last = einfachstesModellAlterProjektion(rates_last, exposure_last)
  Y_last = erg_last$Y

  # plot
  pdf("../../1 Doku/graphics/ErrorSimpleNotFair.pdf", width = 10, height = 8)
  plot(X, (2*pi*par$sigma^2)^(-1/2) * exp(- (X-par$mu)^2/(2*par$sigma^2)), type = "l",
       ylab = "Sterblichkeit")
  points(X, Y)
  points(X,Y_last, pch = 4)
  dev.off()
  erg$Todesfälle
  erg_last$Todesfälle

  # estimate the error
  return(sum((Y_last - (2*pi*par$sigma^2)^(-1/2) * exp(- (X-par$mu)^2/(2*par$sigma^2)))^2))
}


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
  Y = erg$Y

  # estimate death-probs
  v = Whittaker(Y)
  X = 0:95

  # select the other half of the data points
  rates = subset(deathrates1879westmatrix, deathrates1879westmatrix[,1] > firsthalf)
  exposure = subset(exposure1879westmatrix, exposure1879westmatrix[,1] > firsthalf)
  erg = einfachstesModellAlterProjektion(rates, exposure)
  Y_last = erg$Y

  plot(X, v, type = "l")
  points(X, Y)
  points(X,Y_last, pch = 4)

  # estimate the error
  return(sum((v-Y_last)^2))
}

GoodneesLeeData = function(){
  # using the first half of the dataset estimate the second half and calculate the errors

  # estimate the parameters
  firsthalf = min(deathrates1965west[,1]) +
    length(min(deathrates1965west[,1]):max(deathrates1965west[,1]))/2
  Geburtsjahre = min(deathrates1965west[,1]):firsthalf
  Alter = min(deathrates1965west[,2]):95
  Deathrates = subset(deathrates1965west, deathrates1965west[,1] <= firsthalf)
  erg = Lee(min(deathrates1965west[,1]):firsthalf, Alter,
            subset(Deathrates, Deathrates[,1] <= firsthalf))
  alpha_a = erg$alpha_a
  beta_a = erg$beta_a
  gamma_t = erg$gamma_t
  nu = erg$nu

  # # plot to verify
  # plot(Alter, beta_a, type = "l")
  # plot(Geburtsjahre, gamma_t, type = "l")
  # lines(Geburtsjahre, rep(0, length(Geburtsjahre)))
  sigma_xi_vec = rep(NA, length(Geburtsjahre)-1)
  for(i in 1:(length(Geburtsjahre)-1)){
    sigma_xi_vec[i] = (gamma_t[i+1] - gamma_t[i] - nu)^2
  }
  sigma_xi = sqrt((1/(length(Geburtsjahre)-2))*sum(sigma_xi_vec))


  # predict
  YearsToPredict = max(deathrates1965west[,1]) - firsthalf
  nu_vec = seq(from = nu, to = nu * YearsToPredict, by = nu)
  gamma_vec = c(gamma_t, gamma_t[length(gamma_t)] + nu_vec)
  # plot to verify
  plot(min(deathrates1965west[,1]):max(deathrates1965west[,1]), gamma_vec, type = "l")
  beta_vec = matrix(beta_a, nrow = 1)
  beta_gamma = t(beta_vec) %*% t(gamma_vec)
  ones = matrix(rep(1, 1 + max(deathrates1965west[,1]) - min(deathrates1965west[,1])),
                nrow = 1)
  alpha_matrix =  matrix(alpha_a, ncol = 1) %*% ones
  EstRates = exp(alpha_matrix + beta_gamma)
  # # first year
  # plot(Alter, exp(alpha_a + beta_gamma[,1]), type = "l", lty = "dashed")
  # lines(Alter, subset(Deathrates[,3], Deathrates[,1] == 1956))
  # # middle year
  # plot(Alter, exp(alpha_a + beta_gamma[,firsthalf-1956]), type = "l", lty = "dashed")
  # lines(Alter, subset(Deathrates[,3], Deathrates[,1] == firsthalf))
  # # last year
  # plot(Alter, exp(alpha_a + beta_gamma[,62]), type = "l", lty = "dashed")
  # lines(Alter, subset(deathrates1965west[,3], deathrates1965west[,1] == 2017))
  # # estimator dashed

  # calculate the errors
  errors = rep(NA, (1+max(deathrates1965west[,1])) - min(deathrates1965west[,1]))
  Geburtsjahre_all=max(deathrates1965west[,1]) : min(deathrates1965west[,1])
  j=1
  for(i in Geburtsjahre_all){
    errors[j] = sum((EstRates[,i - 1955] - subset(Deathrates[,3], Deathrates[,1] == i))^2)
    j=j+1
  }
  sd(errors)

 errors[length(errors)]
}

#########################################################################################
# estimate parameters einfaches modell
GoodnessEinfachEinfachSim = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem simulierten Datensatz
  #               mit der einfachen Schätzmethode erzeugt wurde.

  # initialize constants
  m = 100
  X = 0:95
  errors = rep(NA, m)

  # estimate the errors
  for(i in (1+2):(m+2)){
    par = einfachstesModellAlterEstimateParameters(simple_data[,1],
                                                   simple_data[,i])
    errors[i-2] = sum((par$mu- mu_data)^2) + sum((par$sigma  - sigma_data)^2)
  }

  # plot the errors to find outliers
  pdf("../../1 Doku/graphics/ErrorSimple.pdf", width = 10, height = 8)
  plot(1:m, errors, pch = 4)
  lines(1:m, rep(mean(errors), m))
  dev.off()
  error_per_parameter = mean(errors)/2

  return(mean(errors))
}


##########################################################################################
# lee carter parameter estimation

GoodneesLeeSim = function(){
  # estimate parameters and errors
  Zeitraum = min(complex_period_data[,1]):max(complex_period_data[,1])
  Alter = min(complex_period_data[,2]):max(complex_period_data[,2])
  est_data = rep(NA, length(Alter)*length(Zeitraum))
  m = 100
  errors = rep(NA, m)
  for(i in 1:m){
    erg = Lee(Zeitraum, Alter, cbind(complex_period_data[,1:2], complex_period_data[,3+i]))
    errors[i] = sum((erg$alpha - alpha_data)^2) + sum((erg$beta - beta_data)^2)
                + sum((erg$gamma - gamma_data)^2) + sum((erg$nu - nu_data)^2)
  }

  # plot the errors to find outliers
  pdf("../../1 Doku/graphics/ErrorLee.pdf", width = 10, height = 8)
  plot(1:m, errors, pch = 4)
  lines(1:m, rep(mean(errors), m))
  dev.off()
  error_per_parameter = mean(errors)/(2*95+40+1)

  return(errors = mean(errors))
}


###########################################################################################
# estimate errors for the simple data set and the whittaker model

GoodnessWhittakerEinfachSim = function(){
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
    erg = Whittaker(simple_data[1:96,i]/sum(simple_data[1:96,i]))
    Y.hat = erg
    errors = (Y-Y.hat)^2
    errors_sum[i-3] = sum(errors)
  }
  plot(X, errors)

  return(sum(errors_sum))
}


