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

  # plot the errors to find outliers
  pdf("../../1 Doku/graphics/ErrorEinfachData.pdf", width = 10, height = 8)
  par(mfrow = c(1,2))
  boxplot(errors)
  hist(errors)
  dev.off()


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

  # calculate sigma xi for the simulations
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
  #EstRates = alpha_matrix + beta_gamma #for error estimation

  # calculate the errors
  errors = rep(NA, ((1+max(deathrates1965west[,1])) - min(deathrates1965west[,1]))/2)
  Geburtsjahre_all=max(deathrates1965west[,1]) : firsthalf # The last year comes first
  j=1
  for(i in Geburtsjahre_all){
    errors[j] = sum((EstRates[,i - 1955] -
                       subset(deathrates1965west[,3], deathrates1965west[,1] == i))^2)
                       #log(subset(Deathrates[,3], Deathrates[,1] == i)))^2)#for error estimation
    j=j+1
  }
  sd(errors)

  # plot the errors to find outliers
  pdf("../../1 Doku/graphics/ErrorLeeData.pdf", width = 10, height = 8)
  par(mfrow = c(1,2))
  boxplot(errors)
  hist(errors)
  dev.off()

 errors[length(errors)]
}

#########################################################################################
GoodnessLeeSimPredict = function(){
  # estimate the goodness of the Lee-Carter Model as predictor; to this end use the first
  # half of the simulated data set to predict the second half and compare to the true values

  firsthalf =  round(min(complex_period_data[,1]) +
    length(min(complex_period_data[,1]):max(complex_period_data[,1]))/2) # Zeitraum 1,...,40
  Zeitraum = min(complex_period_data[,1]):firsthalf
  Alter = min(complex_period_data[,2]):max(complex_period_data[,2])
  est_data = rep(NA, length(Alter)*length(Zeitraum))

  m = 100
  errors = rep(NA, m)
  for(i in 1:m){
    Deathrates = cbind(subset(complex_period_data[,1:2], complex_period_data[,1] <= firsthalf),
                       subset(complex_period_data[,3+i], complex_period_data[,1] <= firsthalf))
    erg = Lee(Zeitraum, Alter, Deathrates)
    alpha_a = erg$alpha_a
    beta_a = erg$beta_a
    gamma_t = erg$gamma_t
    nu = erg$nu
    YearsToPredict = max(complex_period_data[,1]) - firsthalf
    nu_vec = seq(from = nu, to = nu * YearsToPredict, by = nu)
    gamma_vec = c(gamma_t, gamma_t[length(gamma_t)] + nu_vec)
    beta_vec = matrix(beta_a, nrow = 1)
    beta_gamma = t(beta_vec) %*% t(gamma_vec)
    ones = matrix(rep(1, 1 + max(complex_period_data[,1]) - min(complex_period_data[,1])),
                  nrow = 1)
    alpha_matrix =  matrix(alpha_a, ncol = 1) %*% ones
    EstRates = exp(alpha_matrix + beta_gamma)

    # calculate errors
    errors_pro_Jahr = rep(NA, (1+max(complex_period_data[,1])) - firsthalf)
    Geburtsjahre_all=max(complex_period_data[,1]) : firsthalf
    j=1
    for(k in Geburtsjahre_all){
      errors_pro_Jahr[j] = sum((EstRates[,k] -
                         subset(complex_period_data[,3+i], complex_period_data[,1] == k))^2)
      j=j+1
    }
    errors[i] = mean(errors_pro_Jahr)
  }

  pdf("../../1 Doku/graphics/ErrorLeeSimPredict.pdf", width = 10, height = 8)
  par(mfrow = c(1,2))
  boxplot(errors)
  hist(errors)
  dev.off()

  mean(errors)

}

#########################################################################################
# estimate parameters einfaches modell
GoodnessEinfachSim = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem simulierten Datensatz
  #               mit der einfachen Schätzmethode erzeugt wurde.

  # initialize constants
  m = 10000
  X = 0:95
  # storage space for parameter estimators and diviation from true parameter
  ms = rep(NA, m)
  ss = rep(NA, m)
  merror = rep(NA, m)
  serror = rep(NA, m)
  errors = rep(NA, m)

  # estimate the errors
  for(i in (1+2):(m+2)){
    par = einfachstesModellAlterEstimateParameters(simple_data[,1],
                                                   simple_data[,i])
    ms[i-2] = par$mu
    ss[i-2] = par$sigma
    merror[i-2] = sum((par$mu- mu_data)^2)
    serror[i-2] = sum((par$sigma  - sigma_data)^2)
    errors[i-2] = sum((par$mu- mu_data)^2) + sum((par$sigma  - sigma_data)^2)
  }
  #errors_xy = data - f(x, par$mu, par$sigma)

  pdf("../../1 Doku/graphics/ErrorSimpleDetailed.pdf", width = 10, height = 8)
  par(mfrow=c(2,2))
  hist(ms)
  hist(ss)
  hist(merror)
  hist(serror)
  dev.off()

  # plot the errors to find outliers
  pdf("../../1 Doku/graphics/ErrorSimple.pdf", width = 10, height = 8)
  par(mfrow = c(1,2))
  boxplot(errors)
  hist(errors)
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
  m = 10000
  alphaerror = rep(NA, m)
  betaerror = rep(NA, m)
  gammaerror = rep(NA, m)
  nuerror = rep(NA, m)
  errors = rep(NA, m)
  for(i in 1:m){
    erg = Lee(Zeitraum, Alter, cbind(complex_period_data[,1:2], complex_period_data[,3+i]))
    alphaerror[i] = sum((erg$alpha - alpha_data)^2)
    betaerror[i] = sum((erg$beta - beta_data)^2)
    gammaerror[i] = sum((erg$gamma - gamma_data)^2)
    nuerror[i] = sum((erg$nu - nu_data)^2)
    errors[i] = sum((erg$alpha - alpha_data)^2) + sum((erg$beta - beta_data)^2)
                + sum((erg$gamma - gamma_data)^2) + sum((erg$nu - nu_data)^2)
  }

  pdf("../../1 Doku/graphics/ErrorLeeDetailed.pdf", width = 10, height = 8)
  par(mfrow=c(2,2))
  hist(alphaerror)
  hist(betaerror)
  hist(gammaerror)
  hist(nuerror)
  dev.off()

  # plot the errors to find outliers
  pdf("../../1 Doku/graphics/ErrorLee.pdf", width = 10, height = 8)
  par(mfrow = c(1,2))
  boxplot(errors)
  hist(errors)
  dev.off()
  error_per_parameter = mean(errors)/(2*95+40+1)

  return(errors = mean(errors))
}

##########################################################################################
GoodnessLeeSimSprung = function(){
  # Estimate the goodness of the sprung data set; is the sprung visible in the estimator?

  # estimate parameters and errors
  Zeitraum = min(complex_period_data_sprung[,1]):max(complex_period_data_sprung[,1])
  Alter = min(complex_period_data_sprung[,2]):max(complex_period_data_sprung[,2])
  est_data = rep(NA, length(Alter)*length(Zeitraum))
  m = 10000
  gammasprung = matrix(rep(NA, m*length(Zeitraum)), nrow = m)
  for(i in 1:m){
    erg = Lee(Zeitraum, Alter, cbind(complex_period_data_sprung[,1:2],
                                     complex_period_data_sprung[,3+i]))
    gammasprung[i,] = erg$gamma
  }
  est_gamma_sprung = rep(NA, length(Zeitraum))
  for(i in Zeitraum){
    est_gamma_sprung[i+1] = mean(gammasprung[,i+1])
  }
  # plot the mean of the gamma estimates
  pdf("../../1 Doku/graphics/GammaSprung.pdf", width = 10, height = 8)
  plot(Zeitraum, est_gamma_sprung, type = "l")
  dev.off()
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


