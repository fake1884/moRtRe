# This file contains functions to estimate the goodness of fit of the death models

##########################################################################################
# einfaches Modell
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
  Y_first_half = erg$Y

  # estimate the parameters
  par = einfachstesModellAlterEstimateParameters(X, Y_first_half)

  # select the other half of the data points
  rates_last = subset(deathrates1879westmatrix, deathrates1879westmatrix[,1] > firsthalf)
  exposure_last = subset(exposure1879westmatrix, exposure1879westmatrix[,1] > firsthalf)
  erg_last = einfachstesModellAlterProjektion(rates_last, exposure_last)
  Y_second_half = erg_last$Y

  # just to check
  pdf("../../1 Doku/graphics/EinfachData.pdf", width = 10, height = 8)
  plot(X, (2*pi*par$sigma^2)^(-1/2) * exp(- (X-par$mu)^2/(2*par$sigma^2)), type = "l",
       ylim = c(0,0.04), ylab = "Sterbewahrscheinlichkeit", xlab = "Alter")
  points(X, Y_first_half, pch = 1)
  points(X,Y_second_half, pch = 4)
  dev.off()

  # calculate the errors
  errors = (Y_second_half -
              (2*pi*par$sigma^2)^(-1/2) * exp(- (X-par$mu)^2/(2*par$sigma^2)))^2

  # # plot the errors to find outliers
  # pdf("../../1 Doku/graphics/ErrorEinfachDataErrors.pdf", width = 10, height = 8)
  # par(mfrow = c(1,2))
  # boxplot(errors)
  # hist(errors)
  # dev.off()
  # par(mfrow = c(1,1))

  # estimate the error
  return(sum(errors))
}

# estimate parameters einfaches modell
GoodnessEinfachSim = function(){
  # Beschreibung: Diese Funktion bestimmt den Fehler, der auf dem simulierten Datensatz
  #               mit der einfachen Schätzmethode erzeugt wurde.

  # start by generating a plot of the historical data and the generated data
  pdf("../../1 Doku/graphics/SampleDataSimple.pdf", width = 10, height = 8)
  erg = einfachstesModellAlterProjektion(deathrates1879westmatrix, exposure1879westmatrix)
  X = erg$X
  Y = erg$Y
  erg = einfachstesModellAlterEstimateParameters(X, Y)
  plot(simple_data[,1], simple_data[,3], pch = 4, xlab = "Alter", ylab = "Sterblichkeit")
  lines(simple_data[,1], simple_data[,2])
  curve((2*pi*erg$sigma^2)^(-1/2) * exp(- (x-erg$mu)^2/(2*erg$sigma^2)), add = T
        , lty = "dashed")
  legend("topleft", legend=c("errorless", "original"),
         col=c("black", "black"), lty=1:2, cex=1.5)
  dev.off()

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

  # # plot the errors to find outliers
  # pdf("../../1 Doku/graphics/ErrorSimple.pdf", width = 10, height = 8)
  # par(mfrow = c(1,2))
  # boxplot(errors)
  # hist(errors)
  # dev.off()
  # par(mfrow = c(1,1))
  # error_per_parameter = mean(errors)/2

  # plot the different errors
  pdf("../../1 Doku/graphics/ErrorSimpleDetailed.pdf", width = 10, height = 8)
  par(mfrow=c(1,2))
  hist(ms)
  hist(ss)
  # hist(merror)
  # hist(serror)
  dev.off()
  par(mfrow=c(1,1))

  return(mean(errors))
}


##########################################################################################
# The Lee-Carter Modell
GoodnessLeeData = function(){
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

  # generate the alpha matrix and beta matrix -> generate EstRates matrix
  ones = matrix(rep(1, 1 + max(deathrates1965west[,1]) - min(deathrates1965west[,1])),
                nrow = 1)
  alpha_matrix =  matrix(alpha_a, ncol = 1) %*% ones
  beta_vec = matrix(beta_a, nrow = 1)
  beta_gamma = t(beta_vec) %*% t(gamma_vec)
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

  # plot different examples of death probability estimation
  pdf("../../1 Doku/graphics/ErrorLeeData.pdf", width = 10, height = 8)
  par(mfrow = c(2,2))
  # first year:1956
  plot(Alter, subset(deathrates1965west[,3], deathrates1965west[,1] == 1956), type = "l",
       ylab = "Sterblichkeit", main = "1956")
  lines(Alter, EstRates[,1956-1955], lty = "dashed")
  # end of first half:1987
  plot(Alter, subset(deathrates1965west[,3], deathrates1965west[,1] == 1987), type = "l",
       ylab = "Sterblichkeit", main = "1987")
  lines(Alter, EstRates[,1987-1955], lty = "dashed")
  # middle of prediction:2002
  plot(Alter, subset(deathrates1965west[,3], deathrates1965west[,1] == 2002), type = "l",
       ylab = "Sterblichkeit", main = "2002")
  lines(Alter, EstRates[,2002-1955], lty = "dashed")
  # end of prediction:2017
  plot(Alter, subset(deathrates1965west[,3], deathrates1965west[,1] == 2017), type = "l",
       ylab = "Sterblichkeit", main = "2017")
  lines(Alter, EstRates[,2017-1955], lty = "dashed")
  dev.off()
  par(mfrow = c(1,1))

 return(mean(errors))
}



GoodneesLeeSim = function(){
  # estimate parameters and errors on the whole data set
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

  # # plot the errors to find outliers
  # pdf("../../1 Doku/graphics/ErrorLee.pdf", width = 10, height = 8)
  # par(mfrow = c(1,2))
  # boxplot(errors)
  # hist(errors)
  # dev.off()
  error_per_parameter = mean(errors)/(2*95+40+1)

  return(errors = mean(errors))
}


GoodnessLeeSimPredict = function(){
  # estimate the goodness of the Lee-Carter Model as predictor; to this end use the first
  # half of the simulated data set to predict the second half and compare to the true values
  firsthalf =  round(min(complex_period_data[,1]) +
    length(min(complex_period_data[,1]):max(complex_period_data[,1]))/2) # Zeitraum 1,...,40
  Zeitraum = min(complex_period_data[,1]):firsthalf
  Zeitraum_full = min(complex_period_data[,1]):max(complex_period_data[,1])
  Alter = min(complex_period_data[,2]):max(complex_period_data[,2])

  # calculate the mean of EstRates
  m = 100
  EstRatessum = matrix(rep(0, length(Alter)*length(Zeitraum_full)), ncol = 41)
  for(i in 1:m){
    Deathrates = cbind(subset(complex_period_data[,1:2], complex_period_data[,1] <= firsthalf),
                       subset(complex_period_data[,3+i], complex_period_data[,1] <= firsthalf))
    erg = Lee(Zeitraum, Alter, Deathrates)
    ones = matrix(rep(1, 1 + max(complex_period_data[,1]) - min(complex_period_data[,1])),
                  nrow = 1)
    alpha_matrix =  matrix(erg$alpha_a, ncol = 1) %*% ones
    beta_vec = matrix(erg$beta_a, nrow = 1)
    YearsToPredict = max(complex_period_data[,1]) - firsthalf
    gamma_t = erg$gamma_t
    nu = erg$nu
    nu_vec = seq(from = nu, to = nu * YearsToPredict, by = nu)
    gamma_vec = c(gamma_t, gamma_t[length(gamma_t)] + nu_vec)
    beta_gamma = t(beta_vec) %*% t(gamma_vec)

    EstRates = exp(alpha_matrix + beta_gamma)
    EstRatessum = EstRatessum + EstRates
  }
  # now take the mean
  EstRates = EstRatessum/m

  # calculate errors
  errors = rep(NA, (1+max(complex_period_data[,1])))
  for(k in 1:41){
    errors[k] = sum((EstRates[,k] - complex_period_data[(1:96)+96*(k-1),3])^2)
  }

  # estimate the death probabilities on the whole data set
  EstRates_full_sum = matrix(rep(0, length(Alter)*length(Zeitraum_full)), ncol = 41)
  for(i in 1:m){
    est_data_full = Lee(Zeitraum_full, Alter,
                         cbind(complex_period_data[,1:2], complex_period_data[,3+i]))
    ones_full = matrix(rep(1, 1 + max(complex_period_data[,1]) - min(complex_period_data[,1])),
                         nrow = 1)
    alpha_matrix_full =  matrix(est_data_full$alpha_a, ncol = 1) %*% ones
    beta_vec_full = matrix(est_data_full$beta_a, nrow = 1)
    gamma_vec_full= matrix(est_data_full$gamma_t, ncol = 1)
    beta_gamma_full = t(beta_vec_full) %*% t(gamma_vec_full)
    EstRates_full = exp(alpha_matrix_full + beta_gamma_full)
    EstRates_full_sum = EstRates_full_sum + EstRates_full
  }
  # now take the mean
  EstRates_full = EstRates_full_sum/m

  # calculate errors
  errors_full = rep(NA, (1+max(complex_period_data[,1])))
  for(k in 1:41){
    errors_full[k] = sum((EstRates_full[,k] - complex_period_data[(1:96)+96*(k-1),3])^2)
  }

  # plot the parameters
  pdf("../../1 Doku/graphics/ParameterLee.pdf", width = 10, height = 8)
  par(mfrow=c(2,2))
  plot(Alter, alpha_data, type = "l", ylab = "alpha")
  lines(Alter, alpha_a, lty = "dashed")
  plot(Alter, beta_data, type = "l", ylab = "beta")
  lines(Alter, beta_a, lty = "dashed")
  plot(min(complex_period_data[,1]):max(complex_period_data[,1]), gamma_data, type = "l",
       xlab = "Zeitraum", ylab = "gamma")
  lines(min(complex_period_data[,1]):max(complex_period_data[,1]),
        gamma_vec_full, lty = "dashed")
  # is it reasonable to plot the gamma vector for the prediction as well?
  #lines(min(complex_period_data[,1]):max(complex_period_data[,1]), gamma_vec, lty = "dotted")
  lines(min(complex_period_data[,1]):max(complex_period_data[,1]), rep(0,41), lty = "dotdash")
  plot(min(complex_period_data[,1]):max(complex_period_data[,1]),
       gamma_sprung, type = "l", xlab = "Zeitraum", ylab = "gamma")
  # gamma_sprung_est has to be generated first
  #GoodnessLeeSimSprung()
  #devtools::loadall()
  lines(min(complex_period_data[,1]):max(complex_period_data[,1]),
        est_gamma_sprung, lty = "dashed")
  lines(min(complex_period_data[,1]):max(complex_period_data[,1]), rep(0,41), lty = "dotdash")
  dev.off()
  par(mfrow = c(1,1))

  # plot the estimation on the wohle data set versus the prediction
  pdf("../../1 Doku/graphics/ErrorLeeSimPredict.pdf", width = 10, height = 8)
  par(mfrow = c(2,2))
  # start of first half:0
  plot(Alter, complex_period_data[(1:96)+96*(1-1),3], type = "l", ylab = "Sterblichkeit",
       main = "Jahr 0")
  lines(Alter, EstRates[,1], lty = "dashed")
  lines(Alter, EstRates_full[,1], lty = "dotted")
  # end of first half:20
  plot(Alter, complex_period_data[(1:96)+96*(20-1),3], type = "l", ylab = "Sterblichkeit",
       main = "Jahr 20")
  lines(Alter, EstRates[,21], lty = "dashed")
  lines(Alter, EstRates_full[,21], lty = "dotted")
  # middle of prediction:30
  plot(Alter, complex_period_data[(1:96)+96*(30-1),3], type = "l", ylab = "Sterblichkeit",
       main = "Jahr 30")
  lines(Alter, EstRates[,31], lty = "dashed")
  lines(Alter, EstRates_full[,31], lty = "dotted")
  # end of prediction:40
  plot(Alter, complex_period_data[(1:96)+96*(40-1),3], type = "l", ylab = "Sterblichkeit",
       main = "Jahr 40")
  lines(Alter, EstRates[,41], lty = "dashed")
  lines(Alter, EstRates_full[,41], lty = "dotted")
  dev.off()
  par(mfrow = c(1,1))

  return(mean(errors))
}


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
  # # plot the mean of the gamma estimates
  # pdf("../../1 Doku/graphics/GammaSprung.pdf", width = 10, height = 8)
  # plot(Zeitraum, est_gamma_sprung, type = "l")
  # dev.off()
  # save result for the plot in LeeSim
  devtools::use_data(est_gamma_sprung, overwrite = T)
}
