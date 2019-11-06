# This file contains a function to generate test-data

###########################################################################################
# simple model first

MakeData = function(){
  # set parameters
  # mu = 77.51859; sigma = 12.36892; estimates from data
  mu_data = 77
  sigma_data = 12
  sigma_eps = 0.002427235
  m = 10000 # Anzahl an Datensätzen
  Alter = 0:95

  # Set up a place to store the data
  simple_data = matrix(c(Alter, rep(NA, length(Alter) * (m+1))), ncol = m+2)

  # generate errorless data
  simple_linear_function = function(x, m, s){1/sqrt(2*pi*s^2)* exp( - (m-x)^2 / (2*s^2))}
  simple_data[,2] = simple_linear_function(simple_data[,1], mu_data, sigma_data)

  # add errors for each year
  set.seed(5)
  for(i in 1:m){
    obs = simple_data[,2] +
                    rnorm(n = length(Alter), mean = 0, sd = sigma_eps)
    #obs[obs < 0] = 0 # no negative rates
    simple_data[,i+2] = obs

  }

  devtools::use_data(simple_data, overwrite = T)
  devtools::use_data(mu_data, overwrite = T)
  devtools::use_data(sigma_data, overwrite = T)

##########################################################################################
  # A data set with trend generated from a lee-carter model

  # set up parameters
  Zeitraum = 0:40
  Alter = 0:95
  sigma_xi = 2.270456
  gamma_data = rep(NA, length(Zeitraum))
  gamma_data[mean(Zeitraum)+1] = 0
  set.seed(100)
  for(i in 1:((length(Zeitraum)-1)/2)){
    gamma_data[mean(Zeitraum)+1+i] = gamma_data[mean(Zeitraum)+1+i-1] + nu_data +
                                                      rnorm(1, mean = 0, sd = sigma_xi)
  }
  for(i in 1:((length(Zeitraum)-1)/2)){
    gamma_data[mean(Zeitraum)+1-i] = gamma_data[mean(Zeitraum)+1-i+1] - nu_data +
                                                      rnorm(1, mean = 0, sd = sigma_xi)
  }
  gamma_data[1:20] = gamma_data[1:20]/sum(gamma_data[1:20]) # normal condition
  gamma_data[22:41] = -gamma_data[22:41]/sum(gamma_data[22:41])
  gamma_data = gamma_data * 300 # increase signal to noise ratio
  plot(Zeitraum, gamma_data, type = "l") #check generation
  lines(Zeitraum, rep(0, 41))

  gamma_sprung = c(gamma_data[1:20]+1*mean(gamma_data[1:20]),0,
                   gamma_data[22:41]+1*mean(gamma_data[22:41])) # für den Srung datensatz

  gamma_sprung[1:20] = gamma_sprung[1:20]/sum(gamma_sprung[1:20]) # normal condition
  gamma_sprung[22:41] = -gamma_sprung[22:41]/sum(gamma_sprung[22:41])
  gamma_sprung = gamma_sprung * 300 # increase signal to noise ratio
  plot(Zeitraum, gamma_sprung, type = "l") #check generation
  lines(Zeitraum, rep(0, 41))

  # Set up a place to store the data
  m = 10000 # Anzahl an Datensätzen
  complex_period_data = matrix(rep(NA, length(Alter)*length(Zeitraum)*(m+3)), ncol = (m+3))

  # set first two coloums
  for(i in 0:(length(Zeitraum)-1)){
    complex_period_data[(1+i*96):(96+i*96),1] = rep(i, length(Alter))
  }
  complex_period_data[,2] = rep(0:95,length(Zeitraum))


  # generate errorless data
  org_data = rep(NA, length(Alter)*length(Zeitraum))
  for(i in 0:(length(Zeitraum)-1)){
    complex_period_data[(1+i*96):(96+i*96),3] = exp(alpha_data + beta_data * gamma_data[i+1])
  }

  # generate the data
  set.seed(10)
  sd_rates = 0.05933034
  for(i in 1:m){
    complex_period_data[,i+3] = exp(log(complex_period_data[,3]) +
                      rnorm(n = length(Alter) * length(Zeitraum), mean = 0, sd = sd_rates))
  }

  # plot errorless data, observed data and data with white noise
  pdf("../../1 Doku/graphics/SampleDataLee.pdf", width = 10, height = 8)
  par(mfrow = c(1,2))
  plot(Alter, complex_period_data[(1:96)+(10*96),3], type = "l", ylab = "Sterblichkeit")
  lines(Alter, deathrates1965west[(1:96)+(10*96),3], lty = "dashed")
  points(Alter, complex_period_data[(1:96)+(10*96),4], pch=4)
  legend("topleft", legend=c("errorless", "original"),
         col=c("black", "black"), lty=1:2, cex=1.5)

  plot(Alter, log(complex_period_data[(1:96)+(10*96),3]), type = "l", ylab = "Sterblichkeit")
  lines(Alter, log(deathrates1965west[(1:96)+(10*96),3]), lty = "dashed")
  points(Alter, log(complex_period_data[(1:96)+(10*96),4]), pch=4)
  legend("topleft", legend=c("errorless", "original"),
         col=c("black", "black"), lty=1:2, cex=1.5)
  par(mfrow = c(1,1))
  dev.off()

  # save result
  devtools::use_data(complex_period_data, overwrite = T)
  devtools::use_data(gamma_data, overwrite = T)
  devtools::use_data(nu_data, overwrite = T)

##########################################################################################
  # Sprung data

  # Set up a place to store the data
  complex_period_data_sprung = matrix(rep(NA, length(Alter)*length(Zeitraum)*(m+3)),
                                      ncol = (m+3))

  # set first two coloums
  for(i in 0:(length(Zeitraum)-1)){
    complex_period_data_sprung[(1+i*96):(96+i*96),1] = rep(i, length(Alter))
  }
  complex_period_data_sprung[,2] = rep(0:95,length(Zeitraum))


  # generate errorless data
  org_data = rep(NA, length(Alter)*length(Zeitraum))
  for(i in 0:(length(Zeitraum)-1)){
    complex_period_data_sprung[(1+i*96):(96+i*96),3] =
                                          exp(alpha_data + beta_data * gamma_sprung[i+1])
  }

  # generate the data
  set.seed(10)
  sd_rates = 0.05933034
  for(i in 1:m){
    complex_period_data_sprung[,i+3] = exp(log(complex_period_data_sprung[,3]) +
                                      rnorm(n = length(Alter) * length(Zeitraum),
                                            mean = 0, sd = sd_rates))
  }

  # save result
  devtools::use_data(complex_period_data_sprung, overwrite = T)
  devtools::use_data(gamma_sprung, overwrite = T)

}



