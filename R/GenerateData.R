# This file contains a function to generate test-data

MakeData = function(){
  # set parameters
  # mu = 77.00817; sigma = 12.02285; estimates from data
  mu = 70
  sigma = 15
  n = 1000 # Anzahl an Personen die Sterben
  m = 100 # Anzahl an Datensätzen
  Zeitraum = 1900:2000
  Alter = 0:110

  # Set up a place to store the data
  simple_period_data = matrix(c(rep(Zeitraum, length(Alter)),
                                 rep(Alter, length(Zeitraum)) ,
                                 rep(0, length(Alter)*length(Zeitraum)*(m+1))), ncol=m+3)

  # generate errorless data
  simple_linear_trend_true_function = function(x){linear_trend_true_function(x, mu, sigma, 1)}
  simple_period_data[,3] = simple_linear_trend_true_function(simple_period_data[,2])

  # add errors for each year
  for(i in 1:m){
    set.seed(i)
    obs = round(simple_period_data[,3] * n +
                    rnorm(n = length(Alter) * length(Zeitraum), mean = 0, sd = 2))
    obs[obs < 0] = 0
    simple_period_data[,i+3] = obs
  }
  #pdf("../../1 Doku/graphics/einfachstesModell_SampleData.pdf", width = 10, height = 8)
  plot(simple_period_data[1:111,2], simple_period_data[1:111,4], type = "l")
  lines(simple_period_data[1:111,2], simple_period_data[1:111,3] * n)
  #dev.off()

  # Hier wird die Größe des Kollektivs festgelegt
  # -> es fehlt noch eine sinnvolle Kollektivgröße
  simple_exposure = matrix(c(rep(Zeitraum, length(Alter)),
                             rep(Alter, length(Zeitraum)),
                             rep(n, length(Alter) * length(Zeitraum) * (m+1))), ncol=m+3)
  simple_rates = matrix(c(rep(Zeitraum, length(Alter)),
                          rep(Alter, length(Zeitraum)),
                          simple_period_data[,-(1:2)]/n), ncol=m+3)

  devtools::use_data(simple_period_data, overwrite = T)
  devtools::use_data(simple_exposure, overwrite = T)
  devtools::use_data(simple_rates, overwrite = T)

##########################################################################################
  # A data set with a linear trend generated from a simple model
  # trend_period_data = matrix(c(rep(Zeitraum, length(Alter)),
  #                               rep(Alter, length(Zeitraum)) ,
  #                               rep(0, length(Alter)*length(Zeitraum)*m)), ncol=m+3)
  # trend_period_data[,3] = linear_trend_true_function(trend_period_data[,2],
  #                                                    m, s, 1+(trend_period_data[,2]-1900)/100)


}


linear_trend_true_function = function(x, m , s, t){
  # this function gives the mortality of a person given the persons age x, the starting
  # mean m, the starting sigma s and the current year t
  1/sqrt(2*pi*s^2)* exp( - (m*t-x)^2 / (2*s^2))
  }
