# This file contains a function to generate test-data

###########################################################################################
# simple model first

MakeData = function(){
  # set parameters
  # mu = 77.00817; sigma = 12.02285; estimates from data
  mu_data = 77
  sigma_data = 12
  sigma_eps = sqrt(0.000006)
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
  alpha_data = c(-3.92941716, -6.60074001, -7.10431537, -7.33145029, -7.50456590, -7.60778375,
            -7.70171017, -7.79576479, -7.90533596, -8.00484620, -8.10217814, -8.11556268,
            -8.08823471, -8.01841006, -7.83395088, -7.59365755, -7.21806666, -7.07036494,
            -6.83243276, -6.78443122, -6.79724624, -6.81058289, -6.84045457, -6.86960591,
            -6.88017535, -6.89108822, -6.88395892, -6.85994920, -6.83583644, -6.80125540,
            -6.76695895, -6.71895925, -6.66232995, -6.61326537, -6.55704021, -6.48556752,
            -6.41623621, -6.32559591, -6.25022678, -6.16160461, -6.09095072, -6.00186411,
            -5.91640782, -5.82632724, -5.74531145, -5.64557360, -5.55651521, -5.46695202,
            -5.37410778, -5.28677374, -5.18881810, -5.09629649, -5.00817263, -4.92557868,
            -4.82933107, -4.73882028, -4.64558291, -4.55146817, -4.46004263, -4.36937019,
            -4.27021141, -4.17863445, -4.08074162, -3.98471301, -3.88621938, -3.78256509,
            -3.68599879, -3.59050958, -3.49286553, -3.38947272, -3.28484928, -3.18286280,
            -3.07923957, -2.97441469, -2.87043201, -2.76253450, -2.66095238, -2.55707119,
            -2.45620199, -2.35490260, -2.25723357, -2.15335313, -2.05381032, -1.95721660,
            -1.85916896, -1.76307481, -1.67079357, -1.57720206, -1.48770366, -1.40747266,
            -1.31992100, -1.23553029, -1.15541823, -1.08181762, -1.00886995, -0.93192711)
  beta_data = c(0.0277207021, 0.0242392426, 0.0218966099, 0.0213461005, 0.0218794373,
           0.0205469443, 0.0200070252, 0.0189651548, 0.0172886931, 0.0175510477,
           0.0178205012, 0.0161988719, 0.0159281013, 0.0139714412, 0.0139568460,
           0.0115503520, 0.0093309633, 0.0077624165, 0.0059788328, 0.0073973595,
           0.0090035515, 0.0100419643,  0.0102924650,  0.0103748598,  0.0102449423,
           0.0105334244, 0.0110580895,  0.0105092698, 0.0106765119,  0.0109085422,
           0.0099495709,  0.0097887532,  0.0094613834,  0.0091687815,  0.0087649727,
           0.0085866207,  0.0081913185,  0.0072080853,  0.0069885931,  0.0068211730,
           0.0060224662,  0.0055960004, 0.0054923269,  0.0049609722,  0.0047347838,
           0.0043059174,  0.0042640944,  0.0039379602,  0.0040754600, 0.0040651286,
           0.0038109848,  0.0042173141,  0.0042063072,  0.0045650154,  0.0047837349,
           0.0052300482,  0.0053018616,  0.0061625581,  0.0065947098,  0.0072462562,
           0.0076708722,  0.0077881123,  0.0082631679, 0.0081492450,  0.0083979729,
           0.0085867591,  0.0088382383,  0.0086452720,  0.0092074420,  0.0086645976,
           0.0085762845,  0.0084579047,  0.0080482343,  0.0082498801,  0.0080536527,
           0.0077853767,  0.0077988576, 0.0077287390,  0.0076687360,  0.0073833967,
           0.0071247691,  0.0067954586,  0.0068468690,  0.0068262949, 0.0067116838,
           0.0068061315,  0.0066975670,  0.0065363446,  0.0063568291,  0.0062146553,
           0.0059054259, 0.0056200157,  0.0055913828,  0.0050432341,  0.0046684567,
           0.0050052199)
  Zeitraum = 0:40
  Alter = 0:95
  nu_data = -1.679
  sigma_xi = 2.344917
  gamma_data = rep(NA, length(Zeitraum))
  gamma_data[mean(Zeitraum)+1] = 0
  for(i in 1:((length(Zeitraum)-1)/2)){
    gamma_data[mean(Zeitraum)+1+i] = gamma_data[mean(Zeitraum)+1+i-1] + nu_data +
                                                      rnorm(1, mean = 0, sd = sigma_xi)
  }
  for(i in 1:((length(Zeitraum)-1)/2)){
    gamma_data[mean(Zeitraum)+1-i] = gamma_data[mean(Zeitraum)+1-i+1] - nu_data +
                                                      rnorm(1, mean = 0, sd = sigma_xi)
  }
  m = 10000 # Anzahl an Datensätzen

  gamma_sprung = c(gamma_data[1:20]+1*mean(gamma_data[1:20]),
                   gamma_data[21:41]+1*mean(gamma_data[21:41])) # für den Srung datensatz


  # Set up a place to store the data
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
  sd_rates = 0.1621458
  for(i in 1:m){
    complex_period_data[,i+3] = exp(log(complex_period_data[,3]) +
                      rnorm(n = length(Alter) * length(Zeitraum), mean = 0, sd = sd_rates))
  }

  # plot errorless data, observed data and data with white noise
  pdf("../../1 Doku/graphics/SampleDataLee.pdf", width = 10, height = 8)
  par(mfrow = c(1,1))
  plot(Alter, complex_period_data[(1:96)+(10*96),3], type = "l", ylab = "Sterblichkeit")
  lines(Alter, deathrates1965west[(1:96)+(10*96),3], lty = "dashed")
  points(Alter, complex_period_data[(1:96)+(10*96),4], pch=4)
  legend("topleft", legend=c("errorless", "original"),
         col=c("black", "black"), lty=1:2, cex=1.5)
  dev.off()

  # save result
  devtools::use_data(complex_period_data, overwrite = T)
  devtools::use_data(alpha_data, overwrite = T)
  devtools::use_data(beta_data, overwrite = T)
  devtools::use_data(gamma_data, overwrite = T)
  devtools::use_data(nu_data, overwrite = T)

##########################################################################################
  # Sprung data

  # Set up a place to store the data
  complex_period_data_sprung = matrix(rep(NA, length(Alter)*length(Zeitraum)*(m+3)), ncol = (m+3))

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
  sd_rates = 0.1621458
  for(i in 1:m){
    complex_period_data_sprung[,i+3] = exp(log(complex_period_data_sprung[,3]) +
                                      rnorm(n = length(Alter) * length(Zeitraum), mean = 0, sd = sd_rates))
  }

  # save result
  devtools::use_data(complex_period_data_sprung, overwrite = T)
  devtools::use_data(gamma_sprung, overwrite = T)

}



