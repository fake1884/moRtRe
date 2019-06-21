# In dieser Datei sind die Funktionen um eine Sterbetafel wie die DAV es vorschlägt zu
# bestimmen.

##########################################################################################
mittlere_Sterbetafel = function(vec){
  # hier muss man schaune, was man macht, wenn nicht genau ein Jahr in der Mitte liegt.
  Mitte = vec[length(vec)/2]
  return(Mitte)
}

##########################################################################################
Basistafel = function(data_in){
  # Beschreibung : TODO
  Alter = 0:110
  Y = rep(NA, 111)
  Mitte = mittlere_Sterbetafel(1965:2017)

  rates = subset(deathrates1965west, deathrates1965west$Kalenderjahr == Mitte)
  rates = as.numeric(as.vector(rates$Gesamt))

  pdf("../../1 Doku/graphics/Basistafel.pdf")
  plot(Alter, rates, pch = 4)
  dev.off()
}

##########################################################################################
Extrapolation = function(){
  # Beschreibung : This function is supposed to estimate death probabilities for high ages.
  #                However, this is not yet in the thesis -> Ausblick

}

##########################################################################################
Whittaker = function(DataIn){
  # Beschreibung : Diese Funktion glättet die vorgegebenen Daten (DataIn) mittels des
  #                Whittaker-Henderson-Verfahrens

  u = DataIn
  x = 0:(length(DataIn)-1)
  n = length(DataIn)
  p = 3 # is this correct?
  lambdas = c(50, 100) # this is where the possible values of lambda are checked
  GCV = rep(NA, length(lambdas))
  vCandidates = matrix(rep(NA, length(lambdas) * length(u)), ncol = length(lambdas))
  for(k in 1:length(lambdas)){
    I = diag(n)
    K = matrix(rep(0,n*(n-p)), nrow = (n-p), ncol = n)
    for(i in 1:(n-p)){
      for(j in 1:n){
        if(i <= p || j >= i){K[i,j] = (-1)^(p+j-i)*choose(p, j-i)}
      }
    }
    B = I + lambdas[k]*t(K)%*%K
    vCandidates[,k] = solve(B) %*% u

    GCV[k] =  GCV_function(u, vCandidates[,k], n, B)
  }

  return(vCandidates[,which.min(GCV)])
}

GCV_function = function(u, v, n, B){
  # this function has to be minimized to find the best value of lambda
  return( (1/n) * t(u-v)%*%(u-v) * (1-sum(diag(solve(B)))/n)^{-2} )
}

##########################################################################################
Lee = function(){
  # Beschreibung : Diese Funktion schätzt die Parameter im Lee-Carter Modell, wie in dem
  #                Paper von Girosi und King vorgeschlagen.

  # example period data
  periodmatrix = rbind(deathrates1965west$Kalenderjahr, deathrates1965west$Alter,
                       deathrates1965west$Gesamt)

  # initialize constants
  Geburtsjahre = min(deathrates1965west$Kalenderjahr):max(deathrates1965west$Kalenderjahr)
  Alter = 0:110
  M = matrix(rep(NA, length(Alter) * length(Geburtsjahre)))
  m_bar_a = rep(NA, length(Geburtsjahre))

  # calculate m
  # CONTIUE HERE

  # calculate m_bar
  for(i in 1:length(Geburtsjahre)){
    m_bar_a[i] = 0
  }

  # calculate the M matrix
  for(i in 1:length(Geburtsjahre)){
    for(j in Alter){
      M[i,j] = 0
    }
  }

  # do the SVD
  svd_M = svd(M)

  # estimate parameters

  return(list(beta = 0, gamma = 0))
}


##########################################################################################
MakePlotDAV = function (){
  # Beschreibung : This function calls the other functions in this file to generate plots

  #####################################
  # Basistafel


  ######################################
  # Whittaker
  v = Whittaker(simple_period_data[1:111,4]/sum(simple_period_data[1:111,4]))
  X = 0:110
  mu = 70
  sigma = 15
  Y = (2*pi*sigma^2)^(-1/2) * exp(- (X-mu)^2/(2*sigma^2))

  pdf("../../1 Doku/graphics/Whittaker_SampleData.pdf", width = 10, height = 8)
  plot(X, simple_period_data[1:111,4]/sum(simple_period_data[1:111,4]), type = "l",
       xlab = "Alter", ylab = "Sterbewahrscheinlichkeit")
  lines(X, v, lty = 2)
  lines(X, Y, lty = 3)
  legend("topleft", legend=c("Daten", "Whittaker", "Model"),
         col=c("black", "black", "black"), lty=1:3, cex=0.8)
  dev.off()

}
