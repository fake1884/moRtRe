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

  # initialize constants
  Geburtsjahre = min(deathrates1965west[,1]):max(deathrates1965west[,1])
  Alter = min(deathrates1965west[,2]):max(deathrates1965west[,2])
  M = matrix(rep(NA, length(Geburtsjahre) * length(Alter)), nrow = length(Alter),
                 ncol = length(Geburtsjahre))
  m_bar_a = rep(NA, length(Geburtsjahre))

  # calculate the M matrix
  for(i in 1:length(Alter)){
    m_at = log(subset(deathrates1965west[,3], deathrates1965west[,2] == -1+i))
    # setting -Inf to zero might not be the best idea TODO
    m_at[m_at == "-Inf"] = 0
    m_bar_a[i] = mean(m_at)
    M[i,] = m_at - m_bar_a[i]
  }

  # estimate beta
  svd_M = svd(M)
  B = svd_M$u
  beta_a = B[,which.max(svd_M$d)]/ sum(B[,which.max(svd_M$d)])

  # estimate gamma
  gamma_t = rep(NA, length(Geburtsjahre))
  for(i in 1:length(Geburtsjahre)){
    m_at = log(subset(deathrates1965west[,3], deathrates1965west[,1] == 1955+i))
    # setting -Inf to zero might not be the best idea TODO
    m_at[m_at == "-Inf"] = 0
    gamma_t[i] = mean(m_at - m_bar_a)
  }

  # make a plot of k
  # TODO -> move this downwards
  pdf("../../1 Doku/graphics/Lee-Carter-k.pdf", width = 10, height = 8)
  plot(1:length(Geburtsjahre), gamma_t, type = "l",
       xlab = "gamma_t", ylab = "Geburtsjahre")
  dev.off()

  return(list(beta_a = beta_a, gamma_t = gamma_t))
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
