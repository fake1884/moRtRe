# Diese Datei enthälte die Skripte, um einfach Modelle zu bestimmen.

# TODO: Die beiden Modellfunktionen können noch nicht auf beliebige Stichproben angewandt
#       werden. Dies muss repariert werden.

##########################################################################################
einfachstesModellGeburtsjahr = function(){
# lineares Modell, bei dem nur das Geburtsjahr als erklärende Variable benutzt wird

  # initialize two vectors to store data
  Geburtsjahre = 1879:1987
  Alter = 0:110
  Y = rep(NA, 1987-1879+1)

  for(i in Geburtsjahre){
    rates = subset(deathrates1879west, deathrates1879west$Geburtsjahr == i)
    exposure = subset(exposure1879west, exposure1879west$Geburtsjahr == i)

    # convert: 1. factor -> vector
    #          2. string -> integer
    rates_gesamt = as.numeric(as.vector(rates$Gesamt))
    exposure_gesamt = as.numeric(as.vector(exposure$Gesamt))

    Y[i-1879] = sum(rates_gesamt * exposure_gesamt * Alter) /
                sum(rates_gesamt * exposure_gesamt)
  }

  X = Geburtsjahre
  return(list(X = X, Y = Y))
}




##########################################################################################
einfachstesModellAlter = function(){
  # lineares Modell, bei dem nur das Alter als erklärende Variable benutzt wird

  # initialize two vectors to store data
  Geburtsjahre = 1879:1987
  Alter = 0:109

  Y = rep(NA, 111)

  rates_gesamt = as.numeric(as.vector(deathrates1879west$Gesamt))
  exposure_gesamt = as.numeric(as.vector(exposure1879west$Gesamt))
  Todesfälle_gesamt = sum(rates_gesamt * exposure_gesamt)

  for(i in Alter){
    rates = subset(deathrates1879west, deathrates1879west$Alter == i)
    exposure = subset(exposure1879west, exposure1879west$Alter == i)

    # convert: 1. factor -> vector
    #          2. string -> integer
    rates_i = as.numeric(as.vector(rates$Gesamt))
    exposure_i = as.numeric(as.vector(exposure$Gesamt))

    Y[i+1] = sum(rates_i * exposure_i) / (Todesfälle_gesamt)
  }

  # das letzte Alter ist 110+ und nicht 100; deshalb funktioniert die Loop nicht.
  Y[111] = 1 - sum(Y[1:110])

  X = 0:110
  return(list(X = X, Y = Y))
}


##########################################################################################
MakePlotEinfacheModelle = function(){
  #######################################
  # make plot einfachstesModellGeburtsjahr
  pdf("../../1 Doku/graphics/einfachstesModellGeburtsjahr.pdf", width = 10, height = 8)
  erg = einfachstesModellGeburtsjahr()
  X = erg$X
  Y = erg$Y
  plot(X,Y, pch = 4, xlab = "Geburtsjahr", ylab = "Mittleres erreichtes Alter")
  dev.off()

  #######################################
  # make plot heatmap
  pdf("../../1 Doku/graphics/heatmap.pdf", width = 10, height = 8)
  rates_gesamt = as.numeric(as.vector(deathrates1879west$Gesamt))
  exposure_gesamt = as.numeric(as.vector(exposure1879west$Gesamt))
  Todesfälle_gesamt = rates_gesamt * exposure_gesamt
  data = matrix(data = Todesfälle_gesamt, nrow = length(0:110), ncol = length(1876:1987),
                dimnames = list(0:110, 1876:1987))
  names(data) = 1876:1987
  heatmap(data, Rowv = NA, Colv = NA, col = paste("gray",99:1,sep=""))
  dev.off()

  #######################################
  # make plot einfachstesModellAlter
  pdf("../../1 Doku/graphics/einfachstesModellAlter.pdf", width = 10, height = 8)
  erg = einfachstesModellAlter()
  X = erg$X
  Y = erg$Y
  plot(X,Y, pch = 4, xlab = "Alter", ylab = "Sterbewahrscheinlichkeit")
  dev.off()

  pdf("../../1 Doku/graphics/einfachstesModell-log-fitted-Alter.pdf", width = 10, height = 8)
  fit = lm(log(Y) ~ X)
  plot(X, log(Y), pch = 4, xlab = "Alter", ylab = "log(Sterbewahrscheinlichkeit)")
  curve(fit$coefficients[2] * x + fit$coefficients[1], add = T)
  dev.off()

  #######################################
  pdf("../../1 Doku/graphics/einfachstesModell-fitted2-Alter.pdf", width = 10, height = 8)
  l2error = function(vec){sum( ((2*pi*vec[2]^2)^(-1/2) *
                                  exp(- (X-vec[1])^2/(2*vec[2]^2)) - Y )^2 )^(1/2)}
  fit = optim(par = c(80,1), fn = l2error)
  plot(X,Y, pch = 4, xlab = "Alter", ylab = "Sterbewahrscheinlichkeit")
  curve((2*pi*fit$par[2]^2)^(-1/2) * exp(- (x-fit$par[1])^2/(2*fit$par[2]^2)), add = T)
  dev.off()
}
