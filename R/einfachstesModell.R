# Diese Datei enthälte die Skripte, um einfach Modelle zu bestimmen.

# TODO: Die beiden Modellfunktionen können noch nicht auf beliebige Stichproben angewandt
#       werden. Dies muss repariert werden.

##########################################################################################
einfachstesModellGeburtsjahr = function(RatesIn, ExposureIn){
# lineares Modell, bei dem nur das Geburtsjahr als erklärende Variable benutzt wird

  # initialize Geburtsjahr, Alter and the dependend variable
  Geburtsjahre = min(RatesIn[,1]):max(RatesIn[,1])
  Alter = min(RatesIn[,2]):max(RatesIn[,2])
  Y = rep(NA, max(RatesIn[,1])-min(RatesIn[,1])+1) # +1, da Start bei Null

  for(i in Geburtsjahre){
    rates = subset(RatesIn, RatesIn[,1] == i)[,3]
    exposure = subset(ExposureIn, ExposureIn[,1] == i)[,3]

    # Anzahl an Toten gewichtet nach Alter, durch Anzahl an Personen, die gestorben sind
    Y[i-min(RatesIn[,1])+1] = sum(rates * exposure * Alter) / sum(exposure * rates)
  }

  return(list(X = Geburtsjahre, Y = Y))
}




##########################################################################################
einfachstesModellAlterProjektion = function(RatesIn, ExposureIn){
  # Diese Funktion berechnet die beobachteten Sterbehäufigkeiten/Verteilung der Sterbe-
  # fälle über die Alter 0 bis 110. Eingabe ist eine Generationssterbetafel

  # initialize Geburtsjahr, Alter and the dependend variable
  Geburtsjahre = min(RatesIn[,1]):max(RatesIn[,1])
  Alter = min(RatesIn[,2]):max(RatesIn[,2])
  Y = rep(NA, max(RatesIn[,2])-min(RatesIn[,2])+1) # +1, da Start bei Null

  rates_gesamt = RatesIn[,3]
  exposure_gesamt = ExposureIn[,3]
  Todesfälle_gesamt = sum(rates_gesamt * exposure_gesamt)

  for(i in Alter){
    rates = subset(RatesIn, RatesIn[,2] == i)[,3]
    exposure = subset(ExposureIn, ExposureIn[,2] == i)[,3]

    Y[i+1] = sum(rates * exposure) / (Todesfälle_gesamt)
  }
  # das letzte Alter ist 110+ und nicht 100; deshalb funktioniert die Loop nicht.
  Y[111] = 1 - sum(Y[1:110])

  return(list(X = Alter, Y = Y))
}

einfachstesModellAlterEstimateParameters = function(X, Y){
  # Y wird als f(X) geschätzt. Hier ist f die Dichte einer Noramlverteilung. Die ent -
  # sprechenden Parameter mu und sigma werden zurückgegeben.

  l2error = function(vec){sum( ((2*pi*vec[2]^2)^(-1/2) *
                          exp(- (X-vec[1])^2/(2*vec[2]^2)) - Y )^2 )^(1/2)}
  fit = optim(par = c(80,1), fn = l2error)

  return(list(mu = fit$par[1], sigma = fit$par[2]))
}


##########################################################################################
MakePlotEinfacheModelle = function(){
  #######################################
  # make plot einfachstesModellGeburtsjahr
  pdf("../../1 Doku/graphics/einfachstesModellGeburtsjahr.pdf", width = 10, height = 8)
  erg = einfachstesModellGeburtsjahr(deathrates1879westmatrix, exposure1879westmatrix)
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
  # TODO Legende einfügen
  dev.off()

  #######################################
  # make plot einfachstesModellAlter
  pdf("../../1 Doku/graphics/einfachstesModellAlter.pdf", width = 10, height = 8)
  erg = einfachstesModellAlterProjektion(deathrates1879westmatrix, exposure1879westmatrix)
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
  plot(X,Y, pch = 4, xlab = "Alter", ylab = "Sterbewahrscheinlichkeit")
  erg = einfachstesModellAlterEstimateParameters(X, Y)
  curve((2*pi*erg$sigma^2)^(-1/2) * exp(- (x-erg$mu)^2/(2*erg$sigma^2)), add = T)
  dev.off()
}
