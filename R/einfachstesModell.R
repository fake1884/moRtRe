# Diese Datei enthälte die Skripte, um einfach Modelle zu bestimmen.

##########################################################################################
einfachstesModellGeburtsjahr = function(RatesIn, ExposureIn){
  # Diese Funktion berechnet aus einer Generationstafel die Anzahlgewichteten Tode pro
  # Geburtsjahrgang

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

  return(list(X = Alter, Y = Y, Todesfälle = Todesfälle_gesamt))
}

einfachstesModellAlterEstimateParameters = function(X, Y){
  # Y wird als f(X) geschätzt. Hier ist f die Dichte einer Noramlverteilung. Die ent -
  # sprechenden Parameter mu und sigma werden zurückgegeben.

  l2error = function(vec){sum( ((2*pi*vec[2]^2)^(-1/2) *
                          exp(- (X-vec[1])^2/(2*vec[2]^2)) - Y )^2 )^(1/2)}
  fit = optim(par = c(mean(which(Y != 0)),var(which(Y != 0))), fn = l2error)

  return(list(mu = fit$par[1], sigma = fit$par[2]))
}




##########################################################################################
# make plots for the simple model
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
  rates_gesamt = deathrates1879westmatrix[,3]
  exposure_gesamt = exposure1879westmatrix[,3]
  Todesfälle_gesamt = rates_gesamt * exposure_gesamt
  data = matrix(data = Todesfälle_gesamt, nrow = length(0:95), ncol = length(1876:1987),
                dimnames = list(0:95, 1876:1987))
  names(data) = 1876:1987
  library(lattice)
  levelplot(data, col.regions=grey((110*61):0/(110*61)), regions = T,
            xlab = "Alter", ylab = "Geburtsjahr seit 1876", xlim = c(0:95), ylim = c(0:110))
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

  sigma_epsilon_hat =
    sum((Y - (2*pi*erg$sigma^2)^(-1/2) * exp(- (X-erg$mu)^2/(2*erg$sigma^2)))^2)/(length(Y)-2)
  plot(X, (Y - (2*pi*erg$sigma^2)^(-1/2) * exp(- (X-erg$mu)^2/(2*erg$sigma^2)))^2)

}


##########################################################################################
# make plots for the likelihood
einfachstesModellLikelihood = function(X, Y){
  # gesucht sind Nullstellen der log-likelihoodfunktionen
  erg = einfachstesModellAlterProjektion(deathrates1879westmatrix, exposure1879westmatrix)
  X = erg$X
  Y = erg$Y

  # plots for m
  s = 1
  likelihoodm = rep(NA, 111)
  for(m in 0:110){
    likelihoodm[m+1] = sum(((1/sqrt(2*pi*s^2))*(exp(-(m - X)^2/(2*s^2)) - Y))
                           * exp(-(m-X)^2/(2*s^2)) * (m - X))
  }
  pdf("../../1 Doku/graphics/mlikelihood-grob.pdf", width = 10, height = 8)
  plot(0:110, likelihoodm, pch = 4)
  lines(0:110, rep(0, 111))
  dev.off()
  pdf("../../1 Doku/graphics/mlikelihood-fine.pdf", width = 10, height = 8)
  plot(2:108, likelihoodm[3:109], pch = 4)
  lines(0:110, rep(0, 111))
  dev.off()

  #####################################
  # a grob plot for s
  likelihoods = rep(NA, 201)
  m=80
  i=1
  for(s in seq(0, 200, by=1)){
    likelihoods[i] = sum(
      ( (1/sqrt(2*pi*s^2)) * exp(-(m-X)^2/(2*s^2)) - Y ) *
        (  exp(-(m-X)^2/(2*s^2)) )  *
        ( ((m-X)^2/sqrt(s)^5) - (1/sqrt(s)^3) )
    )
    i=i+1
  }
  pdf("../../1 Doku/graphics/slikelihood-grob.pdf", width = 10, height = 8)
  plot(0:200, likelihoods, pch=4)
  lines(0:200, rep(0, 201))
  dev.off()

  pdf("../../1 Doku/graphics/slikelihood-fine.pdf", width = 10, height = 8)
  plot(4:200, likelihoods[4:200], pch = 4)
  lines(4:200, rep(0, 197))
  dev.off()

  # a fine plot for s
  likelihoods = rep(NA, 61)
  m=80
  i=1
  for(s in seq(4, 10, by=0.1)){
    likelihoods[i] = sum(
      ( (1/sqrt(2*pi*s^2)) * exp(-(m-X)^2/(2*s^2)) - Y ) *
        (  exp(-(m-X)^2/(2*s^2)) )  *
        ( ((m-X)^2/sqrt(s)^5) - (1/sqrt(s)^3) )
    )
    i = i+1
  }
  pdf("../../1 Doku/graphics/slikelihood-even-finer.pdf", width = 10, height = 8)
  plot(seq(4, 10, by=0.1), likelihoods, pch = 4)
  lines(seq(4, 10, by=0.1), rep(0, 61))
  dev.off()
}
