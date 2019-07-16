# In dieser Datei liegen die Skripte um Abschnittsweis Kerndichteschäzer und Trends
# zu bestimmen.

##########################################################################################
MakePlotAbschnittsweise = function(){
  # Abschnittsweises Dichteschätzen


  # initialize two vectors to store data
  Geburtsjahre = 1879:1987
  Alter = 0:110

  # Vektor für die mu's anlegen
  mu = rep(NA, length(Geburtsjahre))

  pdf("../../1 Doku/graphics/abschnittsweises-Dichteschaetzen.pdf", width = 10, height = 8,
      xlab = "Alter", ylab = "Sterblichkeit")
  plot(0,0, xlim = c(0, 110), ylim = c(0,1), pch = 7)

  for(i in Geburtsjahre){
    # vielleicht ist es besser, nur jeden zehnten Geburtsjahrgang zu plotten?
    # -> also mittels der Mod-Funktion (%%) jeden zehnten aussuchen.

    # select the correct dataset
    rates = subset(deathrates1879west, deathrates1879west$Geburtsjahr == i)
    exposure = subset(exposure1879west, exposure1879west$Geburtsjahr == i)

    # convert: 1. factor -> vector
    #          2. string -> integer
    rates_i = as.numeric(as.vector(rates$Gesamt))
    exposure_i = as.numeric(as.vector(exposure$Gesamt))
    Todesfälle_i = sum(rates_i * exposure_i)

    # select only rates != 0
    # TODO der Plot enthält eventuell noch zu viele Informationen
    index = which(rates_i != 0)
    lines(Alter[index], rates_i[index], lty = i%%2)

    # estimate mu und sigma
    X = Alter
    Y = rates_i
    l2error = function(vec){sum( ((2*pi*vec[2]^2)^(-1/2) *
                                    exp(- (X-vec[1])^2/(2*vec[2]^2)) - Y )^2 )^(1/2)}
    fit = optim(par = c(80,1), fn = l2error)
    mu[i-1878] = fit$par[1] # i is the Geburtsjahr and not the index
  }
  dev.off()

  # Annahme mu_{t+1} = a * mu_{t} + \epsilon -> predict mu
  pdf("../../1 Doku/graphics/mu-schaetzer.pdf", width = 10, height = 8)
  plot(Geburtsjahre, mu, type = "l")
  dev.off()

}

