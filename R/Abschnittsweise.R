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

  pdf("../../1 Doku/graphics/abschnittsweises-Dichteschaetzen.pdf", width = 10, height = 8)
  plot(0,0, xlim = c(0, 110), ylim = c(0, 0.4), pch = 7, xlab = "Alter",
       ylab = "Sterblichkeit")

  for(i in Geburtsjahre){
    # vielleicht ist es besser, nur jeden zehnten Geburtsjahrgang zu plotten?
    # -> also mittels der Mod-Funktion (%%) jeden zehnten aussuchen.

    # select the correct dataset
    rates_i = subset(deathrates1879westmatrix, deathrates1879westmatrix[,1] == i)
    exposure_i = subset(exposure1879westmatrix, exposure1879westmatrix[,1] == i)

    Y = rates_i[,3] * exposure_i[,3] / sum(rates_i[,3] * exposure_i[,3])

    # select only rates != 0
    # TODO der Plot enthält eventuell noch zu viele Informationen
    index = which(Y != 0)
    if(i %% 20 ==  9){
      lines(Alter[index], Y[index], lty = i%%2)
    }

    # estimate mu und sigma
    mu[i-1878] = einfachstesModellAlterEstimateParameters(Alter,Y)$mu # i is the Geburtsjahr
                                                                      # and not the Index.
  }
  dev.off()

  # Annahme mu_{t+1} = a * mu_{t} + \epsilon -> predict mu
  pdf("../../1 Doku/graphics/mu-schaetzer.pdf", width = 10, height = 8)
  plot(Geburtsjahre, mu, type = "l")
  dev.off()

  mu.hat = (mu[length(mu)] - mu[1]) / (Geburtsjahre[length(Geburtsjahre)] - Geburtsjahre[1])
}

