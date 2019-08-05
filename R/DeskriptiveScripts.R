# Diese Datei enthält eine Funktion, die einen Plot erstellt, aus dem ersichtlich wird/
# werden soll, dass es Sterblichkeitsverbesserungen/Trends gibt.

##########################################################################################
MakePlotEinleitung = function() {
# Diese Funktion erstellt eine einfache Graphik, in der man die späteren Tode der
# Bevölkerung sieht. Genauer werden zwei Balkendiagramme gezeichnet: Links die Verteilung
# der Tode über die Alter im Jahr 1990 und rechts im Jahr 2017.

  years = 1990:2017

  # Sterblichkeit in den Jahren 1990 und 2017
  deaths.1990 = subset(deaths1990, deaths1990$Year == 1990)
  deaths.2017 = subset(deaths1990, deaths1990$Year == 2017)

  pdf("../../1 Doku/graphics/einleitung_barplot.pdf", width = 10, height = 8)
  par(mfrow=c(1,2))
  barplot(deaths.1990$Total, horiz = TRUE, xlab = "Anzahl Tode 1990", ylab = "Alter",
          names.arg = 0:110)
  barplot(deaths.2017$Total, horiz = TRUE, xlab = "Anzahl Tode 2017", ylab = "Alter",
          names.arg = 0:110)
  dev.off()

}


########################################################################################
MakePlotKapitel2 = function(){
  # Diese Funktion erstellt einen Plot, der das Problem der das Problem welche Personen
  # man für das Schätzen von rohen Sterblichkeiten beachten soll darstellt.

  pdf("../../1 Doku/graphics/kapitel2_rohe_wkeit.pdf", width = 10, height = 8)
  plot(x = c(2002.25, 2003.25), y = rep(1, 2), xlim = c(2000,2004), type = "l",
       xlab = "Kalenderjahr", ylab = "Person", ylim = c(0,4))
  lines(x = c(2001.5, 2002.5), y = rep(2,2))
  lines(x = c(2001.5, 2001.95), y = rep(3,2))
  abline(v = 2002)
  abline(v = 2004)
  dev.off()
}

#########################################################################################
MakePlotGrippe = function(){
  # Es soll 1969/1970 eine Grippewelle gegeben haben. Hier soll eine Graphik erzeugt
  # werden, die diese Grippewelle darstellt.

  deathrates1969 = subset(deathrates1965west[,3], deathrates1965west[,1] == 1969)
  deathrates1959 = subset(deathrates1965west[,3], deathrates1965west[,1] == 1959)

  plot(x = 0:110, y = deathrates1969, type = "l")
  lines(x = 0:110, y = deathrates1959, lty = "dashed")

}
