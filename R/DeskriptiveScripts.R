# Diese Datei enthält eine Funktion, die einen Plot erstellt, aus dem ersichtlich wird/
# werden soll, dass es Sterblichkeitsverbesserungen/Trends gibt.

##########################################################################################
MakePlotEinleitung = function() {
# Diese Funktion macht ... TODO

  graphicspath = "../../1 Doku/graphics/einleitung_barplot.pdf"

  years = 1990:2017

  # Sterblichkeit in den Jahren 1990 und 2017
  deaths.1990 = subset(deaths1990, deaths1990$Year == 1990)
  deaths.2017 = subset(deaths1990, deaths1990$Year == 2017)

  pdf(graphicspath, width = 10, height = 8)
  par(mfrow=c(1,2))
  barplot(deaths.1990$Total, horiz = TRUE, xlab = "Anzahl Tode 1990", ylab = "Alter",
          names.arg = 0:110)
  barplot(deaths.2017$Total, horiz = TRUE, xlab = "Anzahl Tode 2017", ylab = "Alter",
          names.arg = 0:110)
  dev.off()

}
