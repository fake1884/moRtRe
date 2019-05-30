# In dieser Datei liegen die Skripte um Abschnittsweis Kerndichteschäzer und Trends
# zu bestimmen.

##########################################################################################
MakePlotAbschnittsweise = function(){

  # Abschnittsweises Dichteschätzen
  pdf("../../1 Doku/graphics/abschnittsweises-Dichteschaetzen.pdf", width = 10, height = 8)

  plot(X,Y)#for now
  dev.off()
}

