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
  # Beschreibung : TODO

}

##########################################################################################
Whittaker = function(){
  # Beschreibung : TODO

}


Trend = function(){


}
