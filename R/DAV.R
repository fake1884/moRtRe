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
  Alter = 0:109
  Y = rep(NA, 111)
  Mitte = mittlere_Sterbetafel(1879:1987)

  rates = subset(deathrates1879west, deathrates1879west$Geburtsjahr == i)
  rates = as.numeric(as.vector(rates$Gesamt))
  exposure = subset(exposure1879west, exposure1879west$Geburtsjahr == i)
  exposure = as.numeric(as.vector(exposure$Gesamt))

  Todesfälle_gesamt = sum(rates * exposure)

 Y = rates * exposure / Todesfälle_gesamt

 plot(Alter, Y)
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
