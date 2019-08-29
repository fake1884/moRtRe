# This file contains functions, to read in relevant data and to save it in \data.
# Thus, the data should easily be availible to other functions in this package.

# The data in this project comes from mortality.org

##########################################################################################
ConvertData = function() {
  # read generation data 1879 west
  deathrates1879west = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/2 west germany (1876 - 1987)/cleaned/generation table - death rates 1876 - 1987 west - cleaned", header = TRUE)
  exposure1879west = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/2 west germany (1876 - 1987)/cleaned/generation table - exposure 1876 - 1987 west - cleaned", header = TRUE)

  # read period data 1965 west
  deathrates1965west = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/2 west germany (1876 - 1987)/cleaned/period table - death rates 1965 - 2017 west - cleaned", header = TRUE)

  # clean data up
  # replace "." by zero; since the data frames have different levels, I have to use
  #                      different zeros;
  deathrates1879west$Gesamt[deathrates1879west$Gesamt == "."] = "0.000000"
  exposure1879west$Gesamt[exposure1879west$Gesamt == "."] = "0.00"
  # here I use 0 for missing values. A problem is, that log(0) = -inf, but this happens
  # anyway, so I see no problem using 0
  deathrates1965west$Gesamt[deathrates1965west$Gesamt == "."] = "0.000000"


  # convert the data to a numeric matrix
  # convert: 1. factor -> vector
  #          2. string -> integer
  rates_gesamt = as.numeric(as.vector(deathrates1879west$Gesamt))
  exposure_gesamt = as.numeric(as.vector(exposure1879west$Gesamt))
  alter = as.numeric(as.vector(deathrates1879west$Alter)) # this line produces a Warning
  alter[is.na(alter) == TRUE] = 110 # but this line handles the Warning
  deathrates1879westmatrix = matrix(c(deathrates1879west$Geburtsjahr, alter,
                                rates_gesamt), ncol = 3)
  dimnames(deathrates1879westmatrix) = list(c(),c("Geburtsjahr", "Alter", "Deathrate"))
  exposure1879westmatrix = matrix(c(deathrates1879west$Geburtsjahr, alter,
                              exposure_gesamt), ncol = 3)
  dimnames(exposure1879westmatrix) = list(c(),c("Geburtsjahr", "Alter", "Exposure"))


  # do the same as above for the 1965 period data
  rates_gesamt = as.numeric(as.vector(deathrates1965west$Gesamt))
  alter = as.numeric(as.vector(deathrates1965west$Alter)) # this line produces a Warning
  alter[is.na(alter) == TRUE] = 110 # but this line handles the Warning
  deathrates1965west = matrix(c(deathrates1965west$Kalenderjahr, alter,
                                      rates_gesamt), ncol = 3)
  dimnames(deathrates1965west) = list(c(),c("Kalenderjahr", "Alter", "Deathrate"))

  # reduce the maximum age to 95
  deathrates1879westmatrix = subset(deathrates1879westmatrix, deathrates1879westmatrix[,2] <= 95)
  exposure1879westmatrix = subset(exposure1879westmatrix, exposure1879westmatrix[,2] <= 95)
  deathrates1965west = subset(deathrates1965west, deathrates1965west[,2] <= 95)

  # save generation data 1879 west
  devtools::use_data(deathrates1879westmatrix, overwrite = T)
  devtools::use_data(exposure1879westmatrix, overwrite = T)

  # save period data 1965
  devtools::use_data(deathrates1965west, overwrite = T)

}

