# This file contains functions, to read in relevant data and to save it in \data.
# Thus, the data should easily be availible to other functions in this package.

# The data in this project comes from mortality.org

##########################################################################################
ConvertData = function() {
  # read period data 1990
  deaths1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-deaths-clean", header = TRUE)
  births1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-births-cleaned", header = TRUE)
  deathrates1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-death-rates-cleaned", header = TRUE)
  popsize1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-popsize-cleaned", header = TRUE)

  # read generation data 1879 west
  deathrates1879west = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/2 west germany (1876 - 1987)/cleaned/generation table - death rates 1876 - 1987 west - cleaned", header = TRUE)
  exposure1879west = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/2 west germany (1876 - 1987)/cleaned/generation table - exposure 1876 - 1987 west - cleaned", header = TRUE)


  # read generation data 1879 east
  deathrates1879east = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/3 east germany (1879 - 1987 )/cleaned/death rates 1879 - 1987 east cleaned", header = TRUE)
  exposure1879east = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/3 east germany (1879 - 1987 )/cleaned/exposure 1879 - 1987 east cleaned", header = TRUE)


  # read period data 1965 west
  deathrates1965west = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/2 west germany (1876 - 1987)/cleaned/period table - death rates 1965 - 2017 west - cleaned", header = TRUE)


  # clean data up
  # replace "." by zero; since the data frames have different levels, I have to use
  #                      different zeros
  deathrates1879west$Gesamt[deathrates1879west$Gesamt == "."] = "0.000000"
  exposure1879west$Gesamt[exposure1879west$Gesamt == "."] = "0.00"
  #deathrates1965west$Gesamt[deathrates1965west$Gesamt == "<NA>"] = "0.0"


  # convert the data to a numeric matrix
  # convert: 1. factor -> vector
  #          2. string -> integer
  # Ein Vergleich mit sum(a != b) liefert nicht immer Null
  # -> TODO
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

  # save period data 1990
  devtools::use_data(deaths1990, overwrite = T)
  devtools::use_data(births1990, overwrite = T)
  devtools::use_data(deathrates1990, overwrite = T)
  devtools::use_data(popsize1990, overwrite = T)

  # save generation data 1879 west
  devtools::use_data(deathrates1879westmatrix, overwrite = T)
  devtools::use_data(exposure1879westmatrix, overwrite = T)

  # save period data 1965
  devtools::use_data(deathrates1965west, overwrite = T)

  # save generation data 1879 east
  devtools::use_data(deathrates1879east, overwrite = T)
  devtools::use_data(exposure1879east, overwrite = T)

}

