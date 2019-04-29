# This file contains functions, to read in relevant data and to save it in \data.
# Thus the data should easily be availible to other functions in this package.

# The data in this project comes from mortality.org

ConvertData = function() {
  # read the data
  deaths1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-deaths-clean", header = TRUE)
  births1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-births-cleaned", header = TRUE)
  deathrates1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-death-rates-cleaned", header = TRUE)
  popsize1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-popsize-cleaned", header = TRUE)

  # save the data
  devtools::use_data(deaths1990, overwrite = T)
  devtools::use_data(births1990, overwrite = T)
  devtools::use_data(deathrates1990, overwrite = T)
  devtools::use_data(popsize1990, overwrite = T)

}
