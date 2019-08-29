ConvertDataUnused = function(){
  # read period data 1990
  # deaths1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-deaths-clean", header = TRUE)
  # births1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-births-cleaned", header = TRUE)
  # deathrates1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-death-rates-cleaned", header = TRUE)
  # popsize1990 = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/1 total population (1990-2017)/cleaned/1-year-germany-popsize-cleaned", header = TRUE)

  # read generation data 1879 east
  # deathrates1879east = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/3 east germany (1879 - 1987 )/cleaned/death rates 1879 - 1987 east cleaned", header = TRUE)
  # exposure1879east = read.table("/home/henning/Desktop/Masterarbeit Sterbetrends/2 Daten/3 east germany (1879 - 1987 )/cleaned/exposure 1879 - 1987 east cleaned", header = TRUE)

  # save period data 1990
  # devtools::use_data(deaths1990, overwrite = T)
  # devtools::use_data(births1990, overwrite = T)
  # devtools::use_data(deathrates1990, overwrite = T)
  # devtools::use_data(popsize1990, overwrite = T)

  # save generation data 1879 east
  # devtools::use_data(deathrates1879east, overwrite = T)
  # devtools::use_data(exposure1879east, overwrite = T)
}
