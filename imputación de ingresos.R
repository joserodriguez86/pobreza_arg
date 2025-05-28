#Librerías
pacman::p_load(tidyverse, eph)

eph <- get_microdata(year = 2024, period = 3, type = "individual")
