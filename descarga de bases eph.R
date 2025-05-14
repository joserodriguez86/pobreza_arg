library(tidyverse)
library(eph)

eph2016 <- get_microdata(2016, type = "individual", period = c(1:4))
eph2017 <- get_microdata(2017, type = "individual", period = c(1:4))
eph2018 <- get_microdata(2018, type = "individual", period = c(1:4))
eph2019 <- get_microdata(2019, type = "individual", period = c(1:4))
eph2020 <- get_microdata(2020, type = "individual", period = c(1:4))
eph2021 <- get_microdata(2021, type = "individual", period = c(1:4))
eph2022 <- get_microdata(2022, type = "individual", period = c(1:4))
eph2023 <- get_microdata(2023, type = "individual", period = c(1:4))
eph2024 <- get_microdata(2024, type = "individual", period = c(1:3))
eph2024_4 <- get_microdata(2024, type = "individual", period = 4)

#arreglo por problema en 2024_4
eph2024_4 <- eph2024_4 %>% 
  select(-CH05)

eph2024 <- bind_rows(eph2024, eph2024_4)
rm(eph2024_4)

eph <- bind_rows(eph2016, eph2017, eph2018, eph2019, eph2020, eph2021, eph2022, eph2023, eph2024)
rm(eph2016, eph2017, eph2018, eph2019, eph2020, eph2021, eph2022, eph2023, eph2024)

eph_hogar2016 <- get_microdata(year = 2016, period = c(1:4), type = "hogar")
eph_hogar2017 <- get_microdata(year = 2017, period = c(1:4), type = "hogar")
eph_hogar2018 <- get_microdata(year = 2018, period = c(1:4), type = "hogar")
eph_hogar2019 <- get_microdata(year = 2019, period = c(1:4), type = "hogar")
eph_hogar2020 <- get_microdata(year = 2020, period = c(1:4), type = "hogar")
eph_hogar2021 <- get_microdata(year = 2021, period = c(1:4), type = "hogar")
eph_hogar2022 <- get_microdata(year = 2022, period = c(1:4), type = "hogar")
eph_hogar2023 <- get_microdata(year = 2023, period = c(1:4), type = "hogar")
eph_hogar2024 <- get_microdata(year = 2024, period = c(1:4), type = "hogar")

eph_hogar <- bind_rows(eph_hogar2016, eph_hogar2017, eph_hogar2018, eph_hogar2019, eph_hogar2020, eph_hogar2021, eph_hogar2022, eph_hogar2023, eph_hogar2024)

rm(eph_hogar2016, eph_hogar2017, eph_hogar2018, eph_hogar2019, eph_hogar2020, eph_hogar2021, eph_hogar2022, eph_hogar2023, eph_hogar2024)

saveRDS(eph, file = "C:/Users/josed/OneDrive/Bases/EPH/eph_individual.RDS")
saveRDS(eph_hogar, file = "C:/Users/josed/OneDrive/Bases/EPH/eph_hogar.RDS")
