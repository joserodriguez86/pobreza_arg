library(eph)
library(tidyverse)

eph <- get_microdata(year = 2010:2024, period = c(1:4), type = "hogar")

eph$ano_trim <- paste(as.character(eph$ANO4), as.character(eph$TRIMESTRE), sep = "-")

tabla <- eph %>% 
  group_by(ano_trim, V13) %>% 
  tally(PONDERA) %>% 
  mutate(porcentaje = n / sum(n) * 100)

tabla %>% 
  filter(V13 == 1) %>% 
  ggplot(aes(x = ano_trim, y = porcentaje, group = 1)) +
  geom_point() +
  geom_line() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
