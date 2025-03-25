# Carga librerías y bases-----------------
library(tidyverse)
library(eph)
library(lubridate)

canastas <- readxl::read_xlsx("fuentes/canastas_serie_lalo.xlsx")
datos_cifra <- readxl::read_xlsx("fuentes/datos_cifra.xlsx")
ipc <- readxl::read_xlsx("fuentes/ipc_trimestral_2024.xlsx")

salario_minimo <- read.csv("fuentes/salario_minimo.csv", sep = ";")
salario_minimo$indice_tiempo <- dmy(salario_minimo$indice_tiempo)

salario_minimo$mes <- month(salario_minimo$indice_tiempo)
salario_minimo$ano <- year(salario_minimo$indice_tiempo)

salario_minimo <- subset(salario_minimo, ano > 2010)

salario_minimo$ano_trim <- paste0(salario_minimo$ano, "-", quarter(salario_minimo$indice_tiempo))

salario_minimo$ano_sem <- paste0(salario_minimo$ano, "-", semester(salario_minimo$indice_tiempo))

salario_minimo <- salario_minimo %>% 
  mutate(salario_minimo_vital_movil_mensual = ifelse(indice_tiempo == "2021-05-01",
                                                     "24407",
                                                    salario_minimo_vital_movil_mensual)) 

salario_minimo <- salario_minimo %>% 
  group_by(ano_sem) %>%
  summarise(salario_minimo = mean(as.numeric(salario_minimo_vital_movil_mensual)))

ipc$ano_trim <- paste(ipc$Año, ipc$Trimestre, sep = "-")
ipc$sem <- case_when(ipc$Trimestre <= 2 ~ 1,
                     ipc$Trimestre > 2 ~ 2)
ipc$ano_sem <- paste(ipc$Año, ipc$sem, sep = "-")

ipc <- ipc %>% 
  select(ano_sem, ipc2024) %>% 
  group_by(ano_sem) %>%
  summarise(ipc2024 = mean(ipc2024))




eph <- get_microdata(year = c(2017:2024), period = c(1:4), type = "individual")

eph <- eph %>% 
  mutate(semestre = case_when(TRIMESTRE <= 2 ~ 1,
                              TRIMESTRE > 2 ~ 2),
         trim = case_when(TRIMESTRE == 1 ~ "1",
                          TRIMESTRE == 2 ~ "2",
                          TRIMESTRE == 3 ~ "3",
                          TRIMESTRE == 4 ~ "4"),
         ano2 = str_sub(as.character(ANO4), 3, 4))

eph$ano_trim <- paste(as.character(eph$ANO4), as.character(eph$trim), sep = "-")
eph$ano_sem <- paste(as.character(eph$ANO4), as.character(eph$semestre), sep = "-")

# Cálculo pobreza-----------
eph <- calculate_poverty(base = eph, basket = canastas, print_summary = FALSE,
                         window = "quarter")

eph <- eph %>% 
  mutate(pobreza_dic = factor(case_when(situacion %in% c("pobre", "indigente") ~ "Pobre",
                                        situacion %in% "no_pobre" ~ "No pobre"),
                              levels = c("Pobre", "No pobre")))

eph <- eph %>% 
  group_by(ano_trim, CODUSU, NRO_HOGAR) %>%
  mutate(miembros = n()) %>% 
  ungroup() %>% 
  mutate(ipcf2 = ITF / miembros)


# Procesamientos promedio---------------

tabla <- eph %>% 
  filter(ANO4 >= 2016, CH03 == 1) %>% 
  group_by(ano_sem, pobreza_dic) %>% 
  summarise(media_ipcf_pobres = weighted.mean(ipcf2, PONDIH)) %>% 
  filter(pobreza_dic == "Pobre")

tabla2 <- eph %>% 
  filter(ANO4 >= 2016, CH03 == 1) %>% 
  group_by(ano_sem) %>% 
  summarise(media_ipcf = weighted.mean(ipcf2, PONDIH))
  

tabla <- tabla %>% 
  left_join(tabla2, by = "ano_sem") %>% 
  left_join(salario_minimo, by = "ano_sem") %>% 
  left_join(ipc, by = "ano_sem") %>%
  select(-pobreza_dic) 

tabla <- tabla %>% 
  mutate(salario_minimo_real = (salario_minimo / ipc2024)*100,
         media_ipcf_real = (media_ipcf / ipc2024)*100,
         media_ipcf_pobres_real = (media_ipcf_pobres / ipc2024)*100) %>% 
  as.data.frame()

xlsx::write.xlsx(tabla, "salidas/salario_minimo.xlsx", row.names = FALSE)


# Procesamientos pobreza-------------
datos_cifra <- datos_cifra %>% 
  mutate(semestre = case_when(trimestre <= 2 ~ 1,
                              trimestre > 2 ~ 2),
         trim = case_when(trimestre == 1 ~ "1",
                          trimestre == 2 ~ "2",
                          trimestre == 3 ~ "3",
                          trimestre== 4 ~ "4"))

datos_cifra$ano_trim <- paste(as.character(datos_cifra$año), as.character(datos_cifra$trim), sep = "-")

datos_cifra$ano_sem <- paste(as.character(datos_cifra$año), as.character(datos_cifra$semestre), sep = "-")

pobreza <- datos_cifra %>% 
  filter(año < 2016) %>% 
  group_by(ano_sem) %>% 
  summarise(pobreza = mean(pobreza_personas))

pobreza_eph <- eph %>% 
  filter(ANO4 >= 2016) %>% 
  group_by(ano_trim, pobreza_dic) %>%
  tally(PONDIH) %>% 
  group_by(ano_trim) %>%
  mutate(prop = (n / sum(n))*100) %>% 
  filter(pobreza_dic == "Pobre") %>%
  rename(pobreza = prop) %>%
  select(ano_trim, pobreza)

pobreza_tot <- pobreza %>%
  add_row(pobreza_eph) 

pobreza_tot <- pobreza_tot %>%
  add_row(ano_sem = "2015-2", pobreza = NA, .after = 24)


pobreza_tot %>% 
  filter(ano_sem != "2024-2") %>% 
  ggplot(aes(x = ano_sem, y = pobreza, group = 1)) +
  geom_line(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))


salario_minimo <- salario_minimo %>% 
  left_join(ipc, by = "ano_trim")

salario_minimo <- salario_minimo %>%
  mutate(salario_minimo_real = (salario_minimo / ipc2024)*100) %>% 
  select(ano_trim, salario_minimo_real)

tabla <- pobreza_eph %>% 
  left_join(salario_minimo, by = "ano_trim") %>% 
  as.data.frame()

xlsx::write.xlsx(tabla, "salidas/salario_minimo2.xlsx", row.names = FALSE)
