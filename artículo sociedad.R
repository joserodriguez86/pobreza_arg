# Librerías y carga de bases-----------------------

rm(list = ls())
pacman::p_load(tidyverse, eph, ggsci, flextable, ggrepel, GDAtools, plotly, patchwork, spatstat, lubridate, ggalluvial, jtools, nnet, huxtable, ggeffects, marginaleffects)

eph <- readRDS("C:/Users/josed/OneDrive/Bases/EPH/eph_individual.RDS")
eph_hogar <- readRDS("C:/Users/josed/OneDrive/Bases/EPH/eph_hogar.RDS")

canastas <- readxl::read_xlsx("fuentes/canastas_serie.xlsx")

#IPC
ipc <- read.csv("https://infra.datos.gob.ar/catalog/sspm/dataset/145/distribution/145.1/download/indice-precios-al-consumidor-nivel-general-base-diciembre-2016-trimestral.csv")

ipc <- ipc %>% 
  select(indice_tiempo, ipc_ng_nacional)

ipc$indice_tiempo <- ymd(ipc$indice_tiempo)

ipc <- ipc %>%
  mutate(
    ipc_actual = (ipc_ng_nacional / ipc_ng_nacional[which(ipc$indice_tiempo == ymd("2024-10-01"))] * 100),
    anio = year(indice_tiempo),
    trimestre = quarter(indice_tiempo),  # sin with_label
    trimestre = case_when(
      trimestre == 1 ~ "I",
      trimestre == 2 ~ "II",
      trimestre == 3 ~ "III",
      trimestre == 4 ~ "IV"
    )
  )


##
theme_set(theme_bw())

#Construcción de variables-----------

#Joins
eph <- eph %>% 
  mutate(semestre = case_when(TRIMESTRE <= 2 ~ 1,
                              TRIMESTRE > 2 ~ 2),
         trim = case_when(TRIMESTRE == 1 ~ "I",
                          TRIMESTRE == 2 ~ "II",
                          TRIMESTRE == 3 ~ "III",
                          TRIMESTRE == 4 ~ "IV"),
         ano2 = str_sub(as.character(ANO4), 3, 4),
         ano_trim = paste(as.character(ANO4), as.character(trim), sep = "-"),
         ano_sem = paste(as.character(ANO4), as.character(semestre), sep = "-"))

ipc <- ipc %>% 
  mutate(ano_trim = paste(as.character(anio), as.character(trimestre), sep = "-")) %>% 
  select(ano_trim, ipc_actual)

eph <- eph %>% 
  left_join(ipc, by = "ano_trim")

eph_hogar <- eph_hogar %>% 
  select(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE, V13, V14, V15)

eph <- eph %>% 
  left_join(eph_hogar, by = c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE"))

#Creación de variables
eph <- calculate_poverty(base = eph, basket = canastas, print_summary = FALSE,
                         window = "semester")

eph <- organize_caes(base = eph)


calcular_tamano <- function(x, x99, a) {
  case_when(
    (x > 0 & x <= 5) | (x == 99 & x99 == 1) ~ 1,
    (x > 5 & x < 99) | (x == 99 & x99 %in% c(2, 3)) ~ 2,
    ((x == 0 & x99 == 0) | (x == 99 & x99 == 9)) & a == 1 ~ 2,
    a == 1 ~ 2,
    TRUE ~ NA_real_
  )
}

eph <- eph %>%
  mutate(
    tamano = calcular_tamano(PP04C, PP04C99, PP04A),
    tamano_desocup = calcular_tamano(PP11C, PP11C99, PP11A),
    tamano = coalesce(tamano, tamano_desocup)
  )


eph <- eph %>% 
  mutate(pobreza_dic = case_when(situacion %in% c("pobre", "indigente") ~ 1,
                                 situacion %in% "no_pobre" ~ 0),
         pobreza3 = case_when(situacion == "indigente" ~ 1,
                              situacion == "pobre" ~ 2,
                              situacion == "no_pobre" ~ 3),
         indigente = case_when(situacion == "indigente" ~ 1,
                               TRUE ~ 0),
         cno = ifelse(is.na(PP04D_COD), PP11D_COD, PP04D_COD),
         cno12 = ifelse(nchar(cno) > 4, str_sub(cno, 1, 2), str_sub(cno, 1, 1)),
         cno3 = ifelse(nchar(cno) > 4, str_sub(cno, 3, 3), str_sub(cno, 2, 2)),
         cno4 = ifelse(nchar(cno) > 4, str_sub(cno, 4, 4), str_sub(cno, 3, 3)),
         cno5 = ifelse(nchar(cno) > 4, str_sub(cno, 5, 5), str_sub(cno, 4, 4)),
         cno12 = as.numeric(cno12),
         cno3 = as.numeric(cno3),
         cno4 = as.numeric(cno4),
         cno5 = as.numeric(cno5),
         cat_ocup_f = case_when(CAT_OCUP == 4 ~ 3,
                                CAT_OCUP == 1 ~ 1,
                                CAT_OCUP == 2 ~ 2,
                                CAT_OCUP == 3 ~ 3,
                                TRUE ~ NA_real_),
         cat_ocup_f = factor(cat_ocup_f,
                             labels = c("Patrón", "Cuenta Propia",
                                        "Asalariado")),
         empleos_cant = case_when(PP03C == 1 ~ "Un empleo",
                                  PP03C == 2 ~ "Más de un empleo",
                                  TRUE ~ NA_character_),
         INTENSI = factor(case_when(INTENSI == 1 ~ "Subocupados",
                                    INTENSI == 2 ~ "Ocupados plenos",
                                    INTENSI == 3 ~ "Sobreocupados",
                                    TRUE ~ NA_character_)),
         rama = factor(substr(caes_eph_label, start = 1, stop = 28)),
         rama2 = substr(caes_seccion_label, start = 1, stop = 60),
         rama2 = factor(str_to_sentence(rama2)),
         rama4 = factor(case_when(caes_seccion_cod %in% c("B", "J", "K", "L", "M", "N",
                                                   "U") ~ 1,
                           caes_seccion_cod %in% c("A", "C", "D", "E", "F", "G", "H", "I", "R", "S") ~ 2,
                           caes_seccion_cod %in% c("O", "P", "Q") ~ 3,
                           caes_seccion_cod == "T" ~ 4),
                        labels = c("Privado1", "Privado2", "Público", "Doméstico")),
         informal = factor(case_when(ESTADO == 1 & CAT_OCUP == 3 & 
                                       (PP07H == 1) ~ "Formal",
                                     ESTADO == 1 & CAT_OCUP == 3 & 
                                       PP07H != 1 ~ "Informal",
                                     ESTADO == 2 & CAT_OCUP == 3 &
                                       PP11N == 1 ~ "Formal",
                                     ESTADO == 2 & CAT_OCUP == 3 &
                                       PP11N != 1 ~ "Informal",
                                     CAT_OCUP == 2 & cno5 <= 2 ~ "Formal",
                                     CAT_OCUP == 2 & cno5 > 2 ~ "Informal",
                                     CAT_OCUP == 1 & tamano == 2 ~ "Formal",
                                     CAT_OCUP == 1 & tamano != 2 & caes_division_cod %in% c(62, 63, 85) ~ "Formal",
                                     CAT_OCUP == 1 & tamano != 2 & !caes_division_cod %in% c(62, 63, 85) ~ "Informal",
                                     CAT_OCUP == 4 | CAT_OCUP == 9 ~ "Informal",
                                     TRUE ~ NA_character_)),
         sector = factor(ifelse(PP04A != 1, "Privado", "Público")),
         horas = PP3E_TOT + PP3F_TOT,
         horas = ifelse(horas <= 0 | horas > 168, NA_real_, horas),
         intensi2 = case_when(horas <= 35 ~ "Subocupados",
                              horas > 35 & horas < 45 ~ "Ocupados plenos",
                              horas >= 45 ~ "Sobreocupados",
                              TRUE ~ NA_character_),
         decil_ipcf = as.numeric(DECCFR),
         nivel_educativo = case_when(NIVEL_ED <= 3 | NIVEL_ED == 7 ~ "Hasta primario completo",
                                     NIVEL_ED >= 4 & NIVEL_ED <= 5 ~ "Hasta secundario completo",
                                     NIVEL_ED == 6 ~ "Hasta terciario/universitario completo",
                                     TRUE ~ NA_character_),
         nivel_educativo = factor(nivel_educativo),
         distancia_pobreza = (ITF - CBT_hogar)/CBT_hogar,
         distancia_pobreza3 = factor(case_when(
           distancia_pobreza <= -.5 & distancia_pobreza >= -1 ~ "-100%/-50%",
           distancia_pobreza <= -.2 & distancia_pobreza > -.5 ~ "-49%/-20%",
           distancia_pobreza < 0 & distancia_pobreza > -.2 ~ "-19%/0%",    
           distancia_pobreza >= 0 & distancia_pobreza <= .2 ~ "1%/20%",
           distancia_pobreza > .2 & distancia_pobreza <= .5 ~ "21%/50%",
           distancia_pobreza > .5 & distancia_pobreza <= 1 ~ "51%/100%",
           distancia_pobreza > 1 ~ "Más de 100%",
           TRUE ~ NA_character_), 
           levels = c("-100%/-50%", "-49%/-20%", "-19%/0%", "1%/20%", "21%/50%", "51%/100%", "Más de 100%")),
         sexo = factor(CH04, labels = c("Varón", "Mujer")),
         grupo_edad = factor(case_when(CH06 >= 18 & CH06 < 25 ~ "Menor de 24 años",
                                            CH06 >= 25 & CH06 < 35 ~ "25 a 34 años",
                                            CH06 >= 35 & CH06 < 65 ~ "35 a 65 años",
                                            CH06 >= 65 ~ "65 años o más"),
                                          levels = c("Menor de 24 años", "25 a 34 años",
                                                     "35 a 65 años", "65 años o más")))


#ingresos

eph <- eph %>% 
  group_by(ano_trim, CODUSU, NRO_HOGAR) %>%
  mutate(miembros = n(),
         ocupados = sum(ESTADO == 1) / sum(CH06 >= 14)) %>% 
  ungroup() %>% 
  mutate(ipcf2 = ITF / miembros,
         ipcf2024 = (ipcf2 / ipc_actual)*100,
         iti2024 = (P47T / ipc_actual)*100,
         p21_2024 = (P21 / ipc_actual)*100,
         ingresos_lab1 = ifelse(P21 == -9, 0, P21),
         ingresos_lab2 = ifelse(TOT_P12 == -9, 0, TOT_P12),
         ing_lab = ingresos_lab1 + ingresos_lab2,
         ing_horario = ing_lab / (horas * 4.3),
         ing_lab2024 = (ing_lab / ipc_actual)*100,
         ing_nolab2024 = iti2024 - ing_lab2024,
         ing_horario2024 = (ing_horario / ipc_actual)*100,
         peso_canasta = ITF / CBT_hogar)


#Variables hogar
eph_hogar <- eph_hogar %>% 
  mutate(semestre = case_when(TRIMESTRE <= 2 ~ 1,
                              TRIMESTRE > 2 ~ 2),
         trim = case_when(TRIMESTRE == 1 ~ "I",
                          TRIMESTRE == 2 ~ "II",
                          TRIMESTRE == 3 ~ "III",
                          TRIMESTRE == 4 ~ "IV"),
         ano2 = str_sub(as.character(ANO4), 3, 4),
         ano_trim = paste(as.character(ANO4), as.character(trim), sep = "-"),
         ano_sem = paste(as.character(ANO4), as.character(semestre), sep = "-"))



# Construcción clase social ----------------------------
##Categoría ocupacional
eph <- eph %>% 
  mutate(categoria = case_when(CAT_OCUP == 1 & cno3 == 0 ~ 1,
                               CAT_OCUP == 2 | cno3 == 1 ~ 2,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) | (cno3 == 2 | cno3 == 3) ~ 3,
                               (CAT_OCUP == 3 | CAT_OCUP == 4) & (cno3 == 0) ~ 3,
                               CAT_OCUP == 1 & cno3 > 1 ~ 1,
                               CAT_OCUP == 9 & cno3 == 0 ~ 1,
                               CAT_OCUP == 9 & cno3 > 1 ~ 3,
                               cno3 == 9 & CAT_OCUP > 2 ~ 3))


## Clasificador

eph <- eph %>% 
  mutate(cobhe = case_when(#Clase I: propietarios > 5 y directivos, gerentes, funcionarios de dirección
    cno12 >= 0 & cno12 <= 4 ~ 1, 
    cno12 == 6 | cno12 == 7 ~ 1,
    
    #Clase II: propietarios < 5 y directivos, gerentes, funcionarios de dirección  
    cno12 == 5 ~ 2,
    
    #Clase III: cuenta propias profesionales/calificados
    (cno12 == 32 | cno12 == 35 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
       cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
       cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
       cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
       cno12 == 80 | cno12 == 82) & categoria == 2 & cno5 < 3 ~ 3,
    
    (cno12 == 10 | cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
       cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
       cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
       cno12 == 91 | cno12 == 92) & categoria == 2 & cno5 <= 3 ~ 3,
    
    cno12 == 34 & categoria == 2 & cno5 <= 2 ~ 3,
    cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,
    cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 > 1 ~ 3,
    
    #Clase IV: trabajadores no manuales > 5
    (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 30 & categoria == 3 & tamano == 2 ~ 4,
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
    cno12 == 35 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
    cno12 == 36 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 44 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 2 ~ 4,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 54 & categoria == 3 & tamano == 2 & cno5 <= 3 ~ 4,
    cno12 == 58 & categoria == 3 & tamano == 2 & cno5 <= 2 ~ 4,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 != 2 ~ 4,
    cno12 == 81 & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 91 & categoria == 3 & tamano == 2 ~ 4,
    cno12 == 92 & categoria == 3 & tamano == 2 & cno5 == 1 ~ 4,
    
    #Clase V: trabajadores manuales > 5
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
    cno12 == 34 & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 35 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
    cno12 == 36 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    cno12 == 44 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    cno12 == 51 & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 53 & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 54 & categoria == 3 & tamano == 2 & cno5 == 4 ~ 5,
    (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 2 ~ 5,
    cno12 == 58 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 2 ~ 5,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 <= 2 & cno4 == 2 ~ 5,
    cno12 == 80 & categoria == 3 & tamano == 2 & cno5 > 2 ~ 5,
    (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 2  ~ 5,
    cno12 == 92 & categoria == 3 & tamano == 2 & cno5 > 1 ~ 5,
    
    #Clase VI: trabajadores no manuales < 5
    (cno12 >= 10 & cno12 <= 20) & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 30 & categoria == 3 & tamano == 1 ~ 6,
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
    cno12 == 35 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
    cno12 == 36 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 >= 40 & cno12 <= 43) & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 44 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 == 45 | cno12 == 46) & categoria == 3 & tamano == 1 ~ 6,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 == 50 | cno12 == 52) & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 54 & categoria == 3 & tamano == 1 & cno5 <= 3 ~ 6,
    cno12 == 58 & categoria == 3 & tamano == 1 & cno5 <= 2 ~ 6,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 != 2 ~ 6,
    cno12 == 81 & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 91 & categoria == 3 & tamano == 1 ~ 6,
    cno12 == 92 & categoria == 3 & tamano == 1 & cno5 == 1 ~ 6,
    
    #Clase VII: trabajadores manuales < 5
    (cno12 == 31 | cno12 == 32) & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
    cno12 == 34 & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 35 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
    cno12 == 36 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    cno12 == 44 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 >= 47 & cno12 <= 49) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    cno12 == 51 & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 53 & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 54 & categoria == 3 & tamano == 1 & cno5 == 4 ~ 7,
    (cno12 == 56 | cno12 == 57)  & categoria == 3 & tamano == 1 ~ 7,
    cno12 == 58 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
    (cno12 >= 60 & cno12 <= 63) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 == 64 | cno12 == 65)  & categoria == 3 & tamano == 1 ~ 7,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
    (cno12 >= 70 & cno12 <= 72) & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 <= 2 & cno4 == 2 ~ 7,
    cno12 == 80 & categoria == 3 & tamano == 1 & cno5 > 2 ~ 7,
    (cno12 == 82 | cno12 == 90) & categoria == 3 & tamano == 1  ~ 7,
    cno12 == 92 & categoria == 3 & tamano == 1 & cno5 > 1 ~ 7,
    cno12 == 55 ~ 7,
    is.na(tamano) & PP04B1 == 1 ~ 7,
    
    #Clase VIII: Cuenta propia semi-calificados y no calificados
    (cno12 == 10 | cno12 == 32 | cno12 == 51 | cno12 == 52 | cno12 == 53 |
       cno12 == 54 | cno12 == 57 | cno12 == 58 | cno12 == 60 |
       cno12 == 61 | cno12 == 62 | cno12 == 63 | cno12 == 64 |
       cno12 == 65 | cno12 == 70 | cno12 == 71 | cno12 == 72 |
       cno12 == 80 | cno12 == 82) & categoria == 2 & (cno5 == 3 | cno5 == 4) ~ 8,
    
    (cno12 == 11 | cno12 == 20 | cno12 == 30 | cno12 == 31 | cno12 == 40 |
       cno12 == 41 | cno12 == 42 | cno12 == 43 | cno12 == 44 | cno12 == 45 |
       cno12 == 46 | cno12 == 47 | cno12 == 50 | cno12 == 81 | cno12 == 90 | 
       cno12 == 91 | cno12 == 92) & categoria == 2 & (cno5 == 4) ~ 8,
    
    cno12 == 34 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
    cno12 == 35 & categoria == 2 & cno5 > 2 & cno4 == 1 ~ 8,
    cno12 == 36 & categoria == 2 ~ 8,
    cno12 == 56 & categoria == 2 ~ 8,
    cno12 == 33 ~ 8,
    categoria == 2 & cno5 == 4 ~ 8,
    
    #Clase IX: Inactivos jubiladores
    (ESTADO == 3 | ESTADO == 4) & CAT_INAC == 1 ~ 9,
    
    #Clase X: Inactivos otros
    (ESTADO == 3 | ESTADO == 4) & CAT_INAC > 1 ~ 10,
  ))

eph$cobhe_f <- factor(eph$cobhe, 
                      labels = c('Propietarios y directivos >5',
                                 'Propietarios y directivos <=5',
                                 'Cuenta propia profesionales / calificados',
                                 'Trabajadores no manuales >5',
                                 'Trabajadores manuales >5',
                                 'Trabajadores no manuales <=5',
                                 'Trabajadores manuales <=5',
                                 'Cuenta propia no calificados',
                                 'Inactivos-Jubiladores',
                                 'Inactivos-Otros'))


#Agregar variables de pobreza individuales
canastas <- canastas %>%
  mutate(REGION.y = REGION) %>%
  select(!c("REGION", "codigo", "engel"))

eph <- eph %>%
  left_join(adulto_equivalente, by = c("CH04", "CH06")) %>% 
  mutate(periodo = paste(ANO4, TRIMESTRE, sep = ".")) %>% 
  left_join(canastas, by = c("periodo", "REGION.y")) %>% 
  group_by(ano_trim, CODUSU, NRO_HOGAR) %>%
  mutate(miembros = n())



# Análisis de panel -----------------------------

# Ponderador sin elevar
eph <- eph %>% 
  group_by(ano_trim) %>% 
  mutate(pondera_sum = sum(PONDERA),
         sum = n(),
         pondera_sin_elevar = (PONDERA / pondera_sum)*sum,
         pondih_sum = sum(PONDIH),
         pondih_sin_elevar = (PONDIH / pondih_sum)*sum) %>% 
  ungroup()

eph <- eph %>% 
  group_by(ano_trim, CODUSU, NRO_HOGAR) %>%
  mutate(cantidad_empleos_h = sum(PP03C, na.rm = TRUE) / sum(ESTADO == 1, na.rm = TRUE),
         cantidad_ocupados_h = sum(ESTADO == 1, na.rm = TRUE),
         cantidad_horas_h = sum(horas, na.rm = TRUE) / sum(ESTADO == 1, na.rm = TRUE),
         edad_prom = mean(CH06, na.rm = TRUE),
         )
  
  
  
  
  

# Paneles
eph2024 <- eph %>% 
  filter(ANO4 == 2024 | ANO4 == 2023) %>% 
  mutate(pobreza_f = factor(pobreza_dic, labels = c("No pobre", "Pobre")))

pool <- organize_panels(eph2024, variables = c("ESTADO", "pobreza_f",
                                               "empleos_cant", "INTENSI",
                                               "informal", "rama4", "rama2",
                                               "sector", "horas", "cobhe_f",
                                               "ocupados", "empleos_cant",
                                               "V13", "V14", "V15", 
                                               "ing_lab2024", "sexo", 
                                               "grupo_edad", "ing_nolab2024",
                                               "ing_horario2024",
                                               "CH03",
                                               "pondih_sin_elevar",
                                               "pondera_sin_elevar"), 
                        window = 'trimestral')

pool <- subset(pool, consistencia == TRUE)

#TRABAJAR CON HOGARESSSSSSSSSSSSSSS

pool %>% 
  filter(Periodo == "2024 Q2", !is.na(pobreza_f), !is.na(pobreza_f_t1)) %>%
  group_by(pobreza_f, pobreza_f_t1) %>%
  tally() %>% 
  ungroup() %>% 
  mutate(percent = n/sum(n)) %>% 
  ggplot(aes(y = percent, axis1 = pobreza_f, axis2 = pobreza_f_t1)) +
  geom_alluvium(aes(fill = pobreza_f), width = .5) +
  geom_stratum(width = .2, alpha = .1, size = .4) +
  geom_text(aes(label = paste0(..stratum.., "\n", scales::percent(..count.., accuracy = .1))), stat = "stratum", size = 3) +
  scale_fill_d3() +
  theme_minimal(base_size = 10) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    legend.position = "none",
    plot.title = element_text(size = 10)
  ) +
  scale_x_discrete(limits = c("2024-II", "2024-III"), expand = c(.05, .05)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), breaks = seq(0, 1, by = 0.2))


ggsave("salidas/paneles_pobreza_2024.png", dpi = 300, width = 7, height = 5, bg = "white")

periodos <- c("2023 Q3", "2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3")

graficar_transiciones <- function(periodo_actual) {
  pool %>% 
    filter(Periodo == periodo_actual, !is.na(pobreza_f), !is.na(pobreza_f_t1)) %>%
    group_by(pobreza_f, pobreza_f_t1) %>%
    tally() %>% 
    ungroup() %>% 
    mutate(percent = n/sum(n)) %>% 
    ggplot(aes(y = percent, axis1 = pobreza_f, axis2 = pobreza_f_t1)) +
    geom_alluvium(aes(fill = pobreza_f), width = .5) +
    geom_stratum(width = .2, alpha = .1, size = .4) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
    scale_fill_d3() +
    labs(title = paste0(periodo_actual)) +
    theme_minimal(base_size = 10) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 7),
      axis.text.x = element_text(size = 8),
      legend.position = "none",
      plot.title = element_text(size = 10)
    ) +
    scale_x_discrete(limits = c("T", "T+1"), expand = c(.05, .05)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L), breaks = seq(0, 1, by = 0.2))
}


graficos <- map(periodos, graficar_transiciones)

grafico_final <- wrap_plots(graficos, ncol = 2) +
  plot_annotation(
    title = "Transiciones en la situación de pobreza por trimestre",
    subtitle = "Argentina urbana, 2023-2024",
    caption = "Fuente: Elaboración propia en base a EPH-INDEC.",
    theme = theme(plot.title = element_text(size = 12))
  )

ggsave("salidas/paneles_pobreza.png", dpi = 300, width = 7, height = 5)


# Análisis multinomial

pool_trans <- pool %>%
  filter(Periodo == "2024 Q2", ESTADO == 1, CH06 >= 18, CH03 == 1) %>%
  mutate(transicion = case_when(
    pobreza_f == "Pobre"     & pobreza_f_t1 == "Pobre"     ~ "Pobre-Pobre",
    pobreza_f == "Pobre"     & pobreza_f_t1 == "No pobre"  ~ "Pobre-NoPobre",
    pobreza_f == "No pobre"  & pobreza_f_t1 == "Pobre"     ~ "NoPobre-Pobre",
    pobreza_f == "No pobre"  & pobreza_f_t1 == "No pobre"  ~ "NoPobre-NoPobre",
    TRUE ~ NA_character_
  )) %>%
  mutate(transicion = factor(transicion,
                             levels = c("Pobre-Pobre", "Pobre-NoPobre", "NoPobre-Pobre", "NoPobre-NoPobre")),
         transicion = relevel(transicion, ref = "NoPobre-NoPobre"),
         empleos_cant = factor(case_when(empleos_cant == "Un empleo" &
                                           empleos_cant_t1 == "Un empleo" ~ "Mantuvo cantidad empleo",
                                         empleos_cant == "Un empleo" &
                                           empleos_cant_t1 == "Más de un empleo" ~ "Aumentó cantidad empleo",
                                         empleos_cant == "Más de un empleo" &
                                           empleos_cant_t1 == "Un empleo" ~ "Disminuyó cantidad empleo",
                                         TRUE ~ NA_character_)),
         empleos_cant = relevel(empleos_cant, ref = "Mantuvo cantidad empleo"),
         ahorros = case_when(V13 == 1 | V13_t1 == 1 ~ "Uso ahorros",
                            TRUE ~ "No uso ahorros"),
         ahorros = factor(ahorros, levels = c("No uso ahorros", "Uso ahorros")),
         prestamo_f = case_when(V14 == 1 | V14_t1 == 1 ~ "Uso préstamo familiar",
                            TRUE ~ "No uso préstamo familiar"),
         prestamo_f = factor(prestamo_f, levels = c("No uso préstamo familiar", "Uso préstamo familiar")),
         prestamo_b = case_when(V15 == 1 | V15_t1 == 1 ~ "Uso préstamo bancario",
                            TRUE ~ "No uso préstamo bancario"),
         prestamo_b = factor(prestamo_b, levels = c("No uso préstamo bancario", "Uso préstamo bancario")),
         ocupados_dif = ocupados_t1 - ocupados,
         horas_dif = horas_t1 - horas,
         cobhe_f = relevel(cobhe_f, ref = "Cuenta propia no calificados"),
         intensi = relevel(INTENSI, ref = "Ocupados plenos"),
         informal = relevel(informal, ref = "Informal"),
         grupo_edad = relevel(grupo_edad, ref = "65 años o más"),
         ingresos_lab = ing_lab2024_t1 - ing_lab2024,
         ingresos_nolab = ing_nolab2024_t1 - ing_nolab2024,
         ingresos_horarios = ing_horario2024_t1 - ing_horario2024)

multinomial <- multinom(transicion ~ sexo + grupo_edad + sector + ocupados_dif +
                          horas_dif + empleos_cant + informal + ahorros + prestamo_b + prestamo_f,
                        data = pool_trans, weights = pondih_sin_elevar_t1, trace = F)

tidied <- tidy(multinomial)
tidied$estimate <- exp(tidied$estimate)

models <- list()
models[["Pobre-NoPobre"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Pobre-NoPobre", ])
models[["NoPobre-Pobre"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "NoPobre-Pobre", ])
models[["Pobre-Pobre"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Pobre-Pobre", ])

export_summs(models,
             error_pos = "below",
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1)
)

DescTools::PseudoR2(multinomial)

# Average marginal effects
ame <- avg_slopes(multinomial)



# estimated marginal means
ggemmeans(multinomial, terms = "ocupados_dif[-1:1 by = .5]") %>% 
  plot(connect_lines = TRUE, show_ci = F) 

ggemmeans(multinomial, terms = "horas_dif[0:25 by = 5]") %>% 
  plot(connect_lines = TRUE, show_ci = F) 
