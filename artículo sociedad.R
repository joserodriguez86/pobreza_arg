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
         rama3 = factor(case_when(caes_seccion_cod %in% c("B", "J", "K", "L", "M", "N",
                                                   "U") ~ 1,
                           caes_seccion_cod %in% c("A", "C", "D", "E", "F", "G", "H", "I", "R", "S", "T") ~ 2,
                           caes_seccion_cod %in% c("O", "P", "Q") ~ 3),
                        labels = c("Privado1", "Privado2", "Público")),
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


# Evolución de la pobreza ------------------------------

pobreza <- eph %>%
  group_by(ano_trim) %>% 
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T),
            porcentaje_indigencia = weighted.mean(indigente, PONDIH, na.rm = T)) %>% 
  pivot_longer(cols = c(porcentaje_pobreza, porcentaje_indigencia), names_to = "situacion", values_to = "porcentaje") 

pobreza %>% 
  ggplot(mapping = aes(x = as.character(ano_trim), y = porcentaje, colour = situacion,
                       group = situacion)) +
  geom_line(linewidth = .7) +
  geom_point(data = filter(pobreza, (ano_trim %in% c("2017-III", "2022-I", "2024-IV") & situacion == "porcentaje_pobreza")), size = 2, color = "black") +
  geom_vline(xintercept = c("2018-IV", "2020-II", "2024-I"),
             linetype = "dashed", color = "black") +
  ggrepel::geom_text_repel(data = . %>% filter(ano_trim %in% c("2017-III", 
                                                               "2022-I", "2024-IV")
                                               & situacion == "porcentaje_pobreza"),
                           aes(label = scales::percent(porcentaje, accuracy = 0.1)),
                           size = 3,
                           nudge_y = -0.01,
                           show.legend = FALSE) +
  # labs(title = "Evolucion de la población bajo la línea de pobreza e indigencia",
  #      subtitle = "Argentina urbana, 2016-2024 (trimestres)",
  #      caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        legend.background = element_blank(),
        legend.position = "bottom") +
  scale_color_d3(labels=c("Indigencia", "Pobreza")) +
  scale_y_continuous(breaks = seq(0, .6, by = 0.05), limits = c(0, .6), 
                     labels = scales::percent_format(accuracy = 1L))

ggsave("salidas_articulo/pobreza_evolucion.png", dpi = 300, width = 8, height = 5)

## Por clase
graf <- eph %>%
  filter(CH06 >= 18, ESTADO == 1) %>% 
  filter(ANO4 %in% c(2023, 2024), 
         !is.na(cobhe_f)) %>%
  filter(!ano_trim %in% c("2023-I", "2023-II")) %>% 
  group_by(ano_trim, cobhe_f) %>% 
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) 

graf_prom <- eph %>% 
  filter(CH06 >= 18, ESTADO == 1) %>% 
  filter(ANO4 %in% c(2023, 2024), 
         !is.na(cobhe_f)) %>%
  filter(!ano_trim %in% c("2023-I", "2023-II")) %>%  
  group_by(ano_trim) %>% 
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) %>% 
  mutate(cobhe_f = "Promedio")

graf <- bind_rows(graf, graf_prom)

graf$cobhe_f <- factor(graf$cobhe_f, levels = c('Propietarios y directivos >5',
                                                'Propietarios y directivos <=5',
                                                'Cuenta propia profesionales / calificados',
                                                'Trabajadores no manuales >5',
                                                'Trabajadores manuales >5',
                                                'Trabajadores no manuales <=5',
                                                'Trabajadores manuales <=5',
                                                'Cuenta propia no calificados',
                                                'Promedio'))

graf %>% 
  ggplot(mapping = aes(x = cobhe_f, y = porcentaje_pobreza, fill = cobhe_f == "Promedio")) +
  geom_col(alpha = .7) +
  geom_text(aes(label = scales::percent(porcentaje_pobreza, accuracy = 0.1)), 
            position = position_stack(0.6),
            size = 2.5) +
  scale_fill_manual(values = c("lightblue", "red")) +
  labs(y = "% Pobreza",
       title = "Evolución de la pobreza según clase social",
       subtitle = "Argentina urbana, 2023-2024 (trimestres). Población ocupada mayor a 18 años.",
       caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7),
        plot.caption = element_text(size = 9, hjust = 1),
        plot.subtitle = element_text(size = 10),
        panel.grid = element_line(size = .2),
        legend.position = "none") +
  scale_x_discrete(limits = rev(levels(graf$cobhe_f)),
                   labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 6, 0.1)) + 
  coord_flip() +
  facet_wrap(~ano_trim)


ggsave("salidas_articulo/pobreza_clase.png", dpi = 300, width = 7, height = 5)

graf %>%
  pivot_wider(names_from = ano_trim, values_from = porcentaje_pobreza) %>% 
  mutate(diferencia = abs((`2024-IV` - `2024-I`)/`2024-I`)) %>% 
  select(cobhe_f, diferencia) %>% 
  ggplot(mapping = aes(x = cobhe_f, y = diferencia, fill = cobhe_f == "Promedio")) +
  geom_col(alpha = .7) +
  geom_text(aes(label = scales::percent(diferencia, accuracy = 0.1, suffix = "")), 
            position = position_stack(0.95),
            size = 3.5) +
  geom_hline(yintercept = 0.371, linetype = "dashed") +
  scale_fill_manual(values = c("lightblue", "red")) +
  # labs(title = "Disminución porcentual (1er y 4to trimestre) de la pobreza \npor clase social",
  #      subtitle = "Argentina urbana, 2024. Población ocupada mayor a 18 años.",
  #      caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        legend.position = "none") +
  scale_x_discrete(limits = rev(levels(graf$cobhe_f)),
                   labels = function(x) str_wrap(x, width = 30)) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 6, 0.1)) + 
  coord_flip()

ggsave("salidas_articulo/pobreza_clase_diferencia.png", dpi = 300, width = 8, height = 5)



## Por rama
graf <- eph %>%
  filter(CH06 >= 18, ESTADO == 1) %>% 
  filter(ANO4 %in% c(2023, 2024), 
         !is.na(rama2)) %>%
  filter(!ano_trim %in% c("2023-I", "2023-II")) %>% 
  group_by(ano_trim, rama2) %>% 
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) 

graf_prom <- eph %>% 
  filter(CH06 >= 18, ESTADO == 1) %>% 
  filter(ANO4 %in% c(2023, 2024), 
         !is.na(rama2)) %>%
  filter(!ano_trim %in% c("2023-I", "2023-II")) %>%  
  group_by(ano_trim) %>% 
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) %>% 
  mutate(rama2 = "Promedio")

graf <- bind_rows(graf, graf_prom)

tabla_pivot <- graf %>%
  pivot_wider(names_from = ano_trim, values_from = porcentaje_pobreza) %>% 
  mutate(variacion = (`2024-IV` - `2023-III`) / `2023-III`) 


tabla_pivot %>%
  filter(!is.na(variacion), rama2 %in% c("Promedio", "Comercio al por mayor y al por menor; reparación de vehículo",
                                         "Industria manufacturera", "Construcción", "Administración pública y defensa; planes de seguro social ob",
                                         "Enseñanza", "Salud humana y servicios sociales",
                                         "Actividades de los hogares como empleadores de personal domé",
                                         "Transporte y almacenamiento", "Alojamiento y servicios de comidas", "Actividades profesionales, científicas y técnicas")) %>% 
  ggplot(aes(x = fct_reorder(rama2, variacion), y = variacion)) +
  geom_segment(aes(x = rama2, xend = rama2, y = 0, yend = variacion)) + 
  geom_point() + # Usar columnas calculadas
  labs(
    # title = "Variación del ingreso según rama de actividad",
    #    subtitle = "Argentina urbana, 2023-III / 2024-IV",
    #    caption = "Fuente: Elaboración propia en base a EPH-INDEC"
    ) +
  theme(legend.position = "none",
        title = element_text(size = 12),
        plot.caption = element_text(size = 9, hjust = 1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), breaks = seq(-0.2, 0.2, by = 0.05)) +
  coord_flip()

ggsave("salidas_articulo/pobreza_rama.png", dpi = 300, width = 9, height = 5)


# Capacidad de compra ---------------------------
cc_total <- eph %>% 
  group_by(ANO4) %>% 
  summarise(cc_total = weighted.mean(peso_canasta, PONDIH, na.rm = T))

cc_nopobre <- eph %>% 
  filter(pobreza_dic == 0) %>% 
  group_by(ANO4) %>% 
  summarise(cc_nopobre = weighted.mean(peso_canasta, PONDIH, na.rm = T))

cc_mayores <- eph %>% 
  filter(CH06 >= 65) %>% 
  group_by(ANO4) %>% 
  summarise(cc_mayores = weighted.mean(peso_canasta, PONDIH, na.rm = T))

cc_jubilacion <- eph %>% 
  filter(CH06 >= 65 & CAT_INAC == 1) %>% 
  group_by(ANO4) %>% 
  mutate(peso_canasta_jub = ifelse(ano_trim != "2024-IV", V2_M / (adequi * CBT),
                                   (V2_01_M + V2_02_M) / (adequi * CBT))) %>% 
  summarise(cc_jubilacion = weighted.mean(peso_canasta_jub, PONDII, na.rm = T))



graf <- left_join(cc_total, cc_nopobre, by = "ANO4") %>% 
  left_join(cc_mayores, by = "ANO4") %>% 
  left_join(cc_jubilacion, by = "ANO4") %>%
  pivot_longer(cols = c(cc_total, cc_nopobre, cc_mayores, cc_jubilacion), names_to = "tipo", values_to = "peso_canasta") %>% 
  mutate(tipo = factor(tipo, levels = c("cc_total", "cc_nopobre", "cc_mayores", "cc_jubilacion"),
                      labels = c("ITF población total", "ITF población no pobre", "ITF mayores de 65 años", "Jubilacion mayores de 65 años")))

graf %>% 
  ggplot(aes(x = as.character(ANO4), y = peso_canasta, colour = tipo, group = tipo)) +
  geom_point() +
  geom_line() +
  ggrepel::geom_text_repel(
    aes(label = ifelse(ANO4 %in% c(2016, 2024), 
                       scales::number(peso_canasta, accuracy = 0.01), 
                       NA)),
    size = 3,
    nudge_y = 0.01,
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  labs(
    # title = "Evolución de la capacidad de compra del ingreso total familiar medida en canastas",
    #    subtitle = "Argentina urbana, 2016-2024. ",
    #    caption = "Fuente: elaboración propia en base a EPH-INDEC."
    ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size =10),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(linewidth = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_color_d3(labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous(breaks = seq(1, 3.5, by = 0.5), limits = c(1, 3.5))


ggsave("salidas_articulo/capacidad_compra.png", dpi = 300, width = 8, height = 5)



rama1 <- eph %>% 
  filter(CAT_OCUP == 3, PP07H == 1, rama2 %in% c("Industria manufacturera",
                                                 "Comercio al por mayor y al por menor; reparación de vehículo",
                                                 "Construcción",
                                                 "Alojamiento y servicios de comidas")) %>% 
  mutate(peso_canastap21 = P21 / (adequi * CBT)) %>% 
  group_by(ANO4, rama2) %>% 
  summarise(peso_canasta = weighted.mean(peso_canastap21, PONDIIO, na.rm = T)) %>% 
  ggplot(aes(x = as.character(ANO4), y = peso_canasta, color = rama2, group = rama2)) +
  geom_line(linewidth = .8) +
  geom_point(size = 1.5) +
  ggrepel::geom_text_repel(aes(label = ifelse(ANO4 %in% c(2016, 2024), 
                                              scales::number(peso_canasta, accuracy = 0.01), 
                                              NA)), 
                           size = 3,
                           nudge_y = 0.1,
                           show.legend = FALSE) +
  labs(
    # title = "Evolución de la capacidad de compra del ingreso laboral medida en canastas",
    #    subtitle = "Argentina urbana, 2016-2024. Ramas de actividad asociadas al sector privado",
    #    caption = "Fuente: elaboración propia en base a EPH-INDEC."
    ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_color_d3(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.5), limits = c(1.5, 5),
                     labels = scales::number_format(accuracy = 0.01))



rama2 <- eph %>% 
  filter(CAT_OCUP == 3, PP07H == 1, rama2 %in% c("Enseñanza",
                                                 "Salud humana y servicios sociales",
                                                 "Administración pública y defensa; planes de seguro social ob")) %>% 
  mutate(peso_canastap21 = P21 / (adequi * CBT)) %>% 
  group_by(ANO4, rama2) %>% 
  summarise(peso_canasta = weighted.mean(peso_canastap21, PONDIIO, na.rm = T)) %>% 
  ggplot(aes(x = as.character(ANO4), y = peso_canasta, color = rama2, group = rama2)) +
  geom_line(linewidth = .8) +
  geom_point(size = 1.5) +
  ggrepel::geom_text_repel(aes(label = ifelse(ANO4 %in% c(2016, 2024), 
                                              scales::number(peso_canasta, accuracy = 0.01), 
                                              NA)), 
                           size = 3,
                           nudge_y = 0.1,
                           show.legend = FALSE) +
  labs(
    # title = "Evolución de la capacidad de compra del ingreso laboral medida en canastas",
    #    subtitle = "Argentina urbana, 2016-2024. Ramas de actividad asociadas al sector público",
    #    caption = "Fuente: elaboración propia en base a EPH-INDEC."
    ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_color_d3(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.5), limits = c(1.5, 5),
                     labels = scales::number_format(accuracy = 0.01))


rama1 + rama2


ggsave("salidas_articulo/peso_canasta_asalariados_rama.png", dpi = 300, width = 9, height = 5)



# Estrategias familiares
tabla1 <- eph %>%
  filter(CH03 == 1) %>% 
  group_by(ANO4, V13) %>% 
  tally(PONDERA) %>% 
  group_by(ANO4) %>%
  mutate(ahorro = n / sum(n)) %>% 
  filter(V13 == 1) %>% 
  select(ANO4, ahorro)

tabla2 <- eph %>%
  filter(CH03 == 1) %>% 
  group_by(ANO4, V14) %>% 
  tally(PONDERA) %>% 
  group_by(ANO4) %>%
  mutate(prestamo_f = n / sum(n)) %>% 
  filter(V14 == 1) %>% 
  select(ANO4, prestamo_f)

tabla3 <- eph %>%
  filter(CH03 == 1) %>% 
  group_by(ANO4, V15) %>% 
  tally(PONDERA) %>% 
  group_by(ANO4) %>%
  mutate(prestamo_b = n / sum(n)) %>% 
  filter(V15 == 1) %>% 
  select(ANO4, prestamo_b)

tabla <- tabla1 %>% 
  left_join(tabla2, by = "ANO4") %>% 
  left_join(tabla3, by = "ANO4") %>% 
  pivot_longer(cols = c(ahorro, prestamo_f, prestamo_b), 
               names_to = "estrategia", values_to = "n")

tabla %>% 
  ggplot(aes(x = as.character(ANO4), y = n, color = estrategia, group = estrategia)) +
  geom_line(size = .7) +
  geom_point(size = 1.5) +
  geom_text(data = . %>% filter(ANO4 %in% c("2016", "2024")),
            aes(label = scales::percent(n, accuracy = 0.1)), 
            size = 3,
            nudge_y = .02) +
  labs(
    # title = "Estrategias económicas familiares",
    #    subtitle = "Argentina urbana, 2016-2024 (Anual).",
    #    caption = "Fuente: elaboración propia en base a EPH-INDEC."
    ) +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_color_d3(labels = function(x) str_wrap(c("Ahorros familiares", "Préstamos bancarios / financieros", "Préstamos familiares / amigos"), width = 25)) +
  scale_y_continuous(breaks = seq(0, .40, by = 0.05), limits = c(0, .40), 
                     labels = scales::percent_format(accuracy = 1L))

ggsave("salidas_articulo/estrategias_ahorro.png", dpi = 300, width = 8, height = 5)



# Análisis de panel -----------------------------

## construcción de variable previas
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
         edad_prom = mean(CH06, na.rm = TRUE)
         ) %>% 
  ungroup()


# Paneles
eph2024 <- eph %>% 
  filter(ANO4 == 2024 | ANO4 == 2023) %>% 
  mutate(pobreza_f = factor(pobreza_dic, labels = c("No pobre", "Pobre")))

pool <- organize_panels(eph2024, variables = c("ESTADO", "pobreza_f",
                                               "informal", "sector", 
                                               "rama3",
                                               "cantidad_horas_h", "cobhe_f",
                                               "cantidad_ocupados_h",
                                               "cantidad_empleos_h",
                                               "empleos_cant",
                                               "horas",
                                               "V13", "V14", "V15", 
                                               "ing_lab2024", "sexo", 
                                               "grupo_edad", "ing_nolab2024",
                                               "ing_horario2024",
                                               "CH03", "edad_prom",
                                               "pondih_sin_elevar",
                                               "pondera_sin_elevar"), 
                        window = 'trimestral')

pool <- subset(pool, consistencia == TRUE)
pool <- pool %>% 
  mutate(Periodo = case_when(Periodo == "2023 Q1" ~ "2023 T1",
                             Periodo == "2023 Q2" ~ "2023 T2",
                             Periodo == "2023 Q3" ~ "2023 T3",
                             Periodo == "2023 Q4" ~ "2023 T4",
                             Periodo == "2024 Q1" ~ "2024 T1",
                             Periodo == "2024 Q2" ~ "2024 T2",
                             Periodo == "2024 Q3" ~ "2024 T3"))


pool %>% 
  filter(Periodo == "2024 T2", !is.na(pobreza_f), !is.na(pobreza_f_t1)) %>%
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

periodos <- c("2023 T3", "2023 T4", "2024 T1", "2024 T2", "2024 T3")

graficar_transiciones <- function(periodo_actual) {
  pool %>% 
    filter(Periodo == periodo_actual, !is.na(pobreza_f)) %>%
    group_by(pobreza_f, pobreza_f_t1) %>%
    tally(pondih_sin_elevar_t1) %>% 
    ungroup() %>% 
    mutate(percent = n/sum(n)) %>% 
    ggplot(aes(y = percent, axis1 = pobreza_f, axis2 = pobreza_f_t1)) +
    geom_alluvium(aes(fill = pobreza_f), width = .5) +
    geom_stratum(width = .2, alpha = .1, size = .4) +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.5) +
    scale_fill_d3() +
    labs(title = paste0(periodo_actual)) +
    theme_void(base_size = 10) +
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

grafico_final <- wrap_plots(graficos, ncol = 2) 

ggsave("salidas_articulo/paneles_pobreza.png", dpi = 300, width = 7, height = 5)



## Análisis multinomial

pool_trans <- pool %>%
  filter(Periodo == "2024 T2", ESTADO == 1, CH03 == 1) %>%
  mutate(transicion = case_when(
    pobreza_f == "Pobre"     & pobreza_f_t1 == "Pobre"     ~ "Pobre - Pobre",
    pobreza_f == "Pobre"     & pobreza_f_t1 == "No pobre"  ~ "Pobre - No pobre",
    pobreza_f == "No pobre"  & pobreza_f_t1 == "Pobre"     ~ "No pobre - Pobre",
    pobreza_f == "No pobre"  & pobreza_f_t1 == "No pobre"  ~ "No pobre - No pobre",
    TRUE ~ NA_character_
  )) %>%
  mutate(transicion = factor(transicion,
                             levels = c("Pobre - Pobre", "Pobre - No pobre", "No pobre - Pobre", "No pobre - No pobre")),
         transicion = relevel(transicion, ref = "No pobre - No pobre"),
         cantidad_empleos_h = cantidad_empleos_h_t1 - cantidad_empleos_h,
         cantidad_empleos_j = case_when(empleos_cant == "Un empleo" & empleos_cant_t1 == "Un empleo" ~ "Mantuvo misma cantidad",
                                        empleos_cant == "Más de un empleo" & empleos_cant_t1 == "Más de un empleo" ~ "Mantuvo misma cantidad",
                                        empleos_cant == "Un empleo" & empleos_cant_t1 == "Más de un empleo" ~ "Aumentó la cantidad de empleos",
                                        empleos_cant == "Más de un empleo" & empleos_cant_t1 == "Un empleo" ~ "Disminuyó la cantidad de empleos"),
         cantidad_empleos_j = factor(cantidad_empleos_j, levels = c("Mantuvo misma cantidad", "Aumentó la cantidad de empleos", "Disminuyó la cantidad de empleos")),
         cantidad_ocupados_h = cantidad_ocupados_h_t1 - cantidad_ocupados_h,
         cantidad_horas_h = cantidad_horas_h_t1 - cantidad_horas_h,
         cantidad_horas_j = horas_t1 - horas,
         ahorros = case_when(V13 == 1 | V13_t1 == 1 ~ "Uso ahorros",
                            TRUE ~ "No uso ahorros"),
         ahorros = factor(ahorros, levels = c("No uso ahorros", "Uso ahorros")),
         prestamo_f = case_when(V14 == 1 | V14_t1 == 1 ~ "Uso préstamo familiar",
                            TRUE ~ "No uso préstamo familiar"),
         prestamo_f = factor(prestamo_f, levels = c("No uso préstamo familiar", "Uso préstamo familiar")),
         prestamo_b = case_when(V15 == 1 | V15_t1 == 1 ~ "Uso préstamo bancario",
                            TRUE ~ "No uso préstamo bancario"),
         prestamo_b = factor(prestamo_b, levels = c("No uso préstamo bancario", "Uso préstamo bancario")),
         cobhe_f = relevel(cobhe_f, ref = "Cuenta propia no calificados"),
         informal = relevel(informal, ref = "Informal"),
         grupo_edad = relevel(grupo_edad, ref = "65 años o más"),
         ingresos_lab = ing_lab2024_t1 - ing_lab2024,
         ingresos_nolab = ing_nolab2024_t1 - ing_nolab2024,
         ingresos_horarios = ing_horario2024_t1 - ing_horario2024)

multinomial <- multinom(transicion ~ sexo + edad_prom + rama3 + cantidad_ocupados_h +
                          cantidad_horas_h + cantidad_empleos_j + informal + ahorros + prestamo_b + prestamo_f,
                        data = pool_trans, weights = pondih_sin_elevar_t1, trace = F,
                        model = TRUE)

tidied <- tidy(multinomial)
tidied$estimate <- exp(tidied$estimate)

models <- list()
models[["Pobre - No pobre"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Pobre - No pobre", ])
models[["No pobre - Pobre"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "No pobre - Pobre", ])
models[["Pobre - Pobre"]] <- tidy_replace(multinomial, tidied[tidied$y.level == "Pobre - Pobre", ])

export_summs(models,
             error_pos = "right",
             model.names = c("Pobre -> No pobre", "No pobre -> Pobre", "Pobre -> Pobre"),
             stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
             scale = T,
             robust = T,
             coefs = c("Mujer (jh)" = "sexoMujer",
                       "Edad promedio del hogar" = "edad_prom",
                       "Privado tradicional (ref = privado dinámico" = "rama3Privado2",
                       "Público" = "rama3Público",
                       "Diferencia cantidad de ocupados hogar" = "cantidad_ocupados_h",
                       "Diferencia cantidad de horas trabajadas hogar" = "cantidad_horas_h",
                       "Aumento número de trabajos (ref = mantuvo)" = "cantidad_empleos_jAumentó la cantidad de empleos",
                       "Disminución número de trabajos " = "cantidad_empleos_jDisminuyó la cantidad de empleos",
                       "Formal (jh)" = "informalFormal",
                       "Uso ahorros" = "ahorrosUso ahorros",
                       "Uso préstamo bancario" = "prestamo_bUso préstamo bancario",
                       "Uso préstamo familiar" = "prestamo_fUso préstamo familiar"),
             to.file = "docx", file.name = "salidas_articulo/reg_multi.docx")


DescTools::PseudoR2(multinomial, which = "all")

# Average marginal effects
ame <- avg_slopes(multinomial)

# ame %>% 
#   filter(term == "rama3") %>% 
#   ggplot(aes(x = contrast, y = estimate)) +
#   geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
#   geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.3) +
#   facet_wrap(~ group, ncol=4)

g1 <- ame %>% 
  filter(term == "cantidad_ocupados_h") %>% 
  ggplot(aes(x=group, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.5) +
  labs(title = "Diferencia total ocupados en el hogar") +
  theme(
    plot.title = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  scale_y_continuous(breaks = seq(-.1, .1, by = .02), limits = c(-.1, .1)) +
  scale_x_discrete(labels = function(x) str_wrap(c("No pobre - No pobre", 
                                                   "Pobre - Pobre",
                                                   "Pobre - No pobre",
                                                   "No pobre - Pobre"), width = 15))


g2 <- ame %>% 
  filter(term == "cantidad_horas_h") %>% 
  ggplot(aes(x=group, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.5) +
  labs(title = "Diferencia horas trabajadas en el hogar") +
  theme(
    
    plot.title = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10)
  ) +
  scale_y_continuous(breaks = seq(-.004, .003, by = .001), limits = c(-.004, .003)) +
  scale_x_discrete(labels = function(x) str_wrap(c("No pobre - No pobre", 
                                                   "Pobre - Pobre",
                                                   "Pobre - No pobre",
                                                   "No pobre - Pobre"), width = 15))

g1 + g2

ggsave("salidas_articulo/ame_ocupados_horas.png", dpi = 300, width = 8, height = 4)

ame %>% 
  filter(term == "cantidad_empleos_j") %>% 
  ggplot(aes(x = contrast, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10),
    strip.text = element_text(size = 11)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(c("Aumentó la cantidad de empleos", "Disminuyó la cantidad de empleos"), width = 20)) +
  scale_y_continuous(breaks = seq(-.2, .2, by = .05), limits = c(-.2, .2)) +
  facet_wrap(~ group)

ggsave("salidas_articulo/ame_empleos.png", dpi = 300, width = 7, height = 5)


ame %>% 
  filter(term %in% c("ahorros", "prestamo_b", "prestamo_f")) %>% 
  ggplot(aes(x = str_wrap(group, width = 10), y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linewidth = .4, linetype = "dashed") +
  geom_pointrange(position = position_jitter(width = 0.3), size = .5) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10),
  ) +
  scale_y_continuous(limits = c(-.25, .2), breaks = seq(-.25, .2, by = .05)) +
  facet_wrap(
    ~ term,
    labeller = as_labeller(c(
      "ahorros" = "Uso de ahorros propios",
      "prestamo_b" = "Pidió préstamo a banco / financiera",
      "prestamo_f" = "Pidió préstamo a familiares o amigos"
    ))
  )

ggsave("salidas_articulo/ame_prestamos.png", dpi = 300, width = 8, height = 4)



