# Librerías y bases-------------------------

rm(list = ls())
pacman::p_load(tidyverse, eph, ggsci, flextable, ggrepel, GDAtools, plotly, patchwork)

eph <- get_microdata(2016:2024, type = "individual", period = c(1:4))

canastas <- readxl::read_xlsx("fuentes/canastas_serie.xlsx")

ipc <- readxl::read_xlsx("fuentes/ipc_trimestral_2024.xlsx")

theme_set(theme_bw())


# Construcción de variables------------------
eph <- eph %>% 
  mutate(semestre = case_when(TRIMESTRE <= 2 ~ 1,
                              TRIMESTRE > 2 ~ 2),
         trim = case_when(TRIMESTRE == 1 ~ "I",
                          TRIMESTRE == 2 ~ "II",
                          TRIMESTRE == 3 ~ "III",
                          TRIMESTRE == 4 ~ "IV"),
         ano2 = str_sub(as.character(ANO4), 3, 4))

eph$ano_trim <- paste(as.character(eph$ANO4), as.character(eph$trim), sep = "-")
eph$ano_sem <- paste(as.character(eph$ANO4), as.character(eph$semestre), sep = "-")
ipc$ano_trim <- paste(as.character(ipc$Año), as.character(ipc$Trimestre), sep = "-")


ipc <- ipc %>% 
  select(ano_trim, ipc2024)

eph <- eph %>% 
  left_join(ipc, by = "ano_trim")


eph <- calculate_poverty(base = eph, basket = canastas, print_summary = T,
                         window = "quarter")



eph <- eph %>% 
  mutate(pobreza_dic = case_when(situacion %in% c("pobre", "indigente") ~ 1,
                                 situacion %in% "no_pobre" ~ 0),
         pobreza3 = case_when(situacion == "indigente" ~ 1,
                              situacion == "pobre" ~ 2,
                              situacion == "no_pobre" ~ 3),
         indigente = case_when(situacion == "indigente" ~ 1,
                               TRUE ~ 0))

#laborales
eph <- organize_caes(base = eph)

eph$cno <- ifelse(is.na(eph$PP04D_COD), eph$PP11D_COD, eph$PP04D_COD)

eph$cno12 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 1, 2), str_sub(eph$cno, 1, 1))
eph$cno3 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 3, 3), str_sub(eph$cno, 2, 2))
eph$cno4 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 4, 4), str_sub(eph$cno, 3, 3))
eph$cno5 <- ifelse(nchar(eph$cno) > 4, str_sub(eph$cno, 5, 5), str_sub(eph$cno, 4, 4))

eph$cno12 <- as.numeric(eph$cno12)
eph$cno3 <- as.numeric(eph$cno3)
eph$cno4 <- as.numeric(eph$cno4)
eph$cno5 <- as.numeric(eph$cno5)


eph <- eph %>% 
  mutate(tamano = case_when((PP04C > 0 & PP04C <= 5) | (PP04C == 99 & PP04C99 == 1) ~ 1,
                            (PP04C > 5 & PP04C < 99) | (PP04C == 99 & (PP04C99 == 2 | PP04C99 == 3)) ~ 2,
                            ((PP04C == 0 & PP04C99 == 0) | (PP04C == 99 & PP04C99 == 9)) & PP04A == 1 ~ 2,
                            PP04A == 1 ~ 2,
                            TRUE ~ NA_real_)) 

eph <- eph %>% 
  mutate(tamano_desocup = case_when((PP11C > 0 & PP11C <= 5) | (PP11C == 99 & PP11C99 == 1) ~ 1,
                                    (PP11C > 5 & PP11C < 99) | (PP11C == 99 & (PP11C99 == 2 | PP11C99 == 3)) ~ 2,
                                    ((PP11C == 0 & PP11C99 == 0) | (PP11C == 99 & PP11C99 == 9)) & PP11A == 1 ~ 2,
                                    PP11A == 1 ~ 2,
                                    TRUE ~ NA_real_)) 

eph$tamano <- ifelse(is.na(eph$tamano), eph$tamano_desocup, eph$tamano)

eph <- eph %>% 
  mutate(empleos_cant = factor(case_when(PP03C == 1 ~ "Un empleo",
                                         PP03C == 2 ~ "Más de un empleo",
                                         TRUE ~ NA_character_)),
         intensi = factor(case_when(INTENSI == 1 ~ "Subocupados",
                                    INTENSI == 2 ~ "Ocupados plenos",
                                    INTENSI == 3 ~ "Sobreocupados",
                                    TRUE ~ NA_character_)),
         rama = substr(caes_eph_label, start = 1, stop = 28),
         rama = factor(rama),
         informalidad = factor(case_when(ESTADO == 1 & CAT_OCUP == 3 & 
                                          (PP07H == 1 | PP07I == 1) ~ "Formal",
                                        ESTADO == 1 & CAT_OCUP == 3 & 
                                          PP07H != 1 & PP07I != 1 ~ "Informal",
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
                                        TRUE ~ NA_character_)))

#ingresos
eph <- eph %>% 
  mutate(decil_ipcf = as.numeric(DECCFR),
         pobreza3 = case_when(situacion == "indigente" ~ "Indigente",
                              situacion == "pobre" ~ "Pobre",
                              situacion == "no_pobre" ~ "No pobre"),
         pobreza3 = factor(pobreza3, levels = c("Indigente", "Pobre", "No pobre")))

eph <- eph %>% 
  group_by(ano_trim, CODUSU, NRO_HOGAR) %>%
  mutate(miembros = n()) %>% 
  ungroup() %>% 
  mutate(ipcf2 = ITF / miembros,
         ipcf2024 = (ipcf2 / ipc2024)*100)


#nivel educativo
eph <- eph %>% 
  mutate(nivel_educativo = case_when(NIVEL_ED <= 3 | NIVEL_ED == 7 ~ "Hasta primario completo",
                                     NIVEL_ED >= 4 & NIVEL_ED <= 5 ~ "Hasta secundario completo",
                                     NIVEL_ED == 6 ~ "Hasta terciario/universitario completo",
                                     TRUE ~ NA_character_),
         nivel_educativo = factor(nivel_educativo))


#Distancias a la pobreza
eph <- eph %>% 
  mutate(distancia_pobreza = (ITF - CBT_hogar)/CBT_hogar) %>% 
  mutate(distancia_pobreza2 = factor(case_when(
    distancia_pobreza <= -.9 & distancia_pobreza > -1 ~ "-100%",
    distancia_pobreza <= -.8 & distancia_pobreza > -.9 ~ "-90%",
    distancia_pobreza <= -.7 & distancia_pobreza > -.8 ~ "-80%",
    distancia_pobreza <= -.6 & distancia_pobreza > -.7 ~ "-70%",
    distancia_pobreza <= -.5 & distancia_pobreza > -.6 ~ "-60%",
    distancia_pobreza <= -.4 & distancia_pobreza > -.5 ~ "-50%",
    distancia_pobreza <= -.3 & distancia_pobreza > -.4 ~ "-40%",
    distancia_pobreza <= -.2 & distancia_pobreza > -.3 ~ "-30%",
    distancia_pobreza <= -.1 & distancia_pobreza > -.2 ~ "-20%",
    distancia_pobreza <= 0 & distancia_pobreza > -.1 ~ "-10%",
    distancia_pobreza > 0 ~ "No pobre",
    TRUE ~ NA_character_), levels = c("-100%", "-90%", "-80%", "-70%", "-60%", "-50%", "-40%", "-30%", "-20%", "-10%", "No pobre"))) %>% 
  mutate(distancia_pobreza3 = factor(case_when(
    distancia_pobreza <= -.5 & distancia_pobreza > -1 ~ "-100%/-50%",
    distancia_pobreza <= -.2 & distancia_pobreza > -.5 ~ "-49%/-20%",
    distancia_pobreza < 0 & distancia_pobreza > -.2 ~ "-19%/0%",
    distancia_pobreza >= 0 & distancia_pobreza <=.2 ~ "1%/20%",
    distancia_pobreza > .2 & distancia_pobreza <=.5 ~ "11%/50%",
    distancia_pobreza > .5 & distancia_pobreza <=1 ~ "51%/100%",
    distancia_pobreza > 1 ~ "Más de 100%",
    TRUE ~ NA_character_), levels = c("-100%/-50%", "-49%/-20%", "-19%/0%", "1%/20%", "11/50%", "51%/100%", "Más de 100%")))


#Clase social-------------------

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


##Clasificador

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



#Resultados-----------------------

##Tendencia pobreza--------------------
pobreza <- eph %>%
  group_by(ano_trim) %>% 
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T),
            porcentaje_indigencia = weighted.mean(indigente, PONDIH, na.rm = T)) %>% 
  pivot_longer(cols = c(porcentaje_pobreza, porcentaje_indigencia), names_to = "situacion", values_to = "porcentaje")

pobreza %>% 
  ggplot(mapping = aes(x = as.character(ano_trim), y = porcentaje, colour = situacion,
                       group = situacion)) +
  geom_point(size = 2) +
  geom_line(data = filter(pobreza, ano_trim != "2024-III"), linewidth = 0.7) +
  geom_line(data = filter(pobreza, ano_trim %in% c("2024-II", "2024-III")), linewidth = 0.7, linetype = "dotted") +
  labs(title = "Evolucion de la población bajo la línea de pobreza e indigencia",
       subtitle = "Argentina urbana, 2016-2024 (trimestres)",
       caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_color_d3(labels=c("Indigencia", "Pobreza")) +
  scale_y_continuous(breaks = seq(0, .6, by = 0.05), limits = c(0, .6), 
                     labels = scales::percent_format(accuracy = 1L))

ggsave("salidas/pobreza_evolucion_trimestres.png", dpi = 300, width = 8, height = 5)


##Gini-------------
eph %>%
  group_by(ano_trim) %>% 
  summarise(gini = dineq::gini.wtd(ipcf2, PONDIH)) %>% 
  ggplot(aes(x = as.character(ano_trim), y = gini, group = 1)) +
  geom_point(size = 2) +
  geom_line(linewidth = .5) +
  geom_text_repel(data = . %>% filter(ano_trim == "2024-III" | ano_trim == "2016-II" |
                                        ano_trim == "2024-I" | ano_trim == "2020-II"), 
                  aes(label = round(gini, digits = 3)),
                  show.legend = FALSE,
                  size = 4,
                  nudge_y = -.002,
                  nudge_x = 1.5,
                  box.padding = .5) +
  labs(title = "Evolucion del coeficiente de gini según ingresos per cápita familiar",
       subtitle = "Argentina urbana, 2016-2024 (trimestres)",
       caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        strip.text = element_text(face = "bold")) +
  scale_y_continuous(limits = c(0.4, .5), breaks = seq(0.4, .5, by = 0.01))

ggsave("salidas/gini_evolucion.png", dpi = 300, width = 7, height = 5)


##Desocupación-------------
eph %>%
  group_by(ano_trim, ESTADO) %>%
  tally(PONDERA) %>% 
  group_by(ano_trim) %>%
  summarise(desocupacion = n[ESTADO == 2] / (n[ESTADO == 1] + n[ESTADO == 2])) %>%
  ggplot(aes(x = as.character(ano_trim), y = desocupacion, group = 1)) +
  geom_point(size = 2) +
  geom_line(linewidth = .7) +
  geom_text_repel(data = . %>% filter(ano_trim == "2024-III" | ano_trim == "2016-II" |
                                        ano_trim == "2020-II" | ano_trim == "2023-IV"), 
                  aes(label = scales::percent(desocupacion, accuracy = 0.1)),
                  show.legend = FALSE,
                  size = 4,
                  nudge_y = .015) +
  labs(title = "Evolucion de la tasa de desocupación",
       subtitle = "Argentina urbana, 2016-2024 (trimestres)",
       caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        strip.text = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(0, .15),
                     breaks = seq(0, .15, .01))

ggsave("salidas/desocupacion_evolucion.png", dpi = 300, width = 7, height = 5)


##Distancias a la pobreza------------
eph %>% 
  filter(!is.na(distancia_pobreza3), ano_sem != "2016-1") %>% 
  group_by(ano_sem, distancia_pobreza3) %>%
  tally(PONDIH) %>%
  group_by(ano_sem) %>%
  mutate(n = n / sum(n)) %>%
  ggplot(aes(x=ano_sem, y=n, fill=fct_rev(distancia_pobreza3))) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label = scales::percent(n, accuracy = 0.1, suffix = "")),
            color = "white",
            position = position_fill(vjust = 0.5), size = 3.5) +
  labs(
    title = "Distancias del ITF a la canasta básica de pobreza",
    subtitle = "Argentina urbana, 2016-2024 (semestres)",
    caption = "Fuente: elaboración propia en base a EPH-INDEC."
  ) +
  scale_fill_viridis_d(direction = -1, option = "inferno",
                       end = .87) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  labs(fill = "Distancia a la CBT") +
  theme(legend.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) 

ggsave("salidas/distancias_pobreza.png", dpi = 300, width = 8, height = 5)


##Informalidad----------
tabla <- eph %>%
  filter(ESTADO == 1, CH06 >= 18) %>%
  group_by(ano_trim, informalidad) %>%
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) 

tabla_prom <- eph %>%
  filter(ESTADO == 1, CH06 >= 18) %>%
  group_by(ano_trim) %>%
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) 

tabla %>% 
  na.omit() %>%
  ggplot(aes(x = as.character(ano_trim), y = porcentaje_pobreza)) +
  geom_line(aes(group=informalidad, color=informalidad), linetype = 1, linewidth = .8, alpha = .6) +
  geom_point(aes(group=informalidad, color=informalidad), size = 1.8, alpha = .6) +
  geom_line(data = tabla_prom, linetype = "dotted", linewidth = .9, color = "black", group = 1) +
  theme_minimal() +
  labs(title = "Evolucion de la población ocupada bajo la línea de pobreza según informalidad laboral",  
       subtitle = "Argentina urbana, 2016-2024 (semestres). Población mayor a 18 años. \nPromedio en línea punteada.",
       caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_color_d3() +
  scale_y_continuous(breaks = seq(0, .75, by = 0.05), limits = c(0, .75), 
                     labels = scales::percent_format(accuracy = 1L))

ggsave("salidas/pobreza_informalidad.png", dpi = 300, width = 8, height = 5, bg = "white")


##Clase social----------------
graf <- eph %>%
  filter(CH06 >= 18, ESTADO == 1) %>% 
  filter(semestre == 1) %>% 
  filter(ANO4 %in% c(2017, 2019, 2021, 2023, 2024), 
         !is.na(cobhe_f)) %>%
  group_by(ANO4, cobhe_f) %>% 
  summarise(porcentaje_pobreza = weighted.mean(pobreza_dic, PONDIH, na.rm = T)) 

graf_prom <- eph %>% 
  filter(CH06 >= 18, ESTADO == 1) %>% 
  filter(semestre == 1) %>% 
  filter(ANO4 %in% c(2017, 2019, 2021, 2023, 2024), 
         !is.na(cobhe_f)) %>%
  group_by(ANO4) %>% 
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
  geom_col() +
  geom_text(aes(label = scales::percent(porcentaje_pobreza, accuracy = 0.1)), 
            position = position_stack(0.6),
            size = 3) +
  scale_fill_manual(values = c("grey", "red")) +
  labs(y = "% Pobreza",
       title = "Evolución de la pobreza según clase social",
       subtitle = "Argentina urbana, 2016-2024 (semestres). Población ocupada mayor a 18 años.",
       caption = "Fuente: elaboración propia en base a EPH-INDEC.") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8.5),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        legend.position = "none") +
  scale_x_discrete(limits = rev(levels(graf$cobhe_f)),
                   labels = function(x) str_wrap(x, width = 35)) +
  scale_y_continuous(labels= scales::percent_format(accuracy = 1L), breaks=seq(0, 6, 0.1)) + 
  coord_flip() +
  facet_wrap(~ANO4)


ggsave("salidas/pobreza_clase.png", dpi = 300, width = 8, height = 5)

tabla <- graf %>%
  pivot_wider(names_from = ANO4, values_from = porcentaje_pobreza) %>% 
  mutate(diferencia = (`2024` - `2017`)/`2017`)
