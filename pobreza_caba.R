# Librerías y descarga de bases-----------------
rm(list = ls())

pacman::p_load(tidyverse, eph)

etoi221 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2022/08/etoi221_base_usuarios.zip",etoi221)
etoi221i <- read.csv(unz(etoi221, "etoi221_usu_ind.txt"), sep = ";")
etoi221h <- read.csv(unz(etoi221, "etoi221_usu_hog.txt"), sep = ";")

etoi222 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2022/11/etoi222_base_usuarios.zip",etoi222)
etoi222i <- read.csv(unz(etoi222, "etoi222_usu_ind.txt"), sep = ";")
etoi222h <- read.csv(unz(etoi222, "etoi222_usu_hog.txt"), sep = ";")

etoi223 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2023/01/etoi223_-base_usuarios.zip",etoi223)
etoi223i <- read.csv(unz(etoi223, "etoi223_ base_usuarios/etoi223_usu_ind.txt"), sep = ";")
etoi223h <- read.csv(unz(etoi223, "etoi223_ base_usuarios/etoi223_usu_hog.txt"), sep = ";")

etoi224 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2023/04/etoi224_base_usuarios.zip",etoi224)
etoi224i <- read.csv(unz(etoi224, "etoi224_usu_ind.txt"), sep = ";")
etoi224h <- read.csv(unz(etoi224, "etoi224_usu_hog.txt"), sep = ";")

etoi231 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2023/08/etoi231_base_usuarios.zip",etoi231)
etoi231i <- read.csv(unz(etoi231, "etoi231_usu_ind.txt"), sep = ";")
etoi231h <- read.csv(unz(etoi231, "etoi231_usu_hog.txt"), sep = ";")

etoi232 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2023/10/etoi232_base_usuarios.zip",etoi232)
etoi232i <- read.csv(unz(etoi232, "etoi232_usu_ind.txt"), sep = ";")
etoi232h <- read.csv(unz(etoi232, "etoi232_usu_hog.txt"), sep = ";")

etoi233 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2024/01/etoi233_base_usuarios.zip",etoi233)
etoi233i <- read.csv(unz(etoi233, "etoi233_usu_ind.txt"), sep = ";")
etoi233h <- read.csv(unz(etoi233, "etoi233_usu_hog.txt"), sep = ";")

etoi234 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2024/05/etoi234_base_usuarios.zip",etoi234)
etoi234i <- read.csv(unz(etoi234, "etoi234_usu_ind.txt"), sep = ";")
etoi234h <- read.csv(unz(etoi234, "etoi234_usu_hog.txt"), sep = ";")

etoi241 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2024/07/etoi241_base_usuarios.zip",etoi241)
etoi241i <- read.csv(unz(etoi241, "etoi241_usu_ind.txt"), sep = ";")
etoi241h <- read.csv(unz(etoi241, "etoi241_usu_hog.txt"), sep = ";")

etoi242 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2024/10/etoi242_base_usuarios.zip",etoi242)
etoi242i <- read.csv(unz(etoi242, "etoi242_usu_ind.txt"), sep = ";")
etoi242h <- read.csv(unz(etoi242, "etoi242_usu_hog.txt"), sep = ";")

etoi243 <- tempfile()
download.file("https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2025/01/etoi243_base_usuarios.zip",etoi243)
etoi243i <- read.csv(unz(etoi243, "etoi243_usu_ind.txt"), sep = ";")
etoi243h <- read.csv(unz(etoi243, "etoi243_usu_hog.txt"), sep = ";")


canastas <- readxl::read_xlsx("fuentes/canastas_serie_lalo.xlsx")


## Unión de bases-----------------
etoi221 <- etoi221i %>% 
  left_join(etoi221h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2022-1",
         ANO4 = 2022,
         TRIMESTRE = 1)

etoi222 <- etoi222i %>%
  left_join(etoi222h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2022-2",
         ANO4 = 2022,
         TRIMESTRE = 2)

etoi223 <- etoi223i %>%
  left_join(etoi223h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2022-3",
         ANO4 = 2022,
         TRIMESTRE = 3)

etoi224 <- etoi224i %>%
  left_join(etoi224h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2022-4",
         ANO4 = 2022,
         TRIMESTRE = 4)

etoi231 <- etoi231i %>%
  left_join(etoi231h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2023-1",
         ANO4 = 2023,
         TRIMESTRE = 1)

etoi232 <- etoi232i %>%
  left_join(etoi232h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2023-2",
         ANO4 = 2023,
         TRIMESTRE = 2)

etoi233 <- etoi233i %>%
  left_join(etoi233h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2023-3",
         ANO4 = 2023,
         TRIMESTRE = 3)

etoi234 <- etoi234i %>%
  left_join(etoi234h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2023-4",
         ANO4 = 2023,
         TRIMESTRE = 4)

etoi241 <- etoi241i %>%
  left_join(etoi241h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2024-1",
         ANO4 = 2024,
         TRIMESTRE = 1)

etoi242 <- etoi242i %>%
  left_join(etoi242h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2024-2",
         ANO4 = 2024,
         TRIMESTRE = 2)

etoi243 <- etoi243i %>%
  left_join(etoi243h, by = c("id", "nhogar", "zona")) %>% 
  mutate(anio_trim = "2024-3",
         ANO4 = 2024,
         TRIMESTRE = 3)

etoi <- bind_rows(etoi221, etoi222, etoi223, etoi224, etoi231, etoi232, etoi233, etoi234, etoi241, etoi242, etoi243)

rm(etoi221, etoi222, etoi223, etoi224, etoi231, etoi232, etoi233, etoi234, etoi241, etoi242, etoi243, etoi221i, etoi221h, etoi222i, etoi222h, etoi223i, etoi223h, etoi224i, etoi224h, etoi231i, etoi231h, etoi232i, etoi232h, etoi233i, etoi233h, etoi234i, etoi234h, etoi241i, etoi241h, etoi242i, etoi242h, etoi243i, etoi243h)

etoi <- etoi %>% 
  mutate(CH04 = sexo,
         CH06 = edad,
         REGION = 1,
         CODUSU = id,
         NRO_HOGAR = nhogar,
         ITF = itfb_2,
         PONDIH = fexp.x)

# Construcción de variables-----------------

etoi <- etoi %>% 
  mutate(pobreza_dic = ifelse(i_pobreza == 1 | i_pobreza == 2, 1, 0),
         indigente = ifelse(i_pobreza == 1, 1, 0))

etoi <- calculate_poverty(base = etoi, basket = canastas)

etoi <- etoi %>% 
  mutate(pobreza_indec = case_when(situacion %in% c("pobre", "indigente") ~ 1,
                                 situacion %in% "no_pobre" ~ 0),
         indigente_indec = case_when(situacion == "indigente" ~ 1,
                               TRUE ~ 0))


# Procesamientos-------------------
etoi %>%
  group_by(anio_trim) %>% 
  summarise(pobreza_IDECBA = weighted.mean(pobreza_dic, fexp.x, na.rm = T),
            indigencia_IDECBA = weighted.mean(indigente, fexp.x, na.rm = T),
            pobreza_INDEC = weighted.mean(pobreza_indec, fexp.x, na.rm = T),
            indigencia_INDEC = weighted.mean(indigente_indec, fexp.x, na.rm = T)) %>% 
  pivot_longer(cols = c(pobreza_IDECBA, indigencia_IDECBA, pobreza_INDEC, indigencia_INDEC), 
               names_to = "situacion", values_to = "porcentaje") %>% 
  ggplot(mapping = aes(x = as.character(anio_trim), y = porcentaje, colour = situacion,
                       linetype = situacion, group = situacion)) +  
  geom_point(size = 2) +
  geom_line(linewidth = .7) +
  labs(title = "Pobreza e indigencia en CABA",
       subtitle = "Comparación según cálculo de LI y LP de IDECBA e INDEC",
       caption = "Fuente: elaboración propia en base a ETOI - IDECBA",
       y = "Porcentaje",
       x = "Año-trimestre") +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_y_continuous(breaks = seq(0, .6, by = 0.05), limits = c(0, .6), 
                     labels = scales::percent_format(accuracy = 1L)) +
  scale_color_manual(values = c("pobreza_IDECBA" = "blue", 
                                "indigencia_IDECBA" = "red", 
                                "pobreza_INDEC" = "blue", 
                                "indigencia_INDEC" = "red")) +
  scale_linetype_manual(values = c("pobreza_IDECBA" = "solid", 
                                   "indigencia_IDECBA" = "solid", 
                                   "pobreza_INDEC" = "dashed", 
                                   "indigencia_INDEC" = "dashed"))

ggsave("salidas/pobreza_caba.png", width = 10, height = 6, dpi = 300)



pobreza_comparada <- etoi %>%
  group_by(anio_trim) %>% 
  summarise(pobreza_IDECBA = weighted.mean(pobreza_dic, fexp.x, na.rm = T),
            indigencia_IDECBA = weighted.mean(indigente, fexp.x, na.rm = T),
            pobreza_INDEC = weighted.mean(pobreza_indec, fexp.x, na.rm = T),
            indigencia_INDEC = weighted.mean(indigente_indec, fexp.x, na.rm = T)) %>% 
  mutate(brecha_pobreza = (pobreza_IDECBA - pobreza_INDEC)/ pobreza_IDECBA,
         brecha_indigencia = (indigencia_IDECBA - indigencia_INDEC)/ indigencia_IDECBA) %>% 
  pivot_longer(cols = c(brecha_pobreza, brecha_indigencia), 
               names_to = "situacion", values_to = "brecha")

pobreza_comparada %>%
  ggplot(mapping = aes(x = as.character(anio_trim), y = brecha, colour = situacion,
                       linetype = situacion, group = situacion)) +  
  geom_point(size = 2) +
  geom_line(linewidth = .7) +
  labs(title = "Brecha de cálculo de pobreza e indigencia en CABA",
       subtitle = "Comparación entre LI y LP de IDECBA e INDEC",
       caption = "Fuente: elaboración propia en base a ETOI - IDECBA",
       y = "Brecha",
       x = "Año-trimestre") +
  theme(plot.title = element_text(size = 11),
        plot.subtitle = element_text(size = 9),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank()) +
  scale_y_continuous(breaks = seq(0, .7, by = 0.1), limits = c(0, .7), 
                     labels = scales::percent_format(accuracy = 1L)) +
  scale_color_manual(values = c("brecha_pobreza" = "blue", 
                                "brecha_indigencia" = "red")) +
  scale_linetype_manual(values = c("brecha_pobreza" = "solid", 
                                   "brecha_indigencia" = "dashed"))

ggsave("salidas/brecha_pobreza_caba.png", width = 10, height = 6, dpi = 300)
