#Random Forest

##Cargo modelos----------

tree_rf1 <- readRDS("modelos entrenados/tree_rf1.rds")
tree_rf2 <- readRDS("modelos entrenados/tree_rf2.rds")
tree_rf3 <- readRDS("modelos entrenados/tree_rf3.rds")
tree_rf4 <- readRDS("modelos entrenados/tree_rf4.rds")

log_reg1 <- readRDS("modelos entrenados/log_reg1.rds")
log_reg2 <- readRDS("modelos entrenados/log_reg2.rds")
log_reg3 <- readRDS("modelos entrenados/log_reg3.rds")
log_reg4 <- readRDS("modelos entrenados/log_reg4.rds")

##Predicción según modelos--------

train1 <- augment(tree_rf1, new_data = train1)
test1 <- augment(tree_rf1, new_data = test1) 

train2 <- augment(tree_rf2, new_data = train2)
test2 <- augment(tree_rf2, new_data = test2)

train3 <- augment(tree_rf3, new_data = train3)
test3 <- augment(tree_rf3, new_data = test3)

train4 <- augment(tree_rf4, new_data = train4)
test4 <- augment(tree_rf4, new_data = test4)


##Predicción según umbrales------------
train1 <- train1 %>% 
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .657, "Pobre", "No pobre"),
         tipo_base = "train1")

test1 <- test1 %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .657, "Pobre", "No pobre"),
         tipo_base = "test1")

train2 <- train2 %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .601, "Pobre", "No pobre"),
         tipo_base = "train2")

test2 <- test2 %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .601, "Pobre", "No pobre"),
         tipo_base = "test2")

train3 <- train3 %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .602, "Pobre", "No pobre"),
         tipo_base = "train3")

test3 <- test3 %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .602, "Pobre", "No pobre"),
         tipo_base = "test3")

train4 <- train4 %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .5738, "Pobre", "No pobre"),
         tipo_base = "train4")

test4 <- test4 %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= .5738, "Pobre", "No pobre"),
         tipo_base = "test4")

bases_predicciones <- train1 %>% 
  bind_rows(test1) %>% 
  bind_rows(train2) %>% 
  bind_rows(test2) %>% 
  bind_rows(train3) %>% 
  bind_rows(test3) %>% 
  bind_rows(train4) %>% 
  bind_rows(test4)

tabla <- bases_predicciones %>%
  group_by(tipo_base, ano_trim, pobreza_dic) %>% 
  tally() %>% 
  group_by(ano_trim) %>%
  mutate(prop.real = n / sum(n)) %>% 
  filter(!is.na(pobreza_dic)) %>% 
  select(-n)

tabla2 <- bases_predicciones %>%
  group_by(tipo_base, ano_trim, pobreza_predicha) %>% 
  tally() %>% 
  group_by(ano_trim) %>%
  mutate(prop.pred = n / sum(n)) %>% 
  rename(pobreza_dic = pobreza_predicha) %>% 
  select(-n)

tabla <- tabla %>% 
  left_join(tabla2, by = c("tipo_base", "ano_trim", "pobreza_dic")) 

tabla <- tabla %>% 
  pivot_longer(cols = c(prop.real, prop.pred), names_to = "tipo", values_to = "prop")

tabla <- tabla %>% 
  filter(pobreza_dic != "No pobre")

tabla %>% 
  ggplot(mapping = aes(x = as.character(ano_trim), y = prop, fill = tipo, colour = tipo,
                       group = tipo)) +
  geom_line(data = filter(tabla, tipo == "prop.real"), size = .6) +
  geom_line(data = filter(tabla, tipo == "prop.pred"), size = .7) +
  geom_point(data = filter(tabla, tipo_base %in% c("train1", "train2", "train3",
                                                     "train4") &
                             tipo == "prop.pred"), size = 1.8, color = "black") +
  geom_point(data = filter(tabla, !(tipo_base %in% c("train1", "train2", "train3",
                                                     "train4")) &
                             tipo == "prop.pred"), size = 1.5) +
  labs(title = "Evolucion de los hogares bajo la línea de pobreza real y predicha",
       subtitle = "Argentina urbana, 2016-2024 (trimestres). Hogares con jefe/a ocupado",
       caption = "Fuente: elaboración propia en base a EPH-INDEC.",
       fill = "Tipo",
       color = "Tipo") +
  theme(plot.title = element_text(size = 13),
        plot.subtitle = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        legend.key.height=unit(1, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        plot.caption = element_text(size = 9, hjust = 1),
        panel.grid = element_line(size = .2),
        strip.text = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.position = "bottom") +
  scale_color_d3(labels=c("Pobreza predicha", "Pobreza real")) +
  scale_fill_d3(labels=c("Pobreza predicha", "Pobreza real")) +
  scale_y_continuous(breaks = seq(.1, .35, by = 0.03), limits = c(.1, .35), 
                     labels = scales::percent_format(accuracy = 1L))

ggsave("salidas/evolucion_pobreza_predicha.png", width = 7, height = 5, dpi = 300)
  